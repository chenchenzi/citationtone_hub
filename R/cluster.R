# =============================================================================
# Unsupervised f0-contour clustering: estimate how many tone categories a
# (possibly under-documented) language has, and group tokens into candidate
# tones from contour shape alone -- no tone labels required.
#
# Pipeline:
#   cluster_features()  long-format f0 -> one feature vector per token
#   choose_k_f0()       feature matrix -> diagnostics (elbow / silhouette /
#                       gap / GMM-BIC) across a range of k, with suggestions
#   cluster_f0()        feature matrix + k -> cluster assignment + mean contours
#   cluster_agreement() clusters vs known labels -> table + adjusted Rand index
#
# This is a hypothesis generator (cf. Kaland 2023, JIPA), not a verdict:
# clusters can reflect speaker, vowel or recording effects as well as tone, so
# results need confirmatory linguistic analysis. Normalise f0 first.
# =============================================================================


# -----------------------------------------------------------------------------
# Internal: resample one token's contour to `n` evenly spaced points over its
# own time range. Returns a length-`n` numeric (NA if too few valid samples).
# -----------------------------------------------------------------------------
.interp_contour <- function(t, y, n) {
  ok <- is.finite(t) & is.finite(y)
  t <- t[ok]; y <- y[ok]
  if (length(t) < 2 || length(unique(t)) < 2) return(rep(NA_real_, n))
  o <- order(t)
  xout <- seq(min(t), max(t), length.out = n)
  stats::approx(t[o], y[o], xout = xout, ties = mean)$y
}

# -----------------------------------------------------------------------------
# Internal: DCT-II basis matrix (n x n). Row k is the k-th cosine component.
# -----------------------------------------------------------------------------
.dct_basis <- function(n) {
  k <- 0:(n - 1)
  outer(k, k, function(kk, nn) cos(pi / n * (nn + 0.5) * kk))
}

# -----------------------------------------------------------------------------
# Internal: mclust's Mclust() calls mclustBIC() unqualified, so mclust must be
# *attached*, not merely namespace-loaded. Attach it on demand. Returns TRUE if
# mclust is usable.
# -----------------------------------------------------------------------------
.ensure_mclust <- function() {
  if (!requireNamespace("mclust", quietly = TRUE)) return(FALSE)
  if (!"package:mclust" %in% search())
    try(suppressMessages(attachNamespace("mclust")), silent = TRUE)
  "package:mclust" %in% search()
}

# Spherical / diagonal Gaussian mixture models only (mclust model names). These
# have O(d) covariance parameters, so they stay fast and well-conditioned even
# for high-dimensional feature spaces; full O(d^2) models are skipped.
.GMM_MODELS <- c("EII", "VII", "EEI", "VVI")

# -----------------------------------------------------------------------------
# Internal: Minimum Description Length / information cost of a clustering, after
# Kaland & Ellison (2023). Total bits to transmit the data =
#   model cost     : encode the cluster-mean vectors under one global normal
# + partition cost : encode the cluster labels (N logN - sum n_c log n_c)
# + residual cost  : encode each token under its cluster's per-dimension normal.
# Each value is coded at precision `eps` (probability mass of the eps-interval
# around it). `bending` (>= 1) down-weights the model + residual costs to allow
# for dependence between adjacent measurement points. The MDL-preferred k is the
# one that minimises the total.
# -----------------------------------------------------------------------------
.mdl_cost <- function(C, cl, eps = 1, bending = 1) {
  ks <- sort(unique(cl)); d <- ncol(C)
  relinfo <- function(x, m, s) {                 # -log P(x in its eps-interval)
    s[!is.finite(s) | s == 0] <- eps
    lb <- floor(x / eps) * eps
    p  <- stats::pnorm(lb + eps, m, s) - stats::pnorm(lb, m, s)
    p[!is.finite(p) | p <= 0] <- .Machine$double.xmin
    -sum(log(p))
  }
  mu  <- vapply(ks, function(g) colMeans(C[cl == g, , drop = FALSE]), numeric(d))
  sdv <- vapply(ks, function(g) apply(C[cl == g, , drop = FALSE], 2, stats::sd), numeric(d))
  if (d == 1L) { mu <- matrix(mu, 1); sdv <- matrix(sdv, 1) }
  gi  <- match(cl, ks)
  residual_cost <- sum(vapply(seq_len(nrow(C)), function(i)
    relinfo(C[i, ], mu[, gi[i]], sdv[, gi[i]]), numeric(1)))
  gmu <- colMeans(C); gsd <- apply(C, 2, stats::sd)
  model_cost <- sum(vapply(seq_along(ks), function(j)
    relinfo(mu[, j], gmu, gsd), numeric(1)))
  n <- tabulate(gi)
  part_cost <- sum(n) * log(sum(n)) - sum(n * log(n))
  model_cost / bending + part_cost + residual_cost / bending
}


#' Build per-token feature vectors for f0-contour clustering
#'
#' @description
#' Resamples every token's f0 contour to a common length and turns it into a
#' fixed-length feature vector suitable for clustering. f0 should already be
#' speaker-normalised (e.g. `f0_st` from [normalise_f0()]) so that clusters
#' reflect tone shape and register rather than who has a high voice.
#'
#' @param data Long-format data frame, one row per f0 sample.
#' @param f0,token,time Column names. `time` is normalised per token.
#' @param speaker Optional speaker column, carried through as metadata.
#' @param tone Optional tone column, carried through for later validation.
#' @param n_points Number of points to resample each contour to. Default 20.
#' @param features Feature representation: `"points"` (the resampled contour),
#'   `"legendre"` (orthogonal-polynomial coefficients of `degree`), `"dct"`
#'   (low-order discrete-cosine coefficients), or `"derivative"` (first
#'   difference: cluster by rate of change / movement, discarding height).
#' @param degree Order for `"legendre"`/`"dct"` features. Default 4.
#' @param register `"level"` keeps each contour's height (so high vs low level
#'   tones separate); `"shape"` centres each contour to mean 0 (cluster on
#'   shape only).
#'
#' @return A list with `features` (token x p matrix used for clustering),
#'   `contours` (token x n_points resampled f0, for plotting cluster means),
#'   `tokens`, and `meta` (token / speaker / tone).
#' @export
#' @importFrom stats approx sd
cluster_features <- function(data,
                             f0       = "f0",
                             token    = "token",
                             time     = "time",
                             speaker  = NULL,
                             tone     = NULL,
                             n_points = 20L,
                             features = c("points", "legendre", "dct", "derivative"),
                             degree   = 4L,
                             register = c("level", "shape")) {
  features <- match.arg(features)
  register <- match.arg(register)
  n_points <- as.integer(n_points)
  if (n_points < 4L) stop("`n_points` must be at least 4.", call. = FALSE)

  required <- c(f0, token, time)
  miss <- setdiff(required, names(data))
  if (length(miss)) stop("Column(s) not found in data: ",
                         paste(miss, collapse = ", "), call. = FALSE)
  if (!is.numeric(data[[f0]]))   stop("Column '", f0, "' must be numeric.", call. = FALSE)
  if (!is.numeric(data[[time]])) stop("Column '", time, "' must be numeric.", call. = FALSE)
  if (features %in% c("legendre", "dct") && degree + 1L > n_points)
    stop("`degree` is too high for `n_points`.", call. = FALSE)

  tok_all <- as.character(data[[token]])
  toks <- unique(tok_all)

  # Resample every token to n_points.
  contours <- matrix(NA_real_, nrow = length(toks), ncol = n_points,
                     dimnames = list(toks, NULL))
  for (i in seq_along(toks)) {
    idx <- tok_all == toks[i]
    contours[i, ] <- .interp_contour(data[[time]][idx], data[[f0]][idx], n_points)
  }
  keep <- apply(contours, 1, function(r) all(is.finite(r)))
  if (!any(keep)) stop("No token had enough valid f0 samples to resample.", call. = FALSE)
  contours <- contours[keep, , drop = FALSE]
  toks <- toks[keep]

  if (register == "shape") contours <- contours - rowMeans(contours)

  # Feature matrix.
  if (features == "points") {
    feat <- contours
    colnames(feat) <- paste0("t", seq_len(n_points))
  } else if (features == "legendre") {
    tn <- seq(-1, 1, length.out = n_points)
    B  <- legendre_basis(tn, degree)                 # reused from polynomial.R
    feat <- t(qr.solve(B, t(contours)))              # tokens x (degree+1)
    colnames(feat) <- paste0("c", 0:degree)
  } else if (features == "dct") {
    M <- .dct_basis(n_points)[seq_len(degree + 1L), , drop = FALSE]
    feat <- contours %*% t(M)
    colnames(feat) <- paste0("d", 0:degree)
  } else {                                           # derivative (rate of change)
    # First difference of the resampled contour: clusters by how f0 *moves*
    # (rise/fall), discarding overall height (a constant offset differentiates
    # away). Cf. Kaland (2023, JASA): the first derivative best tracks perceived
    # contour similarity.
    feat <- t(apply(contours, 1, diff))
    colnames(feat) <- paste0("dt", seq_len(ncol(feat)))
  }
  # Coefficient features live on different scales; standardise so no single
  # dimension dominates the distance. Raw points / derivatives share units, so leave.
  if (features %in% c("legendre", "dct")) {
    s <- apply(feat, 2, stats::sd)
    s[s == 0 | is.na(s)] <- 1
    feat <- sweep(feat, 2, s, "/")
  }
  rownames(feat) <- toks

  # Metadata (first row per token).
  meta <- data.frame(token = toks, stringsAsFactors = FALSE)
  first_of <- function(col) {
    vapply(toks, function(tk) as.character(data[[col]][which(tok_all == tk)[1]]),
           character(1))
  }
  if (!is.null(speaker) && speaker %in% names(data)) meta$speaker <- first_of(speaker)
  if (!is.null(tone)    && tone    %in% names(data)) meta$tone    <- first_of(tone)

  list(features = feat, contours = contours, tokens = toks, meta = meta,
       n_points = n_points, register = register, feature_type = features)
}


#' Diagnostics for choosing the number of clusters (candidate tones)
#'
#' @description
#' Runs k-means across a range of `k` and reports the elbow (total
#' within-cluster sum of squares), average silhouette width, and -- when the
#' `cluster` / `mclust` packages are available -- the gap statistic and the
#' GMM/BIC-preferred `k`. No single index is authoritative; use the spread to
#' read off a plausible *range* for the number of tones.
#'
#' @param feat A list from [cluster_features()], or a numeric feature matrix.
#' @param k_range Integer vector of cluster counts to evaluate. Default `2:8`.
#' @param nstart k-means restarts. Default 25.
#' @param gap Whether to compute the gap statistic (needs the `cluster`
#'   package). Default TRUE.
#' @param bending MDL bending factor (>= 1; Kaland & Ellison 2023). Higher
#'   values down-weight the model/residual cost so fewer clusters are preferred;
#'   tune until the MDL (information-cost) curve is U-shaped. Default 1.
#'
#' @return A list with `table` (k, wss, silhouette, gap, gap_se, mdl),
#'   suggestions `k_silhouette`, `k_gap`, `k_gmm`, `k_mdl` (any may be `NA`),
#'   and `assignments` (the per-k k-means cluster vectors).
#'
#' @references
#' Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation
#' and validation of cluster analysis. \emph{Journal of Computational and
#' Applied Mathematics}, 20, 53--65.
#'
#' Tibshirani, R., Walther, G., & Hastie, T. (2001). Estimating the number of
#' clusters in a data set via the gap statistic. \emph{Journal of the Royal
#' Statistical Society B}, 63(2), 411--423.
#'
#' Kaland, C., & Ellison, T. M. (2023). Evaluating cluster analysis on f0
#' contours: An information theoretic approach on three languages.
#' \emph{Proceedings of the 20th International Congress of Phonetic Sciences},
#' 3448--3452.
#'
#' @export
#' @importFrom stats kmeans dist
choose_k_f0 <- function(feat, k_range = 2:8, nstart = 25L, gap = TRUE, bending = 1) {
  X <- if (is.list(feat) && !is.null(feat$features)) feat$features else feat
  X <- as.matrix(X)
  # MDL is evaluated on the contour values (Kaland & Ellison 2023), not the
  # reduced feature space, when those are available.
  C <- if (is.list(feat) && !is.null(feat$contours)) as.matrix(feat$contours) else X
  k_range <- sort(unique(as.integer(k_range)))
  k_range <- k_range[k_range >= 2 & k_range < nrow(X)]
  if (!length(k_range)) stop("No valid k in `k_range` for this many tokens.", call. = FALSE)

  has_cluster <- requireNamespace("cluster", quietly = TRUE)
  d <- if (has_cluster) stats::dist(X) else NULL

  wss <- sil <- mdl <- rep(NA_real_, length(k_range))
  assignments <- vector("list", length(k_range))
  for (i in seq_along(k_range)) {
    km <- stats::kmeans(X, centers = k_range[i], nstart = nstart, iter.max = 50L)
    wss[i] <- km$tot.withinss
    assignments[[i]] <- km$cluster
    mdl[i] <- tryCatch(.mdl_cost(C, km$cluster, bending = bending),
                       error = function(e) NA_real_)
    if (has_cluster) {
      s <- cluster::silhouette(km$cluster, d)
      sil[i] <- mean(s[, "sil_width"])
    }
  }
  names(assignments) <- k_range

  gap_v <- gap_se <- rep(NA_real_, length(k_range))
  if (isTRUE(gap) && has_cluster) {
    cg <- suppressWarnings(try(cluster::clusGap(X, FUN = stats::kmeans, nstart = nstart,
                               iter.max = 50L, K.max = max(k_range), B = 20), silent = TRUE))
    if (!inherits(cg, "try-error")) {
      tab <- cg$Tab
      gap_v  <- tab[k_range, "gap"]
      gap_se <- tab[k_range, "SE.sim"]
    }
  }

  k_gmm <- NA_integer_
  if (.ensure_mclust()) {
    # Diagonal covariance models only: fast and stable even for high-dimensional
    # feature spaces (raw points / derivatives), where full models are O(d^2)
    # and crawl (tens of seconds).
    mc <- try(mclust::Mclust(X, G = k_range, modelNames = .GMM_MODELS,
                             verbose = FALSE), silent = TRUE)
    if (!inherits(mc, "try-error") && !is.null(mc)) k_gmm <- mc$G
  }

  table <- data.frame(k = k_range, wss = wss, silhouette = sil,
                      gap = gap_v, gap_se = gap_se, mdl = mdl)
  k_sil <- if (any(is.finite(sil))) k_range[which.max(sil)] else NA_integer_
  k_mdl <- if (any(is.finite(mdl))) k_range[which.min(mdl)] else NA_integer_
  # Tibshirani's "1-SE" rule for the gap statistic.
  k_gap <- NA_integer_
  if (any(is.finite(gap_v))) {
    for (i in seq_along(k_range)) {
      nxt <- i + 1L
      if (is.finite(gap_v[i]) && (nxt > length(k_range) ||
          (is.finite(gap_v[nxt]) && gap_v[i] >= gap_v[nxt] - gap_se[nxt]))) {
        k_gap <- k_range[i]; break
      }
    }
    if (is.na(k_gap)) k_gap <- k_range[which.max(gap_v)]
  }

  list(table = table, k_silhouette = k_sil, k_gap = k_gap, k_gmm = k_gmm,
       k_mdl = k_mdl, assignments = assignments)
}


#' Recompute MDL information cost for stored clusterings at a given bending
#'
#' @description
#' A light helper so the app can re-evaluate the MDL curve live as the user
#' tunes the bending factor, without re-running the (slow) gap/GMM diagnostics.
#'
#' @param contours Token x n_points matrix of resampled f0 (e.g.
#'   `cluster_features(...)$contours`).
#' @param assignments A named list of cluster vectors, one per k (e.g.
#'   `choose_k_f0(...)$assignments`).
#' @param bending MDL bending factor (>= 1); higher down-weights model/residual
#'   cost relative to the partition cost.
#'
#' @return Named numeric vector of MDL costs, one per element of `assignments`.
#' @export
cluster_mdl <- function(contours, assignments, bending = 1) {
  C <- as.matrix(contours)
  out <- vapply(assignments, function(cl)
    tryCatch(.mdl_cost(C, cl, bending = bending), error = function(e) NA_real_),
    numeric(1))
  stats::setNames(out, names(assignments))
}


#' Cluster f0 contours into candidate tone categories
#'
#' @param feat A list from [cluster_features()] (needs `features`+`contours`).
#' @param method `"kmeans"`, `"hclust"` (Ward), or `"gmm"` (model-based,
#'   needs `mclust`).
#' @param k Number of clusters.
#' @param nstart k-means restarts. Default 25.
#' @param hclust_method Linkage for `"hclust"`. Default `"ward.D2"`.
#'
#' @return A list with `assignment` (named integer per token), `method`, `k`,
#'   `sizes`, `cluster_means` (k x n_points mean contour per cluster),
#'   `tokens`, `uncertainty` (per-token 1 - max posterior, GMM only; else
#'   `NULL`), and `tree` (the fitted `hclust` object for method `"hclust"`,
#'   else `NULL`).
#'
#' @references
#' Kaland, C. (2023). Contour clustering: A field-data-driven approach for
#' documenting and analysing prototypical f0 contours. \emph{Journal of the
#' International Phonetic Association}, 53(1), 159--188.
#' \doi{10.1017/S0025100321000049}
#'
#' Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means
#' clustering algorithm. \emph{Applied Statistics}, 28(1), 100--108.
#'
#' Ward, J. H. (1963). Hierarchical grouping to optimize an objective
#' function. \emph{Journal of the American Statistical Association}, 58(301),
#' 236--244.
#'
#' Scrucca, L., Fop, M., Murphy, T. B., & Raftery, A. E. (2016). mclust 5:
#' Clustering, classification and density estimation using Gaussian finite
#' mixture models. \emph{The R Journal}, 8(1), 289--317.
#'
#' @export
#' @importFrom stats kmeans hclust dist cutree
cluster_f0 <- function(feat, method = c("kmeans", "hclust", "gmm"), k,
                       nstart = 25L, hclust_method = "ward.D2") {
  method <- match.arg(method)
  if (!is.list(feat) || is.null(feat$features))
    stop("`feat` must come from cluster_features().", call. = FALSE)
  X <- as.matrix(feat$features)
  k <- as.integer(k)
  if (k < 2 || k > nrow(X)) stop("`k` must be between 2 and the number of tokens.", call. = FALSE)

  unc <- NULL                                   # per-token membership uncertainty (gmm)
  tree <- NULL                                  # fitted hclust object (hclust only)
  if (method == "kmeans") {
    cl <- stats::kmeans(X, centers = k, nstart = nstart, iter.max = 50L)$cluster
  } else if (method == "hclust") {
    tree <- stats::hclust(stats::dist(X), method = hclust_method)
    cl <- stats::cutree(tree, k = k)
  } else {
    if (!.ensure_mclust())
      stop("Method 'gmm' needs the 'mclust' package.", call. = FALSE)
    mc <- mclust::Mclust(X, G = k, modelNames = .GMM_MODELS, verbose = FALSE)
    cl <- mc$classification
    unc <- as.numeric(mc$uncertainty)           # 1 - max posterior; label-invariant
  }
  cl <- as.integer(cl)
  names(cl) <- feat$tokens

  # Relabel clusters by mean contour height (low -> high) for stable ordering.
  means <- t(vapply(sort(unique(cl)), function(g)
    colMeans(feat$contours[cl == g, , drop = FALSE]), numeric(ncol(feat$contours))))
  ord <- order(rowMeans(means))
  remap <- match(cl, sort(unique(cl)))
  remap <- match(remap, ord)
  cl <- remap; names(cl) <- feat$tokens
  cluster_means <- means[ord, , drop = FALSE]
  rownames(cluster_means) <- paste0("cluster", seq_len(nrow(cluster_means)))

  list(assignment = cl, method = method, k = k,
       sizes = as.integer(table(factor(cl, levels = seq_len(k)))),
       cluster_means = cluster_means, tokens = feat$tokens,
       uncertainty = if (!is.null(unc)) stats::setNames(unc, feat$tokens) else NULL,
       tree = tree)
}


#' Agreement between clusters and known tone labels
#'
#' @param clusters Integer/character cluster assignment per token.
#' @param labels Known tone labels per token (same length/order).
#'
#' @return A list with `table` (clusters x labels contingency table) and `ari`
#'   (adjusted Rand index; 1 = perfect, ~0 = chance).
#' @export
cluster_agreement <- function(clusters, labels) {
  if (length(clusters) != length(labels))
    stop("`clusters` and `labels` must be the same length.", call. = FALSE)
  ok <- !is.na(clusters) & !is.na(labels)
  a <- as.character(clusters[ok]); b <- as.character(labels[ok])
  tab <- table(cluster = a, label = b)

  # Adjusted Rand index (Hubert & Arabie 1985), implemented without deps.
  n <- sum(tab)
  choose2 <- function(x) x * (x - 1) / 2
  sum_ij <- sum(choose2(tab))
  a_i <- sum(choose2(rowSums(tab)))
  b_j <- sum(choose2(colSums(tab)))
  expected <- a_i * b_j / choose2(n)
  maxidx <- (a_i + b_j) / 2
  ari <- if (maxidx - expected == 0) 0 else (sum_ij - expected) / (maxidx - expected)

  list(table = tab, ari = ari)
}
