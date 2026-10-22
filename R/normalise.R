#' Normalise f0 by speaker
#'
#' @description
#' Adds a within-speaker normalised f0 column to a long-format f0 data
#' frame. Speaker normalisation puts contours from male and female
#' speakers on a comparable scale, and is a near-universal pre-processing
#' step before cross-speaker tone modelling, plotting, or contour
#' summarisation (Rose 1987; Zhu 1999).
#'
#' Three formulae are available. Semitones above or below the speaker's
#' mean give a perceptually uniform scale. By-speaker z-scores combine
#' centring with unit-variance scaling. Log z-scores (LZ) do both on a
#' logarithmic scale: they are z-scores of log f0. Every formula relies on a
#' per-speaker mean (and the z-scores on a per-speaker SD), which can be
#' computed in two ways: over all observations alike, or so that every tone
#' contributes equally whatever its number of tokens.
#'
#' @details
#' ## What the function does internally
#'
#' 1. Compute a per-speaker mean of `f0` (of log f0 for `"lz"`) using
#'    `mean_method`, and for the z-scores a per-speaker SD the same way
#'    (see below).
#' 2. Add that mean to `data` as a new column called `speaker_mean`.
#' 3. Apply the selected normalisation formula sample by sample and
#'    return the augmented data frame.
#'
#' ## Choosing `method`
#'
#' * `"semitone"` is the standard for citation-tone work. The scale is
#'   perceptually uniform (one semitone is roughly one just-noticeable
#'   pitch step), centred on the speaker's own range, and preserves the
#'   logarithmic structure of musical pitch perception.
#' * `"zscore"` is useful when both centring and scaling are desired,
#'   i.e., when you want to remove each speaker's baseline *and* equalise
#'   their f0 variability. Useful for cross-speaker statistical models
#'   where the raw variance differs strongly across speakers.
#' * `"lz"` (log z-score) is the z-score of log f0 (Zhu 1999). Like
#'   semitones it works on ratios, so the same interval counts the same in
#'   a low and a high voice; like `"zscore"` it also divides by each
#'   speaker's spread, so speakers with narrow and wide pitch ranges end up
#'   on one scale. It equals semitones measured from the speaker's
#'   geometric mean (the `speaker_mean` that `"lz"` reports) divided by
#'   the speaker's SD in semitones. `f0_st` is measured from the arithmetic
#'   mean instead: with `mean_method = "simple"`, `f0_st / sd(f0_st)` has
#'   the same shape as LZ, shifted by a small per-speaker constant; with
#'   `"weighted"`, LZ also divides by the tone-weighted SD, so the two
#'   differ slightly in scale as well. It is the usual "z-score" in studies
#'   of tone in languages of China: 37 of the 136 studies reviewed by Xu
#'   and Zhang (2024) used it, against 4 that used a plain z-score.
#'   Because it equalises
#'   ranges, prefer semitones when differences in range are themselves of
#'   interest (e.g. comparing the size of tone spaces across speakers or
#'   varieties).
#'
#' ## Choosing `mean_method`
#'
#' * `"weighted"` (default) lets every tone count once, regardless of how
#'   many tokens it has. The centre is the mean of the per-tone means. For
#'   the z-scores the SD is weighted the same way: each observation gets
#'   weight 1 / (number of tones x observations of its tone), as in
#'   [stats::cov.wt()]. This is the right default for unbalanced designs
#'   in which token counts differ across tones, common in fieldwork and
#'   naturalistic corpora. It is this package's tone-balanced variant.
#' * `"simple"` takes the mean (and SD) of every observation for a speaker.
#'   This is the classic formula: Rose's (1987) z-score and Zhu's (1999)
#'   LZ. Choose it to reproduce published values.
#'
#' The two give identical results when every tone has the same number of
#' observations.
#'
#' ## The SD correction
#'
#' With `sd_correction = TRUE` (default) the z-scores use the corrected
#' (sample) SD, dividing by n - 1 (Bessel's correction) as [stats::sd()] and
#' the published formulas do; with
#' `"weighted"` this is the reliability-weights correction of
#' `stats::cov.wt(method = "unbiased")`, which reduces to n - 1 when every
#' tone has the same number of observations. `FALSE` gives the uncorrected
#' (population) SD, dividing by n. With `"simple"`, or equal numbers of
#' observations per tone, the two differ by a factor of sqrt(n / (n - 1)),
#' negligible for a speaker with hundreds of observations. With
#' `"weighted"` the factor is 1 / sqrt(1 - sum(w^2)), which is set mostly by
#' the tone with the fewest observations: a tone with only a few frames
#' can make it several percent. Semitones use no SD, so the argument does
#' not affect them.
#'
#' ## NA handling
#'
#' All summary statistics use `na.rm = TRUE`. Sample-level `NA` f0
#' values propagate as `NA` in the normalised column. For `"lz"`, f0
#' values of 0 or below (sometimes used to mark unvoiced frames) have no
#' logarithm, so they are treated as `NA` too.
#'
#' @param data A long-format data frame with one row per f0 sample. Must
#'   contain at least the f0, speaker, and (for `"weighted"` means) tone
#'   columns named below.
#' @param f0 Name of the column with raw f0 values, in Hz. Default `"f0"`.
#' @param speaker Name of the speaker ID column. Default `"speaker"`.
#' @param tone Name of the tone category column. Default `"tone"`. Only
#'   read when `mean_method = "weighted"`.
#' @param method Normalisation formula. One of:
#'   * `"semitone"` (default): `ST = 12 * log2(f0 / speaker_mean)`.
#'     Result is 0 at the speaker mean and ±12 at one octave above or
#'     below.
#'   * `"zscore"`: `Z = (f0 - speaker_mean) / s`, where `s` is the
#'     speaker's SD of f0.
#'   * `"lz"`: `LZ = (log10(f0) - m) / s`, where `m` and `s` are the
#'     speaker's mean and SD of log10 f0, as in Zhu (1999). Any base gives
#'     the same LZ, since it only rescales the logs.
#'
#'   With `mean_method = "simple"` both z-scores have mean 0 and SD 1
#'   within each speaker (SD 1 as measured with the same `sd_correction`).
#'   With `"weighted"` the same holds with every tone counting equally:
#'   the per-tone means average to 0, while the plain mean over all
#'   observations moves away from 0 when tones have unequal numbers of
#'   observations.
#' @param mean_method How the per-speaker mean, and the SD for the
#'   z-scores, are computed: `"weighted"` (default; every tone counts
#'   equally) or `"simple"` (all observations alike; the classic formula).
#' @param sd_correction Logical: apply Bessel's correction to the SD.
#'   `TRUE` (default) uses the corrected (sample) SD, dividing by n - 1;
#'   `FALSE` the uncorrected (population) SD, dividing by n. Only affects `"zscore"` and
#'   `"lz"`. See "The SD correction" below.
#'
#' @return The input data frame with two added columns:
#' * `speaker_mean`: the per-speaker mean used for normalisation, in Hz.
#'   For `"lz"` this is the geometric mean, `10^m`.
#' * `f0_st` (if `method = "semitone"`), `f0_zscore` (if
#'   `method = "zscore"`) or `f0_lz` (if `method = "lz"`).
#'
#' @seealso
#' * [fit_polynomial()], [fit_gca()], [fit_gamm()] for downstream contour
#'   modelling that typically operates on the normalised column.
#' * [contour_to_chao()] for converting mean contours to Chao tone
#'   numerals, often called on `normalise_f0()` output.
#'
#' @references
#' Rose, P. (1987). Considerations in the normalisation of the
#' fundamental frequency of linguistic tone. \emph{Speech Communication},
#' 6(4), 343–352. \doi{10.1016/0167-6393(87)90008-1}
#'
#' Zhu, X. (1999). \emph{Shanghai Tonetics}. LINCOM Europa.
#'
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
#'
#' @examples
#' data(sample_f0)
#'
#' out <- normalise_f0(sample_f0,
#'                     f0      = "f0_Hz",
#'                     speaker = "speaker",
#'                     tone    = "tone",
#'                     method  = "semitone")
#' head(out[, c("speaker", "tone", "f0_Hz", "speaker_mean", "f0_st")])
#'
#' # Same data, z-score instead of semitones:
#' out_z <- normalise_f0(sample_f0,
#'                       f0      = "f0_Hz",
#'                       speaker = "speaker",
#'                       tone    = "tone",
#'                       method  = "zscore")
#' head(out_z[, c("speaker", "f0_Hz", "speaker_mean", "f0_zscore")])
#'
#' # Log z-score (LZ), common in studies of Chinese tone:
#' out_lz <- normalise_f0(sample_f0,
#'                        f0      = "f0_Hz",
#'                        speaker = "speaker",
#'                        tone    = "tone",
#'                        method  = "lz")
#' head(out_lz[, c("speaker", "f0_Hz", "speaker_mean", "f0_lz")])
#'
#' @export
#' @importFrom dplyr group_by summarise left_join mutate ungroup
#' @importFrom rlang .data :=
#' @importFrom stats sd var
normalise_f0 <- function(data,
                         f0            = "f0",
                         speaker       = "speaker",
                         tone          = "tone",
                         method        = c("semitone", "zscore", "lz"),
                         mean_method   = c("weighted", "simple"),
                         sd_correction = TRUE) {

  method      <- match.arg(method)
  mean_method <- match.arg(mean_method)

  # --- Input validation ---------------------------------------------------
  required <- c(f0, speaker)
  if (mean_method == "weighted") required <- c(required, tone)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[f0]])) {
    stop("Column '", f0, "' must be numeric (f0 in Hz).", call. = FALSE)
  }
  if (!is.logical(sd_correction) || length(sd_correction) != 1 || is.na(sd_correction)) {
    stop("`sd_correction` must be TRUE or FALSE.", call. = FALSE)
  }

  # --- Values the formula works on ---------------------------------------
  # LZ works on log f0 throughout, so its centre is the mean of log f0 (the
  # log of the geometric mean). f0 <= 0 has no logarithm and counts as NA.
  x   <- data[[f0]]   # read before speaker_mean is written below
  val <- x
  if (method == "lz") {
    ok  <- !is.na(val) & val > 0   # log only valid values: no NaN warning
    val <- rep(NA_real_, length(val))
    val[ok] <- log10(x[ok])
  }

  # --- Per-speaker centre and spread --------------------------------------
  # "simple": mean and SD of all the speaker's values (the classic z-score
  # and LZ). "weighted": every tone counts once, whatever its token count.
  # Each value gets weight 1 / (tones x values in its tone), so the centre is
  # the mean of the per-tone means and the spread is the matching weighted SD
  # (as stats::cov.wt). sd_correction divides the variance by 1 - sum(w^2),
  # which is the n - 1 of sd() when weights are equal, so the two methods
  # agree exactly when every tone has the same number of values.
  spk    <- data[[speaker]]
  groups <- split(seq_along(spk), match(spk, unique(spk)))
  tn     <- if (mean_method == "weighted") data[[tone]] else NULL
  centre <- spread <- rep(NA_real_, length(spk))
  for (g in groups) {
    y  <- val[g]
    ok <- !is.na(y)
    n  <- sum(ok)
    if (n == 0) next
    y  <- y[ok]
    if (mean_method == "simple") {
      m <- mean(y)
      v <- if (n < 2) NA_real_ else if (sd_correction) stats::var(y) else mean((y - m)^2)
    } else {
      tk  <- factor(tn[g][ok], exclude = NULL)   # NA tone: a tone of its own
      n_k <- tabulate(tk, nbins = nlevels(tk))
      m   <- mean(tapply(y, tk, mean))
      w   <- 1 / (nlevels(tk) * n_k[as.integer(tk)])
      v   <- sum(w * (y - m)^2)
      if (sd_correction) {
        d <- 1 - sum(w^2)
        v <- if (d > 0) v / d else NA_real_
      }
    }
    centre[g] <- m
    spread[g] <- sqrt(v)
  }
  data$speaker_mean <- if (method == "lz") 10^centre else centre

  # --- Normalised column --------------------------------------------------
  norm_col <- switch(method, zscore = "f0_zscore", lz = "f0_lz", "f0_st")
  data[[norm_col]] <- switch(method,
    semitone = 12 * log2(x / data$speaker_mean),
    zscore   = (x - centre) / spread,
    lz       = (val - centre) / spread)

  data
}
