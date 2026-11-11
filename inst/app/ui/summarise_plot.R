# Chao tone numerals for the Summarise tab: mean contours on the reference-line FOR
# [1, 5] (digit chips) and the interval FOR (0, 5] (band watermarks), one legend.
# Used by summarise_ui.R (sourced in server.R). Call grid::grid.newpage() first,
# then grid::grid.draw(plot_chao_panels(...)): the layout reads the open device's
# size. base_family = "" leaves the family unset (thematic's font applies).
# Needs ggplot2 >= 3.5.0.

# ---- Style ----------------------------------------------------------------------
.chao_style <- list(
  ink_title = "#1e2321", ink_soft = "#5c6360", ink_tick = "#6b7a75",
  guide = "#e7e9e8", boundary = "#b4bab7",       # hairlines: limits 0 and 5; boundaries
  refline = "#c4dcd3", watermark = "#d2e6dd",    # reference lines; interval band digits
  chip_ink = "#2c5f4f", chip_fill = "#dcefe7",   # reference-line digit chips
  zone = "#f3969a", tag = "#78c2ad",             # boundary +/- tol wash; app mint tag
  # Okabe-Ito without yellow, then indigo, brown, black and raspberry (slot 10 is
  # >= 11.8 OKLab dE from every other slot under protan/deutan simulation).
  tones = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9",
            "#D55E00", "#332288", "#661100", "#000000", "#AD1457"),
  ink_contrast = 4.0,      # digit labels: tone hue darkened to this ratio on white
  pt = c(title = 13.5, subtitle = 11, axis_title = 10.5, axis_text = 10,
         legend = 10.5, digit = 10.5, chip = 10, watermark = 40),   # font sizes
  lw = c(contour = 0.85, refline = 0.55, guide = 0.35, boundary = 0.3,
         leader = 0.3, key = 0.7),                                  # line widths
  alpha = c(zone = 0.15, key_zone = 0.21, leader = 0.8, halo = 0.85),
  dot = c(ring = 3.6, fill = 2.3, halo = 5.2, stroke = 0.5,         # point sizes;
          hollow = 0.9),                                            # hollow ring width
  chip = c(pad = 0.24, r = 0.5),                                    # lines
  inset = 10,                                          # pt: left edge of titles, legend
  tag_pt = c(w = 22, h = 3, row = 13), legend_pad = 22  # pt: title tag; legend row room
)

# ---- Geometry: x in normalised time, y in label heights (see gap) ---------------
.chao_geom <- list(
  app_in   = c(980, 440) / 72,  # app plot size (in) the sizes below are tuned for
  chrome   = c(68, 133),        # pt per panel around the plotting area (w, h)
  xlim     = c(-0.072, 1.155),  # gutters: onset digits | offset digits, chips
  zone_end = 1.088, chip_x = 1.125,  # hairlines and zones stop here; chip centres
  side_dx  = 0.042,             # dot centre to onset/offset digit centre
  label_w  = 0.05, gap = 0.071, # one digit label: width; height as a share of the span
  pad      = c(0.3, 0.6),       # least room below lo / above hi
  wm_half  = c(0.045, 0.32),    # watermark digit half-size at app size (x, band)
  # Label placement, in label widths (x) and label heights (y):
  edge = 0.02, moved = 0.45,    # onset/offset: time this near 0 or 1; leader: shift
  stack = 0.92, overhang = 1.05,  # stacked digit spacing; reach beyond lo and hi
  room = 0.55,                  # half-height kept free around a label
  knock = c(0.3, 0.3),          # white box behind onset/offset digits (half-size)
  dot_keep = c(0.75, 0.62),     # dots as obstacles (half-size)
  tp_off = 0.9,                 # turning point: dot centre to label centre
  tp_dy = seq(0, 3, by = 0.25), tp_dx = -3:3, tp_step = 0.8,  # shifts tried
  tp_cost = c(flip = 0.6, dx = 0.5),  # cost of the other side, of a sideways step
  trim = c(dot = 0.22, label = 0.33)  # leader ends held off the dot / label
)

# Natural tone order: digit runs compare as numbers ("2" < "10", "T2" < "T10").
.chao_natural_levels <- function(x) {
  u <- unique(as.character(x[!is.na(x)]))
  key <- tolower(u)
  m <- gregexpr("[0-9]+", key)
  regmatches(key, m) <- lapply(regmatches(key, m), function(d)
    formatC(as.numeric(d), width = 12, format = "d", flag = "0"))
  u[order(key)]
}

# Darken a colour towards black until its WCAG contrast on white reaches
# `target`, so a digit keeps its tone's hue but stays readable.
.chao_readable_ink <- function(hex, target) {
  lum <- function(v) sum(c(0.2126, 0.7152, 0.0722) *
                           ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4))
  v <- grDevices::col2rgb(hex)[, 1] / 255
  k <- seq(1, 0, by = -0.02)                 # k = 0 (black) always passes
  ok <- vapply(k, function(s) 1.05 / (lum(v * s) + 0.05) >= target, NA)
  v <- v * k[which(ok)[1]]
  grDevices::rgb(v[1], v[2], v[3])
}

# 1-D repel: overlapping labels merge into blocks, each centred on its labels'
# wanted positions and kept inside [lo, hi]; neighbours end at least `gap`
# apart and the vertical order never changes.
.chao_repel_1d <- function(y, gap, lo, hi) {
  if (length(y) < 2) return(pmin(pmax(y, lo), hi))
  o <- order(y); want <- y[o]
  place <- function(m) {
    p <- mean(want[m]) + (seq_along(m) - (length(m) + 1) / 2) * gap
    p <- p + max(0, lo - p[1])
    p - max(0, p[length(p)] - hi)
  }
  blocks <- as.list(seq_along(want))
  repeat {
    pos <- unlist(lapply(blocks, place))
    i <- which(diff(pos) < gap - 1e-9)[1]        # first clash between blocks
    if (is.na(i)) break
    b <- rep(seq_along(blocks), lengths(blocks))[i]
    blocks[[b]] <- c(blocks[[b]], blocks[[b + 1]]); blocks[[b + 1]] <- NULL
  }
  out <- numeric(length(y)); out[o] <- pos; out
}

# Where each sampled digit goes (lx, ly), whether it moved far enough to need a
# leader, and the leader's ends (x0, y0, x1, y1). `gap` is the label height in y
# units; `walls` are boxes to keep clear of (centre x, y; half-sizes hw, hh).
.chao_place_labels <- function(pts, lo, hi, gap, walls = NULL) {
  ge <- .chao_geom; W <- ge$label_w
  pts$side <- ifelse(pts$time <= ge$edge, -1, ifelse(pts$time >= 1 - ge$edge, 1, 0))
  pts$lx <- pts$time; pts$ly <- pts$chao_val
  lim <- c(lo, hi) + c(-1, 1) * ge$overhang * gap
  # 1. Onset and offset digits sit beside their dots, stacked like a slopegraph.
  for (s in c(-1, 1)) {
    i <- which(pts$side == s)
    pts$lx[i] <- pts$time[i] + s * ge$side_dx
    pts$ly[i] <- .chao_repel_1d(pts$chao_val[i], ge$stack * gap, lim[1], lim[2])
  }
  # 2. Turning points: above a peak, below a dip, at the cheapest free spot among
  #    the shifts tried. Obstacles: every dot, labels placed so far, the walls.
  free <- function(x, y, done)
    !any(abs(pts$time - x) < ge$dot_keep[1] * W & abs(pts$chao_val - y) < ge$dot_keep[2] * gap) &&
    !any(abs(pts$lx[done] - x) < W & abs(pts$ly[done] - y) < gap) &&
    !any(abs(walls$x - x) < walls$hw + W / 2 & abs(walls$y - y) < walls$hh + gap / 2)
  cand <- expand.grid(dy = ge$tp_dy, flip = 0:1, dx = ge$tp_dx)
  cand <- cand[order(cand$dy + ge$tp_cost[["flip"]] * cand$flip +
                       ge$tp_cost[["dx"]] * abs(cand$dx)), ]
  done <- pts$side != 0
  for (i in which(!done)[order(pts$time[!done])]) {
    x0 <- pts$time[i]; y0 <- pts$chao_val[i]
    j <- which(pts$tone == pts$tone[i]); tj <- pts$time[j]
    nb <- c(pts$chao_val[j][tj < x0][which.max(tj[tj < x0])],
            pts$chao_val[j][tj > x0][which.min(tj[tj > x0])])
    up <- if (!length(nb) || y0 >= mean(nb)) 1 else -1
    xs <- x0 + cand$dx * ge$tp_step * W
    ys <- y0 + up * (1 - 2 * cand$flip) * (ge$tp_off + cand$dy) * gap
    ok <- which(ys >= lim[1] & ys <= lim[2] & xs >= 0 & xs <= 1)
    k <- Find(function(k) free(xs[k], ys[k], done), ok)
    if (is.null(k)) k <- 1                    # nothing free: stay at home
    pts$lx[i] <- xs[k]; pts$ly[i] <- min(max(ys[k], lim[1]), lim[2])
    done[i] <- TRUE
  }
  # 3. Leaders where a label left home: onsets/offsets run dot edge to label edge,
  #    turning points centre to centre (the dot's ring and label's halo trim them).
  home_dy <- ifelse(pts$side == 0, ge$tp_off * gap, 0)
  pts$moved <- abs(pts$lx - pts$time - pts$side * ge$side_dx) > 1e-9 |
    abs(abs(pts$ly - pts$chao_val) - home_dy) > ge$moved * gap
  pts$x0 <- pts$time + pts$side * ge$trim[["dot"]] * W; pts$y0 <- pts$chao_val
  pts$x1 <- pts$lx - pts$side * ge$trim[["label"]] * W; pts$y1 <- pts$ly
  pts
}

plot_chao_panels <- function(pdat, ref_point_df, int_point_df, tol, base_family = "") {
  st <- .chao_style; ge <- .chao_geom; W <- ge$label_w; .pt <- ggplot2::.pt
  u <- function(x, unit = "pt") grid::unit(x, unit)
  mg <- ggplot2::margin; blank <- ggplot2::element_blank()

  # ---- Inputs ---------------------------------------------------------------------
  tol <- suppressWarnings(as.numeric(tol)[1])
  if (!length(tol) || is.na(tol) || tol < 0) tol <- 0
  zone_label <- paste0("Within \u00b1", format(tol), " of a digit boundary")
  hollow_label <- "Turning point not in the numeral"
  clean_pts <- function(d) {
    if (is.null(d) || !nrow(d)) d <- data.frame(time = numeric(0), chao_val = numeric(0),
                                                chao_digit = character(0), tone = character(0))
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    d$tone <- as.character(d$tone); d$chao_digit <- as.character(d$chao_digit)
    # shown = FALSE: a sampled turning point whose digit is not in the numeral
    d$shown <- if (is.null(d$shown)) rep(TRUE, nrow(d)) else !(d$shown %in% FALSE)
    d[is.finite(d$time) & is.finite(d$chao_val), , drop = FALSE]
  }
  pts <- lapply(list(ref = ref_point_df, int = int_point_df), clean_pts)
  # Natural order; a factor's own level order wins for the tones it lists.
  tone_levels <- .chao_natural_levels(c(as.character(pdat$tone), pts$ref$tone, pts$int$tone))
  if (is.factor(pdat$tone))
    tone_levels <- union(intersect(levels(pdat$tone), tone_levels), tone_levels)
  n_tones <- length(tone_levels)
  tone_cols <- stats::setNames(if (n_tones <= length(st$tones)) st$tones[seq_len(n_tones)]
                               else grDevices::hcl.colors(n_tones, "Dark 3"), tone_levels)
  tone_ink <- vapply(tone_cols, .chao_readable_ink, "", target = st$ink_contrast)
  pdat <- as.data.frame(pdat, stringsAsFactors = FALSE)
  pdat$tone <- factor(as.character(pdat$tone), levels = tone_levels)

  # Rough panel size from the device (the app's if none is open) scales the watermarks.
  dev_in <- if (grDevices::dev.cur() > 1) grDevices::dev.size("in") else ge$app_in
  panel_pt <- pmax(dev_in * 72 / c(2, 1) - ge$chrome, 1)
  app_pt <- ge$app_in * 72 / c(2, 1) - ge$chrome
  wm_scale <- min(1, panel_pt[2] / app_pt[2])
  wm_half <- ge$wm_half * wm_scale * app_pt / panel_pt   # in x units, band heights

  # ---- Panel specs and label layout -------------------------------------------------
  specs <- list(
    ref = list(yvar = "chao_ref", pts = pts$ref, lo = 1, hi = 5, bounds = 1:4 + 0.5,
               guides = NULL, anchors = 1:5, walls = NULL,
               title = "Reference-line FOR [1, 5]", subtitle = "\u00d7 4 + 1, round()"),
    int = list(yvar = "chao_int", pts = pts$int, lo = 0, hi = 5, bounds = 1:4,
               guides = c(0, 5), anchors = NULL,
               walls = data.frame(x = 0.5, y = 1:5 - 0.5, hw = wm_half[1], hh = wm_half[2]),
               wm = data.frame(x = 0.5, y = 1:5 - 0.5),
               title = "Interval-based FOR (0, 5]", subtitle = "\u00d7 5, ceiling()"))
  pad <- ge$pad     # grows to the larger need of the two panels, so they line up
  for (k in names(specs)) {
    s <- specs[[k]]
    s$gap <- ge$gap * (s$hi - s$lo)
    # Unlabelled turning points: hollow dots, kept clear of the digit labels
    s$hollow <- s$pts[!s$pts$shown, , drop = FALSE]; s$pts <- s$pts[s$pts$shown, , drop = FALSE]
    if (nrow(s$hollow)) s$walls <- rbind(s$walls, data.frame(x = s$hollow$time,
      y = s$hollow$chao_val, hw = (ge$dot_keep[1] - 0.5) * W, hh = (ge$dot_keep[2] - 0.5) * s$gap))
    s$pts <- .chao_place_labels(s$pts, s$lo, s$hi, s$gap, s$walls)
    y <- c(s$lo, s$hi, pdat[[s$yvar]], s$pts$ly - ge$room * s$gap, s$pts$ly + ge$room * s$gap)
    pad <- pmax(pad, c(s$lo - min(y, na.rm = TRUE), max(y, na.rm = TRUE) - s$hi) / s$gap)
    # Zones: boundary +/- tol; from tol 0.5 on they merge into one band.
    s$zones <- if (tol >= 0.5) data.frame(ymin = max(s$lo, min(s$bounds) - tol),
                                          ymax = min(s$hi, max(s$bounds) + tol))
               else data.frame(ymin = s$bounds - tol, ymax = s$bounds + tol)
    specs[[k]] <- s
  }

  any_hollow <- any(vapply(specs, function(s) nrow(s$hollow) > 0, NA))

  # ---- Theme (family set only when given) and shared pieces ----------------------
  fam <- if (is.character(base_family) && length(base_family) == 1L &&
             !is.na(base_family) && nzchar(base_family)) list(family = base_family)
  lyr <- function(fun, ..., .extra = NULL) do.call(fun, c(list(...), .extra, fam))
  txt <- function(size, colour = st$ink_soft, ...)
    ggplot2::element_text(size = st$pt[[size]], colour = colour, ...)
  white <- ggplot2::element_rect(fill = "white", colour = NA)
  theme_chao <- ggplot2::theme(
    text = ggplot2::element_text(family = fam$family, colour = st$ink_soft),
    rect = white, panel.background = white, legend.key = white, panel.border = blank,
    panel.grid = blank, axis.line = blank, axis.ticks = blank, axis.ticks.length = u(0),
    plot.title = txt("title", st$ink_title, face = "bold", hjust = 0, vjust = 1,
                     margin = mg(b = 3)),
    plot.subtitle = txt("subtitle", hjust = 0, vjust = 1, margin = mg(b = 12)),
    plot.title.position = "plot", plot.margin = mg(9, 22, 6, st$inset),
    axis.title.x = txt("axis_title", margin = mg(t = 8)),
    axis.title.y = txt("axis_title", angle = 90, margin = mg(r = 8)),
    axis.text.x = txt("axis_text", st$ink_tick, margin = mg(t = 5)),
    axis.text.y = txt("axis_text", st$ink_tick, hjust = 1, margin = mg(r = 12)),
    legend.position = "none", legend.justification = c(0, 0.5), legend.box.just = "left",
    legend.title = txt("legend", st$ink_title, face = "bold", hjust = 0, margin = mg(r = 8)),
    legend.text = txt("legend", margin = mg(l = 4, r = 2)), legend.margin = mg(0, 0, 0, 0),
    legend.key.width = u(20), legend.key.height = u(12), legend.key.spacing.x = u(12),
    legend.key.spacing.y = u(5), legend.spacing.x = u(30), legend.spacing.y = u(8),
    legend.box.spacing = u(0))
  no_border <- if ("border.colour" %in% names(formals(ggplot2::geom_label)))
    list(border.colour = NA) else list(label.size = 0)   # ggplot2 < 4.0.0
  # Legend key for the zones: the wash crossed by a dashed boundary.
  key_zone <- function(...) grid::grobTree(grid::rectGrob(height = u(0.8, "npc"),
      gp = grid::gpar(col = NA, fill = grDevices::adjustcolor(st$zone, st$alpha[["key_zone"]]))),
    grid::segmentsGrob(0, 0.5, 1, 0.5,
                       gp = grid::gpar(col = st$boundary, lwd = st$lw[["key"]], lty = "22")))
  hline <- function(y, xend, colour, linewidth, ...) ggplot2::annotate("segment",
    x = -Inf, xend = xend, y = y, yend = y, colour = colour, linewidth = linewidth, ...)

  # ---- One panel, back to front ------------------------------------------------------
  build_panel <- function(s) {
    d <- s$pts; side <- d[d$side != 0, ]; mid <- d[d$side == 0, ]; lead <- d[d$moved, ]
    p <- ggplot2::ggplot(pdat, ggplot2::aes(.data$time, .data[[s$yvar]],
                                           colour = .data$tone, group = .data$tone))
    # Structure: hairlines; white boxes that cut them behind onset/offset digits;
    # the zone wash (over the boxes); watermarks; chips.
    if (length(s$guides)) p <- p + hline(s$guides, ge$zone_end, st$guide, st$lw[["guide"]])
    p <- p + hline(s$bounds, ge$zone_end, st$boundary, st$lw[["boundary"]], linetype = "22")
    if (length(s$anchors)) p <- p + hline(s$anchors, ge$chip_x, st$refline, st$lw[["refline"]])
    if (nrow(side)) p <- p + ggplot2::annotate("rect", fill = "white", colour = NA,
      xmin = side$lx - ge$knock[1] * W, xmax = side$lx + ge$knock[1] * W,
      ymin = side$ly - ge$knock[2] * s$gap, ymax = side$ly + ge$knock[2] * s$gap)
    if (tol > 0) p <- p + list(
      ggplot2::geom_rect(data = cbind(s$zones, key = zone_label), inherit.aes = FALSE,
        ggplot2::aes(xmin = -Inf, xmax = ge$zone_end, ymin = .data$ymin, ymax = .data$ymax,
                     fill = .data$key), alpha = st$alpha[["zone"]], key_glyph = key_zone),
      ggplot2::scale_fill_manual(values = stats::setNames(st$zone, zone_label), name = NULL))
    if (length(s$wm)) p <- p + lyr(ggplot2::annotate, "text", x = s$wm$x,
      y = s$wm$y, label = ceiling(s$wm$y), colour = st$watermark, fontface = "bold",
      size = st$pt[["watermark"]] * wm_scale / .pt)
    if (length(s$anchors)) p <- p + lyr(ggplot2::annotate, "label", x = ge$chip_x,
      y = s$anchors, label = s$anchors, colour = st$chip_ink, fill = st$chip_fill,
      fontface = "bold", size = st$pt[["chip"]] / .pt, .extra = no_border,
      label.padding = u(st$chip[["pad"]], "lines"), label.r = u(st$chip[["r"]], "lines"))

    # Data: contours, leaders, dots (white ring), turning-point halos, digits.
    p <- p + ggplot2::geom_line(linewidth = st$lw[["contour"]], lineend = "round",
                                linejoin = "round", na.rm = TRUE)
    if (nrow(lead)) p <- p + ggplot2::annotate("segment", x = lead$x0, y = lead$y0,
      xend = lead$x1, yend = lead$y1, colour = unname(tone_ink[lead$tone]),
      linewidth = st$lw[["leader"]], alpha = st$alpha[["leader"]])
    # Hollow dots (turning points not in the numeral) first, so a filled dot
    # with its digit always sits on top where two coincide.
    h <- s$hollow
    if (nrow(h)) p <- p +
      ggplot2::annotate("point", x = h$time, y = h$chao_val, colour = "white",
                        size = st$dot[["ring"]], stroke = st$dot[["stroke"]]) +
      ggplot2::annotate("point", x = h$time, y = h$chao_val, shape = 21, fill = "white",
                        colour = unname(tone_cols[h$tone]), size = st$dot[["fill"]],
                        stroke = st$dot[["hollow"]])
    if (nrow(d)) p <- p +
      ggplot2::geom_point(data = d, ggplot2::aes(.data$time, .data$chao_val), inherit.aes = FALSE,
                          colour = "white", size = st$dot[["ring"]], stroke = st$dot[["stroke"]]) +
      ggplot2::geom_point(data = d, ggplot2::aes(.data$time, .data$chao_val, colour = .data$tone),
                          inherit.aes = FALSE, size = st$dot[["fill"]], stroke = st$dot[["stroke"]])
    if (any_hollow) p <- p +       # legend key only; the data row is NA
      ggplot2::geom_point(data = data.frame(x = NA_real_, y = NA_real_, k = hollow_label),
        ggplot2::aes(.data$x, .data$y, shape = .data$k), inherit.aes = FALSE, na.rm = TRUE,
        colour = st$ink_soft, fill = "white", size = st$dot[["fill"]], stroke = st$dot[["hollow"]]) +
      ggplot2::scale_shape_manual(values = stats::setNames(21, hollow_label), name = NULL)
    if (nrow(mid)) p <- p + ggplot2::annotate("point", x = mid$lx, y = mid$ly, colour = "white",
      size = st$dot[["halo"]], stroke = st$dot[["stroke"]], alpha = st$alpha[["halo"]])
    if (nrow(d)) p <- p + lyr(ggplot2::annotate, "text", x = d$lx, y = d$ly,
      label = d$chao_digit, colour = unname(tone_ink[d$tone]),
      size = st$pt[["digit"]] / .pt, fontface = "bold")
    p + ggplot2::scale_colour_manual(values = tone_cols, limits = tone_levels, name = "Tone") +
      ggplot2::scale_x_continuous(breaks = 0:4 / 4, labels = as.character, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = seq(s$lo, s$hi), expand = c(0, 0)) +
      ggplot2::coord_cartesian(xlim = ge$xlim, clip = "off",
                               ylim = c(s$lo - pad[1] * s$gap, s$hi + pad[2] * s$gap)) +
      ggplot2::labs(title = s$title, subtitle = s$subtitle, x = "Normalised time",
                    y = "Chao scale") + theme_chao
  }

  # ---- Legend: built with the left panel, refitted until it fits the device ----
  with_legend <- function(p, rows = 1, stacked = FALSE) p +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1, nrow = rows, byrow = TRUE),
                    fill = ggplot2::guide_legend(order = 2),
                    shape = ggplot2::guide_legend(order = 3)) +
    ggplot2::theme(legend.position = "bottom",
                   legend.box = if (stacked) "vertical" else "horizontal")
  fixed <- function(x) if (length(x)) sum(x[grid::unitType(x) != "null"]) else u(0)
  box <- function(g) match("guide-box-bottom", g$layout$name)
  p <- lapply(specs, build_panel)
  g <- list(ref = ggplot2::ggplotGrob(with_legend(p$ref)), int = ggplot2::ggplotGrob(p$int))
  avail <- dev_in[1] * 72 - 2 * st$inset
  for (rows in seq_len(n_tones)) {      # too wide: zone key below, then more rows
    if (grid::convertWidth(fixed(g$ref$grobs[[box(g$ref)]]$widths), "pt", TRUE) <= avail) break
    g$ref <- ggplot2::ggplotGrob(with_legend(p$ref, rows, stacked = TRUE))
  }
  legend <- g$ref$grobs[[box(g$ref)]]   # lift it out and close its row
  g$ref$grobs[[box(g$ref)]] <- grid::nullGrob()
  g$ref$heights[g$ref$layout$t[box(g$ref)]] <- u(0)

  # ---- Assemble: tags, two exactly aligned panels, the legend row ----------------
  widths <- grid::unit.pmax(g$ref$widths, g$int$widths)
  heights <- grid::unit.pmax(g$ref$heights, g$int$heights)
  for (k in names(g)) { g[[k]]$widths <- widths; g[[k]]$heights <- heights }
  tag <- function() grid::rectGrob(x = u(st$inset), y = 0, just = c(0, 0), width =
    u(st$tag_pt[["w"]]), height = u(st$tag_pt[["h"]]), gp = grid::gpar(fill = st$tag, col = NA))
  legend_row <- gridExtra::arrangeGrob(grid::nullGrob(), legend, ncol = 2,
                                       widths = grid::unit.c(u(st$inset), u(1, "null")))
  gridExtra::arrangeGrob(tag(), tag(), g$ref, g$int, legend_row,
    layout_matrix = rbind(1:2, 3:4, 5), widths = u(c(1, 1), "null"),
    heights = grid::unit.c(u(st$tag_pt[["row"]]), u(1, "null"),
                           fixed(legend$heights) + u(st$legend_pad)))
}
