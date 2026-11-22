# global.R — sourced by Shiny into the global environment before each
# session. Define helpers here that the ui_*.R modules need to reach,
# since those modules are also defined in globalenv (via source(...)
# calls inside server.R) and cannot see definitions made in server.R's
# own sourcing environment.

# Disable Shiny's automatic loading of files in ./R. We're a package now,
# so Shiny would otherwise auto-source the package functions in addition
# to our explicit loop below — harmless duplication, but it triggers
# `warn_if_app_dir_is_package()` on every launch. We do the loading
# ourselves so we stay in control of order.
options(shiny.autoload.r = FALSE)

# ---------------------------------------------------------------------------
# Shared helper for smart selectInput defaults.
# guess_var(vars, patterns, fallback_idx) returns the first column in
# `vars` whose name matches any of the regex `patterns` (case-insensitive),
# falling back to vars[fallback_idx] (or vars[1]) if nothing matches.
# var_patterns is a registry of common role -> patterns so callers can do
#   selected = guess_var(vars, var_patterns$f0, 2)
# without repeating the pattern list across tabs.
# ---------------------------------------------------------------------------
guess_var <- function(vars, patterns, fallback_idx = 1) {
  for (pat in patterns) {
    m <- grep(pat, vars, ignore.case = TRUE, value = TRUE)
    if (length(m) > 0) return(m[1])
  }
  if (fallback_idx <= length(vars)) vars[fallback_idx] else vars[1]
}

# ---------------------------------------------------------------------------
# source_picker(): the selected entry of a tab's "Select dataset" menu. Keeps
# the user's current choice while it is still offered, but selects `prefer`
# (the curated + normalised data, the furthest-processed dataset) once per
# Normalise run that made it: `stamp` identifies the run (attr "run"), so a
# new run is picked up even if the user had switched away from an older one.
# Create one per menu, in the module body:
#   pick_dataset <- source_picker()
#   selected = pick_dataset(input$model_dataset, ds_choices, "normalised",
#                           stamp = attr(curated_norm_data(), "run"))
# ---------------------------------------------------------------------------
source_picker <- function(prefer = "curated_norm") {
  seen <- NULL
  function(current, choices, fallback, stamp = NULL) {
    has   <- prefer %in% choices
    fresh <- has && !identical(stamp, seen)
    if (has) seen <<- stamp
    if (fresh) prefer
    else if (!is.null(current) && current %in% choices) current
    else fallback
  }
}

# ---------------------------------------------------------------------------
# curated_reminder(): a note under a modelling tab's "Select dataset" menu
# while "Curated data" is chosen. Any normalised columns in it were computed
# before curation, so point to normalising the curated data (About page,
# path 5: Curate, then Normalise).
# ---------------------------------------------------------------------------
curated_reminder <- function(input_id) {
  conditionalPanel(sprintf("input.%s == 'curated'", input_id),
    tags$div(style = paste("font-size: 0.75rem; color: #6b5a1e; background: #fff8e1;",
                           "border-radius: 4px; padding: 5px 8px; margin: -6px 0 10px 0;"),
      icon("lightbulb"),
      HTML(paste(" Any normalised columns here were computed before curation. For analysis,",
                 "normalise the curated data (<strong>Normalise</strong> tab, data source",
                 "<strong>Curated data</strong>) and choose <strong>Curated + normalised data</strong>."))))
}

# ---------------------------------------------------------------------------
# read_data_code(): the lines a tab's "Show R code" uses to read the uploaded
# CSV the way raw_dataset() (server.R) does: any text encoding, Praat's
# --undefined-- as NA and 0 Hz f0 as NA. A plain read.csv() would keep the
# zeros the app dropped, so the snippet would not reproduce the app's plot
# or fit.
# ---------------------------------------------------------------------------
read_data_code <- function(ds_name) {
  c("# Read your data as the app does (adjust the path to where the file is):",
    "# any text encoding, Praat's --undefined-- as NA, and 0 Hz f0 as NA",
    paste0("dat <- shinytone::read_csv_utf8(", encodeString(ds_name, quote = '"'),
           ", stringsAsFactors = FALSE,"),
    '                                na.strings = c("NA", "--undefined--"))',
    "dat <- shinytone::zero_hz_to_na(dat)")
}

# ---------------------------------------------------------------------------
# tone_factor_code(): the "Show R code" line that makes tone column `var` a
# factor (ordered = TRUE: an ordered factor, named `to`) with the levels the
# app uses. as.factor() sorts by locale, which puts Chinese tone names out of
# order, so when shinytone::sort_tones() gives another order for `x` (the
# column as the app has it), the line uses sort_tones() too.
# ---------------------------------------------------------------------------
tone_needs_levels <- function(x) {
  !is.null(x) && !is.factor(x) &&
    !identical(sort_tones(x, natural = FALSE), levels(factor(x)))
}
tone_factor_code <- function(var, x = NULL, ordered = FALSE, to = var) {
  fun <- if (ordered) "ordered" else "factor"
  if (tone_needs_levels(x))
    paste0("dat$", to, " <- ", fun, "(dat$", var,
           ", levels = shinytone::sort_tones(dat$", var, ", natural = FALSE))")
  else paste0("dat$", to, " <- as.", fun, "(dat$", var, ")")
}

# ---------------------------------------------------------------------------
# role_var(): the default column for a role ("tone" or "speaker") in a tab's
# variable menus. Data from Curate carries the variables used there (attr
# "curate_vars"; the Normalise tab updates it for the curated + normalised
# data), e.g. a `cluster` tone, so curated data is analysed with the labels
# that were curated. Otherwise guess as usual.
# ---------------------------------------------------------------------------
role_var <- function(d, role, vars, patterns, fallback_idx = 1) {
  v <- attr(d, "curate_vars")[[role]]
  if (!is.null(v) && v %in% vars) v else guess_var(vars, patterns, fallback_idx)
}

# ---------------------------------------------------------------------------
# curated_key(): a fingerprint of the curated data that curated + normalised
# data depends on: which tokens are kept, their tone labels, speakers and Hz
# values (`cols`). Columns added by a later Normalise run on the base data do
# not change it. Character values, so the "convert to factor" option does not.
# ---------------------------------------------------------------------------
curated_key <- function(d, cols) {
  cols <- intersect(cols, names(d))
  rlang::hash(lapply(d[cols], as.character))
}

# ---------------------------------------------------------------------------
# nows(): render inline children with NO whitespace between them.
# htmltools writes each child of a tag on its own line, and the browser
# renders that newline as a space. So
#   tags$li("In ", tags$span(class = "tab-chip", "Start"), ", upload ...")
# comes out as "In Start , upload ...", with a gap before the comma. Wrapping
# the run in nows() concatenates the rendered HTML directly, so punctuation
# sits flush against the preceding chip / code / bold span:
#   tags$li(nows("In ", tags$span(class = "tab-chip", "Start"), ", upload ..."))
# Only needed where a tag is followed by punctuation; ordinary prose is fine.
# ---------------------------------------------------------------------------
nows <- function(...) {
  parts <- lapply(list(...), function(x) paste0(as.character(x), collapse = ""))
  htmltools::HTML(paste0(unlist(parts), collapse = ""))
}

# ---------------------------------------------------------------------------
# Bulk-discard offer.
# When a corpus is large but only a small share of its tokens is flagged,
# repairing every flagged token one by one is rarely worth the time; the
# F0 Correction tab's "Discard all flagged tokens" + review loop is the
# faster route. The Inspect summary and the F0 Correction flagged-CSV
# loader both call offer_bulk_triage() to decide when to surface that
# suggestion, so the two tabs stay in sync on the thresholds.
# ---------------------------------------------------------------------------
TRIAGE_OFFER_MIN_TOKENS       <- 1000
TRIAGE_OFFER_MAX_FLAGGED_PROP <- 0.15
offer_bulk_triage <- function(n_tokens, n_flagged) {
  isTRUE(n_tokens >= TRIAGE_OFFER_MIN_TOKENS) &&
    isTRUE(n_flagged > 0) &&
    isTRUE(n_flagged / n_tokens <= TRIAGE_OFFER_MAX_FLAGGED_PROP)
}

var_patterns <- list(
  token    = c("^token$", "^token", "^filename", "^basename", "^item$", "^segment", "^id$"),
  f0       = c("^f0_st$", "^f0_zscore$", "^f0_lz$", "^f0_norm", "^f0_hz$", "^f0$", "^f0_", "^pitch"),
  time     = c("^time$", "^time", "^t$", "^timepoint", "^measurement"),
  speaker  = c("^speaker$", "^speaker", "^spk", "^subject", "^subj"),
  tone     = c("^tone$", "^tone", "^category", "^tonecat", "^label"),
  item     = c("^item$", "^word", "^syllable", "^syl"),
  duration = c("^duration$", "^dur")
)

# Normalise a join key so that e.g. "S01_T1.wav" and "s01_t1" match: optionally
# strip a file extension, then lowercase and trim. Used by the metadata joins
# in both the F0 Extraction tab and the Start tab.
make_token_key <- function(x, strip_ext = TRUE) {
  k <- as.character(x)
  if (isTRUE(strip_ext)) k <- tools::file_path_sans_ext(k)
  tolower(trimws(k))
}

# ---------------------------------------------------------------------------
# Shortest audio duration (seconds) the pitch analyser can return a frame for.
# A pitch-floor autocorrelation window spans 3 periods of the floor, so a clip
# shorter than 3 / f0_min yields no f0 (and, being even shorter than the wider
# intensity window, no intensity either). Used to flag/skip too-short files in
# the F0 Processing Start preview and the wrassp extraction loop.
# ---------------------------------------------------------------------------
min_audio_dur <- function(f0_min = 75) {
  f0_min <- suppressWarnings(as.numeric(f0_min)[1])
  if (length(f0_min) == 0 || is.na(f0_min) || f0_min <= 0) f0_min <- 75
  3 / f0_min
}

# ---------------------------------------------------------------------------
# Landmark sets in a column-name vector. A "set" is a base name X for which
# both X_start and X_end exist (as written by the F0 Extraction landmark step,
# e.g. syllable_start / syllable_end). X_i (segment index) is optional. Returns
# a named list base -> list(start, end, idx, has_i). Used by the Visualise tab
# to offer landmark-based time alignment.
# ---------------------------------------------------------------------------
landmark_sets <- function(vars) {
  out <- list()
  for (s in grep("_start$", vars, value = TRUE)) {
    base <- sub("_start$", "", s)
    e <- paste0(base, "_end")
    if (nzchar(base) && e %in% vars) {
      out[[base]] <- list(start = s, end = e,
                          idx = paste0(base, "_i"),
                          has_i = paste0(base, "_i") %in% vars)
    }
  }
  out
}

# First column name matching a token-like pattern, or NULL if none. Used to
# connect points into per-token contour lines when aligning by landmarks.
vis_token_col <- function(vars) {
  for (pat in var_patterns$token) {
    m <- grep(pat, vars, ignore.case = TRUE, value = TRUE)
    if (length(m)) return(m[1])
  }
  NULL
}

# Smart default for a model's Time variable: prefer a landmark sequential-time
# column (<tier>_tseq) when one exists, so multisyllabic data lands on the
# syllable-aligned axis automatically; otherwise fall back to the usual time
# guess. (Not the within-segment <tier>_t01, which is non-monotonic and invalid
# as a model time axis.)
guess_time_var <- function(vars, fallback_idx = 1) {
  tseq <- grep("_tseq$", vars, value = TRUE)
  if (length(tseq)) return(tseq[1])
  guess_var(vars, var_patterns$time, fallback_idx)
}

# ---------------------------------------------------------------------------
# Pretty y-axis label for an f0 column. Maps the column *name* to a typeset
# label with a subscript zero (f0 -> f₀):
#   *_hz / raw f0 -> "f₀ (Hz)"        *_st / semitone -> "f₀ (semitone)"
#   *_zscore, *_lz -> "normalised f₀"  other *_norm    -> "normalised f₀"
# Anything unrecognised falls back to the column name unchanged, so a custom
# column still gets a sensible axis title. Shared by the Visualise and Curate
# plots so both label the f0 axis consistently.
# ---------------------------------------------------------------------------
f0_axis_label <- function(col) {
  if (is.null(col) || !nzchar(col)) return("f₀")
  c0 <- tolower(col)
  if (grepl("zscore|z[._-]?score|_lz$", c0)) return("normalised f₀")
  if (grepl("semitone|_st$|_st[._]", c0))  return("f₀ (st)")
  if (grepl("hz", c0))                     return("f₀ (Hz)")
  if (grepl("norm", c0))                   return("normalised f₀")
  if (grepl("^f_?0$|^pitch", c0))          return("f₀ (Hz)")
  col
}

# ---------------------------------------------------------------------------
# Foldable guide box. Wraps a tab's explanatory guide in a native
# <details>/<summary> element so the (often long) guide can be collapsed to
# free up room for plots and output. Open by default. The visual styling
# lives in ui.R (selector `details.guide-box`).
#   guide_box("Inspect guide", tags$ul(...), tags$p(...))
# ---------------------------------------------------------------------------
guide_box <- function(title, ..., open = TRUE) {
  tags$details(
    class = "guide-box",
    open  = if (isTRUE(open)) NA else NULL,
    tags$summary(class = "guide-summary", style = "font-weight: 700;", title),
    tags$div(class = "guide-body", ...)
  )
}

# ---------------------------------------------------------------------------
# Chinese characters in static plots.
# renderPlot() and ggsave() draw with ragg, one font family per text element.
# The app font (Open Sans) has no Chinese glyphs, and the system fallback can
# pick a font without simplified characters (on macOS 阴 and 阳 vanish, so
# 阴平 and 阳平 both read 平). So when a plot's text contains CJK, that plot
# switches to a font that has it:
#   1. a font file in the app's cjk-font/ folder, if there is one (none is
#      bundled now; see cjk-font/README.md). A file with "bold" in its name
#      is used for bold text.
#   2. else the first installed family in .cjk_candidates that has both
#      simplified and traditional characters (checked glyph by glyph).
# Plots whose text has no CJK are left exactly as they were. If no font is
# found, one notification per session says so. Interactive (plotly) plots
# draw text in the browser and need none of this.
#   p + cjk_family_theme(p)    # a ggplot: add after any complete theme
#   label_family(tones)        # a family name, or "" (no CJK, or no font)
# ---------------------------------------------------------------------------
.cjk_font_dir <- file.path(getwd(), "cjk-font")   # global.R runs with the app dir as wd
# Glyphs a font must have: 阴阳发调 (simplified), 陰陽發調 (traditional), 平上去入声聲
.cjk_probe <- "阴阳发调陰陽發調平上去入声聲"
# Installed families to try, in order. Never "PingFang SC": FreeType cannot
# open its file on macOS. Names are checked against system_fonts() first,
# because match_fonts() can hang on a name the system does not have.
.cjk_candidates <- c(
  "Noto Sans CJK SC", "Noto Sans SC", "Source Han Sans SC", "Source Han Sans CN",
  "Hiragino Sans GB", "Microsoft YaHei", "WenQuanYi Zen Hei", "WenQuanYi Micro Hei",
  "Droid Sans Fallback", "Noto Sans CJK TC", "AR PL UMing CN", "AR PL SungtiL GB",
  "Songti SC", "SimHei", "Arial Unicode MS")

# Character values in x: vectors, factors (the levels in use), nested lists,
# plotmath expressions. Numbers, functions and waivers give nothing.
.text_values <- function(x) {
  if (is.null(x) || is.function(x) || is.environment(x)) return(character(0))
  if (is.factor(x)) return(as.character(unique(x)))
  if (is.character(x)) return(unique(x))
  if (is.expression(x)) return(as.character(x))
  if (is.language(x)) return(paste(deparse(x), collapse = " "))
  if (is.list(x)) return(unlist(lapply(unclass(x), .text_values), use.names = FALSE))
  character(0)
}

# TRUE if any text in x has a CJK character. Code points, not a regex class:
# a \x{3000} class errors in PCRE when R is not in UTF-8 mode.
has_cjk <- function(x) {
  x <- .text_values(x)
  x <- enc2utf8(x[!is.na(x) & nzchar(x)])
  x <- x[validUTF8(x)]
  if (!length(x)) return(FALSE)
  cp <- utf8ToInt(paste(x, collapse = ""))
  any((cp >= 0x2E80 & cp <= 0x9FFF) |    # radicals, CJK punctuation, kana, ideographs
      (cp >= 0xAC00 & cp <= 0xD7AF) |    # Hangul syllables
      (cp >= 0xF900 & cp <= 0xFAFF) |    # compatibility ideographs
      (cp >= 0xFF00 & cp <= 0xFFEF) |    # full-width forms
      (cp >= 0x20000 & cp <= 0x3134F))   # ideograph extensions B to G
}

# The CJK family to use, or "". Not cached: cjk_family() caches it.
.find_cjk_family <- function(dir = .cjk_font_dir, candidates = .cjk_candidates,
                             probe = .cjk_probe) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) return("")
  covers <- function(path, index = 0L) {
    gi <- tryCatch(systemfonts::glyph_info(probe, path = path, index = index),
                   error = function(e) NULL)
    !is.null(gi) && nrow(gi) > 0 && all(gi$index > 0)
  }
  # 1. A font file dropped into cjk-font/
  files <- list.files(dir, pattern = "\\.(otf|ttf|ttc)$", ignore.case = TRUE,
                      full.names = TRUE)
  if (length(files)) {
    bold  <- grepl("bold", basename(files), ignore.case = TRUE)
    slant <- grepl("italic|oblique", basename(files), ignore.case = TRUE)
    plain <- Filter(covers, files[!bold & !slant])
    if (length(plain)) {
      bold <- c(Filter(covers, files[bold & !slant]), plain)[1]
      fam <- "shinytone CJK"
      if (!fam %in% systemfonts::registry_fonts()$family)
        systemfonts::register_font(fam, plain = plain[1], bold = bold)
      return(fam)
    }
  }
  # 2. An installed family
  have <- tryCatch(unique(systemfonts::system_fonts()$family),
                   error = function(e) character(0))
  for (f in intersect(candidates, have)) {
    m <- tryCatch(systemfonts::match_fonts(f), error = function(e) NULL)
    if (!is.null(m) && nrow(m) && covers(m$path[1], m$index[1])) return(f)
  }
  ""
}

# Cached for the R process: the font lookup runs once, the first time a plot
# has CJK text.
cjk_family <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- tryCatch(.find_cjk_family(), error = function(e) "")
    cache
  }
})

# One notification per session when CJK text has no font. Silent outside a
# session.
cjk_font_notice <- function() {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) return(invisible(FALSE))
  tryCatch({
    ud <- session$userData
    if (!is.environment(ud) || isTRUE(ud$cjk_font_notice)) return(invisible(FALSE))
    ud$cjk_font_notice <- TRUE
    shiny::showNotification(
      paste("No Chinese font was found on this server, so static plots may drop",
            "Chinese characters. Interactive plots are unaffected."),
      type = "warning", duration = 15, session = session)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

# The family for text with these labels: a CJK family when they have CJK
# (and one is found), else "" (leave the family alone).
label_family <- function(...) {
  if (!has_cjk(list(...))) return("")
  fam <- cjk_family()
  if (!nzchar(fam)) cjk_font_notice()
  fam
}

# The text a ggplot can show: its labels, the text values of its mappings
# (except `group`) and facets, text-layer labels and scale names and labels.
.plot_text <- function(p) {
  value_of <- function(q, d, name = TRUE) {
    e <- rlang::quo_get_expr(q)
    nm <- NULL                                  # a default axis or legend title
    if (name && is.symbol(e)) nm <- as.character(e)
    if (name && is.call(e) && length(e) == 3 && identical(e[[2]], quote(.data))) {
      nm <- if (identical(e[[1]], quote(`$`))) as.character(e[[3]])
            else tryCatch(rlang::eval_tidy(e[[3]], env = rlang::quo_get_env(q)),
                          error = function(err) NULL)
    }
    v <- if (is.data.frame(d))
      tryCatch(rlang::eval_tidy(q, data = d), error = function(err) NULL)
    c(.text_values(nm), if (is.character(v) || is.factor(v)) .text_values(v))
  }
  maps <- function(m, d) lapply(m[setdiff(names(m), "group")], value_of, d = d)
  dat <- p$data
  out <- list(p$labels, maps(p$mapping, dat))
  for (l in p$layers) {
    ld <- if (is.data.frame(l$data)) l$data else dat
    out <- c(out, list(maps(l$mapping, ld), l$aes_params$label))
    if (isTRUE(l$inherit.aes) && is.data.frame(l$data)) out <- c(out, list(maps(p$mapping, ld)))
  }
  fp <- p$facet$params
  for (q in c(fp$facets, fp$rows, fp$cols)) out <- c(out, list(value_of(q, dat, name = FALSE)))
  for (s in p$scales$scales) out <- c(out, list(s$name, s$labels, s$limits))
  .text_values(out)
}

# Theme that switches a ggplot to the CJK family when its text has CJK; NULL
# (no change) otherwise. Add it after any complete theme such as theme_bw(),
# which would reset it. With ggplot2 >= 4 it also sets the family of text
# geoms (annotate(), geom_text()); thematic overrides that in renderPlot, so
# pass `family` to such layers too.
cjk_family_theme <- function(p) {
  txt <- tryCatch(.plot_text(p), error = function(e) character(0))
  fam <- label_family(txt)
  if (!nzchar(fam)) return(NULL)
  th <- ggplot2::theme(text = ggplot2::element_text(family = fam))
  if (utils::packageVersion("ggplot2") >= "4.0.0")
    th <- th + ggplot2::theme(geom = ggplot2::element_geom(family = fam))
  th
}

# ---------------------------------------------------------------------------
# Make the package's analytical functions (normalise_f0, fit_gca, ...)
# reachable from the running app. Three launch paths to support:
#
#  1. `shinytone::run_app()` after the package is installed
#       -> library(shinytone) attaches the exported functions.
#  2. `shiny::shinyAppDir("inst/app")` from the repo root (also the
#     shinyapps.io case — the whole repo gets deployed)
#       -> source ../../R/*.R  (the package source tree).
#  3. `shiny::runApp("inst/app")` started directly from inside inst/app
#       -> same as #2; CWD is inst/app, so ../../R resolves.
# ---------------------------------------------------------------------------
# Three cases, in priority order:
#  1. The package namespace is already loaded (e.g. via devtools::load_all
#     during development, or already library()'d in this session). Just
#     attach it and stop — sourcing the files again would create
#     globalenv copies that mask the namespace ones and trigger conflict
#     warnings.
#  2. We're sitting in the package source tree (dev repo, or shinyapps.io
#     deploy bundling the whole repo). Source ../../R/*.R into globalenv.
#  3. We're running from an installed copy with no source tree nearby
#     (e.g. shinytone::run_app() after install). library() it.
if ("shinytone" %in% loadedNamespaces()) {
  if (!"package:shinytone" %in% search()) {
    suppressPackageStartupMessages(library(shinytone))
  }
} else {
  pkg_r <- if (dir.exists("../../R") && file.exists("../../DESCRIPTION")) {
    "../../R"
  } else if (dir.exists("R") && file.exists("DESCRIPTION")) {
    "R"
  } else {
    NULL
  }

  if (!is.null(pkg_r)) {
    for (.f in list.files(pkg_r, pattern = "\\.R$", full.names = TRUE)) {
      source(.f, local = FALSE)
    }
    rm(.f)
  } else if (requireNamespace("shinytone", quietly = TRUE)) {
    suppressPackageStartupMessages(library(shinytone))
  } else {
    stop("shinytone package functions not found. ",
         "Either install the shinytone package, or run the app from a ",
         "checkout of the source repository.", call. = FALSE)
  }
}
