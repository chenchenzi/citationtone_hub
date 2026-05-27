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

var_patterns <- list(
  token    = c("^token$", "^token", "^filename", "^basename", "^item$", "^segment", "^id$"),
  f0       = c("^f0_st$", "^f0_zscore$", "^f0_norm", "^f0_hz$", "^f0$", "^f0_", "^pitch"),
  time     = c("^time$", "^time", "^t$", "^timepoint", "^measurement"),
  speaker  = c("^speaker$", "^speaker", "^spk", "^subject", "^subj"),
  tone     = c("^tone$", "^tone", "^category", "^tonecat", "^label"),
  item     = c("^item$", "^word", "^syllable", "^syl"),
  duration = c("^duration$", "^dur")
)

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
