# global.R — sourced by Shiny into the global environment before each
# session. Define helpers here that the ui_*.R modules need to reach,
# since those modules are also defined in globalenv (via source(...)
# calls inside server.R) and cannot see definitions made in server.R's
# own sourcing environment.

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
