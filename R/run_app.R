#' Launch the Shinytone app locally
#'
#' Boots the bundled Shiny application that ships inside the installed
#' package. Equivalent to the hosted version at
#' <https://chenzixu.shinyapps.io/shinytone/> but running on your own
#' machine, so it has no file-size, memory, or session limits and your
#' audio / CSV files never leave your computer.
#'
#' @param ... Additional arguments forwarded to [shiny::runApp()] — e.g.
#'   `port = 4321`, `launch.browser = FALSE`, `host = "0.0.0.0"`.
#'
#' @details
#' GitHub-installed packages have no `update.packages()` channel, so on launch
#' `run_app()` quietly checks the package repository for a newer shinytone
#' release (2-second timeout; skipped silently when offline). If one exists, a
#' reminder with the update command is printed to the console and shown once
#' inside the app. Disable the check with
#' `options(shinytone.check_updates = FALSE)`.
#'
#' @return Does not return; runs until the app is stopped.
#'
#' @examples
#' \dontrun{
#'   shinytone::run_app()
#'   shinytone::run_app(port = 4321, launch.browser = FALSE)
#' }
#'
#' @export
run_app <- function(...) {
  # The bundled Shiny app uses a richer set of packages than the bare
  # programmatic API needs. To keep `install.packages("shinytone")`
  # lightweight, those packages live in `Suggests:` rather than
  # `Imports:`. Check that they are present before booting the app, and
  # offer to install them if not.
  app_deps <- c("bslib", "cluster", "DT", "emmeans", "ggplot2", "gridExtra",
                "lme4", "mclust", "mgcv", "plotly", "praatpicture", "RColorBrewer",
                "rPraat", "thematic", "tuneR", "uwot", "wrassp")
  missing_deps <- app_deps[!vapply(app_deps, requireNamespace,
                                    logical(1), quietly = TRUE)]
  if (length(missing_deps) > 0) {
    msg <- paste0(
      "The Shinytone app needs these packages, which are not installed:\n  ",
      paste(missing_deps, collapse = ", "), "\n"
    )
    if (interactive()) {
      message(msg)
      ans <- utils::menu(c("Install them now", "Cancel"),
                         title = "Install missing app dependencies?")
      if (ans == 1) {
        utils::install.packages(missing_deps)
        still_missing <- missing_deps[!vapply(missing_deps,
                                              requireNamespace,
                                              logical(1), quietly = TRUE)]
        if (length(still_missing) > 0) {
          stop("Installation did not resolve all missing packages: ",
               paste(still_missing, collapse = ", "), call. = FALSE)
        }
      } else {
        stop("Cannot launch the Shinytone app without these packages.",
             call. = FALSE)
      }
    } else {
      stop(msg, "Install them with `install.packages(c(",
           paste(sprintf('\"%s\"', missing_deps), collapse = ", "),
           "))`.", call. = FALSE)
    }
  }

  app_dir <- system.file("app", package = "shinytone")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Bundled Shinytone app not found. Reinstall the package with ",
         "`remotes::install_github(\"chenchenzi/citationtone_hub\")` and ",
         "try again.", call. = FALSE)
  }

  # One quiet update check per launch (see @details). Sets an option the app's
  # server reads to show an in-app notification; cleared when up to date so a
  # stale value never survives an update within the same R session.
  upd <- check_for_update()
  if (!is.null(upd)) {
    message(
      "shinytone ", upd, " is available (you have ",
      utils::packageVersion("shinytone"), "). Update with:\n",
      "  remotes::install_github(\"chenchenzi/citationtone_hub\")\n",
      "Disable this check with options(shinytone.check_updates = FALSE).")
    options(shinytone.update_available = as.character(upd))
  } else {
    options(shinytone.update_available = NULL)
  }

  shiny::runApp(app_dir, ...)
}

# Latest shinytone version on GitHub (parsed from the DESCRIPTION on the
# default branch) when it is newer than the installed one, else NULL. Also
# NULL when offline, opted out via options(shinytone.check_updates = FALSE),
# or on any parse problem. Never signals a condition; capped by `timeout_s`.
check_for_update <- function(timeout_s = 2) {
  if (!isTRUE(getOption("shinytone.check_updates", TRUE))) return(NULL)
  desc_url <- paste0("https://raw.githubusercontent.com/",
                     "chenchenzi/citationtone_hub/main/DESCRIPTION")
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout_s)
  lines <- tryCatch(suppressWarnings(readLines(desc_url, n = 25L, warn = FALSE)),
                    error = function(e) NULL)
  if (is.null(lines)) return(NULL)
  vline <- grep("^Version:", lines, value = TRUE)
  if (length(vline) != 1L) return(NULL)
  remote <- trimws(sub("^Version:", "", vline[1]))
  newer <- tryCatch(
    package_version(remote) > utils::packageVersion("shinytone"),
    error = function(e) FALSE)
  if (isTRUE(newer)) remote else NULL
}
