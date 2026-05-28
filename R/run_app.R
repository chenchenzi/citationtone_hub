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
  app_deps <- c("bslib", "DT", "emmeans", "ggplot2", "gridExtra",
                "lme4", "mgcv", "plotly", "praatpicture", "RColorBrewer",
                "rPraat", "thematic", "tuneR")
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
  shiny::runApp(app_dir, ...)
}
