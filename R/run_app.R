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
  app_dir <- system.file("app", package = "shinytone")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Bundled Shinytone app not found. Reinstall the package with ",
         "`remotes::install_github(\"chenchenzi/citationtone_hub\")` and ",
         "try again.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
