# Entry point for shinyapps.io and for RStudio's "Run App" button.
# The actual Shiny app lives in inst/app/. This thin shim lets the
# whole repo deploy as one unit (the package source tree + the bundled
# app) and keeps a single source of truth for the UI.
#
# Locally installed users should prefer:  shinytone::run_app()

shiny::shinyAppDir("inst/app")
