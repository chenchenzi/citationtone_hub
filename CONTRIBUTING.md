# Contributing to shinytone

Thanks for your interest in helping improve shinytone! This document
outlines a few practical guidelines for filing issues and contributing
code or documentation.

## Filing issues

The best place to report bugs or suggest features is the GitHub issue
tracker:

<https://github.com/chenchenzi/citationtone_hub/issues>

When filing a bug report, please include:

- A short reproducible example, ideally using the bundled
  `data(sample_f0)` dataset so we can run it without your data.
- The output of `sessionInfo()` (or `devtools::session_info()`).
- The exact error message or behaviour you saw, and what you expected.

For feature requests, briefly describe the use case and (if possible)
how the feature would fit into the existing API.

## Contributing code

Small fixes (typos, doc clarifications, obvious bugs) are welcome as
direct pull requests. For larger changes, please open an issue first
to discuss the approach before investing significant time.

The development workflow is the standard R-package one:

1. Fork the repo and clone your fork.
2. Create a feature branch off `main`.
3. Make your changes.
4. Run `devtools::document()` (regenerate `man/`) and
   `devtools::test()` (run all unit tests).
5. Run `devtools::check()` and aim for **0 errors / 0 warnings**.
   (One persistent NOTE about "unused Imports" is expected and
   ignorable; it relates to the bundled Shiny app.)
6. Open a pull request against `main` with a clear description of what
   changed and why.

### Style notes

- R code: follow the [tidyverse style guide](https://style.tidyverse.org/).
- Function docs use roxygen2 markdown (`Roxygen: list(markdown = TRUE)`
  is already set in `DESCRIPTION`).
- Tests live in `tests/testthat/` and use the testthat 3 edition.
- Keep new exported functions consistent with the existing API:
  long-format data in, named column arguments, defaults that match
  the bundled `sample_f0` schema where reasonable.

## Adding to the Shiny app

The Shiny app lives in `inst/app/`. UI module files are organised under
`inst/app/ui/`. When extending a tab, please:

- Keep analytical logic in a function under `R/`, not inline in the
  observer, so it can be tested independently.
- Use the existing `guess_var()` helper for smart `selectInput`
  defaults (see `inst/app/global.R`).
- Run the app with `shinytone::run_app()` and confirm the affected
  tab still works end to end with `data(sample_f0)`.

## Documentation

Function documentation is generated from roxygen2 comments above each
function in `R/`. To regenerate the help files after editing a doc
block, run `devtools::document()`.

The pkgdown docs site rebuilds automatically on every push to `main`
via GitHub Actions. To preview locally, run `pkgdown::build_site()`.

## Code of conduct

Please note that contributions are subject to the
[Code of Conduct](CODE_OF_CONDUCT.md).
