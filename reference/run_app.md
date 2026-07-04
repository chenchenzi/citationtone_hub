# Launch the Shinytone app locally

Boots the bundled Shiny application that ships inside the installed
package. Equivalent to the hosted version at
<https://chenzixu.shinyapps.io/shinytone/> but running on your own
machine, so it has no file-size, memory, or session limits and your
audio / CSV files never leave your computer.

## Usage

``` r
run_app(...)
```

## Arguments

- ...:

  Additional arguments forwarded to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) — e.g.
  `port = 4321`, `launch.browser = FALSE`, `host = "0.0.0.0"`.

## Value

Does not return; runs until the app is stopped.

## Details

GitHub-installed packages have no
[`update.packages()`](https://rdrr.io/r/utils/update.packages.html)
channel, so on launch `run_app()` quietly checks the package repository
for a newer shinytone release (2-second timeout; skipped silently when
offline). If one exists, a reminder with the update command is printed
to the console and shown once inside the app. Disable the check with
`options(shinytone.check_updates = FALSE)`.

## Examples

``` r
if (FALSE) { # \dontrun{
  shinytone::run_app()
  shinytone::run_app(port = 4321, launch.browser = FALSE)
} # }
```
