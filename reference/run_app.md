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

## Examples

``` r
if (FALSE) { # \dontrun{
  shinytone::run_app()
  shinytone::run_app(port = 4321, launch.browser = FALSE)
} # }
```
