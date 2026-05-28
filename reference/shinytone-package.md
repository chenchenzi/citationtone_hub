# shinytone: A Citation Tone Research Hub

An interactive Shiny application and accompanying R functions for
citation tone research across tone languages. Integrates the full
workflow of pitch extraction, by-speaker f0 normalisation, growth-curve
and generalised additive mixed modelling, outlier inspection, and Chao
tone numeral summarisation.

## Online app

The hosted version of the app is available at
<https://chenzixu.shinyapps.io/shinytone/>. A local launcher
([`run_app()`](https://chenchenzi.github.io/citationtone_hub/reference/run_app.md))
will be added in a future release so the same UI can be served offline
once the package is installed.

## Programmatic API

Standalone analytical functions
([`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md),
[`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md),
[`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md),
[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md),
...) are being extracted from the Shiny app in stages. See `NEWS.md` for
the current state.

## See also

Useful links:

- <https://chenchenzi.github.io/citationtone_hub/>

- <https://github.com/chenchenzi/citationtone_hub>

- <https://chenzixu.shinyapps.io/shinytone/>

- Report bugs at <https://github.com/chenchenzi/citationtone_hub/issues>

## Author

**Maintainer**: Chenzi Xu <chenzi.xu@ntu.edu.sg>

Authors:

- Chenzi Xu <chenzi.xu@ntu.edu.sg>

- Cong Zhang
