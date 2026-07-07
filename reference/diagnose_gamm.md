# Diagnose a fitted GAMM

Extracts the standard model-checking diagnostics for a fitted
[`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
object, packaged as plain numeric tables and data frames so a caller
(e.g. the Shiny app) can render them however it likes. Mirrors what
[`mgcv::gam.check()`](https://rdrr.io/pkg/mgcv/man/gam.check.html)
reports, but returns the pieces as data rather than drawing
base-graphics plots.

## Usage

``` r
diagnose_gamm(gamm_obj)
```

## Arguments

- gamm_obj:

  An object of class `"shinytone_gamm"` from
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md).

## Value

An S3 object of class `"shinytone_gamm_diag"`, a list with:

- `resid_df`: data frame with `residual` (deviance), `fitted`, and
  `observed`.

- `k_check`: data frame version of
  [`mgcv::k.check()`](https://rdrr.io/pkg/mgcv/man/k.check.html) with a
  `Smooth` column and a `k_flag` column (`"ok"`, `"low"`, or `"na"`), or
  `NULL` if it could not be computed.

- `concurvity`: data frame version of
  [`mgcv::concurvity()`](https://rdrr.io/pkg/mgcv/man/concurvity.html),
  or `NULL`.

- `acf`: data frame with `lag` and `acf` (per token; AR1-whitened when
  the fit used an AR1 correction).

- `acf_ci`: half-width of the white-noise confidence band.

- `acf_grouped`, `acf_whitened`: logicals recording whether the ACF was
  computed per token and whether it was AR1-whitened.

- `n`: number of residuals.

- `rho`, `use_ar1`: AR1 information carried over from the fit.

- `family`: the model's error family.

## Details

The returned diagnostics answer the three questions that decide whether
a GAMM fit can be trusted:

- **Are the residuals well-behaved?** `resid_df` holds the deviance
  residuals, fitted values, and observed response, enough to draw a Q-Q
  plot, a residuals-vs-fitted plot, a residual histogram, and an
  observed-vs-fitted plot.

- **Is the basis dimension `k` large enough?** `k_check` is
  [`mgcv::k.check()`](https://rdrr.io/pkg/mgcv/man/k.check.html)'s table
  (k', edf, k-index, p-value) with the term names mapped back to the
  user's columns. A `k_flag` column marks the spline smooths where a low
  k-index together with a small p-value suggests `k` is too low and the
  model should be refitted with a larger `k`. Random-effect terms
  (by-speaker / by-item) are flagged `"na"` because the k-index is not a
  meaningful check for them.

- **Is there leftover temporal autocorrelation?** `acf` is the lag /
  autocorrelation of the residuals with the white-noise band in
  `acf_ci`. Autocorrelation decaying slowly across lags is the signal
  that an AR1 correction (`use_ar1 = TRUE` in
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md))
  is worth turning on.

`concurvity` is
[`mgcv::concurvity()`](https://rdrr.io/pkg/mgcv/man/concurvity.html)'s
full table (0-1, higher means more confounding between smooths) when it
can be computed.

The ACF is computed **per token** (the residuals are split by token and
the within-token lag products are pooled, in the spirit of
`itsadug::acf_resid()`) so that token boundaries do not contaminate the
low lags — important because f0 tokens are short. When the fit used an
AR1 correction the residuals are first whitened within each token
(`e_i - rho * e_{i-1}`, reset at each token start), so the ACF then
shows whether the AR1 term actually removed the autocorrelation rather
than the pre-correction picture. `acf_grouped` / `acf_whitened` record
which path was taken (a plain concatenated ACF is used only as a
fallback if residuals cannot be aligned to tokens).

## References

Wood, S. N. (2017). *Generalized Additive Models: An Introduction with
R* (2nd ed.). Chapman and Hall/CRC.

## See also

[`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
for the model fit,
[`mgcv::gam.check()`](https://rdrr.io/pkg/mgcv/man/gam.check.html) and
[`mgcv::k.check()`](https://rdrr.io/pkg/mgcv/man/k.check.html) for the
underlying diagnostics.

## Examples

``` r
if (FALSE) { # \dontrun{
data(sample_f0)
normed <- normalise_f0(sample_f0, f0 = "f0_Hz",
                       speaker = "speaker", tone = "tone")
gamm <- fit_gamm(normed, f0 = "f0_st", time = "time", token = "token",
                 tone = "tone", speaker = "speaker", item = "char")
diag <- diagnose_gamm(gamm)
head(diag$k_check)
} # }
```
