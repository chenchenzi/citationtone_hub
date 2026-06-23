# Sonify an f0 contour

Render a fundamental-frequency contour as an audible waveform: an
oscillator whose instantaneous pitch follows `f0_hz`. Useful for
*listening* to a mean tone contour (e.g. a cluster prototype) rather
than only seeing it.

## Usage

``` r
sonify_f0(
  f0_hz,
  fs = 16000,
  dur = 0.7,
  source = c("tone", "complex", "vowel"),
  vowel = c("a", "i", "u"),
  n_harmonics = 12L,
  rolloff = 0.7,
  fade = 0.02,
  amp = 0.9,
  intensity = NULL,
  dyn_range = 30
)
```

## Arguments

- f0_hz:

  Numeric vector of f0 values in Hz (the contour to play). Leading and
  trailing `NA`s are trimmed; internal `NA`s are interpolated.

- fs:

  Output sample rate (Hz). Default 16000.

- dur:

  Playback duration in seconds. Default 0.7.

- source:

  `"tone"` for a pure sine, `"complex"` for a sine plus harmonics with
  amplitude roll-off (warmer), or `"vowel"` for a source-filter
  synthesised vowel (glottal source through formant resonators). The
  pitch is identical across all three; only the timbre differs.

- vowel:

  Vowel quality for `source = "vowel"`: `"a"`, `"i"` or `"u"` (each is a
  formant set).

- n_harmonics:

  Number of harmonics for `source = "complex"`.

- rolloff:

  Per-harmonic amplitude factor for `"complex"` (0-1).

- fade:

  Raised-cosine fade in/out duration (s), to avoid clicks.

- amp:

  Peak amplitude (0-1).

- intensity:

  Optional per-frame intensity (dB), the same length as `f0_hz`. When
  supplied, the rendered waveform's loudness follows it
  (`dB -> linear amplitude`), so the tone swells and fades like the
  original syllable. `NULL` (default) gives a flat amplitude.

- dyn_range:

  Loudness range (dB) when `intensity` is used: frames more than this
  far below the peak are floored, so quiet frames attenuate without
  becoming inaudible. Default 30.

## Value

A 16-bit mono [`Wave`](https://rdrr.io/pkg/tuneR/man/Wave.html) object.

## Details

The contour has no inherent duration, so it is time-scaled to `dur`
seconds. Pitch is rendered by phase accumulation, so the glide is
smooth.

## Examples

``` r
if (FALSE) { # \dontrun{
w <- sonify_f0(c(120, 140, 160, 150, 130), dur = 0.6, source = "complex")
tuneR::play(w)
} # }
```
