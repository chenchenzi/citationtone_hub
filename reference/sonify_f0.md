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

All three timbres share one pitch generator and differ only in spectral
shape:

1.  **Contour conditioning.** Leading/trailing `NA`s are trimmed and
    internal gaps linearly interpolated; the contour is then resampled
    to `round(fs * dur)` samples and clamped to 40-2000 Hz.

2.  **Pitch by phase accumulation.** The running phase is
    `phase = cumsum(2 * pi * f0 / fs)`, so `sin(phase)` has an
    instantaneous frequency equal to the contour at every sample; the
    pitch glides continuously with no discontinuities.

3.  **Source waveform.** `"tone"` is the bare sine `sin(phase)`.
    `"complex"` sums harmonics with a geometric roll-off,
    `sum_h rolloff^(h-1) * sin(h * phase)` (`n_harmonics` terms, capped
    so the highest stays below the Nyquist frequency). `"vowel"` uses
    source-filter synthesis (below).

4.  **Loudness, fade, normalisation.** An optional `intensity` envelope
    scales amplitude by `10^((dB - max(dB)) / 20)` (floored at
    `-dyn_range` dB); a raised-cosine fade removes onset/offset clicks;
    the waveform is peak-normalised to `amp` and quantised to 16-bit.

**Vowel (source-filter) synthesis.** Following the linear source-filter
theory of speech production (Fant, 1960), a glottal *source* is passed
through a vocal-tract *filter*. The source is a band-limited harmonic
buzz `sum_h (1/h) * sin(h * phase)` (the `1/h` roll-off approximates the
spectral tilt of glottal flow). The filter is a cascade of three
two-pole resonators, one per formant, each the recursive difference
equation `y[n] = x[n] + 2*r*cos(theta)*y[n-1] - r^2*y[n-2]`, with
resonance `theta = 2*pi*F/fs` and bandwidth set by `r = exp(-pi*BW/fs)`
(poles at radius `r`, angle `theta`). The three formant centres (Hz)
define the vowel:

|       |        |        |        |
|-------|--------|--------|--------|
|       | **F1** | **F2** | **F3** |
| `"a"` | 700    | 1220   | 2600   |
| `"i"` | 300    | 2300   | 3000   |
| `"u"` | 350    | 800    | 2400   |

The formants are static (no transitions) and the result is a stylised
vowel timbre, not a reconstruction of the recording's actual vowel.

## References

Fant, G. (1960). *Acoustic Theory of Speech Production*. Mouton.

Klatt, D. H. (1980). Software for a cascade/parallel formant
synthesiser. *Journal of the Acoustical Society of America*, 67(3),
971-995.

## Examples

``` r
if (FALSE) { # \dontrun{
w <- sonify_f0(c(120, 140, 160, 150, 130), dur = 0.6, source = "complex")
tuneR::play(w)
} # }
```
