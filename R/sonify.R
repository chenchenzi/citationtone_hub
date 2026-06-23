#' Sonify an f0 contour
#'
#' Render a fundamental-frequency contour as an audible waveform: an oscillator
#' whose instantaneous pitch follows \code{f0_hz}. Useful for *listening* to a
#' mean tone contour (e.g. a cluster prototype) rather than only seeing it.
#'
#' The contour has no inherent duration, so it is time-scaled to \code{dur}
#' seconds. Pitch is rendered by phase accumulation, so the glide is smooth.
#'
#' @details
#' All three timbres share one pitch generator and differ only in spectral shape:
#'
#' \enumerate{
#'   \item \strong{Contour conditioning.} Leading/trailing \code{NA}s are
#'     trimmed and internal gaps linearly interpolated; the contour is then
#'     resampled to \code{round(fs * dur)} samples and clamped to 40-2000 Hz.
#'   \item \strong{Pitch by phase accumulation.} The running phase is
#'     \code{phase = cumsum(2 * pi * f0 / fs)}, so \code{sin(phase)} has an
#'     instantaneous frequency equal to the contour at every sample; the pitch
#'     glides continuously with no discontinuities.
#'   \item \strong{Source waveform.} \code{"tone"} is the bare sine
#'     \code{sin(phase)}. \code{"complex"} sums harmonics with a geometric
#'     roll-off, \code{sum_h rolloff^(h-1) * sin(h * phase)} (\code{n_harmonics}
#'     terms, capped so the highest stays below the Nyquist frequency).
#'     \code{"vowel"} uses source-filter synthesis (below).
#'   \item \strong{Loudness, fade, normalisation.} An optional \code{intensity}
#'     envelope scales amplitude by \code{10^((dB - max(dB)) / 20)} (floored at
#'     \code{-dyn_range} dB); a raised-cosine fade removes onset/offset clicks;
#'     the waveform is peak-normalised to \code{amp} and quantised to 16-bit.
#' }
#'
#' \strong{Vowel (source-filter) synthesis.} Following the linear source-filter
#' theory of speech production (Fant, 1960), a glottal \emph{source} is passed
#' through a vocal-tract \emph{filter}. The source is a band-limited harmonic
#' buzz \code{sum_h (1/h) * sin(h * phase)} (the \code{1/h} roll-off approximates
#' the spectral tilt of glottal flow). The filter is a cascade of three two-pole
#' resonators, one per formant, each the recursive difference equation
#' \code{y[n] = x[n] + 2*r*cos(theta)*y[n-1] - r^2*y[n-2]}, with resonance
#' \code{theta = 2*pi*F/fs} and bandwidth set by \code{r = exp(-pi*BW/fs)} (poles
#' at radius \code{r}, angle \code{theta}). The three formant centres (Hz) define
#' the vowel:
#' \tabular{llll}{
#'   \tab \strong{F1} \tab \strong{F2} \tab \strong{F3} \cr
#'   \code{"a"} \tab 700 \tab 1220 \tab 2600 \cr
#'   \code{"i"} \tab 300 \tab 2300 \tab 3000 \cr
#'   \code{"u"} \tab 350 \tab  800 \tab 2400 \cr
#' }
#' The formants are static (no transitions) and the result is a stylised vowel
#' timbre, not a reconstruction of the recording's actual vowel.
#'
#' @references
#' Fant, G. (1960). \emph{Acoustic Theory of Speech Production}. Mouton.
#'
#' Klatt, D. H. (1980). Software for a cascade/parallel formant synthesiser.
#' \emph{Journal of the Acoustical Society of America}, 67(3), 971-995.
#'
#' @param f0_hz Numeric vector of f0 values in Hz (the contour to play). Leading
#'   and trailing \code{NA}s are trimmed; internal \code{NA}s are interpolated.
#' @param fs Output sample rate (Hz). Default 16000.
#' @param dur Playback duration in seconds. Default 0.7.
#' @param source \code{"tone"} for a pure sine, \code{"complex"} for a sine plus
#'   harmonics with amplitude roll-off (warmer), or \code{"vowel"} for a
#'   source-filter synthesised vowel (glottal source through formant resonators).
#'   The pitch is identical across all three; only the timbre differs.
#' @param vowel Vowel quality for \code{source = "vowel"}: \code{"a"}, \code{"i"}
#'   or \code{"u"} (each is a formant set).
#' @param n_harmonics Number of harmonics for \code{source = "complex"}.
#' @param rolloff Per-harmonic amplitude factor for \code{"complex"} (0-1).
#' @param fade Raised-cosine fade in/out duration (s), to avoid clicks.
#' @param amp Peak amplitude (0-1).
#' @param intensity Optional per-frame intensity (dB), the same length as
#'   \code{f0_hz}. When supplied, the rendered waveform's loudness follows it
#'   (\code{dB -> linear amplitude}), so the tone swells and fades like the
#'   original syllable. \code{NULL} (default) gives a flat amplitude.
#' @param dyn_range Loudness range (dB) when \code{intensity} is used: frames
#'   more than this far below the peak are floored, so quiet frames attenuate
#'   without becoming inaudible. Default 30.
#'
#' @return A 16-bit mono \code{\link[tuneR]{Wave}} object.
#'
#' @examples
#' \dontrun{
#' w <- sonify_f0(c(120, 140, 160, 150, 130), dur = 0.6, source = "complex")
#' tuneR::play(w)
#' }
#' @importFrom stats approx
#' @export
sonify_f0 <- function(f0_hz, fs = 16000, dur = 0.7,
                      source = c("tone", "complex", "vowel"),
                      vowel = c("a", "i", "u"),
                      n_harmonics = 12L, rolloff = 0.7, fade = 0.02, amp = 0.9,
                      intensity = NULL, dyn_range = 30) {
  source <- match.arg(source)
  vowel  <- match.arg(vowel)
  f0_hz <- as.numeric(f0_hz)
  fin <- which(is.finite(f0_hz))
  if (length(fin) < 2) stop("`f0_hz` needs at least 2 finite values.", call. = FALSE)
  keep <- fin[1]:fin[length(fin)]
  if (!is.null(intensity)) {                              # align to the trimmed f0 frames
    intensity <- suppressWarnings(as.numeric(intensity))
    intensity <- if (length(intensity) == length(f0_hz)) intensity[keep] else NULL
  }
  f0_hz <- f0_hz[keep]                                    # trim leading/trailing NA
  gap <- !is.finite(f0_hz)
  if (any(gap))                                           # fill internal gaps
    f0_hz[gap] <- stats::approx(which(!gap), f0_hz[!gap], xout = which(gap))$y

  n <- max(2L, round(fs * dur))
  cont <- stats::approx(seq(0, 1, length.out = length(f0_hz)), f0_hz,
                        xout = seq(0, 1, length.out = n))$y
  cont <- pmin(pmax(cont, 40), 2000)                      # keep in a sane audible band

  phase <- cumsum(2 * pi * cont / fs)
  if (source == "tone") {
    w <- sin(phase)
  } else if (source == "complex") {
    nh <- max(1L, min(as.integer(n_harmonics), floor((fs / 2) / max(cont))))  # cap below Nyquist
    w <- numeric(n)
    for (h in seq_len(nh))
      w <- w + (rolloff^(h - 1)) * sin(h * phase)
  } else {                                                  # vowel: source-filter
    nh <- max(1L, min(30L, floor((fs / 2) / max(cont))))    # band-limited glottal source
    w <- numeric(n)
    for (h in seq_len(nh)) w <- w + (1 / h) * sin(h * phase)
    fmt <- switch(vowel,                                    # formant centres / bandwidths (Hz)
      a = list(f = c(700, 1220, 2600), bw = c(110, 120, 150)),
      i = list(f = c(300, 2300, 3000), bw = c(60, 100, 150)),
      u = list(f = c(350, 800, 2400),  bw = c(60, 90, 150)))
    for (j in seq_along(fmt$f)) {                           # cascade of 2-pole resonators
      r  <- exp(-pi * fmt$bw[j] / fs)
      th <- 2 * pi * fmt$f[j] / fs
      w  <- as.numeric(stats::filter(w, filter = c(2 * r * cos(th), -(r^2)),
                                     method = "recursive"))
    }
    w[!is.finite(w)] <- 0
  }

  if (!is.null(intensity) && any(is.finite(intensity))) { # shape loudness by measured dB
    iv <- intensity
    iv[!is.finite(iv)] <- min(iv[is.finite(iv)])          # floor unvoiced/NA frames
    env <- stats::approx(seq(0, 1, length.out = length(iv)), iv,
                         xout = seq(0, 1, length.out = n))$y
    rel <- pmax(env - max(env), -abs(dyn_range))          # dB below peak, floored
    w   <- w * 10 ^ (rel / 20)                            # dB -> linear amplitude
  }

  nf <- min(floor(n / 2), max(1L, round(fs * fade)))      # raised-cosine fade
  if (nf >= 1) {
    ramp <- (1 - cos(pi * seq_len(nf) / nf)) / 2          # 0 -> 1
    w[seq_len(nf)]          <- w[seq_len(nf)] * ramp
    w[(n - nf + 1):n]       <- w[(n - nf + 1):n] * rev(ramp)
  }

  m <- max(abs(w))
  if (m > 0) w <- w / m * amp
  tuneR::Wave(left = round(w * 32767), samp.rate = fs, bit = 16)
}
