###############################################
# Data Collection -> "f0 artefacts" subtab
# A guide to reading the waveform: why f0 correction is needed, the two
# kinds of "correction" (genuine tracking error vs. analytic choice),
# the glottal period, octave errors, voice-quality-related errors
# (creaky utterance edges), and f0 patterns that are correctly tracked
# but awkward to analyse. Lives under Data Collection so it has room;
# F0 Correction links here.
###############################################

waveform_guide_ui <- function(input, output, session) {

  # Numbered green section heading (the six main sections are peers).
  sec <- function(n, title) {
    tags$div(class = "wf-sec",
      tags$span(class = "wf-num", n),
      tags$span(class = "wf-sec-t", title))
  }

  # Card in the voice-quality gallery.
  vq_card <- function(img, name, waveform, tracker, fix) {
    tags$div(class = "vq-card",
      tags$img(src = paste0("waveforms/", img),
               alt = paste(name, "waveform schematic"), class = "vq-img"),
      tags$div(class = "vq-name", name),
      tags$div(class = "vq-row",
               tags$span(class = "vq-lab", "Waveform"), waveform),
      tags$div(class = "vq-row",
               tags$span(class = "vq-lab", "Tracker"), tracker),
      tags$div(class = "vq-row vq-fix",
               tags$span(class = "vq-lab", "Fix"), fix))
  }

  output$waveform_guide_content <- renderUI({
    tagList(
      h2("How to identify and correct f0 artefacts: a guide to reading the waveform",
         class = "wf-title"),
      tags$p(class = "wf-lead",
        "A pitch tracker turns sound into an f0 contour automatically, but it makes ",
        "mistakes. This guide shows how to read the waveform underneath the contour so ",
        "you can tell a real error from real-but-messy speech, and decide what to do."),

      tags$style(HTML("
        .wf-fig {
          width: 100%; height: auto; display: block;
          margin: 8px 0 4px 0;
          border: 1px solid #e6dec3; border-radius: 6px;
          background: #ffffff;
        }
        .wf-fig.narrow { max-width: 720px; }
        .wf-cap {
          font-size: 0.78rem; color: #777; margin: 0 0 14px 0;
          font-style: italic; max-width: 80ch;
        }
        .wf-title { margin-bottom: 6px; }
        .wf-lead {
          margin: 0 0 8px 0; font-size: 0.95rem; color: #555;
          max-width: 82ch; line-height: 1.5;
        }
        /* the six main sections are peers: numbered, green, top rule */
        .wf-sec {
          display: flex; align-items: center; gap: 11px;
          margin: 34px 0 6px 0; padding-top: 14px;
          border-top: 1px solid #e6dec3;
        }
        .wf-sec .wf-num {
          flex: none; width: 27px; height: 27px;
          border: 2px solid #78c2ad; border-radius: 50%;
          color: #2c8a63; font-weight: 700; font-size: 0.92rem;
          text-align: center; line-height: 23px;
        }
        .wf-sec .wf-sec-t {
          color: #2c5f4f; font-weight: 700; font-size: 1.18rem;
        }
        /* appendix sections are subordinate: muted, smaller, no number */
        .wf-appx {
          color: #6b7a73; font-weight: 700; font-size: 0.86rem;
          text-transform: uppercase; letter-spacing: 0.05em;
          margin: 30px 0 4px 0; padding-top: 10px;
          border-top: 1px dashed #e6dec3;
        }
        .wf-p { margin: 2px 0 0 0; font-size: 0.9rem; color: #444; max-width: 78ch; }
        .vq-gallery {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
          gap: 12px; margin-top: 10px;
        }
        .vq-card {
          background: #ffffff; border: 1px solid #e6dec3;
          border-radius: 8px; padding: 12px 14px;
          font-size: 0.82rem; color: #444; line-height: 1.45;
          box-shadow: 0 1px 3px rgba(123, 99, 26, 0.05);
        }
        .vq-img {
          width: 100%; height: auto; display: block;
          border: 1px solid #eee; border-radius: 4px; margin-bottom: 8px;
        }
        .vq-name { color: #2c5f4f; font-weight: 700; font-size: 0.92rem; margin-bottom: 6px; }
        .vq-row { margin-bottom: 3px; }
        .vq-lab {
          display: inline-block; min-width: 74px;
          color: #8a7a45; font-weight: 600; font-size: 0.72rem;
          text-transform: uppercase; letter-spacing: 0.02em;
        }
        .vq-fix .vq-lab { color: #2c5f4f; }
        .wf-refs { font-size: 0.8rem; color: #555; line-height: 1.5; max-width: 90ch; }
        .wf-refs li { margin-bottom: 6px; }
        /* two-cases callout */
        .tc-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
          gap: 14px; margin: 10px 0 4px 0;
        }
        .tc-card { border-radius: 8px; padding: 14px 16px; font-size: 0.88rem; line-height: 1.5; }
        .tc-fix { background: #eef8f3; border: 1px solid #bfe3d4; border-left: 5px solid #2c8a63; }
        .tc-keep { background: #fff6e9; border: 1px solid #f0dcb4; border-left: 5px solid #d8902a; }
        .tc-head { font-weight: 700; font-size: 0.95rem; margin-bottom: 4px; }
        .tc-fix .tc-head { color: #1f6f4d; }
        .tc-keep .tc-head { color: #9a5f12; }
        .tc-tag { font-size: 0.72rem; font-weight: 600; text-transform: uppercase; letter-spacing: 0.03em; opacity: 0.8; }
        .wf-real { border: 1px solid #d6e7df; border-radius: 8px; background: #fbfdfc; padding: 12px 16px; margin-top: 12px; }
      ")),

      # ===================== 1. Why correct f0 at all =====================
      sec("1", "Why correct f0?"),
      tags$p(class = "wf-p",
        "f0 is extracted automatically by a pitch tracker, and no tracker is perfect. ",
        "It can report the wrong octave, get confused by noise or by a change in ",
        tags$strong("voice quality"), ", or drop out at the quiet edges of voicing. ",
        "Before you model tone, it is worth checking these and sometimes fixing them. ",
        "The waveform itself is the ground truth: when a tracked value looks wrong, ",
        "read the glottal pulses underneath it before deciding what to do."),

      # ===================== 2. Two kinds of correction =====================
      sec("2", "Two kinds of f0 “correction”"),
      tags$p(class = "wf-p",
        "“Correction” means two quite different things. They look similar on screen ",
        "but call for opposite habits, and confusing them is the fastest way to ",
        tags$strong("over-clean"), " your data."),
      tags$div(class = "tc-grid",
        tags$div(class = "tc-card tc-fix",
          tags$div(class = "tc-tag", "Case 1"),
          tags$div(class = "tc-head", "A genuine tracking error: do correct it"),
          tags$p(style = "margin:0;",
            "The voice has a clear period, but the tracker reported the wrong number: ",
            "an octave jump (", tags$code("÷ 2"), " / ", tags$code("× 2"),
            "), a spurious value in silence, or a wild spike. The waveform shows what ",
            "the f0 ", tags$em("should"), " have been, so fixing it recovers the true value. ",
            "Correct it with confidence.")),
        tags$div(class = "tc-card tc-keep",
          tags$div(class = "tc-tag", "Case 2"),
          tags$div(class = "tc-head", "An analytic choice: do not necessarily “fix”"),
          tags$p(style = "margin:0;",
            "The tracker is faithfully following the signal, but the signal itself is ",
            "messy or not what you want to model (creak at the phrase end, a breathy ",
            "onset, genuine diplophonia). Deleting or smoothing it is not a ",
            tags$em("correction"), "; it is an ", tags$strong("analytic decision"),
            " about what counts as your tone's shape."))),
      tags$p(class = "wf-p", style = "margin-top: 10px;",
        tags$strong("For Case 2, prefer a principle over hand-editing."),
        " Decide a rule once and apply it to every token the same way (for example, ",
        "analyse a fixed proportion of the vowel, or drop frames below a voicing or ",
        "strength threshold) rather than tidying contours one at a time by eye. Better ",
        "still, where the variation is real, ", tags$strong("keep it"),
        " and let the statistical model handle it: random effects, by-token curves and ",
        "smooths are designed to absorb this. Hand-cleaning each token bakes your ",
        "expectations into the data and biases what you later “find”."),
      tags$div(style = "background:#f0faf7; border-left:4px solid #78c2ad; padding:8px 14px; border-radius:4px; font-size:0.85rem; color:#3c5a4f; max-width: 80ch;",
        tags$strong("Rule of thumb: "),
        "edit the contour only when the waveform proves the tracker was wrong (Case 1). ",
        "If you find yourself deciding what the curve ", tags$em("ought"),
        " to look like, that is analysis (Case 2): make it a documented, consistent rule, ",
        "not a per-token clean-up."),

      # ===================== 3. The glottal period =====================
      sec("3", "The core: the glottal period (f0 = 1 / T)"),
      tags$p(class = "wf-p",
        "Voiced speech is a train of repeating glottal pulses. The time between two ",
        "successive pulses is the period ", tags$strong("T"),
        "; the pitch is its reciprocal, ", tags$strong("f0 = 1 / T"),
        ". So a 5 ms period is about 200 Hz, a 10 ms period about 100 Hz. Counting ",
        "pulses by eye is the most reliable check on any tracked value, and every ",
        "section below comes back to it."),
      tags$img(src = "waveforms/modal_period.png",
               alt = "Modal vowel waveform with the glottal period T marked between two pulses",
               class = "wf-fig narrow"),
      tags$p(class = "wf-cap",
        "A steady vowel from the sample set (~198 Hz). Orange markers sit one glottal ",
        "pulse apart; the spacing between them is T."),

      # ===================== 4. Octave errors =====================
      sec("4", "The commonest tracking error: octaves"),
      tags$p(class = "wf-p",
        "The single most common mistake is reporting the pitch an octave off, and it ",
        "comes down to ", tags$em("which"), " distance the tracker measures between pulses. ",
        "Read one full period and you get the right f0; read half a period or two ",
        "periods and you are an octave out:"),
      tags$img(src = "waveforms/octave_readings.png",
               alt = "One vowel period read three ways: T gives the correct f0, half T gives double, two T gives half",
               class = "wf-fig"),
      tags$p(class = "wf-cap",
        "Same pulse train read three ways. Reading the true period T gives the correct ",
        "f0; reading half a period reports double (fix with ÷ 2); reading two periods ",
        "reports half (fix with × 2)."),
      tags$ul(style = "margin: 4px 0 4px 0; padding-left: 18px; font-size: 0.9rem; color: #444; max-width: 78ch;",
        tags$li(tags$strong("Doubling"),
          " : the tracker locks onto a harmonic (a secondary peak inside each period) ",
          "and reports ", tags$strong("2 × f0"), ", so the contour jumps up an octave. ",
          "Select the bad stretch and click ", tags$code("÷ 2"), "."),
        tags$li(tags$strong("Halving"),
          " : the tracker counts only every second pulse (common when alternate pulses ",
          "differ in size or timing) and reports ", tags$strong("f0 / 2"),
          ", dropping an octave. Select it and click ", tags$code("× 2"), ".")),
      tags$p(class = "wf-p",
        "To confirm before editing, turn on the glottal-pulse overlay in ",
        tags$strong("F0 Processing → F0 Correction"),
        " and count pulses against the contour. For tokens loaded from a ",
        tags$code(".Pitch"), " file, the top-3 ", tags$strong("Praat candidates"),
        " in the sidebar usually include the correct octave; clicking one is often ",
        "quicker than halving or doubling by hand."),

      # ===================== 5. Voice-quality-related errors =====================
      sec("5", "Voice-quality-related errors: creak and utterance edges"),
      tags$p(class = "wf-p",
        "Trackers are most fragile where voice quality departs from clean modal voicing, ",
        "and that happens most reliably at the ", tags$strong("edges of an utterance"),
        ". Voicing starts and stops gradually: the onset is often breathy or creaky while ",
        "the folds settle, and the offset frequently tails off into creak. The regularity ",
        "and shape of the pulses tell you how far to trust the tracker. These idealised ",
        "shapes (after ",
        tags$a(href = "https://www.phonetik.uni-muenchen.de/studium/skripten/languagedemos/Demos/laver.html",
               target = "_blank", rel = "noopener", "Laver's"),
        " phonation categories) are what to look for under the contour:"),
      tags$div(class = "vq-gallery",
        vq_card("vq_modal.png", "Modal voice",
          "Regular, evenly spaced pulses; one clear peak per period.",
          "Reliable.",
          "Usually no correction needed."),
        vq_card("vq_creaky.png", "Creaky voice / vocal fry",
          "Low, irregular, widely spaced pulses, often with damped ringing; spacing and size vary pulse to pulse.",
          "Drops out, halves, or reports erratic values.",
          tagList(tags$code("× 2"), " if halved; ", tags$code("Delete"),
                  " clearly spurious frames; ", tags$em("smooth"),
                  " mild jitter. But if the creak is part of the tone, that is Case 2 (see below).")),
        vq_card("vq_breathy.png", "Breathy voice",
          "Periodicity present but weak, with aspiration noise filling the gaps between pulses.",
          "Jittery estimates and occasional dropout.",
          tagList(tags$em("Smooth"), " (median) over the jittery stretch; ",
                  tags$code("Delete"), " frames sitting in pure noise.")),
        vq_card("vq_falsetto.png", "Falsetto",
          "High f0, near-sinusoidal, low amplitude; few harmonics.",
          "Usually reliable but high; can slip at on/offsets.",
          tagList("Rarely needs octave fixes; ", tags$em("interpolate"),
                  " or ", tags$em("smooth"), " brief glitches.")),
        vq_card("vq_diplophonia.png", "Diplophonia / period-doubling",
          "Two interleaved pulse trains: pulses alternate strong/weak, so the period can be read as one pulse or two (subharmonics).",
          "Commonly halves, or flips between octaves.",
          tagList(tags$code("× 2"), " / ", tags$code("÷ 2"),
                  " only if it is a clear error; otherwise this is the Case 2 example below."))),
      tags$div(class = "wf-real",
        tags$div(style = "font-weight:700; color:#2c5f4f; margin-bottom:4px;",
          "Real speech: the beginning and end of an utterance"),
        tags$p(class = "wf-p",
          "A real token from this project. The shaded edges are where f0 is least reliable, ",
          "because the voice is just starting or trailing off."),
        tags$img(src = "waveforms/edges.png",
                 alt = "Waveform and f0 contour of one utterance, with onset and offset regions shaded; f0 is sparse and jumpy at the edges",
                 class = "wf-fig"),
        tags$p(class = "wf-cap",
          "Shaded: the onset and offset. Spurious or sparse frames sitting in near-silence ",
          "are Case 1, so delete them. A real but irregular creak at the very end is Case 2: ",
          "it may be the speaker's genuine phrase-final phonation, not an error to scrub."),
        tags$p(class = "wf-p", style = "font-size: 0.85rem;",
          tags$strong("Decision: "),
          "delete frames that sit in noise or jump to impossible values; keep (or ",
          "interpolate, consistently) a real but messy edge if its timing falls inside ",
          "your analysis window. Whatever you choose, do it the same way for every token.")),

      # ===================== 6. Correct-but-awkward patterns =====================
      sec("6", "Awkward f0 patterns that are (arguably) correct"),
      tags$p(class = "wf-p",
        "Sometimes the tracker is right and the f0 is still hard to deal with. Creak can ",
        "be tracked perfectly accurately yet you may not want it in a model of tonal ",
        "shape; and some voices are genuinely ", tags$strong("ambiguous by an octave"),
        ". How you handle these is not a bug-fix; it depends on the ",
        tags$strong("type of pattern"), ", your ", tags$strong("analytical framework"),
        ", and your ", tags$strong("statistical method"), "."),
      tags$div(class = "wf-real",
        tags$div(style = "font-weight:700; color:#2c5f4f; margin-bottom:4px;",
          "Real speech: a diplophonic voice with two defensible pitches"),
        tags$p(class = "wf-p",
          "In this token the voice is modal and steady, yet even in the ", tags$em("middle"),
          " of the vowel it carries two periodicities at once. Zooming in and marking the ",
          "glottal pulses shows why: the pulses recur every ~5 ms (200 Hz), but every ",
          "second pulse is subtly stronger, so the pattern also repeats every ~10 ms (100 Hz)."),
        tags$img(src = "waveforms/diplophonia.png",
                 alt = "Top: mid-vowel waveform with glottal pulses marked, bracketed as 200 Hz pulse-to-pulse and 100 Hz every other pulse. Bottom: autocorrelation with near-equal peaks at 5 ms and 10 ms.",
                 class = "wf-fig"),
        tags$p(class = "wf-cap",
          "Top: a mid-vowel slice with the glottal pulses marked (vertical lines). Bottom: ",
          "the vowel's self-similarity (autocorrelation) peaks almost equally at 5 ms ",
          "(200 Hz) and 10 ms (100 Hz). Because both lags are strong, different trackers ",
          "land an octave apart: Praat's raw autocorrelation often reports ~100 Hz here, ",
          "while other settings report ~200 Hz. Neither is simply “wrong”."),
        tags$p(class = "wf-p", style = "font-size: 0.85rem;",
          tags$strong("Decision: "),
          "if you treat the vowel as one modal target, pick the octave consistently across ",
          "the dataset (and use ", tags$code("× 2"), " / ", tags$code("÷ 2"),
          " to enforce it). If the period-doubling is a real feature you care about, keep ",
          "it, or exclude the region by a fixed rule. What you must ", tags$em("not"),
          " do is silently snap it to 200 Hz on some tokens and 100 Hz on others.")),
      tags$p(class = "wf-p", style = "margin-top: 10px;",
        "The same logic covers correctly tracked creak you would rather not model: exclude ",
        "it by a consistent criterion (a voicing or strength threshold, a fixed analysis ",
        "window), or keep it and let by-token random effects and smooths soak up the ",
        "variation. Decide once, document it, and apply it to every token."),

      # ===================== Listen =====================
      tags$div(class = "wf-appx", "Hear the difference"),
      tags$p(class = "wf-p",
        "Training your ear helps you trust your eyes. These open teaching collections let ",
        "you listen to each phonation type (external sites):"),
      tags$ul(style = "margin: 4px 0 0 0; padding-left: 18px; font-size: 0.88rem; color: #444; max-width: 80ch;",
        tags$li(tags$a(href = "https://www.phonetik.uni-muenchen.de/studium/skripten/languagedemos/Demos/laver.html",
                       target = "_blank", rel = "noopener",
                       "LMU Munich — Laver voice-quality demos"),
                ": 16 phonation types (modal, creak, breathy, falsetto, whispery, harsh, and combinations)."),
        tags$li(tags$a(href = "http://archive.phonetics.ucla.edu/",
                       target = "_blank", rel = "noopener",
                       "UCLA Phonetics Lab Archive"),
                ": recordings from hundreds of languages, many with phonation contrasts.")),

      # ===================== References =====================
      tags$div(class = "wf-appx", "Where this comes from"),
      tags$ul(class = "wf-refs",
        tags$li("Laver, J. (1980). ", tags$em("The Phonetic Description of Voice Quality"),
                ". Cambridge University Press. ",
                tags$span(style = "color:#888;", "— the standard taxonomy of phonation types.")),
        tags$li("Gordon, M., & Ladefoged, P. (2001). Phonation types: a cross-linguistic overview. ",
                tags$em("Journal of Phonetics"), ", 29(4), 383–406. ",
                tags$a(href = "https://doi.org/10.1006/jpho.2001.0147",
                       target = "_blank", rel = "noopener", "doi:10.1006/jpho.2001.0147"), "."),
        tags$li("Keating, P., Garellek, M., & Kreiman, J. (2015). Acoustic properties of different kinds of creaky voice. ",
                tags$em("Proc. 18th ICPhS"), ", Glasgow. ",
                tags$a(href = "https://idiom.ucsd.edu/~mgarellek/files/Keating_etal_2015_ICPhS.pdf",
                       target = "_blank", rel = "noopener", "PDF"),
                tags$span(style = "color:#888;", " — distinguishes several phonetically distinct kinds of creak.")),
        tags$li("Garellek, M. (2019). The phonetics of voice. In W. F. Katz & P. F. Assmann (Eds.), ",
                tags$em("The Routledge Handbook of Phonetics"), " (pp. 75–106). Routledge. ",
                tags$span(style = "color:#888;", "— overview of phonation and its acoustic correlates.")),
        tags$li("Boersma, P. (1993). Accurate short-term analysis of the fundamental frequency and the harmonics-to-noise ratio of a sampled sound. ",
                tags$em("Proc. Institute of Phonetic Sciences (Amsterdam)"), ", 17, 97–110. ",
                tags$a(href = "https://www.fon.hum.uva.nl/paul/papers/Proceedings_1993.pdf",
                       target = "_blank", rel = "noopener", "PDF"),
                " — the autocorrelation algorithm behind Praat's pitch tracking and its octave-jump cost."),
        tags$li("de Cheveigné, A., & Kawahara, H. (2002). YIN, a fundamental frequency estimator for speech and music. ",
                tags$em("JASA"), ", 111(4), 1917–1930. ",
                tags$a(href = "http://audition.ens.fr/adc/pdf/2002_JASA_YIN.pdf",
                       target = "_blank", rel = "noopener", "PDF"), "."),
        tags$li("Talkin, D. (1995). A robust algorithm for pitch tracking (RAPT). In W. B. Kleijn & K. K. Paliwal (Eds.), ",
                tags$em("Speech Coding and Synthesis"), " (pp. 495–518). Elsevier."),
        tags$li("Hess, W. (1983). ", tags$em("Pitch Determination of Speech Signals"),
                ". Springer. ",
                tags$span(style = "color:#888;", "— gross errors (octave/halving) vs. fine errors in pitch detection.")),
        tags$li("Background on reading waveforms and periodicity: Ladefoged & Johnson, ",
                tags$em("A Course in Phonetics"), "; Johnson, ",
                tags$em("Acoustic and Auditory Phonetics"), ".")),
      tags$p(style = "margin: 8px 0 0 0; font-size: 0.76rem; color: #999; max-width: 80ch;",
        "The voice-quality gallery uses idealised schematic waveforms; the period, octave, ",
        "and real-recording figures are generated from this project's bundled sample recordings.")
    )
  })
}
