###############################################
# Data Collection -> "Reading waveform" subtab
# A guide to reading the waveform. Structure: (1) why correct f0, (2) the two
# kinds of "correction" framework, (3) the glottal period, (4) a voice-quality
# gallery, then the two kinds applied: (5) Case 1 = genuine tracking errors to
# fix (octaves, spurious frames) and (6) Case 2 = correctly tracked but maybe
# unwanted patterns that are an analytic choice (creaky utterance edges, diplophonia).
# Lives under Data Collection so it has room; F0 Correction links here.
###############################################

waveform_guide_ui <- function(input, output, session) {

  # Numbered green section heading (the six main sections are peers).
  # cls: optional modifier ("case1"/"case2") to tint the number badge so the
  # two catalogue sections echo the Case 1 / Case 2 cards.
  sec <- function(n, title, cls = NULL) {
    tags$div(class = paste(c("wf-sec", cls), collapse = " "),
      id = paste0("wf-s", n),
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
      tags$nav(class = "wf-toc",
        tags$div(class = "wf-toc-h", "On this page"),
        tags$a(href = "#wf-s1", "1. Why correct f0?"),
        tags$a(href = "#wf-s2", "2. Two kinds of correction"),
        tags$a(href = "#wf-s3", "3. The glottal period"),
        tags$a(href = "#wf-s4", "4. Voice qualities"),
        tags$a(href = "#wf-s5", "5. Case 1: tracking errors"),
        tags$a(href = "#wf-s6", "6. Case 2: analytic choice"),
        tags$a(href = "#wf-listen", "Hear & see"),
        tags$a(href = "#wf-refs", "References")),
      tags$div(class = "wf-page",
      h2("How to identify and correct f0 artefacts: a guide to reading the waveform",
         class = "wf-title"),
      tags$p(class = "wf-lead",
        "Pitch trackers estimate an f0 contour from the audio automatically, and no tracker ",
        "is infallible. This guide shows how to read the waveform underneath the contour, so ",
        "you can tell a genuine error from real-but-messy speech and decide what to do."),

      tags$style(HTML("
        /* one centred reading column: title, text and figures share its width */
        .wf-page { max-width: 720px; margin: 0 auto; }
        /* floating table of contents, pinned in the left margin */
        .wf-toc {
          position: fixed; top: 138px; left: 32px; width: 168px;
          font-size: 0.82rem; line-height: 1.5;
          border-left: 2px solid #e6dec3; padding-left: 12px;
          max-height: calc(100vh - 170px); overflow-y: auto;
        }
        .wf-toc-h {
          font-weight: 700; color: #2c5f4f; font-size: 0.7rem;
          text-transform: uppercase; letter-spacing: 0.06em; margin-bottom: 8px;
        }
        .wf-toc a {
          display: block; color: #5a6b63; text-decoration: none; padding: 3px 0;
        }
        .wf-toc a:hover { color: #2c8a63; }
        @media (max-width: 1150px) { .wf-toc { display: none; } }
        .wf-fig {
          width: 100%; height: auto; display: block;
          margin: 8px 0 4px 0;
          border: 1px solid #e6dec3; border-radius: 6px;
          background: #ffffff;
        }
        .wf-fig.narrow { max-width: 560px; }
        .wf-cap {
          font-size: 0.78rem; color: #777; margin: 0 0 14px 0;
          font-style: italic;
        }
        .wf-title { margin-bottom: 14px; }
        .wf-lead {
          margin: 0 0 22px 0; font-size: 0.95rem; color: #555;
          line-height: 1.5;
        }
        /* the six main sections are peers: numbered, green, top rule */
        .wf-sec {
          display: flex; align-items: center; gap: 11px;
          margin: 34px 0 6px 0; padding-top: 14px;
          border-top: 1px solid #e6dec3; scroll-margin-top: 80px;
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
        /* the two catalogue sections tint their badge to match the cards */
        .wf-sec.case1 .wf-num { background: #2c8a63; border-color: #2c8a63; color: #fff; }
        .wf-sec.case2 .wf-num { background: #d8902a; border-color: #d8902a; color: #fff; }
        /* sub-topic label within a section */
        .wf-sub { color: #2c5f4f; font-weight: 700; font-size: 1.0rem; margin: 18px 0 2px 0; }
        /* a typeset equation (serif, centred, real fraction bar) */
        .wf-eq {
          text-align: center; margin: 14px 0; color: #2c5f4f;
          font-family: Georgia, 'Times New Roman', serif; font-size: 1.3rem;
        }
        .wf-eq .frac {
          display: inline-flex; flex-direction: column; vertical-align: middle;
          text-align: center; line-height: 1.05;
        }
        .wf-eq .frac .num { border-bottom: 1.5px solid currentColor; padding: 0 7px; }
        .wf-eq .frac .den { padding: 2px 7px 0 7px; }
        /* appendix sections are subordinate: muted, smaller, no number */
        .wf-appx {
          color: #6b7a73; font-weight: 700; font-size: 0.86rem;
          text-transform: uppercase; letter-spacing: 0.05em;
          margin: 30px 0 4px 0; padding-top: 10px;
          border-top: 1px dashed #e6dec3; scroll-margin-top: 80px;
        }
        .wf-p { margin: 2px 0 0 0; font-size: 0.9rem; color: #444; }
        .vq-gallery {
          display: flex; gap: 12px; margin-top: 6px;
          overflow-x: auto; padding: 4px 2px 12px 2px;
          scroll-snap-type: x proximity;
          scrollbar-width: thin;
        }
        .vq-gallery::-webkit-scrollbar { height: 8px; }
        .vq-gallery::-webkit-scrollbar-thumb { background: #d8cfae; border-radius: 4px; }
        .vq-scroll-hint { font-size: 0.74rem; color: #9a8a55; font-style: italic; margin: 6px 0 0 0; }
        .vq-card {
          flex: 0 0 264px; scroll-snap-align: start;
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
        .wf-refs { font-size: 0.8rem; color: #555; line-height: 1.5; }
        .wf-refs li { margin-bottom: 6px; }
        /* two-cases callout */
        .tc-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
          gap: 14px; margin: 10px 0 4px 0; max-width: 720px;
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
        "“Correction” can mean two quite different things. They look similar on screen ",
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
          tags$div(class = "tc-head", "A genuine messy signal? Do not necessarily “fix”"),
          tags$p(style = "margin:0;",
            "The tracker is faithfully following the signal, but the signal itself is ",
            "messy or not what you want to model (creak at the phrase end, a breathy ",
            "onset, genuine diplophonia). Deleting or smoothing it is not a ",
            tags$em("correction"), "; it is an ", tags$strong("analytic decision"),
            " about what counts as your tone's shape."))),
      tags$p(class = "wf-p", style = "margin-top: 10px;",
        tags$strong("For Case 2, prefer a principle over hand-editing."),
        " Decide a rule once and apply it to every token the same way (for example, ",
        "you are measuring only the modal-voiced portion of each vowel, analysing a ",
        "fixed proportion of the rhyme, or dropping frames below a voicing or strength ",
        "threshold) rather than tidying contours one at a time by eye. Sometimes it is ",
        "better still to ", tags$strong("keep the variation"),
        " where it is real and let the statistical model handle it: random effects, ",
        "by-token curves and smooths are designed to absorb this. Hand-cleaning each ",
        "token bakes your expectations into the data and biases what you later “find”."),
      tags$div(style = "background:#eef1f6; border-left:4px solid #6b7aa0; padding:10px 14px; border-radius:4px; font-size:0.85rem; color:#3a4660; margin-top: 18px;",
        tags$strong("Rule of thumb: "),
        "edit the contour only when the waveform proves the tracker was wrong (Case 1). ",
        "If you find yourself deciding what the curve ", tags$em("ought"),
        " to look like, that is analysis (Case 2): make it a documented, consistent rule, ",
        "not a per-token clean-up."),

      # ===================== 3. The glottal period =====================
      sec("3", "The core: the glottal period"),
      tags$p(class = "wf-p",
        "Voiced speech is a train of repeating glottal pulses. The time between two ",
        "successive pulses is the period ", HTML("<i>T</i>"),
        ", and the pitch is its reciprocal:"),
      tags$div(class = "wf-eq",
        HTML("<i>f</i><sub>0</sub> &nbsp;=&nbsp; <span class='frac'><span class='num'>1</span><span class='den'><i>T</i></span></span>")),
      tags$p(class = "wf-p",
        "So a 5 ms period is about 200 Hz, and a 10 ms period about 100 Hz. Counting ",
        "pulses by eye is the most reliable check on any tracked value, and every ",
        "section below comes back to it."),
      tags$img(src = "waveforms/modal_period.png",
               alt = "Modal vowel waveform with the glottal period T marked between two pulses",
               class = "wf-fig narrow"),
      tags$p(class = "wf-cap",
        tags$strong("Figure 1. "),
        "A steady vowel from the sample set (~198 Hz). Orange markers sit one glottal ",
        "pulse apart; the spacing between them is T."),

      # ===================== 4. Voice-quality gallery =====================
      sec("4", "What different voice qualities look like"),
      tags$p(class = "wf-p",
        "Trackers are most fragile where voice quality departs from clean modal voicing. ",
        "The regularity and shape of the glottal pulses tell you how far to trust a tracked ",
        "value, and they also tell you ", tags$em("which"), " of the two cases you are in. ",
        "These idealised shapes (after ",
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
          tagList(tags$code("× 2"), " if genuinely halved (Case 1); but if the creak is ",
                  "real, whether to keep it is a Case 2 choice (see section 6).")),
        vq_card("vq_breathy.png", "Breathy voice",
          "Periodicity present but weak, with aspiration noise filling the gaps between pulses.",
          "Jittery estimates and occasional dropout.",
          tagList(tags$em("Smooth"), " (median) over the jittery stretch; ",
                  tags$code("Delete"), " frames sitting in pure noise (Case 1).")),
        vq_card("vq_falsetto.png", "Falsetto",
          "High f0, near-sinusoidal, low amplitude; few harmonics.",
          "Usually reliable but high; can slip at on/offsets.",
          tagList("Rarely needs octave fixes; ", tags$em("interpolate"),
                  " or ", tags$em("smooth"), " brief glitches.")),
        vq_card("vq_diplophonia.png", "Diplophonia / period-doubling",
          "Two interleaved pulse trains: pulses alternate strong/weak, so the period can be read as one pulse or two (subharmonics).",
          "Commonly halves, or flips between octaves.",
          tagList(tags$code("× 2"), " / ", tags$code("÷ 2"),
                  " only if it is a clear error; otherwise this is the Case 2 example in section 6."))),
      tags$p(class = "vq-scroll-hint", "← scroll sideways to see all phonation types →"),
      tags$p(class = "wf-p", style = "margin-top: 8px;",
        "Creak in particular is not a single thing: ",
        tags$a(href = "https://idiom.ucsd.edu/~mgarellek/files/Keating_etal_2015_ICPhS.pdf",
               target = "_blank", rel = "noopener", "Keating, Garellek & Kreiman (2015)"),
        " distinguish several phonetically distinct kinds of creak, which is part of why ",
        "creaky f0 is both hard to track and hard to interpret."),

      # ===================== 5. CASE 1: genuine tracking errors =====================
      sec("5", "Case 1: genuine tracking errors — fix them", "case1"),
      tags$p(class = "wf-p",
        "When the voice has a clear period but the tracker reports the wrong number, ",
        "the waveform shows what the f0 ", tags$em("should"), " have been, so fixing it ",
        "recovers the true value. Correct these with confidence."),

      tags$div(class = "wf-sub", "Octave errors (the commonest)"),
      tags$p(class = "wf-p",
        "The single most common mistake is reporting the pitch an octave off, and it ",
        "comes down to ", tags$em("which"), " distance the tracker measures between pulses. ",
        "Read one full period and you get the right f0; read half a period or two ",
        "periods and you are an octave out:"),
      tags$img(src = "waveforms/octave_readings.png",
               alt = "One vowel period read three ways: T gives the correct f0, half T gives double, two T gives half",
               class = "wf-fig"),
      tags$p(class = "wf-cap",
        tags$strong("Figure 2. "),
        "Same pulse train read three ways. Reading the true period T gives the correct ",
        "f0; reading half a period reports double (fix with ÷ 2); reading two periods ",
        "reports half (fix with × 2)."),
      tags$ul(style = "margin: 4px 0 4px 0; padding-left: 18px; font-size: 0.9rem; color: #444;",
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

      tags$div(class = "wf-sub", "Spurious frames and dropout"),
      tags$p(class = "wf-p",
        "At the quiet edges of voicing the tracker can place f0 values in near-silence or ",
        "jump to impossible numbers, and brief glitches can appear mid-vowel where breathy ",
        "noise fills the gaps between pulses. When the waveform shows no real periodicity ",
        "behind a value, it is an error: ", tags$code("Delete"),
        " frames sitting in noise, and ", tags$em("smooth"),
        " or ", tags$em("interpolate"), " brief isolated spikes."),

      # ===================== 6. CASE 2: correct but you may not want it =====================
      sec("6", "Case 2: correctly tracked, but you may not want it — an analytic choice", "case2"),
      tags$p(class = "wf-p",
        "In some cases, the tracker is right and the f0 is still hard to deal with. The ",
        "folds really are producing that pattern, so it is not a bug to fix. How you handle ",
        "it depends on the ", HTML("<strong>type of pattern</strong>,"), " your ",
        HTML("<strong>analytical framework</strong>,"), " and your ",
        HTML("<strong>statistical method</strong>."),
        " For instance, a GAMM with by-token smooths can absorb a good deal of this ",
        "variation, whereas a low-order polynomial fit may have its overall shape distorted ",
        "by it. The safe move is a consistent rule, not per-token editing."),

      tags$div(class = "wf-real",
        tags$div(style = "font-weight:700; color:#2c5f4f; margin-bottom:4px;",
          "Real speech, Example 1: a creaky utterance edge (a Case 1 error and a Case 2 choice in one)"),
        tags$p(class = "wf-p",
          "A real token from my 2025 Phonetica paper; voicing tails off into creak at the offset. The ",
          "tracker gets it wrong in a revealing way: it locks onto the formant ringing ",
          "inside each creak burst and reports a spurious ", tags$strong("high"),
          " f0 of about 300 Hz (the jump above the line in ", tags$strong("Figure 3"),
          "), when the voice is actually very low. The glottal pulses here are only about ",
          "10–20 ms apart, a true f0 of roughly ", tags$strong("50–90 Hz"),
          ", far below the ~200 Hz modal body of the vowel. That high jump is a ",
          tags$strong("Case 1 error"),
          ", so fix it by lowering the pitch floor until the tracker follows the true low ",
          "creaky f0, or delete those frames."),
        tags$img(src = "waveforms/edges.png",
                 alt = "Waveform and f0 contour of one utterance; the onset and offset regions are shaded, and at the offset the tracked f0 jumps spuriously high because creak pulses are mis-tracked",
                 class = "wf-fig"),
        tags$p(class = "wf-cap",
          tags$strong("Figure 3. "),
          "Onset and offset shaded. At the offset the tracked f0 jumps to ~300 Hz: the ",
          "tracker is mis-reading the ringing inside each creak burst, not the true pitch, ",
          "so this is a Case 1 error. The real glottal pulses are ~10–20 ms apart (~50–90 Hz); ",
          "lowering the pitch floor recovers that low creaky f0."),
        tags$p(class = "wf-p", style = "font-size: 0.85rem;",
          tags$strong("Do we include it even when it is tracked correctly? "),
          "Suppose you lower the floor and the creak is now tracked accurately, low and ",
          "irregular. You might still leave it out. Phrase-final creak may add little to the ",
          "shape of the tone, how listeners perceive pitch in non-modal (creaky, breathy) ",
          "voice is not well understood, and creak itself comes in several distinct kinds ",
          "(see section 4). If you do exclude it, make that an analytic choice applied by a ",
          "consistent rule (a voicing or strength threshold, or a fixed analysis window), the ",
          "same way for every token, rather than trimming each contour by eye.")),

      tags$div(class = "wf-real",
        tags$div(style = "font-weight:700; color:#2c5f4f; margin-bottom:4px;",
          "Real speech, Example 2: a diplophonic voice with two defensible pitches"),
        tags$p(class = "wf-p",
          "In this token the voice is steady and clearly periodic, yet the glottal pulses ",
          "alternate ", tags$strong("strong and weak"), ". Zooming in shows why two pitches ",
          "are defensible: the pulses recur about every 5 ms (~188 Hz), but because every ",
          "second pulse is stronger, the pattern also repeats every 10 ms (~94 Hz)."),
        tags$img(src = "waveforms/diplophonia.png",
                 alt = "Top: waveform slice with glottal pulses marked, alternating weak and strong, bracketed as ~188 Hz pulse-to-pulse and ~94 Hz every other pulse. Bottom: autocorrelation with near-equal peaks at 5 ms and 10 ms.",
                 class = "wf-fig"),
        tags$p(class = "wf-cap",
          tags$strong("Figure 4. "),
          "Top: a slice of the vowel with the glottal pulses marked; they alternate ",
          "weak / strong, so you can read the period as one pulse (~188 Hz) or two ",
          "(~94 Hz). Bottom: the vowel's self-similarity (autocorrelation) peaks almost ",
          "equally at 5 ms (188 Hz) and 10 ms (94 Hz). Because both lags are strong, ",
          "different trackers land an octave apart. Praat's raw autocorrelation often ",
          "reports the lower one. Neither is simply “wrong”."),
        tags$p(class = "wf-p", style = "font-size: 0.85rem;",
          tags$strong("Decision: "),
          "if you treat the vowel as one modal target, pick the octave consistently across ",
          "the dataset (and use ", tags$code("× 2"), " / ", tags$code("÷ 2"),
          " to enforce it). If the period-doubling is a real feature you care about, keep ",
          "it, or exclude the region by a fixed rule. What you must ", tags$em("not"),
          " do is silently snap it to 188 Hz on some tokens and 94 Hz on others.")),

      tags$p(class = "wf-p", style = "margin-top: 12px;",
        tags$strong("In short: "),
        "for Case 2 patterns, exclude them by a consistent criterion (a voicing or strength ",
        "threshold, a fixed analysis window), or keep them and let by-token random effects ",
        "and smooths soak up the variation. Decide once, document it, and apply it to every ",
        "token."),

      # ===================== Listen =====================
      tags$div(class = "wf-appx", id = "wf-listen", "Hear (and see) the difference"),
      tags$p(class = "wf-p",
        "Training your ear helps you trust your eyes. These open teaching collections let ",
        "you listen to each phonation type, and it is worth opening the recordings in Praat ",
        "or Audacity to study their waveform patterns alongside the sound (external sites):"),
      tags$ul(style = "margin: 4px 0 0 0; padding-left: 18px; font-size: 0.88rem; color: #444;",
        tags$li(tags$a(href = "https://www.phonetik.uni-muenchen.de/studium/skripten/languagedemos/Demos/laver.html",
                       target = "_blank", rel = "noopener",
                       "LMU Munich — Laver voice-quality demos"),
                ": 16 phonation types (modal, creak, breathy, falsetto, whispery, harsh, and combinations)."),
        tags$li(tags$a(href = "http://archive.phonetics.ucla.edu/",
                       target = "_blank", rel = "noopener",
                       "UCLA Phonetics Lab Archive"),
                ": recordings from hundreds of languages, many with phonation contrasts.")),

      # ===================== References =====================
      tags$div(class = "wf-appx", id = "wf-refs", "References"),
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
      tags$p(style = "margin: 8px 0 0 0; font-size: 0.76rem; color: #999;",
        "The voice-quality gallery uses idealised schematic waveforms; the period, octave, ",
        "and real-recording figures are generated from recordings in my 2025 Phonetica paper.")
      )
    )
  })
}
