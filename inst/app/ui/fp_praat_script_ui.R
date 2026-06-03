###############################################
# F0 Processing → Praat script subtab
#
# Tutorial page that walks the user through using Praat to extract f0
# from a folder of .wav files. Provides a ready-to-run Praat script
# that outputs one .Pitch per .wav + one combined long-format CSV, both
# of which feed directly into the rest of Shinytone.
###############################################

fp_praat_script_ui <- function(input, output, session) {

  # The Praat script content lives here as a single string so the
  # download handler and the rendered code block share one source of truth.
  # Editable parameters sit in the top USER CONFIG block. The seven
  # supported pitch methods are listed in the header comments.
  praat_script_text <- '###############################################################
# Shinytone | F0 extraction with Praat
#
# Reads every .wav file in the chosen input folder, extracts pitch with
# the chosen algorithm, saves a binary `.Pitch` file per recording (which
# preserves all pitch candidates so Shinytone\'s F0 Correction tab can
# show the top-3 candidate markers on the plot), and writes a combined
# long-format CSV (one row per frame per token).
#
# Usage:
#   1. Open Praat. From the menu: Praat > Open Praat script...
#   2. Open this file.
#   3. Run the script (Ctrl/Cmd + R). TWO dialogs appear:
#        (a) basic settings: folders, pitch range, time step, method.
#        (b) method-specific advanced parameters (silence threshold,
#            voicing threshold, octave costs, ...) pre-filled with the
#            Praat default values for the method you chose.
#      Click OK on each.
#
# Outputs (both ready for Shinytone):
#   * <output_folder>/*.Pitch     -> upload alongside the .wav in
#                                    F0 Processing > Start, then in
#                                    F0 Extraction pick
#                                    "Use uploaded .Pitch / .PitchTier (Praat)".
#   * <output_folder>/f0_long.csv -> upload directly in F0 Analysis > Start
#                                    if you do not need frame-level correction.
###############################################################

# ============================================================
# 1) First dialog: basic settings
# ============================================================
form Extract f0 with Praat
  comment === Folders ===
  folder Input_folder /path/to/your/wav/files
  folder Output_folder /path/to/output
  word Output_csv f0_long.csv

  comment === Pitch range (Hz) -- tighten to your speakers ===
  positive F0_min 75
  positive F0_max 600

  comment === Time step (s) -- 0 means Praat default (~0.0025 s) ===
  positive Time_step 0.010

  comment === Pitch-extraction method ===
  optionmenu Method: 1
    option filtered ac
    option filtered cc
    option ac
    option cc
    option raw ac
    option raw cc
    option shs
endform

# ============================================================
# 2) Second dialog: method-specific advanced parameters
#    Defaults match Praat\'s built-in "To Pitch (...)" UI for each method.
# ============================================================
if method$ = "filtered ac" or method$ = "filtered cc"
  beginPause: "Advanced parameters (filtered)"
    comment: "Defaults shown. Change only if you know what each does."
    integer: "Maximum number of candidates", 15
    boolean: "Very accurate", 0
    real: "Attenuation at top", 0.03
    real: "Silence threshold", 0.09
    real: "Voicing threshold", 0.50
    real: "Octave cost", 0.055
    real: "Octave jump cost", 0.35
    real: "Voiced unvoiced cost", 0.14
  clicked = endPause: "Cancel", "OK", 2, 1
  if clicked = 1
    exitScript: "Cancelled by user."
  endif
elsif method$ = "ac" or method$ = "cc" or method$ = "raw ac" or method$ = "raw cc"
  beginPause: "Advanced parameters (classic / raw)"
    comment: "Defaults shown. Change only if you know what each does."
    integer: "Maximum number of candidates", 15
    boolean: "Very accurate", 0
    real: "Silence threshold", 0.03
    real: "Voicing threshold", 0.45
    real: "Octave cost", 0.01
    real: "Octave jump cost", 0.35
    real: "Voiced unvoiced cost", 0.14
  clicked = endPause: "Cancel", "OK", 2, 1
  if clicked = 1
    exitScript: "Cancelled by user."
  endif
elsif method$ = "shs"
  beginPause: "Advanced parameters (shs)"
    comment: "Defaults shown. Change only if you know what each does."
    integer: "Maximum number of candidates", 15
    real: "Maximum frequency", 1250
    integer: "Maximum subharmonics", 15
    real: "Compression factor", 0.84
    integer: "Number of points per octave", 48
  clicked = endPause: "Cancel", "OK", 2, 1
  if clicked = 1
    exitScript: "Cancelled by user."
  endif
endif

# Convert the boolean "Very accurate" to the "yes" / "no" string Praat needs.
# (Only relevant for the non-shs methods; safe to compute either way.)
very_accurate$ = "no"
if very_accurate
  very_accurate$ = "yes"
endif

# Make sure the output folder exists
createFolder: output_folder$

# Open (overwrite) the combined CSV with a header row
csv_path$ = output_folder$ + "/" + output_csv$
writeFileLine: csv_path$, "token,time,f0"

# Enumerate .wav files
file_list = Create Strings as file list: "wavs", input_folder$ + "/*.wav"
n_files   = Get number of strings

writeInfoLine: "Found ", n_files, " .wav file(s) in ", input_folder$

for i to n_files
  selectObject: file_list
  filename$ = Get string: i
  basename$ = filename$ - ".wav"
  appendInfoLine: "[", i, "/", n_files, "] ", basename$

  sound = Read from file: input_folder$ + "/" + filename$

  # Run the chosen pitch-extraction method.
  # Parameter values come from the second dialog (or its defaults).
  if method$ = "filtered ac"
    pitch = noprogress To Pitch (filtered ac): time_step, f0_min, f0_max, maximum_number_of_candidates, very_accurate$, attenuation_at_top, silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, voiced_unvoiced_cost
  elsif method$ = "filtered cc"
    pitch = noprogress To Pitch (filtered cc): time_step, f0_min, f0_max, maximum_number_of_candidates, very_accurate$, attenuation_at_top, silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, voiced_unvoiced_cost
  elsif method$ = "ac"
    pitch = noprogress To Pitch (ac): time_step, f0_min, maximum_number_of_candidates, very_accurate$, silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, voiced_unvoiced_cost, f0_max
  elsif method$ = "cc"
    pitch = noprogress To Pitch (cc): time_step, f0_min, maximum_number_of_candidates, very_accurate$, silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, voiced_unvoiced_cost, f0_max
  elsif method$ = "raw ac"
    pitch = noprogress To Pitch (raw ac): time_step, f0_min, f0_max, maximum_number_of_candidates, very_accurate$, silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, voiced_unvoiced_cost
  elsif method$ = "raw cc"
    pitch = noprogress To Pitch (raw cc): time_step, f0_min, f0_max, maximum_number_of_candidates, very_accurate$, silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, voiced_unvoiced_cost
  elsif method$ = "shs"
    pitch = noprogress To Pitch (shs): time_step, f0_min, maximum_number_of_candidates, maximum_frequency, maximum_subharmonics, compression_factor, f0_max, number_of_points_per_octave
  else
    exitScript: "Unknown method: \'", method$, "\'. See the script header for valid values."
  endif

  # Save the binary .Pitch (preserves all candidates).
  selectObject: pitch
  Save as binary file: output_folder$ + "/" + basename$ + ".Pitch"

  # Append every frame to the combined CSV.
  n_frames = Get number of frames
  for f to n_frames
    t      = Get time from frame: f
    f0_val = Get value in frame: f, "Hertz"
    if f0_val = undefined
      f0_val$ = "NA"
    else
      f0_val$ = fixed$(f0_val, 4)
    endif
    appendFileLine: csv_path$, basename$ + "," + fixed$(t, 4) + "," + f0_val$
  endfor

  # Tidy up so the Object list does not balloon over thousands of files.
  selectObject: sound, pitch
  Remove
endfor

selectObject: file_list
Remove

appendInfoLine: ""
appendInfoLine: "Done."
appendInfoLine: "  ", n_files, " .Pitch file(s) in: ", output_folder$
appendInfoLine: "  Combined CSV: ", csv_path$
'

  # ---- Sidebar (shown when this tab is active) ----
  # Praat icon + offline-workflow note + download button. Keeps the main
  # panel uncluttered and makes the script's "you'll need Praat on your
  # machine" framing visible at all times.
  output$ui_fp_praat_sidebar <- renderUI({
    tagList(
      # Icon + label header: 48px icon on the left, "Praat" link + subtitle on the right.
      tags$div(
        style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
        tags$img(src = "Praat_icon.png", height = "48px", alt = "Praat",
                 style = "flex-shrink: 0;"),
        tags$div(
          tags$div(
            tags$a(href = "https://www.fon.hum.uva.nl/praat/",
                   target = "_blank", rel = "noopener noreferrer",
                   style = "font-weight: 700; font-size: 1.05rem; color: #222; line-height: 1.1; text-decoration: none;",
                   "Praat")
          ),
          tags$div(style = "color: #888; font-size: 0.75rem; margin-top: 2px;",
                   "Open-source acoustic analysis")
        )
      ),
      tags$div(
        style = "background-color: #fff8e1; border-left: 4px solid #e0a800; padding: 10px 12px; margin-bottom: 12px; border-radius: 4px; font-size: 0.82rem; color: #555;",
        tags$strong("Offline workflow."),
        " The script runs in Praat on your machine, not in the browser. ",
        "Use it when you want algorithm choice or fine-grained control ",
        "beyond what ", tags$code("wrassp::ksvF0()"), " in ",
        tags$strong("F0 Extraction"), " offers."
      ),
      tags$hr(),
      h5("Get the script"),
      downloadButton("fp_praat_script_download", "Download .praat",
                     icon = icon("download")),
      tags$div(style = "color: #888; font-size: 0.78rem; margin-top: 6px; font-style: italic;",
               "Saves as ", tags$code("shinytone_f0_extract.praat"), ".")
    )
  })

  output$fp_praat_script_content <- renderUI({
    tagList(
      h2(style = "display: flex; align-items: center; gap: 12px;",
         tags$img(src = "Praat_icon.png", height = "32px",
                  alt = "Praat",
                  style = "display: inline-block;"),
         tags$span("Extract f0 with Praat")),
      tags$p(
        tags$a(href = "https://www.fon.hum.uva.nl/praat/",
               target = "_blank", rel = "noopener noreferrer", "Praat"),
        " is one of the most established tools for f0 extraction, with several algorithm choices ",
        "and fine-grained control over voicing and octave-jump parameters. ",
        "The script below batch-processes a folder of ", tags$code(".wav"),
        " files and produces output that integrates cleanly with the rest of Shinytone."),

      h4("Why use Praat?"),
      tags$ul(
        tags$li(tags$strong("Wider algorithm choice."),
                " Seven pitch-detection methods including the newer ",
                tags$em("filtered autocorrelation"),
                " (Praat 6.4+), classic autocorrelation / cross-correlation, raw variants, and SHS."),
        tags$li(tags$strong("Custom voicing settings."),
                " Tune silence threshold, voicing threshold, octave-jump cost, and voiced/unvoiced cost ",
                "per your speakers and recording conditions."),
        tags$li(tags$strong("Familiar workflow."),
                " Most phoneticians already run Praat scripts; the output formats are well documented."),
        tags$li(tags$strong("All candidates preserved."),
                " The binary ", tags$code(".Pitch"), " files contain every candidate frequency and ",
                "strength per frame, which Shinytone's F0 Correction tab uses to draw the top-3 ",
                "candidate markers on the plot.")
      ),

      h4("What this script does"),
      tags$ol(
        tags$li("Reads every ", tags$code(".wav"), " file in the input folder."),
        tags$li("Runs the chosen pitch-extraction method (default: ",
                tags$code("filtered ac"), ", Praat 6.4+)."),
        tags$li("Saves a binary ", tags$code("<basename>.Pitch"),
                " file per recording into the output folder."),
        tags$li("Writes a single ", tags$code("f0_long.csv"),
                " with columns ", tags$code("token, time, f0"),
                " (long format), one row per frame per token.")
      ),

      h4("The script"),
      tags$p(style = "color: #777; font-size: 0.85rem; margin-bottom: 6px;",
        "Use the ", tags$strong("Download .praat"),
        " button in the left sidebar to save the script as ",
        tags$code("shinytone_f0_extract.praat"), ", or copy from the block below."),
      tags$pre(style = "background: #f6f8fa; border: 1px solid #d6e7df; padding: 12px; border-radius: 4px; font-size: 0.78rem; line-height: 1.4; overflow-x: auto; max-height: 480px;",
        tags$code(praat_script_text)
      ),

      h4("How to run it in Praat"),
      tags$ol(
        tags$li("Open Praat (6.4 or newer recommended for the ",
                tags$code("filtered"), " methods)."),
        tags$li("From the menu: ", tags$strong("Praat"), " > ",
                tags$strong("Open Praat script..."),
                ", then choose the ", tags$code("shinytone_f0_extract.praat"),
                " file you downloaded."),
        tags$li("Press ", tags$kbd("Ctrl"), " / ", tags$kbd("Cmd"), " + ",
                tags$kbd("R"), " to run. ", tags$strong("Two dialogs"),
                " will appear in sequence:"),
        tags$ul(style = "margin-top: -6px;",
          tags$li(tags$strong("Basic settings:"),
                  " input / output folders, pitch range, time step, and method."),
          tags$li(tags$strong("Advanced parameters:"),
                  " silence threshold, voicing threshold, octave costs, etc., ",
                  "pre-filled with Praat's defaults for the method you chose ",
                  "(same defaults as Praat's native ", tags$code("To Pitch (...)"),
                  " dialog).")
        ),
        tags$li("Click ", tags$strong("OK"),
                " on each. Progress and the final summary appear in the Info window.")
      ),

      h4("Tuning advanced parameters"),
      tags$p("The second dialog exposes the same parameters as Praat's native ",
             tags$code("To Pitch (...)"),
             " interface, so you can tune them without editing code. ",
             "Defaults differ by method family (e.g., voicing threshold is ",
             tags$code("0.50"), " for filtered, ", tags$code("0.45"),
             " for classic), and the dialog loads the right ones automatically."),
      tags$ul(
        tags$li(tags$strong("Silence threshold"),
                " - frames quieter than this are treated as unvoiced."),
        tags$li(tags$strong("Voicing threshold"),
                " - minimum local correlation strength for a frame to count as voiced."),
        tags$li(tags$strong("Octave cost / octave-jump cost"),
                " - bias the path-search toward same-octave neighbours; raise to reduce jumps."),
        tags$li(tags$strong("Voiced/unvoiced cost"),
                " - penalty for switching voicing between consecutive frames."),
        tags$li(tags$strong("Max candidates"), " and ", tags$strong("Very accurate"),
                " - more candidates / accurate mode slow the analysis but can help with noisy recordings.")
      ),
      tags$p("See the Praat manual entries ", tags$code("Sound: To Pitch (filtered ac)"),
             ", ", tags$code("...(cc)"), ", etc., for the meaning of each argument."),

      h4("Bring the results back to Shinytone"),
      tags$ul(
        tags$li(tags$strong("If you want to review / correct f0 frame by frame:"),
                " upload the ", tags$code(".wav"), " files ", tags$em("and"),
                " their matching ", tags$code(".Pitch"),
                " files together in ",
                tags$strong("F0 Processing > Start"), ". Then in ",
                tags$strong("F0 Extraction"), " select ",
                tags$em("Use uploaded .Pitch / .PitchTier (Praat)"),
                " and click Run extraction. In ", tags$strong("F0 Correction"),
                " the top-3 candidate markers will show up automatically."),
        tags$li(tags$strong("If the f0 is already clean enough:"),
                " upload ", tags$code("f0_long.csv"), " directly in ",
                tags$strong("F0 Analysis > Start"), " and proceed to ",
                tags$em("Normalise"), ", ", tags$em("Visualise"), ", ",
                tags$em("Inspect"), ", or any of the modelling tabs.")
      )
    )
  })

  output$fp_praat_script_download <- downloadHandler(
    filename = function() "shinytone_f0_extract.praat",
    content  = function(file) writeLines(praat_script_text, file)
  )
}
