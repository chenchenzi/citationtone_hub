# global.R — sourced by Shiny into the global environment before each
# session. Define helpers here that the ui_*.R modules need to reach,
# since those modules are also defined in globalenv (via source(...)
# calls inside server.R) and cannot see definitions made in server.R's
# own sourcing environment.

# Disable Shiny's automatic loading of files in ./R. We're a package now,
# so Shiny would otherwise auto-source the package functions in addition
# to our explicit loop below — harmless duplication, but it triggers
# `warn_if_app_dir_is_package()` on every launch. We do the loading
# ourselves so we stay in control of order.
options(shiny.autoload.r = FALSE)

# ---------------------------------------------------------------------------
# Shared helper for smart selectInput defaults.
# guess_var(vars, patterns, fallback_idx) returns the first column in
# `vars` whose name matches any of the regex `patterns` (case-insensitive),
# falling back to vars[fallback_idx] (or vars[1]) if nothing matches.
# var_patterns is a registry of common role -> patterns so callers can do
#   selected = guess_var(vars, var_patterns$f0, 2)
# without repeating the pattern list across tabs.
# ---------------------------------------------------------------------------
guess_var <- function(vars, patterns, fallback_idx = 1) {
  for (pat in patterns) {
    m <- grep(pat, vars, ignore.case = TRUE, value = TRUE)
    if (length(m) > 0) return(m[1])
  }
  # The fallback never picks voiced_run, the gap column the Start page adds
  # (D4): it is no token, tone, speaker or f0.
  pool <- vars[vars != "voiced_run"]
  if (!length(pool)) pool <- vars
  if (fallback_idx <= length(pool)) pool[fallback_idx] else pool[1]
}

# ---------------------------------------------------------------------------
# source_picker(): the selected entry of a tab's "Select dataset" menu. Keeps
# the user's current choice while it is still offered, but selects `prefer`
# (the curated + normalised data, the furthest-processed dataset) once per
# Normalise run that made it: `stamp` identifies the run (attr "run"), so a
# new run is picked up even if the user had switched away from an older one.
# Create one per menu, in the module body:
#   pick_dataset <- source_picker()
#   selected = pick_dataset(input$model_dataset, ds_choices, "normalised",
#                           stamp = attr(curated_norm_data(), "run"))
# ---------------------------------------------------------------------------
source_picker <- function(prefer = "curated_norm") {
  seen <- NULL
  function(current, choices, fallback, stamp = NULL) {
    has   <- prefer %in% choices
    fresh <- has && !identical(stamp, seen)
    if (has) seen <<- stamp
    if (fresh) prefer
    else if (!is.null(current) && current %in% choices) current
    else fallback
  }
}

# ---------------------------------------------------------------------------
# column_picker(): the user's own picks in a tab's menus. A value a menu
# sends that differs from the default it was last drawn with is the user's
# pick. It is kept until the user picks the menu's current default again, or
# the data no longer has it, so a default that changes and changes back (a
# Start-page column, say) never undoes it. The value a menu sends when it is
# drawn is its default, so it is not a pick. Create one per tab, in the
# module body, with the ids of the menus it covers:
#   pick <- column_picker(input, c("model_token_var", "model_f0_var"))
#   selectInput("model_f0_var", ..., selected = pick("model_f0_var", vars, default))
# ---------------------------------------------------------------------------
column_picker <- function(input, ids) {
  shown  <- new.env(parent = emptyenv())   # the default each menu was last drawn with
  picked <- new.env(parent = emptyenv())   # the user's own picks
  for (i in ids) local({
    id <- i
    observeEvent(input[[id]], {
      v <- input[[id]]
      picked[[id]] <- if (!identical(v, shown[[id]])) v
    }, ignoreInit = TRUE)
  })
  function(id, choices, default) {
    shown[[id]] <- default
    v <- picked[[id]]
    if (is.character(v) && length(v) == 1 && !is.na(v) && v %in% choices) return(v)
    picked[[id]] <- NULL
    default
  }
}

# ---------------------------------------------------------------------------
# curated_reminder(): a note under a modelling tab's "Select dataset" menu
# while "Curated data" is chosen. Any normalised columns in it were computed
# before curation, so point to normalising the curated data (About page,
# path 5: Curate, then Normalise).
# ---------------------------------------------------------------------------
curated_reminder <- function(input_id) {
  conditionalPanel(sprintf("input.%s == 'curated'", input_id),
    tags$div(style = paste("font-size: 0.75rem; color: #6b5a1e; background: #fff8e1;",
                           "border-radius: 4px; padding: 5px 8px; margin: -6px 0 10px 0;"),
      icon("lightbulb"),
      HTML(paste(" Any normalised columns here were computed before curation. For analysis,",
                 "normalise the curated data (<strong>Normalise</strong> tab, data source",
                 "<strong>Curated data</strong>) and choose <strong>Curated + normalised data</strong>."))))
}

# ---------------------------------------------------------------------------
# read_data_code(): the lines a tab's "Show R code" uses to read the uploaded
# CSV the way raw_dataset() (server.R) does: any text encoding, Praat's
# --undefined-- as NA and 0 Hz f0 as NA. A plain read.csv() would keep the
# zeros the app reads as NA, so the snippet would not reproduce the app's
# plot or fit.
# With `start` (server.R's start_settings(), which the tabs pass on as it is)
# it also repeats the Start-page steps, in the order dataset() runs them:
# the discarded-token exclusion, the metadata join, the duration columns and
# the screen of rows without f0 (after the flags and the drop of the
# flagged tokens, when done), with their counts in comments. Without
# `start` the lines are the read and zero_hz_to_na() only, as before.
# ---------------------------------------------------------------------------
read_data_code <- function(ds_name, start = NULL) {
  read <- c("# Read your data as the app does (adjust the path to where the file is):",
            "# any text encoding, Praat's --undefined-- as NA, and 0 Hz f0 as NA",
            paste0("dat <- shinytone::read_csv_utf8(", encodeString(ds_name, quote = '"'),
                   ", stringsAsFactors = FALSE,"),
            '                                na.strings = c("NA", "--undefined--"))',
            "dat <- shinytone::zero_hz_to_na(dat)")
  if (is.null(start)) return(read)
  q <- function(x) encodeString(as.character(x), quote = '"')
  out <- character(0)
  if (length(start$version) && !is.na(start$version)) {
    out <- c(out,
      "# The Start-page steps below need this version of shinytone or later",
      sprintf('stopifnot(utils::packageVersion("shinytone") >= %s)', q(start$version)))
  }
  out <- c(out, read)
  z <- start$zero_hz
  if (length(z) && sum(z) > 0) {
    out <- c(out, paste0("# 0 Hz read as NA: ",
                         paste(sprintf("%s (%s)", names(z), count_label(z, "row")),
                               collapse = ", ")))
  }

  # 1. Tokens discarded in F0 Correction
  dr <- start$dropped
  if (!is.null(dr)) {
    what <- if (isTRUE(dr$rows == 0)) "no token is marked"
            else if (!is.null(dr$tokens)) sprintf("%s, %s", count_label(dr$tokens, "token"),
                                                  count_label(dr$rows, "row"))
            else count_label(dr$rows, "row")
    out <- c(out, "",
      sprintf("# Start page: exclude what F0 Correction discarded (token_dropped = TRUE: %s)", what),
      "dat <- dat[!(as.logical(dat$token_dropped) %in% TRUE), ]",
      "dat$token_dropped <- NULL",
      "rownames(dat) <- NULL")
  }

  # 2. Attached metadata
  m <- start$metadata
  if (!is.null(m) && length(m$cols)) {
    src <- m$source
    if (identical(src$type, "csv") && length(src$file) && length(src$meta_key)) {
      key_fun <- if (isTRUE(src$strip))
        "key  <- function(x) tolower(trimws(tools::file_path_sans_ext(as.character(x))))"
      else "key  <- function(x) tolower(trimws(as.character(x)))"
      out <- c(out, "",
        sprintf("# Start page: attach %s from %s, matching %s to its %s column",
                paste(m$cols, collapse = ", "), src$file, m$key, src$meta_key),
        sprintf("# (case and spaces ignored%s; adjust the path)",
                if (isTRUE(src$strip)) ", file extensions stripped" else ""),
        paste0("meta <- shinytone::read_csv_utf8(", q(src$file), ", stringsAsFactors = FALSE)"),
        key_fun,
        sprintf("meta <- meta[!duplicated(key(meta[[%s]])), ]", q(src$meta_key)),
        sprintf("row  <- match(key(dat[[%s]]), key(meta[[%s]]))", q(m$key), q(src$meta_key)),
        sprintf("for (col in %s) dat[[col]] <- meta[[col]][row]", code_vector(m$cols)))
    } else if (identical(src$type, "derive") && length(src$sep) && length(src$names)) {
      n <- length(src$names)
      out <- c(out, "",
        sprintf("# Start page: split %s at %s into %s", m$key, q(src$sep),
                paste(src$names, collapse = ", ")),
        sprintf("seg <- lapply(strsplit(as.character(dat[[%s]]), %s, fixed = TRUE), `[`, seq_len(%d))",
                q(m$key), q(src$sep), n))
      for (cn in m$cols) {
        k <- match(cn, src$names)
        if (!is.na(k)) out <- c(out, sprintf('dat[[%s]] <- vapply(seg, `[`, "", %d)', q(cn), k))
      }
    } else {
      out <- c(out, "",
        sprintf("# Start page: metadata columns %s were attached by %s; join them to dat",
                paste(m$cols, collapse = ", "), m$key))
    }
  }

  # 3. Contour-duration columns
  for (s in start$durations) {
    args <- paste0("token = ", q(s$token), ", time = ", q(s$time), ", f0 = ", q(s$f0),
                   if (!is.null(s$set)) paste0(", set = ", q(s$set)) else "",
                   ", name = ", q(s$name))
    if (length(s$unvoiced)) {
      # Measured with the values the screen reads as no f0 set to NA, on a
      # copy: dat keeps them until the screen below.
      out <- c(out, "",
        sprintf("# Start page: contour duration (%s), reading %s in %s as no f0, as the screen does",
                s$name, paste(s$unvoiced, collapse = " and "), s$f0),
        "dur <- dat",
        sprintf("dur[[%s]][dur[[%s]] %%in%% %s] <- NA", q(s$f0), q(s$f0),
                deparse(as.numeric(s$unvoiced))),
        sprintf("dat[[%s]] <- shinytone::contour_duration(dur, %s)[[%s]]", q(s$name), args, q(s$name)),
        "rm(dur)")
    } else {
      out <- c(out, "", sprintf("# Start page: contour duration (%s)", s$name),
        paste0("dat <- shinytone::contour_duration(dat, ", args, ")"))
    }
  }

  # 4. Rows without f0
  sc <- start$screen
  if (!is.null(sc) && length(sc$f0)) {
    unv <- if (length(sc$unvoiced)) paste0(", unvoiced = ", deparse(as.numeric(sc$unvoiced)))
           else ""
    args <- if (length(sc$token)) paste0("dat, token = ", q(sc$token), ", time = ", q(sc$time),
                                         ", f0 = ", q(sc$f0)) else NULL
    out <- c(out, "")
    if (!length(sc$token)) {
      out <- c(out, if (!is.null(sc$error))
        sprintf("# Start page: the rows without f0 in column %s could not be screened (%s)",
                sc$f0, sc$error)
      else if (isTRUE(sc$without == 0))
        sprintf(paste("# Start page: every row has f0 in column %s (%s); no token variable,",
                      "so gaps were not checked"),
                sc$f0, count_label(sc$rows, "row"))
      else sprintf(paste("# Start page: %s of %s %s no f0 in column %s; choose the token variable",
                         "under Data screening on the Start page to remove them"),
                   fmt_count(sc$without), count_label(sc$rows, "row"),
                   n_verb(sc$without, "has", "have"), sc$f0))
    } else {
      out <- c(out, "# The Start page's Data screening report:",
               paste0("# shinytone::screen_f0_rows(", args, unv, ")"))
      # Before the removal: flag the tokens with an issue (when some token
      # is flagged) and drop them (when ticked). The values read as no f0
      # become NA first, so the flags read them as no f0.
      fg <- sc$flag
      if (!is.null(fg)) {
        if (length(sc$unvoiced)) {
          out <- c(out, sprintf("# Read %s in %s as no f0", paste(sc$unvoiced, collapse = " and "), sc$f0),
                   sprintf("dat[[%s]][dat[[%s]] %%in%% %s] <- NA", q(sc$f0), q(sc$f0),
                           deparse(as.numeric(sc$unvoiced))))
        }
        n_f <- fg$tokens
        out <- c(out,
          sprintf('# Flag %s in a screen_flag column ("" for the other tokens)', count_label(n_f, "token")),
          paste0("dat <- shinytone::flag_f0_tokens(", args, ")"))
        if (isTRUE(fg$drop)) {
          out <- c(out,
            sprintf("# Drop the %s (%s)",
                    if (n_f == 1) "flagged token" else paste(fmt_count(n_f), "flagged tokens"),
                    count_label(fg$rows, "row")),
            'dat <- dat[dat[["screen_flag"]] == "", , drop = FALSE]',
            "rownames(dat) <- NULL",
            'dat[["screen_flag"]] <- NULL')
        }
      }
      # An upload's own screen_flag column, from an earlier screening, when
      # no token is flagged now
      if (isTRUE(sc$old_flag)) {
        out <- c(out, "# No token is flagged now, so the file's own screen_flag column goes",
                 'dat[["screen_flag"]] <- NULL')
      }
      # The removal's counts: after the drop, when done
      s <- if (!is.null(sc$post)) sc$post else sc$summary
      # "the row" for one row, "the 16 rows" for more
      n_w <- s[["rows_without_f0"]]
      rows_w <- if (n_w == 1) "row" else paste(fmt_count(n_w), "rows")
      if (identical(sc$action, "remove")) {
        out <- c(out, sprintf("# Remove the %s without f0 and add voiced_run", rows_w))
      } else if (identical(sc$action, "mark")) {
        out <- c(out, strwrap(sprintf("Every row %shas f0. Mark the %s the time steps show in voiced_run.",
                                      if (!is.null(sc$post)) "left " else "",
                                      count_label(sc$gaps_marked, "gap")),
                              width = 78, prefix = "# "))
      } else if (n_w > 0) {
        out <- c(out, sprintf("# The %s without f0 %s (removal unticked)", rows_w,
                              n_verb(n_w, "stays", "stay")))
      } else if (isTRUE(sc$rows_after == 0)) {
        out <- c(out, "# No rows are left")
      } else if (!is.null(sc$post)) {
        out <- c(out, sprintf("# Every row left has f0 in column %s; nothing to remove", sc$f0))
      } else {
        out <- c(out, sprintf("# Every row has f0 in column %s; nothing to remove", sc$f0))
      }
      if (sc$action %in% c("remove", "mark") && isTRUE(sc$min_run > 1)) {
        out <- c(out, sprintf("# min_run = 3 also removes %s in short runs at a token's start or end",
                              count_label(sc$short_rows, "sample")))
      }
      if (sc$action %in% c("remove", "mark")) {
        out <- c(out, paste0("dat <- shinytone::drop_rows_without_f0(", args,
                             if (isTRUE(sc$min_run > 1)) paste0(", min_run = ", sc$min_run) else "",
                             unv, ")"))
      }
    }
    # (Already done above when tokens were flagged)
    if (!(sc$action %in% c("remove", "mark")) && length(sc$unvoiced) && is.null(sc$flag)) {
      out <- c(out, sprintf("# Read %s in %s as no f0", paste(sc$unvoiced, collapse = " and "), sc$f0),
               sprintf("dat[[%s]][dat[[%s]] %%in%% %s] <- NA", q(sc$f0), q(sc$f0),
                       deparse(as.numeric(sc$unvoiced))))
    }
  }
  out
}

# Counts as text: 30870 -> "30,870"; count_label(3, "token") -> "3 tokens".
fmt_count <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
count_label <- function(n, unit) {
  paste(fmt_count(n), ifelse(n == 1, unit, paste0(unit, "s")))
}
# The verb for a count: n_verb(1, "has", "have") -> "has", for "1 of 20 rows has".
n_verb <- function(n, one, many) if (isTRUE(n == 1)) one else many

# compute_mean_contour() and cluster_features() send a message when a token
# has two or more rows at the same time in one segment, so their f0 values
# there are averaged (note_averaged_ties() in R/time_norm.R). A message only
# reaches the R console, so this handler for withCallingHandlers() shows that
# one as a warning toast and muffles it, with the count of tokens written as
# 1,200 like the app's other counts. Other messages, such as "already
# normalised", pass on unchanged.
averaged_ties_toast <- function(m) {
  txt <- conditionMessage(m)
  if (grepl("have the same time, so their f0 values there are averaged", txt, fixed = TRUE)) {
    n <- regmatches(txt, regexec("^In ([0-9]+) token", txt))[[1]]
    if (length(n) == 2L)
      txt <- paste0("In ", fmt_count(as.numeric(n[2])), substring(txt, nchar(n[1]) - 5L))
    showNotification(trimws(txt), type = "warning", duration = 15)
    invokeRestart("muffleMessage")
  }
}
# A character vector as R code: c("a", "b"), or "a" for one value.
code_vector <- function(x) {
  x <- encodeString(as.character(x), quote = '"')
  if (length(x) == 1) x else paste0("c(", paste(x, collapse = ", "), ")")
}
# The first few values, then "and N more": "a, b, c and 12 more".
list_some <- function(x, n = 5) {
  x <- as.character(x)
  if (length(x) <= n) {
    if (length(x) <= 1) return(paste(x, collapse = ""))
    return(paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)]))
  }
  paste0(paste(x[seq_len(n)], collapse = ", "), " and ", fmt_count(length(x) - n), " more")
}

# The version of shinytone the app runs on, for the version check in the R
# code: the loaded package, or the source tree's DESCRIPTION when the app
# runs from a checkout. NULL when neither can be read.
shinytone_version <- function() {
  v <- tryCatch(if ("shinytone" %in% loadedNamespaces())
                  as.character(getNamespaceVersion("shinytone")),
                error = function(e) NULL)
  if (length(v)) return(unname(v))
  for (f in c("../../DESCRIPTION", "DESCRIPTION")) {
    d <- tryCatch(read.dcf(f, fields = c("Package", "Version")), error = function(e) NULL)
    if (!is.null(d) && identical(unname(d[1, "Package"]), "shinytone")) return(unname(d[1, "Version"]))
  }
  tryCatch(as.character(utils::packageVersion("shinytone")), error = function(e) NULL)
}

# ---------------------------------------------------------------------------
# Start-page columns: the token, time and f0 pickers in the "Rows without
# f0" box of the Start sidebar. The screen runs on them (server.R:
# start_cols(), f0_screen(), dataset()) and every tab takes its defaults
# from them.
#   start_screen_choices(raw)   the columns each picker offers
#   start_screen_defaults(raw)  the column each picker selects first
#   resolve_start_cols(raw, token, time, f0)
#     the picks checked against the upload, so a NULL or stale value falls
#     back to the default the picker shows. token "" means "no token
#     column" and gives NULL; it is offered only for a file where no column
#     looks like a token column.
# Token: vis_token_col(), and never a fallback such as the first column, as
# a file without a token column (point-per-row data such as dc21f0.csv) would
# then be grouped by its point number. Time: never a *_t01, *_tseq or
# *_tbound axis, which restart or change step at every segment and read as
# false gaps.
# f0: f0_corrected next to f0 (F0 Correction's file), f0 next to f0_original
# (F0 Extraction's export with corrections), otherwise the first Hz column.
# ---------------------------------------------------------------------------
start_screen_choices <- function(raw) {
  vars <- names(raw)
  num  <- vars[vapply(raw, function(x) is.numeric(x) || is.logical(x), logical(1))]
  # "" (no token column) is offered only when no column looks like one, so a
  # "" left from a file without a token column is not kept for the next file.
  list(token = if (is.null(vis_token_col(vars))) c("", vars) else vars,
       time  = vars[!grepl("_(t01|tseq|tbound)$", vars)],
       f0    = if (length(num)) num else vars)
}

start_screen_defaults <- function(raw) {
  vars <- names(raw)
  ch   <- start_screen_choices(raw)
  tok  <- vis_token_col(vars)
  f0 <- if (all(c("f0", "f0_corrected") %in% ch$f0)) "f0_corrected"
        else if ("f0" %in% ch$f0 && "f0_original" %in% vars) "f0"
        else {
          hz <- intersect(tryCatch(hz_f0_columns(raw), error = function(e) character(0)), ch$f0)
          # The guess prefers an f0-like name with some values: an all-NA f0
          # next to another f0 column would leave no rows after removal.
          pats <- c("^f0_hz$", "^f0$", var_patterns$f0)
          some <- ch$f0[vapply(ch$f0, function(cn) {
            x <- raw[[cn]]
            any(is.finite(suppressWarnings(as.numeric(if (is.factor(x)) as.character(x) else x))))
          }, logical(1))]
          named <- unlist(lapply(pats, grep, x = some, ignore.case = TRUE, value = TRUE))
          if (length(hz)) hz[1]
          else if (length(named)) named[1]
          else if (length(ch$f0)) guess_var(ch$f0, pats, 1)
          else NULL
        }
  time <- NULL
  if (length(ch$time)) {
    m <- unlist(lapply(var_patterns$time, grep, x = ch$time, ignore.case = TRUE, value = TRUE))
    num_t <- setdiff(intersect(ch$time, ch$f0), c(tok, f0))
    time <- if (length(m)) m[1] else if (length(num_t)) num_t[1] else ch$time[1]
  }
  list(token = tok, time = time, f0 = f0)
}

resolve_start_cols <- function(raw, token = NULL, time = NULL, f0 = NULL) {
  if (is.null(raw) || !length(names(raw))) return(NULL)
  ch  <- start_screen_choices(raw)
  def <- start_screen_defaults(raw)
  keep <- function(cur, choices, fallback) {
    if (is.character(cur) && length(cur) == 1 && !is.na(cur) && cur %in% choices) cur
    else fallback
  }
  tok <- keep(token, ch$token, def$token)
  list(token = if (identical(tok, "")) NULL else tok,
       time  = keep(time, ch$time, def$time),
       f0    = keep(f0, ch$f0, def$f0))
}

# start_change_text(): what changed on the Start page between two snapshots
# of its settings (server.R's start_state()), for the toast that says why
# results were cleared: '"Remove rows without f0" was unticked'.
start_change_text <- function(old, new) {
  if (is.null(old) || is.null(new)) return("the data changed")
  out <- character(0)
  boxes <- c(exclude = "Exclude discarded tokens", remove = "Remove rows without f0",
             short = "Also remove runs of 1\u20132 samples", unvoiced = "Read 0 / -1 as no f0",
             mark = "Mark the gaps in a voiced_run column",
             drop_flagged = "Drop the flagged tokens")
  for (k in names(boxes)) {
    if (!identical(isTRUE(old[[k]]), isTRUE(new[[k]])))
      out <- c(out, sprintf("\"%s\" was %s", boxes[[k]],
                            if (isTRUE(new[[k]])) "ticked" else "unticked"))
  }
  for (k in c("token", "time", "f0")) {
    if (!identical(old[[k]], new[[k]]))
      out <- c(out, sprintf("the %s variable is now %s", k,
                            if (is.null(new[[k]])) "none" else new[[k]]))
  }
  if (!identical(old$meta, new$meta))
    out <- c(out, if (is.null(new$meta)) "the metadata was detached" else "metadata was attached")
  if (!identical(old$dur, new$dur))
    out <- c(out, if (length(new$dur) > length(old$dur)) "a duration column was added"
                  else "the duration columns changed")
  if (!length(out)) return("the data changed")
  list_some(out, 4)
}

# ---------------------------------------------------------------------------
# start_col(): one of the Start-page columns ("token", "time" or "f0") from
# the start_cols reactive the server passes to each tab, or NULL: before an
# upload, when there is no such column, or when the tab got no start_cols.
# A tab's picker defaults to it when it is among the picker's choices.
# ---------------------------------------------------------------------------
start_col <- function(start_cols, key) {
  if (!is.function(start_cols)) return(NULL)
  sc <- tryCatch(start_cols(), error = function(e) NULL)
  if (!is.list(sc)) return(NULL)
  v <- sc[[key]]
  if (is.character(v) && length(v) == 1 && !is.na(v) && nzchar(v)) v else NULL
}

# ===========================================================================
# Start page data report (start_ui.R, output$f0_screen_report): the facts in
# its "Your data" card. Everything is read from the uploaded rows
# (prepared(): discarded tokens excluded, metadata joined), before any
# removal, so the counts do not change with the Start page's boxes and are
# the same on a raw file and on the same file with its rows without f0
# removed beforehand.
#   start_role_col(vars, role)    the speaker or tone column, by the name
#                                 patterns the tabs guess with; NULL when no
#                                 name matches (no fallback column)
#   start_step_text(x, unit)      a time step as text in ms: "10 ms"
#   start_f0_kind(d, token, time) how the f0 is sampled: "frames", "points"
#                                 (equidistant) or "uneven", with the step,
#                                 the points per token and the tokens with
#                                 fewer or more
#   start_grid_problems(sc, kind, has_run)
#                                 the time-step problems that matter: only
#                                 when gaps could come from the time steps
#                                 alone and the steps could not be read
#   start_data_facts(d, scr)      all of it, for one upload
# ===========================================================================
start_role_col <- function(vars, role) {
  vars <- setdiff(vars, "voiced_run")
  for (pat in var_patterns[[role]]) {
    m <- grep(pat, vars, ignore.case = TRUE, value = TRUE)
    if (length(m)) return(m[1])
  }
  NULL
}

# A step in the time column's units as milliseconds, by Inspect's rule: a
# median step below 1 means seconds. A no-break space keeps "10 ms" on one
# line.
start_step_text <- function(x, unit) {
  ms <- if (identical(unit, "s")) x * 1000 else x
  paste0(format(signif(ms, 3), trim = TRUE, scientific = FALSE), "\u00a0ms")
}

# How finely the times are written: 10^-d for the fewest decimals d (0 to 9)
# that every time has, 0 when they need more than 9 (as screen_f0_rows()).
start_time_resolution <- function(x) {
  x <- unique(x[is.finite(x)])
  if (!length(x)) return(NA_real_)
  for (dd in 0:9) {
    s <- x * 10^dd
    if (all(abs(s - round(s)) <= 1e-6)) return(10^-dd)
  }
  0
}

# A step read from times written too coarsely for it, as shown: a whole
# number of units of the rounding when it is within 2% of one (10 ms frames
# written to 0.01), else to the nearest 0.5 ms (12.5 ms, not 12.56).
start_snap_step <- function(x, res, unit) {
  if (!is.finite(x) || !isTRUE(res > 0)) return(x)
  k <- round(x / res)
  if (k >= 1 && abs(x / res - k) <= 0.02 * k) return(k * res)
  f <- if (identical(unit, "s")) 2000 else 2
  round(x * f) / f
}

# Frames, equidistant points or uneven points. Steps are taken between
# consecutive rows of a token in time order (rows without f0 included, as
# Inspect does). When the times are written too coarsely for the steps
# (written to 0.01 for 12.5 ms frames), a token's step is its span over its
# steps instead, which the rounding moves by at most one unit over the span,
# and `about` is TRUE unless the step is a whole number of units.
#   frames: most tokens (90%) share one step, within 5%
#   points: a time_prop column, or most tokens (80%) have the same number of
#           rows and are evenly spaced within the token
#   uneven: neither
start_f0_kind <- function(d, token, time) {
  num <- function(x) suppressWarnings(as.numeric(if (is.factor(x)) as.character(x) else x))
  tk   <- as.character(d[[token]])
  utok <- unique(tk)
  tid  <- match(tk, utok)
  nt   <- length(utok)
  n_rows <- tabulate(tid, nt)
  tv <- num(d[[time]])
  ok <- which(is.finite(tv))
  o  <- ok[order(tid[ok], tv[ok], method = "radix")]
  g  <- tid[o]
  t  <- tv[o]
  m  <- length(o)
  same <- c(FALSE, g[-1L] == g[-m])
  st <- c(NA_real_, diff(t))
  st[!same] <- NA_real_
  pos <- !is.na(st) & st > 0
  out <- list(kind = NA_character_, tokens = nt, n_rows = n_rows, unit = NA_character_,
              step = NA_real_, resolution = NA_real_)
  if (!any(pos)) return(out)
  unit <- if (stats::median(st[pos]) < 1) "s" else "ms"
  fg   <- factor(g, levels = seq_len(nt))
  med  <- vapply(split(st[pos], fg[pos]), function(v) if (length(v)) stats::median(v) else NA_real_,
                 numeric(1), USE.NAMES = FALSE)
  n_t  <- tabulate(g, nt)
  lo   <- vapply(split(t, fg), function(v) if (length(v)) min(v) else NA_real_, numeric(1), USE.NAMES = FALSE)
  hi   <- vapply(split(t, fg), function(v) if (length(v)) max(v) else NA_real_, numeric(1), USE.NAMES = FALSE)
  span_st <- ifelse(n_t >= 2L, (hi - lo) / pmax(n_t - 1L, 1L), NA_real_)
  res  <- start_time_resolution(t)
  coarse <- isTRUE(res > stats::median(med, na.rm = TRUE) / 5 * (1 + 1e-9))
  est  <- if (coarse) span_st else med
  est[!is.finite(est) | est <= 0] <- NA_real_
  step <- stats::median(est, na.rm = TRUE)
  # Rounded times move a token's span by up to one unit of the rounding
  slack <- if (coarse) res / pmax(n_t - 1L, 1L) else 0
  common <- is.finite(step) &&
    mean(abs(est - step) <= 0.05 * step + slack, na.rm = TRUE) >= 0.9
  # Equal row counts, and evenly spaced: every step of the token within 5%
  # of its usual step (or one unit of the rounding).
  tab   <- tabulate(n_rows)
  n_pts <- which.max(tab)
  share <- max(tab) / nt
  tol   <- pmax(0.05 * med[g], 1.01 * res)
  bad   <- !is.na(st) & abs(st - med[g]) > tol
  even  <- tabulate(g[bad], nt) == 0L & n_t >= 2L
  at_n  <- n_rows == n_pts
  has_tp <- "time_prop" %in% names(d)
  kind <- if (has_tp) "points"
          else if (common) "frames"
          else if (nt >= 2L && n_pts >= 3L && share >= 0.8 && mean(even[at_n]) >= 0.8) "points"
          else "uneven"
  out$kind <- kind
  out$unit <- unit
  # Times written to the step itself (10 ms frames written to 0.01): nearly
  # every step is one unit, and the longer ones are gaps, which a span would
  # count in. Otherwise the span gives the step (12.5 ms written to 0.01).
  if (coarse) {
    ku <- round(st[pos] / res)
    step <- if (stats::median(ku) == 1 && mean(ku == 1) >= 0.85) res
            else start_snap_step(step, res, unit)
  }
  out$step <- step
  out$about <- coarse && !isTRUE(abs(step / res - round(step / res)) < 1e-6)
  out$resolution <- res
  out$span_step <- span_st
  if (kind == "points") {
    out$n_points <- n_pts
    grp <- function(sel, decreasing) {
      if (!any(sel)) return(list())
      cnt <- sort(unique(n_rows[sel]), decreasing = decreasing)
      lapply(stats::setNames(cnt, cnt), function(k) utok[sel & n_rows == k])
    }
    out$fewer <- grp(n_rows < n_pts, TRUE)
    out$more  <- grp(n_rows > n_pts, FALSE)
  } else if (kind == "uneven") {
    q <- stats::quantile(st[pos], c(0.05, 0.95), names = FALSE)
    out$range <- q
  }
  out
}

# The time-step problems the report shows, each a list(key, n, tokens, ...)
# that start_ui.R words. They matter only when gaps inside tokens could come
# from the time steps alone: the file (or, for a token set aside, the token)
# has no rows without f0 inside its f0 span, and no voiced_run column marks
# its gaps. Equidistant points are left out: a missing point shows in the
# count of points per token instead. When everything can be read, the list
# is empty.
start_grid_problems <- function(sc, kind, has_run = FALSE) {
  out <- list()
  if (is.null(sc) || isTRUE(has_run) || identical(kind$kind, "points")) return(out)
  s    <- attr(sc, "summary")
  grid <- attr(sc, "grid")
  why  <- names(grid$why)
  # Frames with the same number in every token are counted as points, in
  # the Your data card and in the Data screening card (equal_rows)
  noun <- if (identical(kind$kind, "frames") && !isTRUE(attr(sc, "equal_rows"))) "frames"
          else "points"
  if (!isTRUE(grid$regular) && s[["inside"]] == 0L) {
    if (any(c("units", "resolution", "set_aside") %in% why) && isTRUE(grid$resolution > 0)) {
      out$rounded <- list(resolution = grid$resolution, step = kind$step, unit = kind$unit,
                          noun = noun)
    } else if ("ambiguous" %in% why) {
      out$uneven <- list(noun = noun)
    } else if ("nonincreasing" %in% why) {
      out$repeated <- list(all = TRUE, tokens = character(0))
    }
  }
  if (isTRUE(grid$regular)) {
    inside <- as.character(sc[[1L]])[sc$n_inside > 0L]
    rep <- setdiff(attr(sc, "repeated_time"), inside)
    oth <- setdiff(attr(sc, "other_step"), inside)
    if (length(rep)) out$repeated <- list(all = FALSE, tokens = rep)
    if (length(oth)) {
      sp <- kind$span_step
      tk <- as.character(sc[[1L]])
      st <- start_snap_step(stats::median(sp[match(oth, tk)], na.rm = TRUE), grid$resolution,
                            kind$unit)
      out$other_step <- list(tokens = oth, resolution = grid$resolution, step = st,
                             usual = kind$step, unit = kind$unit, noun = noun)
    }
  }
  out
}

# Decimals to write a step of `x` (in the time column's units) so that gaps
# can be read from it: enough for its 3 significant digits, and a rounding
# of at most a fifth of the step.
start_decimals <- function(x) {
  if (!is.finite(x) || x <= 0) return(NA_integer_)
  s <- signif(x, 3)
  d1 <- 0L
  while (d1 < 9L && abs(s * 10^d1 - round(s * 10^d1)) > 1e-6) d1 <- d1 + 1L
  max(d1, as.integer(ceiling(-log10(x / 5) - 1e-9)), 0L)
}

start_data_facts <- function(d, scr) {
  if (is.null(d) || is.null(scr) || is.null(scr$cols)) return(NULL)
  cols <- scr$cols
  vars <- names(d)
  used <- unlist(cols[c("token", "time", "f0")], use.names = FALSE)
  out  <- list(rows = nrow(d), without = scr$without, cols = cols)
  spk  <- start_role_col(setdiff(vars, used), "speaker")
  if (!is.null(spk)) {
    v <- as.character(d[[spk]])
    out$speakers <- list(col = spk, n = length(unique(v[!is.na(v) & nzchar(v)])))
  }
  tone <- start_role_col(setdiff(vars, c(used, spk)), "tone")
  if (!is.null(tone)) {
    v <- tryCatch(sort_tones(d[[tone]]), error = function(e) unique(as.character(d[[tone]])))
    v <- v[!is.na(v) & nzchar(v)]
    out$tones <- list(col = tone, n = length(v), values = if (length(v) <= 10) v)
  }
  sc <- scr$screen
  if (!is.null(sc) && !is.null(cols$token) && !is.null(cols$time) &&
      all(c(cols$token, cols$time) %in% vars)) {
    s <- attr(sc, "summary")
    out$tokens <- s[["tokens"]]
    kind <- tryCatch(start_f0_kind(d, cols$token, cols$time), error = function(e) NULL)
    if (!is.null(kind) && identical(kind$kind, "frames")) {
      # As voiced_run counts them: rows without f0 inside a token's f0 span,
      # steps the time steps show with no such rows in them, and gaps an
      # existing voiced_run column marks.
      kind$gaps <- s[["gaps"]] + s[["gaps_time_only"]] + s[["gaps_run"]]
      kind$gap_tokens <- s[["run_tokens"]]
    }
    out$kind <- kind
    out$problems <- if (!is.null(kind))
      start_grid_problems(sc, kind, "voiced_run" %in% vars) else list()
  }
  out
}
# ===================== end of the Start page data report ===================

# ---------------------------------------------------------------------------
# Contour-duration columns from the Start page (server.R's prepared(), the
# toast in start_ui.R and the R code in read_data_code()).
#   duration_unvoiced(d, f0, start_cols, screen_box)
#     the values the duration reads as no f0: those the "Read 0 / -1 as no
#     f0" box reads in the screened column (suggest_unvoiced()), when the
#     duration is measured on that column and the box is ticked. NULL
#     otherwise. contour_duration() counts 0 and -1 as f0, and the durations
#     come before the screen (D7), so without this a duration would count
#     samples the screen reads as no f0.
#   add_duration(d, spec, unvoiced)
#     d with the column spec$name from contour_duration(), measured with the
#     `unvoiced` values read as no f0 (the other columns are not changed).
# ---------------------------------------------------------------------------
duration_unvoiced <- function(d, f0, start_cols, screen_box) {
  scr <- start_col(start_cols, "f0")
  if (is.null(d) || is.null(scr) || !identical(f0, scr) || !f0 %in% names(d) ||
      !is.function(screen_box)) return(NULL)
  sug <- suggest_unvoiced(d[[f0]])
  if (length(sug) && isTRUE(screen_box("unvoiced"))) sug else NULL
}
add_duration <- function(d, spec, unvoiced = NULL) {
  src <- d
  if (length(unvoiced)) {
    x <- src[[spec$f0]]
    src[[spec$f0]][!is.na(x) & x %in% unvoiced] <- NA
  }
  res <- contour_duration(src, token = spec$token, time = spec$time, f0 = spec$f0,
                          set = spec$set, name = spec$name)
  d[[spec$name]] <- res[[spec$name]]
  d
}

# ---------------------------------------------------------------------------
# screened_f0_note(): a one-line note under a tab's f0 picker when the tab
# analyses another f0 column than the one the Start page screens, and that
# column still has rows without f0 in `data`. NULL otherwise.
# ---------------------------------------------------------------------------
screened_f0_note <- function(data, f0_col, start_cols) {
  scr <- start_col(start_cols, "f0")
  if (is.null(data) || is.null(scr) || !is.character(f0_col) || length(f0_col) != 1 ||
      identical(f0_col, scr) || !f0_col %in% names(data)) return(NULL)
  x <- data[[f0_col]]
  if (is.factor(x)) x <- as.character(x)
  n <- sum(!is.finite(suppressWarnings(as.numeric(x))))
  if (n == 0) return(NULL)
  tags$div(style = paste("font-size: 0.75rem; color: #6b5a1e; background: #fff8e1;",
                         "border-radius: 4px; padding: 5px 8px; margin: -6px 0 10px 0;"),
    icon("lightbulb"),
    sprintf(" %s has %s without f0. The Start page screens %s.",
            f0_col, count_label(n, "row"), scr))
}

# ---------------------------------------------------------------------------
# tone_factor_code(): the "Show R code" line that makes tone column `var` a
# factor (ordered = TRUE: an ordered factor, named `to`) with the levels the
# app uses. as.factor() sorts by locale, which puts Chinese tone names out of
# order, so when shinytone::sort_tones() gives another order for `x` (the
# column as the app has it), the line uses sort_tones() too.
# ---------------------------------------------------------------------------
tone_needs_levels <- function(x) {
  !is.null(x) && !is.factor(x) &&
    !identical(sort_tones(x, natural = FALSE), levels(factor(x)))
}
tone_factor_code <- function(var, x = NULL, ordered = FALSE, to = var) {
  fun <- if (ordered) "ordered" else "factor"
  if (tone_needs_levels(x))
    paste0("dat$", to, " <- ", fun, "(dat$", var,
           ", levels = shinytone::sort_tones(dat$", var, ", natural = FALSE))")
  else paste0("dat$", to, " <- as.", fun, "(dat$", var, ")")
}

# ---------------------------------------------------------------------------
# role_var(): the default column for a role ("tone" or "speaker") in a tab's
# variable menus. Data from Curate carries the variables used there (attr
# "curate_vars"; the Normalise tab updates it for the curated + normalised
# data), e.g. a `cluster` tone, so curated data is analysed with the labels
# that were curated. Otherwise guess as usual.
# ---------------------------------------------------------------------------
role_var <- function(d, role, vars, patterns, fallback_idx = 1) {
  v <- attr(d, "curate_vars")[[role]]
  if (!is.null(v) && v %in% vars) v else guess_var(vars, patterns, fallback_idx)
}

# ---------------------------------------------------------------------------
# curated_key(): a fingerprint of the curated data that curated + normalised
# data depends on: which tokens are kept, their tone labels, speakers and Hz
# values (`cols`). Columns added by a later Normalise run on the base data do
# not change it. Character values, so the "convert to factor" option does not.
# ---------------------------------------------------------------------------
curated_key <- function(d, cols) {
  cols <- intersect(cols, names(d))
  rlang::hash(lapply(d[cols], as.character))
}

# ---------------------------------------------------------------------------
# nows(): render inline children with NO whitespace between them.
# htmltools writes each child of a tag on its own line, and the browser
# renders that newline as a space. So
#   tags$li("In ", tags$span(class = "tab-chip", "Start"), ", upload ...")
# comes out as "In Start , upload ...", with a gap before the comma. Wrapping
# the run in nows() concatenates the rendered HTML directly, so punctuation
# sits flush against the preceding chip / code / bold span:
#   tags$li(nows("In ", tags$span(class = "tab-chip", "Start"), ", upload ..."))
# Only needed where a tag is followed by punctuation; ordinary prose is fine.
# ---------------------------------------------------------------------------
nows <- function(...) {
  parts <- lapply(list(...), function(x) paste0(as.character(x), collapse = ""))
  htmltools::HTML(paste0(unlist(parts), collapse = ""))
}

# ---------------------------------------------------------------------------
# Bulk-discard offer.
# When a corpus is large but only a small share of its tokens is flagged,
# repairing every flagged token one by one is rarely worth the time; the
# F0 Correction tab's "Discard all flagged tokens" + review loop is the
# faster route. The Inspect summary and the F0 Correction flagged-CSV
# loader both call offer_bulk_triage() to decide when to surface that
# suggestion, so the two tabs stay in sync on the thresholds.
# ---------------------------------------------------------------------------
TRIAGE_OFFER_MIN_TOKENS       <- 1000
TRIAGE_OFFER_MAX_FLAGGED_PROP <- 0.15
offer_bulk_triage <- function(n_tokens, n_flagged) {
  isTRUE(n_tokens >= TRIAGE_OFFER_MIN_TOKENS) &&
    isTRUE(n_flagged > 0) &&
    isTRUE(n_flagged / n_tokens <= TRIAGE_OFFER_MAX_FLAGGED_PROP)
}

var_patterns <- list(
  token    = c("^token$", "^token", "^filename", "^basename", "^item$", "^segment", "^id$"),
  f0       = c("^f0_st$", "^f0_zscore$", "^f0_lz$", "^f0_norm", "^f0_hz$", "^f0$", "^f0_", "^pitch"),
  time     = c("^time$", "^time", "^t$", "^timepoint", "^measurement"),
  speaker  = c("^speaker$", "^speaker", "^spk", "^subject", "^subj"),
  tone     = c("^tone$", "^tone", "^category", "^tonecat", "^label"),
  item     = c("^item$", "^word", "^syllable", "^syl"),
  duration = c("^duration$", "^dur")
)

# Normalise a join key so that e.g. "S01_T1.wav" and "s01_t1" match: optionally
# strip a file extension, then lowercase and trim. Used by the metadata joins
# in both the F0 Extraction tab and the Start tab.
make_token_key <- function(x, strip_ext = TRUE) {
  k <- as.character(x)
  if (isTRUE(strip_ext)) k <- tools::file_path_sans_ext(k)
  tolower(trimws(k))
}

# ---------------------------------------------------------------------------
# Shortest audio duration (seconds) the pitch analyser can return a frame for.
# A pitch-floor autocorrelation window spans 3 periods of the floor, so a clip
# shorter than 3 / f0_min yields no f0 (and, being even shorter than the wider
# intensity window, no intensity either). Used to flag/skip too-short files in
# the F0 Processing Start preview and the wrassp extraction loop.
# ---------------------------------------------------------------------------
min_audio_dur <- function(f0_min = 75) {
  f0_min <- suppressWarnings(as.numeric(f0_min)[1])
  if (length(f0_min) == 0 || is.na(f0_min) || f0_min <= 0) f0_min <- 75
  3 / f0_min
}

# ---------------------------------------------------------------------------
# Landmark sets in a column-name vector. A "set" is a base name X for which
# both X_start and X_end exist (as written by the F0 Extraction landmark step,
# e.g. syllable_start / syllable_end). X_i (segment index) is optional. Returns
# a named list base -> list(start, end, idx, has_i). Used by the Visualise tab
# to offer landmark-based time alignment.
# ---------------------------------------------------------------------------
landmark_sets <- function(vars) {
  out <- list()
  for (s in grep("_start$", vars, value = TRUE)) {
    base <- sub("_start$", "", s)
    e <- paste0(base, "_end")
    if (nzchar(base) && e %in% vars) {
      out[[base]] <- list(start = s, end = e,
                          idx = paste0(base, "_i"),
                          has_i = paste0(base, "_i") %in% vars)
    }
  }
  out
}

# First column name matching a token-like pattern, or NULL if none. Used to
# connect points into per-token contour lines when aligning by landmarks.
# A normalised time axis (token_t01, <tier>_t01, <tier>_tseq,
# <tier>_tbound) is never the token, though token_t01 starts with "token".
vis_token_col <- function(vars) {
  vars <- vars[!grepl("_(t01|tseq|tbound)$", vars)]
  for (pat in var_patterns$token) {
    m <- grep(pat, vars, ignore.case = TRUE, value = TRUE)
    if (length(m)) return(m[1])
  }
  NULL
}

# Smart default for a model's Time variable: prefer a landmark in-sequence
# column when one exists, so multisyllabic data lands on the
# syllable-aligned axis automatically; otherwise fall back to the usual time
# guess. `prefer` sets which comes first: "tseq" (Model and GCA, the
# default) tries every <tier>_tseq, then every <tier>_tbound; "tbound" (the
# GAMM tab) the other way round, since on <tier>_tbound the gap at a
# boundary is kept, so one smooth can follow an f0 jump there. A <tier>_t01
# of a tier with several parts per token restarts at 0 in every part, so it
# is never the default for a whole-word model; it suits syllables modelled
# one by one, each its own token.
# With the data `d` (and its token column), an in-sequence axis
# (<tier>_tseq or <tier>_tbound, held in `stacked` below) is preferred only
# when some token has more than one part (t01_parts()). A tier with one part
# per token (a rhyme or vowel tier) gives the same values in <tier>_tseq,
# <tier>_tbound and <tier>_t01, so its <tier>_t01, a 0-1 axis by name, is
# picked instead. An in-sequence column of `d` with no value at all is passed
# over first: normalise_time_landmarks() leaves <tier>_tbound NA for a token
# whose time is not on the TextGrid's clock, so the column is all NA when no
# token is. When only some tokens are off the clock, <tier>_tbound is NA in
# those tokens while its <tier>_tseq twin (same tier) has values there, so
# a model on <tier>_tbound would fit fewer tokens than one on <tier>_tseq.
# So with `d` and its token column, a <tier>_tbound is kept only when it has
# a value in every token where its <tier>_tseq has one; otherwise it is
# passed over, in either order, and <tier>_tseq is used. A <tier>_tbound
# with no <tier>_tseq twin is kept as before. Other NA parts (the set's
# columns or the token are missing) count as several, as without `d`.
guess_time_var <- function(vars, fallback_idx = 1, d = NULL, token = NULL,
                           prefer = c("tseq", "tbound")) {
  prefer <- match.arg(prefer)
  ends <- c("_tseq$", "_tbound$")
  if (prefer == "tbound") ends <- rev(ends)
  stacked <- unlist(lapply(ends, grep, vars, value = TRUE))
  if (!is.null(d) && length(stacked)) {
    fin <- function(x) {
      if (is.factor(x)) x <- as.character(x)
      is.finite(suppressWarnings(as.numeric(x)))
    }
    filled <- vapply(stacked, function(v) {
      x <- d[[v]]
      if (is.null(x)) return(TRUE)
      any(fin(x))
    }, logical(1))
    stacked <- stacked[filled]
    if (is.character(token) && length(token) == 1 && !is.na(token) &&
        token %in% names(d)) {
      tk <- as.character(d[[token]])
      covers <- vapply(stacked, function(v) {
        twin <- sub("_tbound$", "_tseq", v)
        if (identical(twin, v) || !twin %in% vars || is.null(d[[v]]) ||
            is.null(d[[twin]])) return(TRUE)
        has_seq <- tapply(fin(d[[twin]]), tk, any)
        has_bnd <- tapply(fin(d[[v]]), tk, any)
        !any(has_seq & !has_bnd, na.rm = TRUE)
      }, logical(1))
      stacked <- stacked[covers]
    }
    one <- vapply(stacked, function(v) identical(t01_parts(d, v, token), 1L),
                  logical(1))
    if (any(!one)) return(stacked[!one][1])
    t01 <- intersect(sub("_(tseq|tbound)$", "_t01", stacked), vars)
    if (length(t01)) return(t01[1])
    return(guess_var(vars, var_patterns$time, fallback_idx))
  }
  if (length(stacked)) return(stacked[1])
  guess_var(vars, var_patterns$time, fallback_idx)
}

# ---------------------------------------------------------------------------
# Pretty y-axis label for an f0 column. Maps the column *name* to a typeset
# label with a subscript zero (f0 -> f₀):
#   *_hz / raw f0 -> "f₀ (Hz)"        *_st / semitone -> "f₀ (semitone)"
#   *_zscore, *_lz -> "normalised f₀"  other *_norm    -> "normalised f₀"
# Anything unrecognised falls back to the column name unchanged, so a custom
# column still gets a sensible axis title. Shared by the Visualise and Curate
# plots so both label the f0 axis consistently.
# ---------------------------------------------------------------------------
f0_axis_label <- function(col) {
  if (is.null(col) || !nzchar(col)) return("f₀")
  c0 <- tolower(col)
  if (grepl("zscore|z[._-]?score|_lz$", c0)) return("normalised f₀")
  if (grepl("semitone|_st$|_st[._]", c0))  return("f₀ (st)")
  if (grepl("hz", c0))                     return("f₀ (Hz)")
  if (grepl("norm", c0))                   return("normalised f₀")
  if (grepl("^f_?0$|^pitch", c0))          return("f₀ (Hz)")
  col
}

# ---------------------------------------------------------------------------
# Foldable guide box. Wraps a tab's explanatory guide in a native
# <details>/<summary> element so the (often long) guide can be collapsed to
# free up room for plots and output. Open by default. The visual styling
# lives in ui.R (selector `details.guide-box`).
#   guide_box("Inspect guide", tags$ul(...), tags$p(...))
# ---------------------------------------------------------------------------
guide_box <- function(title, ..., open = TRUE) {
  tags$details(
    class = "guide-box",
    open  = if (isTRUE(open)) NA else NULL,
    tags$summary(class = "guide-summary", style = "font-weight: 700;", title),
    tags$div(class = "guide-body", ...)
  )
}

# ---------------------------------------------------------------------------
# "What each tab does" on the two Start pages: F0 Analysis Start
# (ui/start_ui.R) and F0 Processing Start (ui/fp_start_ui.R). Both build
# their list with these two helpers, so the lists cannot drift apart.
# tab_item(): one row. The tab's icon (`ic`, a Font Awesome name for
# shiny::icon(), as in the rest of the app) sits on a pale circle, then the
# tab's name and, in `...`, what the tab does: text and inline tags such as
# tags$code(), joined with no whitespace between them (as nows()), so no
# stray space comes before punctuation; the text is escaped. `stage`
# colours the circle and the name by the stage of the work, from the app's
# theme colours: data (blue), prep(are) (mint), expl(ore) (yellow), model
# (pink), sum(marise) (orange). The name takes the stage's dark shade.
#   tab_list(tab_item("folder-open", "Start", "data", "Upload your data."))
# tab_list(): the rows in a <ul class="st-tabs">, after the style they need.
# ---------------------------------------------------------------------------
tab_item <- function(ic, name, stage = c("data", "prep", "expl", "model", "sum"), ...) {
  stage <- match.arg(stage)
  desc <- lapply(list(...), function(x) {
    if (is.null(x)) return("")
    if (is.character(x) && !inherits(x, "html")) return(htmltools::htmlEscape(paste(x, collapse = "")))
    paste(as.character(x), collapse = "")
  })
  tags$li(class = paste0("st-tab-", stage),
          tags$span(class = "st-tab-ico", icon(ic)),
          tags$span(tags$strong(name), " ", HTML(paste0(unlist(desc), collapse = ""))))
}
tab_list <- function(...) {
  tagList(
    tags$style(HTML(paste(
      "ul.st-tabs{list-style:none;padding:0;margin:0 0 12px;}",
      "ul.st-tabs>li{display:flex;align-items:flex-start;gap:10px;padding:3px 0;}",
      "ul.st-tabs .st-tab-ico{flex:none;width:26px;height:26px;border-radius:50%;font-size:0.74rem;",
      "display:inline-flex;align-items:center;justify-content:center;}",
      ".st-tab-data .st-tab-ico{background:#e3f4f8;color:#24707f;}.st-tab-data strong{color:#24707f;}",
      ".st-tab-prep .st-tab-ico{background:#e3f3ec;color:#2c5f4f;}.st-tab-prep strong{color:#2c5f4f;}",
      ".st-tab-expl .st-tab-ico{background:#fff3d1;color:#7a5a00;}.st-tab-expl strong{color:#7a5a00;}",
      ".st-tab-model .st-tab-ico{background:#fde6e7;color:#9e3c43;}.st-tab-model strong{color:#9e3c43;}",
      ".st-tab-sum .st-tab-ico{background:#ffe6dd;color:#a83a17;}.st-tab-sum strong{color:#a83a17;}"))),
    tags$ul(class = "st-tabs", ...)
  )
}

# ---------------------------------------------------------------------------
# edge_f0_line(): the amber notice at the top of the Model, GCA, GAMM and
# Summarise tabs (F4, D12). The fits rescale each token's time from its
# first f0 (0) to its last (1) (resolve_time_norm(f0 =)), so rows without f0
# at a token's edges move nothing. Only a time column the fits use as it is
# (already on 0-1, by its name or its values: the fits' own decision) can
# leave f0 short of 0 or 1: time_prop over a region whose end points have
# no f0, a 0-1 column made by an older version or from another f0 column,
# or a subset taken after the column was made.
# A <set>_t01 (not token_t01) runs from 0 to 1 within each part of the set,
# so it is judged per part (token x <set>_i, or x <set>_start and
# <set>_end); any other column per token. The notice counts the tokens or
# parts whose f0 misses 0 or 1, and its advice keeps the same unit: make
# the same column again in Normalise (so a per-syllable analysis stays per
# syllable), or, for time_prop and other columns, use the real time column
# or token_t01; from an f0 column in Hz, as Normalise needs. What removing
# the rows without f0 does is said for this column: nothing for a 0-1 name,
# and for a column taken as 0-1 by its values, the rescale when the rows
# with f0 alone no longer look like 0-1 per token. The text says "the app",
# not "the fits": Summarise averages contours and fits nothing.
# NULL when none miss, or the columns cannot be read.
# ---------------------------------------------------------------------------
edge_f0_line <- function(d, token, f0, time) {
  one <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  if (is.null(d) || !one(token) || !one(f0) || !one(time) ||
      !all(c(token, f0, time) %in% names(d))) return(NULL)
  if (!isTRUE(tryCatch(time_already_normalised(d[[time]], d[[token]], name = time),
                       error = function(e) FALSE))) return(NULL)
  num <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    suppressWarnings(as.numeric(x))
  }
  tm <- num(d[[time]])
  x  <- num(d[[f0]])
  # Has f0 (D3): above 0 in an Hz column, any number in another column
  hz <- f0 %in% hz_f0_columns(d[f0])
  ok <- is.finite(tm) & is.finite(x) & (!hz | x > 0) & !is.na(d[[token]])
  set  <- if (grepl("_t01$", time) && !identical(time, "token_t01")) sub("_t01$", "", time)
  part <- NULL
  if (!is.null(set)) {
    ic <- paste0(set, "_i"); sc <- paste0(set, "_start"); ec <- paste0(set, "_end")
    if (ic %in% names(d)) {
      part <- as.character(d[[ic]])
    } else if (all(c(sc, ec) %in% names(d))) {
      st <- num(d[[sc]]); en <- num(d[[ec]])
      part <- ifelse(is.finite(st) & is.finite(en), paste(st, en), NA_character_)
    }
    if (!is.null(part)) ok <- ok & !is.na(part)
  }
  if (!any(ok)) return(NULL)
  tok <- match(d[[token]][ok], unique(d[[token]][ok]))
  key <- if (is.null(part)) tok else paste(tok, part[ok], sep = "\r")
  key <- match(key, unique(key))
  lo  <- tapply(tm[ok], key, min)
  hi  <- tapply(tm[ok], key, max)
  n_unit <- length(lo)
  n_edge <- sum(lo > 1e-8 | hi < 1 - 1e-8)
  if (n_edge == 0) return(NULL)
  big <- function(n) format(n, big.mark = ",")
  # "in 3 of 8 tokens", "in both tokens", "in all 1,853 tokens"
  count_in <- function(noun) {
    if (n_edge < n_unit) sprintf("in %s of %s %s the f0 does", big(n_edge), big(n_unit), noun)
    else if (n_unit == 2) sprintf("in both %s the f0 does", noun)
    else sprintf("in all %s %s the f0 does", big(n_unit), noun)
  }
  who <- if (!is.null(part)) {
           if (n_unit == 1) sprintf("the f0 of its one <code>%s</code> part does", set)
           else count_in(sprintf("<code>%s</code> parts", set))
         } else if (n_unit == 1) "the token&rsquo;s f0 does"
         else count_in("tokens")
  # Normalise builds the axes from an f0 column in Hz, and refuses a
  # normalised one (f0_st, f0_lz, ...).
  from_f0 <- if (hz) "from the same f0 variable"
             else sprintf("from the f0 (Hz) variable that <code>%s</code> was made from", f0)
  to_token <- sprintf(paste("To run each token from its first f0 (0) to its last (1), choose the",
                            "real time variable, which the app rescales that way, or make",
                            "<code>token_t01</code> in <strong>Normalise</strong> %s."), from_f0)
  advice <- if (identical(time, "token_t01")) {
    sprintf(paste("Make <code>token_t01</code> again in <strong>Normalise</strong> with",
                  "<em>Whole token (0&ndash;1)</em> %s: it then runs each token from its first",
                  "f0 (0) to its last (1). Or choose the real time variable, which the app",
                  "rescales that way."), from_f0)
  } else if (!is.null(set)) {
    sprintf(paste("Make <code>%s</code> again in <strong>Normalise</strong> (by the",
                  "<code>%s</code> tier) %s: it then runs each part from its first f0 (0)",
                  "to its last (1)."), time, set, from_f0)
  } else if (identical(time, "time_prop")) {
    paste("<code>time_prop</code> runs over the frames of the extracted region, so its",
          "0 and 1 are the region&rsquo;s first and last frames, which can have no f0.",
          to_token)
  } else to_token
  # A <tier>_t01, token_t01 or time_prop is taken as 0-1 by its name, with
  # or without the rows without f0. Any other column only by its values
  # (time_already_normalised()), and with those rows gone it may no longer
  # look like 0-1 per token: the app then rescales it instead, and the
  # notice goes. Say which, for this column.
  by_name <- grepl("_t01$", time) || identical(time, "time_prop")
  has_f0  <- is.finite(x) & (!hz | x > 0)
  stays <- by_name || isTRUE(tryCatch(
    time_already_normalised(d[[time]][has_f0], d[[token]][has_f0], name = time),
    error = function(e) FALSE))
  removal <- if (stays) "Removing rows on the Start page does not change this. "
             else paste("With the rows without f0 removed on the Start page, the column no",
                        "longer looks like 0&ndash;1 per token, so the app rescales each token",
                        "from its first f0 to its last instead. ")
  tags$div(
    style = paste("background-color:#fff8e1; border-left:4px solid #e0a800;",
                  "padding:10px 14px; margin:8px 0; border-radius:4px;",
                  "color:#7a5d00; font-size:0.9rem;"),
    tags$span(style = "color:#e0a800;", icon("lightbulb")),
    HTML(sprintf(paste(
      " <code>%s</code> is already on 0&ndash;1 and is used as it is, but %s not",
      "reach 0 or 1 on it (f0 variable <code>%s</code>). %s%s"), time, who, f0, removal, advice)))
}

# ---------------------------------------------------------------------------
# t01_parts(): for a <set>_t01, <set>_tseq or <set>_tbound time column (not
# token_t01), the most parts of the set that one token has among its rows
# with a value on the column: 1 for a word, rhyme or vowel tier, 2 or more
# for a syllable tier of longer words. NA when `time` is not such a column or
# the set's columns are missing. A <set>_t01, <set>_tseq or <set>_tbound with
# one part per token is a whole-token axis, like token_t01 (Model, GCA and
# GAMM notes, and guess_time_var()).
# ---------------------------------------------------------------------------
t01_parts <- function(d, time, token) {
  if (is.null(d) || !is.character(time) || length(time) != 1 || is.na(time) ||
      !grepl("_(t01|tseq|tbound)$", time) || identical(time, "token_t01") ||
      !is.character(token) || length(token) != 1 || is.na(token) ||
      !all(c(time, token) %in% names(d))) return(NA_integer_)
  set <- sub("_(t01|tseq|tbound)$", "", time)
  ic <- paste0(set, "_i"); sc <- paste0(set, "_start"); ec <- paste0(set, "_end")
  part <- if (ic %in% names(d)) as.character(d[[ic]])
          else if (all(c(sc, ec) %in% names(d)))
            ifelse(is.na(d[[sc]]) | is.na(d[[ec]]), NA_character_, paste(d[[sc]], d[[ec]]))
          else return(NA_integer_)
  tv <- d[[time]]
  if (is.factor(tv)) tv <- as.character(tv)
  ok <- is.finite(suppressWarnings(as.numeric(tv))) & !is.na(part) & !is.na(d[[token]])
  if (!any(ok)) return(NA_integer_)
  tk  <- match(d[[token]][ok], unique(d[[token]][ok]))
  one <- !duplicated(paste(tk, part[ok], sep = "\r"))
  as.integer(max(tabulate(tk[one])))
}

# ---------------------------------------------------------------------------
# Chinese characters in static plots.
# renderPlot() and ggsave() draw with ragg, one font family per text element.
# The app font (Open Sans) has no Chinese glyphs, and the system fallback can
# pick a font without simplified characters (on macOS 阴 and 阳 vanish, so
# 阴平 and 阳平 both read 平). So when a plot's text contains CJK, that plot
# switches to a font that has it:
#   1. a font file in the app's cjk-font/ folder, if there is one (none is
#      bundled now; see cjk-font/README.md). A file with "bold" in its name
#      is used for bold text.
#   2. else the first installed family in .cjk_candidates that has both
#      simplified and traditional characters (checked glyph by glyph).
# Plots whose text has no CJK are left exactly as they were. If no font is
# found, one notification per session says so. Interactive (plotly) plots
# draw text in the browser and need none of this.
#   p + cjk_family_theme(p)    # a ggplot: add after any complete theme
#   label_family(tones)        # a family name, or "" (no CJK, or no font)
# ---------------------------------------------------------------------------
.cjk_font_dir <- file.path(getwd(), "cjk-font")   # global.R runs with the app dir as wd
# Glyphs a font must have: 阴阳发调 (simplified), 陰陽發調 (traditional), 平上去入声聲
.cjk_probe <- "阴阳发调陰陽發調平上去入声聲"
# Installed families to try, in order. Never "PingFang SC": FreeType cannot
# open its file on macOS. Names are checked against system_fonts() first,
# because match_fonts() can hang on a name the system does not have.
.cjk_candidates <- c(
  "Noto Sans CJK SC", "Noto Sans SC", "Source Han Sans SC", "Source Han Sans CN",
  "Hiragino Sans GB", "Microsoft YaHei", "WenQuanYi Zen Hei", "WenQuanYi Micro Hei",
  "Droid Sans Fallback", "Noto Sans CJK TC", "AR PL UMing CN", "AR PL SungtiL GB",
  "Songti SC", "SimHei", "Arial Unicode MS")

# Character values in x: vectors, factors (the levels in use), nested lists,
# plotmath expressions. Numbers, functions and waivers give nothing.
.text_values <- function(x) {
  if (is.null(x) || is.function(x) || is.environment(x)) return(character(0))
  if (is.factor(x)) return(as.character(unique(x)))
  if (is.character(x)) return(unique(x))
  if (is.expression(x)) return(as.character(x))
  if (is.language(x)) return(paste(deparse(x), collapse = " "))
  if (is.list(x)) return(unlist(lapply(unclass(x), .text_values), use.names = FALSE))
  character(0)
}

# TRUE if any text in x has a CJK character. Code points, not a regex class:
# a \x{3000} class errors in PCRE when R is not in UTF-8 mode.
has_cjk <- function(x) {
  x <- .text_values(x)
  x <- enc2utf8(x[!is.na(x) & nzchar(x)])
  x <- x[validUTF8(x)]
  if (!length(x)) return(FALSE)
  cp <- utf8ToInt(paste(x, collapse = ""))
  any((cp >= 0x2E80 & cp <= 0x9FFF) |    # radicals, CJK punctuation, kana, ideographs
      (cp >= 0xAC00 & cp <= 0xD7AF) |    # Hangul syllables
      (cp >= 0xF900 & cp <= 0xFAFF) |    # compatibility ideographs
      (cp >= 0xFF00 & cp <= 0xFFEF) |    # full-width forms
      (cp >= 0x20000 & cp <= 0x3134F))   # ideograph extensions B to G
}

# The CJK family to use, or "". Not cached: cjk_family() caches it.
.find_cjk_family <- function(dir = .cjk_font_dir, candidates = .cjk_candidates,
                             probe = .cjk_probe) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) return("")
  covers <- function(path, index = 0L) {
    gi <- tryCatch(systemfonts::glyph_info(probe, path = path, index = index),
                   error = function(e) NULL)
    !is.null(gi) && nrow(gi) > 0 && all(gi$index > 0)
  }
  # 1. A font file dropped into cjk-font/
  files <- list.files(dir, pattern = "\\.(otf|ttf|ttc)$", ignore.case = TRUE,
                      full.names = TRUE)
  if (length(files)) {
    bold  <- grepl("bold", basename(files), ignore.case = TRUE)
    slant <- grepl("italic|oblique", basename(files), ignore.case = TRUE)
    plain <- Filter(covers, files[!bold & !slant])
    if (length(plain)) {
      bold <- c(Filter(covers, files[bold & !slant]), plain)[1]
      fam <- "shinytone CJK"
      if (!fam %in% systemfonts::registry_fonts()$family)
        systemfonts::register_font(fam, plain = plain[1], bold = bold)
      return(fam)
    }
  }
  # 2. An installed family
  have <- tryCatch(unique(systemfonts::system_fonts()$family),
                   error = function(e) character(0))
  for (f in intersect(candidates, have)) {
    m <- tryCatch(systemfonts::match_fonts(f), error = function(e) NULL)
    if (!is.null(m) && nrow(m) && covers(m$path[1], m$index[1])) return(f)
  }
  ""
}

# Cached for the R process: the font lookup runs once, the first time a plot
# has CJK text.
cjk_family <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- tryCatch(.find_cjk_family(), error = function(e) "")
    cache
  }
})

# One notification per session when CJK text has no font. Silent outside a
# session.
cjk_font_notice <- function() {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) return(invisible(FALSE))
  tryCatch({
    ud <- session$userData
    if (!is.environment(ud) || isTRUE(ud$cjk_font_notice)) return(invisible(FALSE))
    ud$cjk_font_notice <- TRUE
    shiny::showNotification(
      paste("No Chinese font was found on this server, so static plots may drop",
            "Chinese characters. Interactive plots are unaffected."),
      type = "warning", duration = 15, session = session)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

# The family for text with these labels: a CJK family when they have CJK
# (and one is found), else "" (leave the family alone).
label_family <- function(...) {
  if (!has_cjk(list(...))) return("")
  fam <- cjk_family()
  if (!nzchar(fam)) cjk_font_notice()
  fam
}

# The text a ggplot can show: its labels, the text values of its mappings
# (except `group`) and facets, text-layer labels and scale names and labels.
.plot_text <- function(p) {
  value_of <- function(q, d, name = TRUE) {
    e <- rlang::quo_get_expr(q)
    nm <- NULL                                  # a default axis or legend title
    if (name && is.symbol(e)) nm <- as.character(e)
    if (name && is.call(e) && length(e) == 3 && identical(e[[2]], quote(.data))) {
      nm <- if (identical(e[[1]], quote(`$`))) as.character(e[[3]])
            else tryCatch(rlang::eval_tidy(e[[3]], env = rlang::quo_get_env(q)),
                          error = function(err) NULL)
    }
    v <- if (is.data.frame(d))
      tryCatch(rlang::eval_tidy(q, data = d), error = function(err) NULL)
    c(.text_values(nm), if (is.character(v) || is.factor(v)) .text_values(v))
  }
  maps <- function(m, d) lapply(m[setdiff(names(m), "group")], value_of, d = d)
  dat <- p$data
  out <- list(p$labels, maps(p$mapping, dat))
  for (l in p$layers) {
    ld <- if (is.data.frame(l$data)) l$data else dat
    out <- c(out, list(maps(l$mapping, ld), l$aes_params$label))
    if (isTRUE(l$inherit.aes) && is.data.frame(l$data)) out <- c(out, list(maps(p$mapping, ld)))
  }
  fp <- p$facet$params
  for (q in c(fp$facets, fp$rows, fp$cols)) out <- c(out, list(value_of(q, dat, name = FALSE)))
  for (s in p$scales$scales) out <- c(out, list(s$name, s$labels, s$limits))
  .text_values(out)
}

# Theme that switches a ggplot to the CJK family when its text has CJK; NULL
# (no change) otherwise. Add it after any complete theme such as theme_bw(),
# which would reset it. With ggplot2 >= 4 it also sets the family of text
# geoms (annotate(), geom_text()); thematic overrides that in renderPlot, so
# pass `family` to such layers too.
cjk_family_theme <- function(p) {
  txt <- tryCatch(.plot_text(p), error = function(e) character(0))
  fam <- label_family(txt)
  if (!nzchar(fam)) return(NULL)
  th <- ggplot2::theme(text = ggplot2::element_text(family = fam))
  if (utils::packageVersion("ggplot2") >= "4.0.0")
    th <- th + ggplot2::theme(geom = ggplot2::element_geom(family = fam))
  th
}

# ---------------------------------------------------------------------------
# Make the package's analytical functions (normalise_f0, fit_gca, ...)
# reachable from the running app. Three launch paths to support:
#
#  1. `shinytone::run_app()` after the package is installed
#       -> library(shinytone) attaches the exported functions.
#  2. `shiny::shinyAppDir("inst/app")` from the repo root (also the
#     shinyapps.io case — the whole repo gets deployed)
#       -> source ../../R/*.R  (the package source tree).
#  3. `shiny::runApp("inst/app")` started directly from inside inst/app
#       -> same as #2; CWD is inst/app, so ../../R resolves.
# ---------------------------------------------------------------------------
# Three cases, in priority order:
#  1. The package namespace is already loaded (e.g. via devtools::load_all
#     during development, or already library()'d in this session). Just
#     attach it and stop — sourcing the files again would create
#     globalenv copies that mask the namespace ones and trigger conflict
#     warnings.
#  2. We're sitting in the package source tree (dev repo, or shinyapps.io
#     deploy bundling the whole repo). Source ../../R/*.R into globalenv.
#  3. We're running from an installed copy with no source tree nearby
#     (e.g. shinytone::run_app() after install). library() it.
if ("shinytone" %in% loadedNamespaces()) {
  if (!"package:shinytone" %in% search()) {
    suppressPackageStartupMessages(library(shinytone))
  }
} else {
  pkg_r <- if (dir.exists("../../R") && file.exists("../../DESCRIPTION")) {
    "../../R"
  } else if (dir.exists("R") && file.exists("DESCRIPTION")) {
    "R"
  } else {
    NULL
  }

  if (!is.null(pkg_r)) {
    for (.f in list.files(pkg_r, pattern = "\\.R$", full.names = TRUE)) {
      source(.f, local = FALSE)
    }
    rm(.f)
  } else if (requireNamespace("shinytone", quietly = TRUE)) {
    suppressPackageStartupMessages(library(shinytone))
  } else {
    stop("shinytone package functions not found. ",
         "Either install the shinytone package, or run the app from a ",
         "checkout of the source repository.", call. = FALSE)
  }
}
