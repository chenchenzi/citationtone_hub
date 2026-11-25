# Part F in the app's data tabs: the 0-1 time axes run from the first f0 to
# the last (inst/app/global.R, ui.R, ui/normalise_ui.R).
# - edge_f0_line(): the amber notice on the Model, GCA, GAMM and Summarise
#   tabs shows only for a time column used as it is whose f0 does not reach
#   0 or 1, per part for a <tier>_t01, with advice that keeps the unit.
# - t01_parts(): parts of a tier per token, for <tier>_t01, <tier>_tseq and
#   <tier>_tbound.
# - guess_time_var(): a <tier>_tseq (or <tier>_tbound) only when some token
#   has several parts.
# - track_reminder() is gone; ui.R holds the plain outputs.
# - Normalise builds token_t01, <tier>_t01, <tier>_tseq and <tier>_tbound
#   from the f0 (Hz) column, with the token, and says what 0 and 1 are.

# global.R's definitions and the Normalise module, with shiny's and
# ggplot2's exports in reach, as in the running app. showNotification()
# records its messages.
vf_env <- function() {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pkgs <- new.env(parent = globalenv())
  for (p in c("shiny", "ggplot2"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  sys.source(file.path(app, "ui", "normalise_ui.R"), env, keep.source = FALSE)
  env$toasts <- list()
  env$showNotification <- function(ui, ..., id = NULL, type = "default", duration = 5) {
    env$toasts[[length(env$toasts) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                  type = type)
    invisible(id)
  }
  env
}

# The Normalise module bound to its reactives, for shiny::testServer(). The
# test reads the result from `nd`, the module's normalised_data.
vf_norm <- function(env, d, start_cols = NULL, nd = shiny::reactiveVal(NULL)) {
  bound <- function(input, output, session) NULL
  body(bound) <- body(env$normalised_ui)
  environment(bound) <- list2env(list(
    dataset = shiny::reactiveVal(d), normalised_data = nd,
    curated_data = NULL, curated_norm_data = NULL,
    start_cols = if (!is.null(start_cols)) shiny::reactiveVal(start_cols)), parent = env)
  bound
}
vf_html <- function(x) paste(as.character(x$html), collapse = " ")
vf_options <- function(html, id) {
  sel <- regmatches(html, regexpr(sprintf('(?s)<select id="%s".*?</select>', id), html, perl = TRUE))
  if (!length(sel)) return(character(0))
  m <- regmatches(sel, gregexpr('<option value="[^"]*"', sel))[[1]]
  sub('<option value="([^"]*)"', "\\1", m)
}
vf_toasts <- function(env) vapply(env$toasts, `[[`, "", "text")

# A 0-1 column built over every row (time_prop, or a token_t01 from
# shinytone 1.0.0): every token has rows without f0 at both edges, so its f0
# does not reach 0 or 1.
vf_padded_t01 <- function(axis = "token_t01") {
  d <- do.call(rbind, lapply(1:6, function(i) {
    t01 <- seq(0, 1, length.out = 21)
    f0  <- 150 + 30 * t01 + i
    f0[seq_len(3 + i %% 3)]      <- NA
    f0[22 - seq_len(4 + i %% 2)] <- NA
    data.frame(token = paste0("tok", i), time = 0.05 * i + 0.25 * t01,
               t01 = t01, f0 = f0, tone = "T1", speaker = "s1")
  }))
  names(d)[names(d) == "t01"] <- axis
  d
}

# Two-syllable tokens, 10 ms frames, syllable 1 at 0-0.1 s and syllable 2 at
# 0.1-0.2 s. The tracker's f0 has no f0 at the first and last frame. In
# f0_corrected the user also deleted the second and last-but-one frames, one
# frame inside syllable 1 and the two frames at the boundary.
vf_norm_data <- function() {
  mk <- function(tok, spk, tone, shift) {
    t  <- round(0.005 + 0.01 * (0:19), 4)
    f0 <- 180 + 2 * (0:19) + shift
    f0[c(1, 20)] <- NA
    fc <- f0
    fc[c(2, 19, 7, 10, 11)] <- NA
    s1 <- t < 0.1
    data.frame(token = tok, time = t, f0 = f0, f0_corrected = fc, speaker = spk, tone = tone,
               syllable = ifelse(s1, "ma", "ba"),
               syllable_start = ifelse(s1, 0, 0.1), syllable_end = ifelse(s1, 0.1, 0.2),
               syllable_i = ifelse(s1, 1L, 2L), stringsAsFactors = FALSE)
  }
  rbind(mk("a1", "S1", "T1", 0), mk("a2", "S1", "T2", 20),
        mk("b1", "S2", "T1", 30), mk("b2", "S2", "T2", 50))
}
vf_norm_inputs <- function(session, f0 = "f0_corrected", set = "__token__", button = 1) {
  session$setInputs(norm_f0_var = f0, norm_speaker_var = "speaker", norm_tone_var = "tone",
                    mean_calc_method = "simple", normalisation_method = "semitone",
                    sd_correction = TRUE, norm_time_set = set, norm_time_var = "time",
                    norm_time_token_var = "token")
  session$setInputs(normalise_button = button)
}
# First and last value of `col` over the rows of each unit with f0 in `f0`
vf_span <- function(d, col, f0, by) {
  ok <- is.finite(d[[f0]]) & d[[f0]] > 0
  k  <- do.call(paste, unname(as.list(d[ok, by, drop = FALSE])))
  cbind(lo = c(tapply(d[[col]][ok], k, min)), hi = c(tapply(d[[col]][ok], k, max)))
}

# ---- edge_f0_line() -----------------------------------------------------------

test_that("edge_f0_line() shows only for a 0-1 column used as it is whose f0 misses 0 or 1", {
  env <- vf_env()
  d <- vf_padded_t01()
  html <- function(x) paste(as.character(x), collapse = " ")
  # token_t01 built over every row: used as it is, and no token's f0 reaches
  # 0 or 1. Every token has 21 rows (an equal-point file), where the Start
  # page removes the rows without f0 as for frames, which does not fix it
  out <- env$edge_f0_line(d, "token", "f0", "token_t01")
  expect_match(html(out), "is already on 0&ndash;1 and is used as it is", fixed = TRUE)
  expect_match(html(out), "in all 6 tokens the f0 does not", fixed = TRUE)
  expect_match(html(out), "on it (f0 variable <code>f0</code>)", fixed = TRUE)
  expect_no_match(html(out), "removed on the Start page", fixed = TRUE)
  expect_match(html(out),
               "(f0 variable <code>f0</code>). Removing rows on the Start page does not change this. Make",
               fixed = TRUE)
  # Tokens of different lengths: the same
  uneq <- d[-1, ]
  expect_match(html(env$edge_f0_line(uneq, "token", "f0", "token_t01")),
               "(f0 variable <code>f0</code>). Removing rows on the Start page does not change this. Make",
               fixed = TRUE)
  expect_match(html(out), paste("Make <code>token_t01</code> again in <strong>Normalise</strong> with",
                                "<em>Whole token (0&ndash;1)</em> from the same f0 variable"), fixed = TRUE)
  expect_match(html(out), "which the app rescales that way", fixed = TRUE)
  expect_no_match(html(out), "the fits", fixed = TRUE)
  expect_no_match(html(out), "</em>)", fixed = TRUE)
  expect_match(html(out), "background-color:#fff8e1; border-left:4px solid #e0a800;", fixed = TRUE)
  kept <- d[!is.na(d$f0), ]
  expect_match(html(env$edge_f0_line(kept, "token", "f0", "token_t01")), "in all 6 tokens", fixed = TRUE)
  # On a normalised f0 column, the advice names the Hz column Normalise needs
  kept$f0_st <- 12 * log2(kept$f0 / 160)
  expect_match(html(env$edge_f0_line(kept, "token", "f0_st", "token_t01")),
               "from the f0 (Hz) variable that <code>f0_st</code> was made from", fixed = TRUE)
  expect_match(html(env$edge_f0_line(kept[kept$token %in% c("tok1", "tok2"), ], "token", "f0", "token_t01")),
               "in both tokens the f0 does not", fixed = TRUE)
  # Raw time: the fits rescale it from each token's first f0 to its last
  expect_null(env$edge_f0_line(d, "token", "f0", "time"))
  expect_null(env$edge_f0_line(kept, "token", "f0", "time"))
  # token_t01 from this version starts at the first f0 and ends at the last
  nt <- normalise_time_token(d, "time", "token", f0 = "f0")
  expect_null(env$edge_f0_line(nt, "token", "f0", "token_t01"))
  expect_null(env$edge_f0_line(nt[!is.na(nt$f0), ], "token", "f0", "token_t01"))
  # A plain name whose values do not look like 0-1 per token: rescaled
  d$other <- d$time
  expect_null(env$edge_f0_line(d, "token", "f0", "other"))
  # Columns that cannot be read
  expect_null(env$edge_f0_line(NULL, "token", "f0", "token_t01"))
  expect_null(env$edge_f0_line(d, "token", "f0", "nope"))
  expect_null(env$edge_f0_line(d, NULL, "f0", "token_t01"))

  # time_prop: its 0 and 1 are the region's first and last frames; the
  # advice is the real time column or token_t01
  tp <- vf_padded_t01("time_prop")
  out <- html(env$edge_f0_line(tp, "token", "f0", "time_prop"))
  expect_match(out, "0 and 1 are the region&rsquo;s first and last frames", fixed = TRUE)
  expect_match(out, "choose the real time variable", fixed = TRUE)
  # One token
  expect_match(html(env$edge_f0_line(tp[tp$token == "tok1", ], "token", "f0", "time_prop")),
               "the token&rsquo;s f0 does not", fixed = TRUE)
  # Only the tokens that miss count
  tp$f0[tp$token == "tok2"] <- 160
  expect_match(html(env$edge_f0_line(tp, "token", "f0", "time_prop")), "in 5 of 6 tokens", fixed = TRUE)
  # 0 Hz is no f0 in an Hz column; in a signed column, 0 is f0
  tp$f0[tp$token != "tok2"] <- 160
  tp$f0[tp$token == "tok3"][1] <- 0
  expect_match(html(env$edge_f0_line(tp, "token", "f0", "time_prop")), "in 1 of 6 tokens", fixed = TRUE)
  tp$f0_st <- 12 * log2(tp$f0 / 160)
  tp$f0_st[tp$f0 == 0] <- 0
  expect_null(env$edge_f0_line(tp, "token", "f0_st", "time_prop"))
})

test_that("edge_f0_line() judges a <tier>_t01 per part, and its advice keeps the tier", {
  env <- vf_env()
  html <- function(x) paste(as.character(x), collapse = " ")
  # A syllable_t01 made by an older version, from boundary to boundary: in
  # every token syllable 1's f0 reaches 0 and 1, syllable 2's misses 1
  d <- do.call(rbind, lapply(1:4, function(i) {
    p  <- rep(seq(0, 1, length.out = 6), 2)
    f0 <- 200 + seq_len(12)
    f0[12] <- NA
    data.frame(token = paste0("w", i), syllable_i = rep(1:2, each = 6),
               syllable_start = rep(c(0, 0.3), each = 6), syllable_end = rep(c(0.3, 0.6), each = 6),
               syllable_t01 = p, f0 = f0)
  }))
  out <- html(env$edge_f0_line(d, "token", "f0", "syllable_t01"))
  expect_match(out, "in 4 of 8 <code>syllable</code> parts the f0 does not", fixed = TRUE)
  expect_match(out, "Make <code>syllable_t01</code> again in <strong>Normalise</strong> (by the <code>syllable</code> tier)",
               fixed = TRUE)
  expect_false(grepl("token_t01", out, fixed = TRUE))
  # Without <set>_i the parts come from the boundaries
  expect_match(html(env$edge_f0_line(d[setdiff(names(d), "syllable_i")], "token", "f0", "syllable_t01")),
               "in 4 of 8 <code>syllable</code> parts", fixed = TRUE)
  # From this version: no notice
  d$time <- rep(seq(0, 0.55, by = 0.05), 4)
  nl <- suppressMessages(normalise_time_landmarks(d[setdiff(names(d), "syllable_t01")], "time",
                                                  "syllable", f0 = "f0", token = "token"))
  expect_null(env$edge_f0_line(nl, "token", "f0", "syllable_t01"))
})

test_that("edge_f0_line() on f0_long.csv: never on raw time or the new token_t01", {
  f <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(f), "test/f0_long.csv not found")
  env <- vf_env()
  html <- function(x) paste(as.character(x), collapse = " ")
  raw <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE,
                                     na.strings = c("NA", "--undefined--")))
  rem <- raw[is.finite(raw$f0) & raw$f0 > 0, ]
  expect_null(env$edge_f0_line(raw, "token", "f0", "time"))
  expect_null(env$edge_f0_line(rem, "token", "f0", "time"))
  # A token_t01 built from each token's first row to its last (1.0.0)
  old <- raw
  old$token_t01 <- ave(old$time, old$token, FUN = function(t) (t - min(t)) / (max(t) - min(t)))
  expect_match(html(env$edge_f0_line(old, "token", "f0", "token_t01")), "in all 1,853 tokens",
               fixed = TRUE)
  expect_match(html(env$edge_f0_line(old[is.finite(old$f0), ], "token", "f0", "token_t01")),
               "in all 1,853 tokens", fixed = TRUE)
  new <- normalise_time_token(raw, "time", "token", f0 = "f0")
  expect_null(env$edge_f0_line(new, "token", "f0", "token_t01"))
  expect_null(env$edge_f0_line(new[is.finite(new$f0), ], "token", "f0", "token_t01"))
})

# ---- t01_parts(), guess_time_var(), ui.R ----------------------------------------

test_that("t01_parts() counts a tier's parts per token, for _t01, _tseq and _tbound", {
  env <- vf_env()
  d <- vf_norm_data()
  d$word_start <- 0; d$word_end <- 0.2; d$word_i <- 1L
  d <- suppressMessages(normalise_time_landmarks(d, "time", "syllable", f0 = "f0", token = "token"))
  d <- suppressMessages(normalise_time_landmarks(d, "time", "word", f0 = "f0", token = "token"))
  expect_identical(env$t01_parts(d, "syllable_t01", "token"), 2L)
  expect_identical(env$t01_parts(d, "syllable_tseq", "token"), 2L)
  expect_identical(env$t01_parts(d, "word_t01", "token"), 1L)
  expect_identical(env$t01_parts(d, "word_tseq", "token"), 1L)
  expect_identical(env$t01_parts(d, "syllable_tbound", "token"), 2L)
  expect_identical(env$t01_parts(d, "word_tbound", "token"), 1L)
  # Without <set>_i, the boundaries say which part a row is in
  expect_identical(env$t01_parts(d[setdiff(names(d), "syllable_i")], "syllable_t01", "token"), 2L)
  # Not such a column, or the set's columns are missing
  expect_identical(env$t01_parts(d, "token_t01", "token"), NA_integer_)
  expect_identical(env$t01_parts(d, "time", "token"), NA_integer_)
  expect_identical(env$t01_parts(d[setdiff(names(d), c("word_i", "word_start", "word_end"))],
                                 "word_t01", "token"), NA_integer_)
  expect_identical(env$t01_parts(d, "syllable_t01", NULL), NA_integer_)
  expect_identical(env$t01_parts(NULL, "syllable_t01", "token"), NA_integer_)

  # guess_time_var(): with the data, a _tseq only when some token has
  # several parts; a one-part tier's _t01 (same values, a 0-1 name) instead
  v <- names(d)
  expect_identical(env$guess_time_var(v, 3), "syllable_tseq")                      # without data
  expect_identical(env$guess_time_var(v, 3, d = d, token = "token"), "syllable_tseq")
  # Only the _tbound of a tier: it is the stacked axis then
  b <- d[setdiff(names(d), c("syllable_tseq", "word_tseq"))]
  expect_identical(env$guess_time_var(names(b), 3), "syllable_tbound")
  expect_identical(env$guess_time_var(names(b), 3, d = b, token = "token"), "syllable_tbound")
  w <- d[setdiff(names(d), c("syllable_t01", "syllable_tseq", "syllable_tbound"))]
  expect_identical(env$guess_time_var(names(w), 3), "word_tseq")
  expect_identical(env$guess_time_var(names(w), 3, d = w, token = "token"), "word_t01")
  w$word_t01 <- NULL
  expect_identical(env$guess_time_var(names(w), 3, d = w, token = "token"), "time")
  # Token unknown: as without data
  expect_identical(env$guess_time_var(names(w), 3, d = w), "word_tseq")
})

test_that("track_reminder() is gone and ui.R holds the four notice outputs", {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  env <- vf_env()
  expect_false(exists("track_reminder", envir = env, inherits = FALSE))
  calls <- character(0)
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name("uiOutput")) && is.character(e[[2]])) calls <<- c(calls, e[[2]])
      if (identical(e[[1]], as.name("track_reminder"))) calls <<- c(calls, "track_reminder")
      for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
    }
  }
  for (e in as.list(parse(file.path(app, "ui.R"), keep.source = FALSE, encoding = "UTF-8"))) walk(e)
  expect_true(all(c("model_track_edges", "gca_track_edges", "gamm_track_edges", "sum_track_edges")
                  %in% calls))
  expect_false("track_reminder" %in% calls)
})

test_that("the exported code checks for the version the app runs on", {
  env <- vf_env()
  v <- env$shinytone_version()
  expect_identical(v, as.character(utils::packageVersion("shinytone")))
  expect_true(utils::compareVersion(v, "1.0.0.9000") >= 0)
  code <- env$read_data_code("x.csv", start = list(version = v))
  expect_true(any(code == sprintf('stopifnot(utils::packageVersion("shinytone") >= "%s")', v)))
})

# ---- Normalise ----------------------------------------------------------------

test_that("Normalise runs each token from its first f0 to its last, in the f0 (Hz) column", {
  env <- vf_env()
  d <- vf_norm_data()
  nd  <- shiny::reactiveVal(NULL)
  srv <- vf_norm(env, d, list(token = "token", time = "time", f0 = "f0_corrected"), nd = nd)
  shiny::testServer(srv, {
    vf_norm_inputs(session)
    out <- nd()
    expect_equal(out$token_t01,
                 normalise_time_token(d, "time", "token", f0 = "f0_corrected")$token_t01)
    sp <- vf_span(out, "token_t01", "f0_corrected", "token")
    expect_equal(unname(sp[, "lo"]), rep(0, 4))
    expect_equal(unname(sp[, "hi"]), rep(1, 4))
    # NA before the first f0 and after the last; the deleted frames inside
    # keep their place
    for (tk in unique(d$token)) {
      x <- out$token_t01[out$token == tk]
      expect_true(all(is.na(x[c(1, 2, 19, 20)])))
      expect_true(all(is.finite(x[c(7, 10, 11)])))
    }
    # The table shows the time, token and f0 columns next to the new axis
    expect_true(all(c("time", "token", "f0_corrected", "token_t01") %in% names(norm_display())))
    msg <- vf_toasts(env)
    expect_true(any(grepl(paste("Added token_t01, with the tokens in column token. Each token runs",
                                "from its first f0 (0) to its last f0 (1), read from column f0_corrected.",
                                "Rows before the first f0 or after the last are NA."), msg, fixed = TRUE)))
    # A rebuild replaces the column and still shows it
    vf_norm_inputs(session, button = 2)
    expect_true("token_t01" %in% names(norm_display()))

    # The user's own f0 (Hz) pick decides which rows have f0
    vf_norm_inputs(session, f0 = "f0", button = 3)
    out <- nd()
    expect_equal(out$token_t01, normalise_time_token(d, "time", "token", f0 = "f0")$token_t01)
    expect_equal(out$token_t01[out$token == "a1"][2], 0)
    expect_true(any(grepl("last f0 (1), read from column f0.", vf_toasts(env), fixed = TRUE)))
  })
})

test_that("Normalise stacks the syllables from the first f0 to the last in each", {
  env <- vf_env()
  d <- vf_norm_data()
  nd  <- shiny::reactiveVal(NULL)
  srv <- vf_norm(env, d, list(token = "token", time = "time", f0 = "f0_corrected"), nd = nd)
  shiny::testServer(srv, {
    html <- vf_html(output$norm_time_landmark_ui)
    expect_identical(vf_options(html, "norm_time_set"), c("__none__", "__token__", "syllable"))
    # The token picker shows for a tier too
    expect_true(grepl('id="norm_time_token_var"', html, fixed = TRUE))
    expect_false(grepl("input.norm_time_set == &#39;__token__&#39;", html, fixed = TRUE))
    expect_match(html, "Removing rows without f0 does not change these columns.", fixed = TRUE)
    # The detail on lone samples is in the guide, not under the menu
    expect_no_match(html, "lone f0 sample", fixed = TRUE)
    vf_norm_inputs(session, set = "syllable")
    out <- nd()
    ref <- normalise_time_landmarks(d, "time", "syllable", f0 = "f0_corrected", token = "token")
    expect_equal(out$syllable_t01, ref$syllable_t01)
    expect_equal(out$syllable_tseq, ref$syllable_tseq)
    expect_equal(out$syllable_tbound, ref$syllable_tbound)
    # syllable_tbound: 0 at each token's first f0, 1 at the TextGrid
    # boundary (0.1 s), 2 at its last f0; the gap is kept on both sides
    tb <- vf_span(out, "syllable_tbound", "f0_corrected", "token")
    expect_equal(unname(tb[, "lo"]), rep(0, 4))
    expect_equal(unname(tb[, "hi"]), rep(2, 4))
    a1 <- out[out$token == "a1" & is.finite(out$f0_corrected), ]
    expect_equal(max(a1$syllable_tbound[a1$syllable_i == 1]), (0.085 - 0.025) / (0.1 - 0.025))
    expect_equal(min(a1$syllable_tbound[a1$syllable_i == 2]), 1 + (0.115 - 0.1) / (0.175 - 0.1))
    sp <- vf_span(out, "syllable_tseq", "f0_corrected", c("token", "syllable_i"))
    i  <- as.integer(sub("^.* ", "", rownames(sp)))
    expect_equal(unname(sp[, "lo"]), i - 1)
    expect_equal(unname(sp[, "hi"]), as.numeric(i))
    # The boundary frames without f0 take no room
    expect_true(all(is.na(out$syllable_tseq[out$time %in% c(0.095, 0.105)])))
    expect_true(all(c("time", "token", "f0_corrected", "syllable_i", "syllable_t01", "syllable_tseq",
                      "syllable_tbound") %in% names(norm_display())))
    msg <- vf_toasts(env)
    expect_true(any(grepl(paste("Added syllable_t01, syllable_tseq and syllable_tbound, with the tokens",
                                "in column token. Each syllable runs from its first f0 (0) to its last f0",
                                "(1), read from column f0_corrected. In syllable_tseq, syllable 2 runs from",
                                "1 to 2, and so on. In syllable_tbound, the TextGrid boundaries sit at 1, 2,",
                                "and so on, and the gaps are kept. Rows before the first f0 or after the",
                                "last are NA."), msg, fixed = TRUE)))
    # No message about the clock: time and the boundaries are both in seconds
    expect_false(any(vapply(env$toasts, function(t) t$type == "warning", NA)))
  })
})

test_that("Normalise without <set>_i adds only <set>_t01, and says the clock must match", {
  env <- vf_env()
  d <- vf_norm_data()
  nd  <- shiny::reactiveVal(NULL)
  # No syllable_i: no syllable_tseq or syllable_tbound
  srv <- vf_norm(env, d[setdiff(names(d), "syllable_i")],
                 list(token = "token", time = "time", f0 = "f0_corrected"), nd = nd)
  shiny::testServer(srv, {
    vf_norm_inputs(session, set = "syllable")
    expect_true("syllable_t01" %in% names(nd()))
    expect_false(any(c("syllable_tseq", "syllable_tbound") %in% names(nd())))
    expect_true(any(grepl(paste("Added syllable_t01 (no syllable_i column, so no syllable_tseq or",
                                "syllable_tbound), with the tokens in column token."),
                          vf_toasts(env), fixed = TRUE)))
    expect_false(any(grepl("In syllable_tbound", vf_toasts(env), fixed = TRUE)))
  })
  # Time in ms, boundaries in s: syllable_tbound is NA, and a warning says why
  env <- vf_env()
  d$time_ms <- d$time * 1000
  nd  <- shiny::reactiveVal(NULL)
  srv <- vf_norm(env, d, list(token = "token", time = "time", f0 = "f0_corrected"), nd = nd)
  shiny::testServer(srv, {
    vf_norm_inputs(session, set = "syllable")
    session$setInputs(norm_time_var = "time_ms", normalise_button = 2)
    out <- nd()
    expect_true(all(is.na(out$syllable_tbound)))
    # The other two need only the order and spacing of time
    expect_equal(out$syllable_tseq,
                 normalise_time_landmarks(d, "time", "syllable", f0 = "f0_corrected")$syllable_tseq)
    warn <- Filter(function(t) t$type == "warning", env$toasts)
    expect_length(warn, 1L)
    expect_match(warn[[1]]$text, "syllable_tbound is NA in 4 token", fixed = TRUE)
    expect_match(warn[[1]]$text, "same clock", fixed = TRUE)
  })
})

test_that("Normalise: the Time menu leaves out 0-1 axes, and no token column means no option", {
  env <- vf_env()
  d <- vf_norm_data()
  d <- suppressMessages(normalise_time_landmarks(d, "time", "syllable", f0 = "f0", token = "token"))
  d <- normalise_time_token(d, "time", "token", f0 = "f0")
  srv <- vf_norm(env, d, list(token = "token", time = "time", f0 = "f0"))
  shiny::testServer(srv, {
    ch <- vf_options(vf_html(output$norm_time_landmark_ui), "norm_time_var")
    expect_true("time" %in% ch)
    expect_false(any(grepl("_(t01|tseq|tbound)$", ch)))
    expect_true("syllable_tbound" %in% names(d))
  })
  nt <- d
  names(nt)[names(nt) == "token"] <- "rec"
  srv <- vf_norm(env, nt)
  shiny::testServer(srv, {
    html <- vf_html(output$norm_time_landmark_ui)
    expect_identical(vf_options(html, "norm_time_set"), character(0))
    expect_match(html, "No token variable, so time cannot be normalised here.", fixed = TRUE)
  })
})

test_that("Normalise counts parts with f0 at fewer than two points, with a factor token", {
  env <- vf_env()
  d <- vf_norm_data()
  # b2's syllable 2 keeps one f0 frame; a token dropped from a factor with
  # its level kept (Curate, with string variables as factors)
  d$f0_corrected[d$token == "b2" & d$syllable_i == 2][-3] <- NA
  d <- d[d$token != "a2", ]
  d$token <- factor(d$token, levels = c("a1", "a2", "b1", "b2"))
  d$syllable_i[d$token == "a1" & d$time > 0.18] <- NA     # an empty-label stretch
  nd  <- shiny::reactiveVal(NULL)
  srv <- vf_norm(env, d, list(token = "token", time = "time", f0 = "f0_corrected"), nd = nd)
  shiny::testServer(srv, {
    expect_no_error(vf_norm_inputs(session, set = "syllable"))
    out <- nd()
    b2s2 <- out$token == "b2" & out$syllable_i %in% 2
    expect_true(all(is.na(out$syllable_t01[b2s2])))
    expect_true(all(is.na(out$syllable_tseq[b2s2])))
    # On syllable_tbound it keeps values: from the TextGrid boundary at 1 to
    # its one f0, the token's last
    expect_true(any(is.finite(out$syllable_tbound[b2s2])))
    expect_equal(max(out$syllable_tbound[b2s2], na.rm = TRUE), 2)
    msg <- vf_toasts(env)
    # So the toast names the two columns where it is NA
    expect_true(any(grepl(paste("1 syllable has f0 at fewer than two points, so it is NA in syllable_t01",
                                "and syllable_tseq."), msg, fixed = TRUE)))
    expect_no_error(vf_norm_inputs(session, set = "__token__", button = 2))
    expect_false(any(grepl("token has f0 at fewer", vf_toasts(env), fixed = TRUE)))
  })
})

test_that("Normalise's Whole token runs on the bundled sample's f0_Hz", {
  app <- system.file("app", package = "shinytone")
  f <- file.path(app, "www", "dc21f0_test.csv")
  skip_if_not(file.exists(f), "sample not found")
  env <- vf_env()
  d <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE, na.strings = c("NA", "--undefined--")))
  nd  <- shiny::reactiveVal(NULL)
  srv <- vf_norm(env, d, list(token = "token", time = "time", f0 = "f0_Hz"), nd = nd)
  shiny::testServer(srv, {
    session$setInputs(norm_f0_var = "f0_Hz", norm_speaker_var = "speaker", norm_tone_var = "tone",
                      mean_calc_method = "simple", normalisation_method = "lz",
                      sd_correction = TRUE, norm_time_set = "__token__", norm_time_var = "time",
                      norm_time_token_var = "token")
    expect_no_error(session$setInputs(normalise_button = 1))
    out <- nd()
    expect_equal(out$token_t01, normalise_time_token(d, "time", "token", f0 = "f0_Hz")$token_t01)
    expect_true(all(c("f0_lz", "token_t01") %in% names(out)))
    expect_false(any(vapply(env$toasts, function(t) t$type %in% c("error", "warning"), NA)))
  })
})

test_that("Normalise's illustrated guide shows Token A on top and the three axes in a row", {
  env <- vf_env()
  srv <- vf_norm(env, vf_norm_data())
  shiny::testServer(srv, {
    g <- vf_html(output$normalise_guide)
    # Token A twice (wide, and narrow for a phone), then the three axes for
    # several syllables, in this order
    titles <- regmatches(g, gregexpr("<title>[^<]*</title>", g))[[1]]
    expect_identical(sub("<title>([^<]*)</title>", "\\1", titles),
                     c("Token A in time: where 0 and 1 fall", "Token A in time: where 0 and 1 fall",
                       "In sequence: syllable_tseq", "In sequence, gap kept: syllable_tbound",
                       "Whole word: token_t01"))
    expect_identical(lengths(regmatches(g, gregexpr("<svg ", g))), 5L)
    expect_identical(lengths(regmatches(g, gregexpr('viewBox="0 0 680 272"', g, fixed = TRUE))), 1L)
    expect_identical(lengths(regmatches(g, gregexpr('viewBox="0 0 320 364"', g, fixed = TRUE))), 1L)
    expect_identical(lengths(regmatches(g, gregexpr('viewBox="0 0 270 236"', g, fixed = TRUE))), 3L)
    expect_match(g, '<div class="msg-a-wide"><svg width="100%" viewBox="0 0 680 272"', fixed = TRUE)
    expect_match(g, '<div class="msg-a-narrow"><svg width="100%" viewBox="0 0 320 364"', fixed = TRUE)
    expect_no_match(g, "stacked", ignore.case = TRUE)
    expect_no_match(g, "aligned, gap kept", fixed = TRUE)
    # Each panel's title: the layout, then the column. A tier column shows
    # the tier's name light and the ending dark; token_t01 is one fixed name
    # (it is called so whatever the token variable is called)
    suf <- function(pre, end) {
      if (pre == "token") return('<tspan fill="#2c5d80" font-weight="700">token_t01</tspan>')
      sprintf('<tspan fill="#a9b4bb">%s</tspan><tspan fill="#2c5d80" font-weight="700">%s</tspan>', pre, end)
    }
    for (t in list(c("In sequence", "syllable", "_tseq"), c("In sequence, gap kept", "syllable", "_tbound"),
                   c("Whole word", "token", "_t01"))) {
      expect_match(g, paste0('text-anchor="middle">', t[1], "</text>"), fixed = TRUE)
      expect_match(g, paste0('<text x="135" y="30" font-size="11.5" text-anchor="middle">', suf(t[2], t[3]),
                             "</text>"), fixed = TRUE)
    }
    expect_no_match(g, '<tspan fill="#a9b4bb">token</tspan>', fixed = TRUE)
    # The three axes sit under one heading, each picture with its card under
    # it; the cards have no titles of their own (the pictures name them)
    expect_match(g, paste('Several syllables: two tokens of one word, <span style="color:#3a7ca5;">A</span>',
                          'and <span style="color:#2f9e79;">B</span>, on each axis'), fixed = TRUE)
    expect_no_match(g, "msg-stitle", fixed = TRUE)
    expect_no_match(g, "One syllable, or one region", fixed = TRUE)
    expect_no_match(g, "are two tokens of one word.", fixed = TRUE)
    trio <- regmatches(g, regexpr('<div class="msg-trio">.*', g))
    pos  <- vapply(c("<title>In sequence: syllable_tseq</title>", "Syllable 1 fills 0 to 1",
                     "<title>In sequence, gap kept: syllable_tbound</title>", "The TextGrid boundaries between syllables",
                     "<title>Whole word: token_t01</title>", "The gap and the"),
                   function(x) regexpr(x, trio, fixed = TRUE)[[1]], 1L)
    expect_true(all(pos > 0))
    expect_false(is.unsorted(pos))
    expect_identical(lengths(regmatches(trio, gregexpr('<div class="msg-step">', trio, fixed = TRUE))), 3L)
    expect_match(g, paste("Pick the <code>syllable</code> tier and use <code>syllable_tbound</code>.",
                          "The TextGrid boundaries between syllables sit at 1, 2, ..., so they line up",
                          "across tokens, and the gap at each is kept. <strong>Use it</strong> to model",
                          "the whole word with the syllables lined up."), fixed = TRUE)
    # The whole-word card: pick, why, use; then what "more than the word"
    # means, and that only the target word may be labelled (each labelled
    # word gets its own 0-1); the durations kept are relative ones
    expect_match(g, paste("Pick <em>Whole token (0&ndash;1)</em>, which adds <code>token_t01</code>.",
                          "The gap and the syllables&rsquo; relative durations are kept, so the boundary",
                          "moves from token to token. <strong>Use it</strong> to keep the word&rsquo;s own",
                          "timing. If each token holds more than the word, such as a carrier phrase,",
                          "pick a <code>word</code> tier with only the target word labelled",
                          "(<code>word_t01</code>)."), fixed = TRUE)
    expect_no_match(g, "longer than the word", fixed = TRUE)
    expect_no_match(g, "The gap and the syllable durations are kept", fixed = TRUE)
    # The layout follows the guide's own width. The narrow layout is the
    # default (so a browser without container queries, such as an older
    # RStudio viewer, shows it): one panel per row, each picture above its
    # card, and the narrow Token A. Container queries on .msg-fit then widen
    # it: each picture beside its card from 521 px, the wide Token A from
    # 651 px (before the narrow one's text gets small), three in a row from
    # 781 px, each picture above its card again
    expect_match(g, ".msg-fit{container-type:inline-size;}", fixed = TRUE)
    expect_match(g, ".msg-trio{display:grid;grid-template-columns:minmax(0,1fr);gap:16px;}", fixed = TRUE)
    expect_match(g, ".msg-trio>div{display:flex;flex-direction:column;min-width:0;}", fixed = TRUE)
    expect_match(g, ".msg-trio .msg-step{flex:1 0 auto;margin-top:6px;}", fixed = TRUE)
    expect_match(g, "\\.msg-a-wide\\{display:none;\\}\\s+\\.msg-a-wide svg\\{max-width:760px;\\}")
    expect_match(g, ".msg-a-narrow svg{max-width:360px;}", fixed = TRUE)
    expect_match(g, paste0("@container (min-width:521px){.msg-trio>div{flex-direction:row;align-items:center;gap:14px;}",
                           ".msg-trio>div>svg{flex:0 0 46%;}.msg-trio .msg-step{flex:1 1 0;margin-top:0;}}"), fixed = TRUE)
    expect_match(g, "@container (min-width:651px){.msg-a-wide{display:block;}.msg-a-narrow{display:none;}}",
                 fixed = TRUE)
    expect_match(g, paste0("@container (min-width:781px){.msg-trio{grid-template-columns:repeat(3,minmax(0,1fr));gap:14px;}",
                           ".msg-trio>div{flex-direction:column;align-items:stretch;gap:0;}.msg-trio>div>svg{flex:none;}",
                           ".msg-trio .msg-step{flex:1 0 auto;margin-top:6px;}}"), fixed = TRUE)
    # No narrowing queries; the defaults come first, then the widening
    # queries from narrow to wide (a later rule wins)
    expect_no_match(g, "@container (max-width", fixed = TRUE)
    expect_identical(lengths(regmatches(g, gregexpr(".msg-a-narrow{display:none;}", g, fixed = TRUE))), 1L)
    at <- vapply(c(".msg-trio{display:grid;", "@container (min-width:521px)", "@container (min-width:651px)",
                   "@container (min-width:781px)"), function(x) regexpr(x, g, fixed = TRUE)[[1]], 1L)
    expect_true(all(at > 0))
    expect_false(is.unsorted(at))
    # The tier's name light and italic, the ending dark and bold, in the text
    expect_match(g, ".msg-tier{color:#8d9aa3;font-style:italic;}", fixed = TRUE)
    expect_match(g, ".msg-suf{color:#2c5d80;font-weight:700;}", fixed = TRUE)
    expect_no_match(g, "msg-grid", fixed = TRUE)
    expect_no_match(g, "msg-row", fixed = TRUE)
    expect_no_match(g, "Scroll sideways", fixed = TRUE)
    # Token A has a ruler for each axis, labelled in both drawings (the
    # tier's name and the ending apart; token_t01 as one name), and its
    # description names them
    for (ax in list(c("syllable", "_t01"), c("syllable", "_tseq"), c("syllable", "_tbound"), c("token", "_t01"))) {
      expect_match(g, paste0('text-anchor="end">', suf(ax[1], ax[2]), "</text>"), fixed = TRUE)
      expect_match(g, paste0('paint-order="stroke">', suf(ax[1], ax[2]), "</text>"), fixed = TRUE)
    }
    expect_identical(lengths(regmatches(g, gregexpr(
      "On syllable_tbound 0 is the first f0, 1 the TextGrid boundary and 2 the last f0.", g, fixed = TRUE))), 2L)
    # Both drawings of Token A hold the same rulers: the numbers on each row,
    # left to right (t01: 0 1 0 1; tseq: 0 1 1 2; tbound: 0 1 2; token: 0 1)
    svgs <- regmatches(g, gregexpr('<svg width="100%" viewBox="0 0 (680 272|320 364)".*?</svg>', g))[[1]]
    expect_length(svgs, 2L)
    for (sv in svgs) {
      m <- regmatches(sv, gregexpr(paste0('<text x="([0-9.]+)" y="([0-9.]+)" font-size="1[12](\\.5)?" ',
                                          'fill="#2c5d80" text-anchor="middle">([0-9])</text>'), sv))[[1]]
      xs <- as.numeric(sub('^<text x="([0-9.]+)".*', "\\1", m))
      ys <- as.numeric(sub('^<text x="[0-9.]+" y="([0-9.]+)".*', "\\1", m))
      ds <- sub(".*>([0-9])</text>$", "\\1", m)
      rows <- vapply(split(seq_along(ds), ys), function(i) paste(ds[i][order(xs[i])], collapse = ""), "")
      expect_identical(unname(rows), c("0101", "0112", "012", "01"))
    }
    # The caption under the rulers: the edges take no room (one line wide,
    # two lines narrow); on syllable_tbound 1 is the boundary, not the last f0
    expect_match(g, ">0 is the first f0, the end is the last f0; edges without f0 take no room</text>",
                 fixed = TRUE)
    expect_match(g, ">0 is the first f0, the end is the last f0;</text>", fixed = TRUE)
    expect_no_match(g, "stretches without f0 take no room", fixed = TRUE)
    expect_no_match(g, "1 the last f0;</text>", fixed = TRUE)
    # Three "no f0" labels in each drawing of Token A
    expect_identical(lengths(regmatches(g, gregexpr('font-size="10.5" fill="#a8b3bb" text-anchor="middle">no f0<',
                                                    g, fixed = TRUE))), 3L)
    expect_identical(lengths(regmatches(g, gregexpr('font-size="10" fill="#a8b3bb" text-anchor="middle">no f0<',
                                                    g, fixed = TRUE))), 3L)
    expect_no_match(g, 'font-size="8.5"', fixed = TRUE)
    expect_match(g, "keeps its length on syllable_tbound and token_t01.</desc>", fixed = TRUE)
    # The gap kept, the boundaries at 1
    expect_match(g, "the gap is kept;</text>", fixed = TRUE)
    expect_match(g, "each token keeps its own gap around it", fixed = TRUE)
    # The intro: what the column is and why; one syllable or part of one;
    # the tier columns named by their endings
    expect_match(g, paste("Time normalisation adds a time column that runs from 0 to 1 within each token",
                          "(or each syllable). Tokens differ in length, so on raw time their contours start",
                          "and end at different points."), fixed = TRUE)
    expect_match(g, paste("On the new column their shapes share one axis, so they can be compared,",
                          "averaged and modelled together (see tokens"), fixed = TRUE)
    # The voiced-span rule is said once, in Token A's caption, not in the intro
    expect_no_match(g, "0 is the first f0 and 1 the last f0, so a voiceless onset", fixed = TRUE)
    expect_no_match(g, "(e.g.", fixed = TRUE)
    expect_match(g, paste("<em>Whole token</em> adds <code>token_t01</code>, whatever your token variable is",
                          "called. That is enough when each token is one syllable; for part of a syllable,",
                          "pick a <code>rhyme</code> or <code>vowel</code> tier instead (<code>vowel_t01</code>).",
                          "For multisyllabic data, your syllable tier adds three columns, named after it:"),
                 fixed = TRUE)
    for (end in c("_t01", "_tseq", "_tbound"))
      expect_match(g, sprintf(paste0('<code><span class="msg-tier">&lt;syllable_tier&gt;</span>',
                                     '<span class="msg-suf">%s</span></code>'), end), fixed = TRUE)
    expect_no_match(g, "&lt;tier&gt;", fixed = TRUE)
    # The intro names the three columns; the pictures and cards below say
    # what each does
    expect_match(g, paste0('<span class="msg-suf">_t01</span></code> puts each syllable on its own 0&ndash;1, ',
                           'and'), fixed = TRUE)
    expect_match(g, '<span class="msg-suf">_tbound</span></code> put the syllables in sequence, as below.',
                 fixed = TRUE)
    expect_no_match(g, "(syllables in sequence: 0&ndash;1, 1&ndash;2, ...)", fixed = TRUE)
    expect_no_match(g, "sit on the TextGrid boundaries)", fixed = TRUE)
    expect_no_match(g, "For the first two", fixed = TRUE)
    # The first tip: where to use the new column, every tab as a chip; no
    # note on NA rows (nothing on this tab acts on it)
    expect_match(g, paste(' Use the new column as the X axis in <span class="msg-tab">Visualise</span>, or as',
                          'the Time variable in <span class="msg-tab">Model: Polynomials</span>,',
                          '<span class="msg-tab">Model: GCA</span>, <span class="msg-tab">Model: GAMM</span>,',
                          '<span class="msg-tab">Cluster</span> and <span class="msg-tab">Summarise</span>.'),
                 fixed = TRUE)
    expect_no_match(g, "Rows outside the span", fixed = TRUE)
    expect_match(g, "In the pictures the syllable tier is called <code>syllable</code>.", fixed = TRUE)
    expect_no_match(g, "norm_tier_names", fixed = TRUE)
    expect_no_match(g, "Your data have", fixed = TRUE)
    expect_no_match(g, "one region", fixed = TRUE)
    # The GAMM tip states the limit and gives the routes
    expect_match(g, "one GAMM smooth cannot follow an f0 jump", fixed = TRUE)
    expect_match(g, paste("To model the whole word, use <code>syllable_tbound</code> (in sequence, gap kept)",
                          "or <code>token_t01</code>, or model each syllable as its own token on",
                          "<code>syllable_t01</code>."), fixed = TRUE)
  })
})

test_that("the Normalise, Visualise, Start and Inspect sources are ASCII, with no em dash in their texts", {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  # Inspect's menus once read "\u2014 none \u2014"; now "(none)", as elsewhere
  for (f in c("normalise_ui.R", "visualise_ui.R", "start_ui.R", "inspect_ui.R")) {
    ex <- parse(file.path(app, "ui", f), keep.source = TRUE, encoding = "UTF-8")
    pd <- utils::getParseData(ex)
    s  <- utils::getParseText(pd, pd$id[pd$token == "STR_CONST"])
    expect_identical(s[grepl("[^\\x01-\\x7F]", s, perl = TRUE)], character(0), label = f)
    v <- vapply(s, function(x) eval(parse(text = x, keep.source = FALSE)), "", USE.NAMES = FALSE)
    expect_identical(v[grepl("\u2014|&mdash;|&#8212;", v)], character(0), label = f)
  }
})

# ---- Review fixes ---------------------------------------------------------------

# 8 tokens on a 0-1 column `tn` over every row, without a 0-1 name: the app
# takes it as 0-1 by its values. f0 is missing at the first and last 3
# points, so with those rows gone the column no longer looks like 0-1.
vf_tn_fixture <- function() {
  do.call(rbind, lapply(1:8, function(i) {
    n  <- 21 + (i == 1)
    tn <- seq(0, 1, length.out = n)
    f0 <- 150 + 40 * tn + i
    f0[c(1:3, (n - 2):n)] <- NA
    data.frame(token = paste0("t", i), time = round(0.01 * (0:(n - 1)), 2), tn = tn, f0 = f0,
               speaker = paste0("s", i %% 2 + 1), tone = paste0("T", i %% 2 + 1))
  }))
}
vf_text <- function(x) {
  h <- gsub("<[^>]*>", "", paste(as.character(if (is.list(x) && !is.null(x$html)) x$html else x),
                                 collapse = " "))
  for (e in list(c("&#39;", "'"), c("&quot;", "\""), c("&gt;", ">"), c("&lt;", "<"), c("&amp;", "&")))
    h <- gsub(e[1], e[2], h, fixed = TRUE)
  trimws(gsub("\\s+", " ", h))
}

test_that("edge_f0_line() on a 0-1 column found by its values says what removal does", {
  env <- vf_env()
  html <- function(x) paste(as.character(x), collapse = " ")
  d <- vf_tn_fixture()
  out <- html(env$edge_f0_line(d, "token", "f0", "tn"))
  expect_match(out, "in all 8 tokens the f0 does not", fixed = TRUE)
  expect_match(out, paste("With the rows without f0 removed on the Start page, the column no",
                          "longer looks like 0&ndash;1 per token, so the app rescales each token",
                          "from its first f0 to its last instead."), fixed = TRUE)
  expect_no_match(out, "does not change this", fixed = TRUE)
  # And so it is: with the rows removed the app rescales, and no notice shows
  ok <- is.finite(d$f0)
  expect_false(time_already_normalised(d$tn[ok], d$token[ok], name = "tn"))
  expect_null(env$edge_f0_line(d[ok, ], "token", "f0", "tn"))
  # Rows with f0 that still look 0-1 per token: removal changes nothing
  d$f0 <- 150 + 40 * d$tn
  d$f0[d$tn == 0] <- NA
  out <- html(env$edge_f0_line(d, "token", "f0", "tn"))
  expect_match(out, "Removing rows on the Start page does not change this.", fixed = TRUE)
  expect_match(html(env$edge_f0_line(d[is.finite(d$f0), ], "token", "f0", "tn")),
               "Removing rows on the Start page does not change this.", fixed = TRUE)
})

test_that("the Start report notes a 0-1 column that flips; 'Your raw file is not changed' says where to save", {
  f <- start_write_csv(vf_tn_fixture())
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "flip.csv"))
    session$setInputs(screen_time = "tn")
    rep <- vf_text(output$f0_screen_report)
    expect_match(rep, paste("The models take tn as already on 0\u20131 with these rows, but rescale",
                            "each token from its first f0 to its last without them."), fixed = TRUE)
    expect_match(rep, paste("Your raw file is not changed The boxes change only the app's copy of the data.",
                            "To save that copy, download it in Normalise or Curate."), fixed = TRUE)
    expect_no_match(rep, "notes above say", fixed = TRUE)
    # On the real time column nothing flips, and the folded text is the same
    session$setInputs(screen_time = "time")
    rep <- vf_text(output$f0_screen_report)
    expect_no_match(rep, "The models take time", fixed = TRUE)
    expect_match(rep, "To save that copy, download it in Normalise or Curate.", fixed = TRUE)
    expect_no_match(rep, "stay the same", fixed = TRUE)
  })
})

test_that("the Start report never takes landmark columns for token-level values", {
  # Every F0 Extraction export with a syllable tier: syllable, _start, _end
  # and _i change within a token by design, and so do the axes Normalise
  # makes from it (a _tbound too, of a tier or not: '^word' and '^syllable'
  # would otherwise read them as the item column)
  d <- vf_norm_data()
  d$word_f0_dur <- d$time
  d <- suppressMessages(normalise_time_landmarks(d, "time", "syllable", f0 = "f0", token = "token"))
  d$word_tbound <- d$time
  f <- start_write_csv(d)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "landmarks.csv"))
    expect_identical(start_cols()$token, "token")
    rep <- vf_text(output$f0_screen_report)
    expect_match(rep, "Data screening", fixed = TRUE)
    expect_no_match(rep, "Not the same on every row", fixed = TRUE)
  })
  # A token-level column that does vary is still reported. Every token has
  # 20 rows (an equal-point file), where the rows without f0 are removed
  # as for frames, so the note speaks of removing them
  d$speaker[d$token == "a1"][1:3] <- "S9"
  f <- start_write_csv(d)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "landmarks.csv"))
    expect_true(screen_plan()$points)
    rep <- vf_text(output$f0_screen_report)
    expect_match(rep, paste("Not the same on every row of a token: speaker (1 token). Removing rows can",
                            "change the value such a token gets."), fixed = TRUE)
  })
})

test_that("Normalise builds no time axis from menus its panel no longer offers", {
  env <- vf_env()
  # No token column (as test/dc21f0.csv): the panel says time cannot be
  # normalised, but the inputs keep the last file's values
  d <- vf_norm_data()
  names(d)[names(d) == "token"] <- "rec"
  nd  <- shiny::reactiveVal(NULL)
  srv <- vf_norm(env, d, nd = nd)
  shiny::testServer(srv, {
    expect_match(vf_html(output$norm_time_landmark_ui),
                 "No token variable, so time cannot be normalised here.", fixed = TRUE)
    session$setInputs(norm_f0_var = "f0", norm_speaker_var = "speaker", norm_tone_var = "tone",
                      mean_calc_method = "simple", normalisation_method = "semitone",
                      sd_correction = TRUE, norm_time_set = "__token__", norm_time_var = "time",
                      norm_time_token_var = "speaker")
    session$setInputs(normalise_button = 1)
    out <- nd()
    expect_true("f0_st" %in% names(out))
    expect_false("token_t01" %in% names(out))
    expect_false(any(grepl("Added token_t01", vf_toasts(env), fixed = TRUE)))
    # A tier these columns do not have
    session$setInputs(norm_time_set = "word", normalise_button = 2)
    expect_false(any(grepl("_(t01|tseq|tbound)$", names(nd()))))
  })
})

test_that("the Normalise table shows the new axes first, rounded, in a box that scrolls", {
  env <- vf_env()
  d <- vf_norm_data()
  d$f0_corrected <- d$f0_corrected + 1 / 3
  nd  <- shiny::reactiveVal(NULL)
  srv <- vf_norm(env, d, list(token = "token", time = "time", f0 = "f0_corrected"), nd = nd)
  shiny::testServer(srv, {
    vf_norm_inputs(session, set = "syllable")
    dt <- norm_table(norm_display(), norm_lead())
    expect_identical(names(dt$x$data)[2:8],
                     c("time", "token", "syllable_i", "syllable_t01", "syllable_tseq", "syllable_tbound",
                       "f0_corrected"))
    expect_true(isTRUE(dt$x$options$scrollX))
    rnd <- Filter(function(cd) !is.null(cd$render), dt$x$options$columnDefs)
    digits <- vapply(rnd, function(cd) as.integer(sub(".*formatRound\\(data, (\\d+).*", "\\1", cd$render)), 1L)
    cols <- vapply(rnd, function(cd) paste(names(dt$x$data)[cd$targets + 1], collapse = ","), "")
    # time keeps its 3 decimals; f0, f0_st and the axes show 4
    expect_identical(digits[cols == "time"], 3L)
    expect_true(all(c("f0_corrected", "f0_st", "syllable_t01", "syllable_tseq", "syllable_tbound") %in%
                      unlist(strsplit(cols[digits == 4L], ","))))
    # The download keeps the columns and values as they are, the three axes too
    expect_identical(names(norm_display())[1], "f0_corrected")
    expect_equal(norm_display()$f0_corrected, d$f0_corrected)
    session$setInputs(output_filename = "out")
    dl <- utils::read.csv(output$download_data, stringsAsFactors = FALSE)
    expect_true(all(c("syllable_t01", "syllable_tseq", "syllable_tbound") %in% names(dl)))
    expect_equal(dl$syllable_tbound, norm_display()$syllable_tbound)
  })
})
