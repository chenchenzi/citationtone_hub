# Part E3: the R code that Model: Polynomials, Model: GCA and Model: GAMM
# show under "Show R code". Run on its own, it must reproduce the tab's fit,
# with the Start page's removal of rows without f0 ticked and unticked:
#   - the per-token time rescale runs from the token's first f0 to its last
#     (min() and max() over the rows with a time and f0), as
#     resolve_time_norm(f0 =) does, so rows without f0 at a token's edges do
#     not move it, kept or removed;
#   - GCA: the rows lmer() fits are kept before poly() builds its basis;
#   - GAMM: the rows bam() fits are kept before the factors, the AR1 start
#     and the prediction grid are made;
#   - Model: speaker and tone per token by the rule of fit_polynomial()
#     (the most frequent non-empty value among the rows with f0), not first().

# The app's server (helper-app-start.R) with the real Model, GCA and GAMM
# tabs in place of the stubs. Each tab leaves its environment in `cap`, so
# a test can read the fit the tab made.
e3_server <- function(.local = parent.frame()) {
  s <- start_server(.local)
  app <- system.file("app", package = "shinytone")
  pkgs <- parent.env(s$env)
  for (p in c("ggplot2", "dplyr"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  for (f in c("model_ui.R", "gca_ui.R", "gamm_ui.R"))
    sys.source(file.path(app, "ui", f), s$env, keep.source = FALSE)
  cap <- new.env()
  for (m in c("model_ui", "gca_ui", "gamm_ui")) local({
    name <- m
    real <- s$env[[name]]
    body(real) <- as.call(c(as.list(body(real)), bquote(cap[[.(name)]] <- environment())))
    environment(real) <- list2env(list(cap = cap), parent = s$env)
    s$env[[name]] <- real
  })
  s$cap <- cap
  s
}

# The text of a "Show R code" block, unescaped, so it can be run.
e3_code <- function(x) {
  html <- paste(as.character(x$html), collapse = "")
  code <- sub("(?s).*<code>(.*)</code>.*", "\\1", html, perl = TRUE)
  for (p in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&#39;", "'"),
                 c("&amp;", "&")))
    code <- gsub(p[1], p[2], code, fixed = TRUE)
  code
}

# shinytone's own functions that it does not export, hidden: under
# load_all() they are all in reach, but a fresh session with
# library(shinytone) has only the exports. Looking one up stops, as it would
# there. (Names that another package the code uses also has stay in reach.)
e3_hidden <- function() {
  ns    <- asNamespace("shinytone")
  own   <- setdiff(ls(ns), getNamespaceExports("shinytone"))
  other <- unlist(lapply(c("base", "stats", "utils", "graphics", "grDevices", "methods",
                           "dplyr", "ggplot2", "lme4", "mgcv"), function(p)
    if (requireNamespace(p, quietly = TRUE)) getNamespaceExports(p)))
  hide <- new.env(parent = globalenv())
  for (nm in setdiff(own, other)) local({
    name <- nm
    makeActiveBinding(name, function(v) {
      stop(sprintf("could not find function \"%s\" (not exported by shinytone)", name),
           call. = FALSE)
    }, hide)
  })
  hide
}

# Run the code in an environment of its own, as a fresh session would, and
# return that environment. The GAMM code's diagnostics block (plots,
# itsadug) is left out unless `diagnostics` is TRUE: it reads the model but
# changes nothing.
e3_run <- function(code, diagnostics = FALSE) {
  if (!diagnostics)
    code <- sub("(?s)# --- Model diagnostics ---.*?(# Predict population-level)", "\\1", code,
                perl = TRUE)
  env <- new.env(parent = e3_hidden())
  # Plots go to a null device; invisible(): the code ends with a plot,
  # which must not be drawn here
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  utils::capture.output(invisible(
    suppressWarnings(suppressMessages(eval(parse(text = code, encoding = "UTF-8"), env)))))
  env
}

test_that("e3_run() stops on a function shinytone does not export", {
  expect_error(e3_run('x <- tone_factor(c("T2", "T1"))'), "could not find function \"tone_factor\"")
  expect_error(e3_run("legendre_basis(0.5, 2)"), "not exported by shinytone")
  # The code's own functions and the exported ones are found
  env <- e3_run(paste('legendre_basis <- function(t, degree) t', 'x <- legendre_basis(1, 2)',
                      'y <- sort_tones(c("T2", "T1"))', sep = "\n"))
  expect_identical(env$x, 1)
  expect_identical(env$y, c("T1", "T2"))
})

e3_pos <- function(code, x) regexpr(x, code, fixed = TRUE)[[1]]

# The padded fixture of helper-app-start.R (rows without f0 at both ends of
# every token, gaps inside, a token with no f0, 0 Hz), with two tones, an
# item column and a tone tier `label` that changes within a token: "" on the
# rows without f0, and the first row with f0 in S1_b carries the tone of the
# syllable before. Two rows have no time: one with f0 inside S1_a and the
# first row of S2_c. Three more tokens test the rule for the label and the
# case of one time point:
#   - S1_spill: its 6 rows without f0 say T1, its 4 rows with f0 say T2;
#   - S2_tie: a 2-2 tie among the rows with f0, where T2 comes first in time
#     and T1 sorts first;
#   - S1_one: its samples all share one time point.
e3_fixture <- function() {
  fx <- start_padded_fixture()
  fx$tone  <- ifelse(fx$token %in% c("S1_a", "S2_a"), "T1", "T2")
  fx$item  <- fx$token
  fx$label <- ifelse(is.na(fx$f0) | fx$f0 <= 0, "", fx$tone)
  fx$label[fx$token == "S1_b" & !is.na(fx$f0)][1] <- "T1"
  fx$time[fx$token == "S1_a"][8] <- NA
  fx$time[fx$token == "S2_c"][1] <- NA
  mk <- function(tok, f0, label, time = round(0.005 + 0.01 * (seq_along(f0) - 1), 4))
    data.frame(token = tok, time = time, f0 = f0, intensity = 50 + seq_along(f0),
               speaker = substr(tok, 1, 2), tone = "T2", item = tok, label = label,
               stringsAsFactors = FALSE)
  rbind(fx,
        mk("S1_spill", c(NA, NA, NA, 180, 184, 188, 192, NA, NA, NA),
           c(rep("T1", 3), rep("T2", 4), rep("T1", 3))),
        mk("S2_tie", c(NA, 160, 162, 164, 166, NA), c("", "T2", "T2", "T1", "T1", "")),
        mk("S1_one", c(NA, 210, 212, 214, NA), "T2",
           time = c(0.005, 0.015, 0.015, 0.015, 0.025)))
}

# The rows in another order, so code that does not sort by time within a
# token gives another result
e3_shuffle <- function(fx, seed = 3) {
  set.seed(seed)
  fx[sample(nrow(fx)), ]
}

# Upload `fx` on the Start page and leave removal ticked (its default here)
# or untick it.
e3_upload <- function(session, f, remove) {
  session$setInputs(uploadfile = start_upload(f, "padded.csv"), dataset_name = sub("\\.csv$", "", f))
  if (!remove) session$setInputs(screen_remove = FALSE)
}

test_that("Model's R code reproduces the fit with removal ticked and unticked", {
  f <- start_write_csv(e3_fixture())
  for (remove in c(TRUE, FALSE)) {
    s <- e3_server()
    shiny::testServer(s$server, {
      e3_upload(session, f, remove)
      expect_identical(isTRUE(start_settings()$screen$action == "remove"), remove)
      session$setInputs(model_dataset = "uploaded", model_token_var = "token",
                        model_f0_var = "f0", model_time_var = "time",
                        model_speaker_var = "speaker", model_tone_var = "label",
                        model_degree = "2", model_button = 1, model_show_code = 1)
      app  <- as.data.frame(s$cap$model_ui$model_result())
      code <- e3_code(output$model_r_code)

      # The time rescale runs from the token's first f0 to its last: over the
      # rows with a time and f0 (ok), so rows without f0 or time do not move it
      expect_match(code, "t_min  <- min(t_raw[ok])", fixed = TRUE)
      expect_match(code, "t_max  <- max(t_raw[ok])", fixed = TRUE)
      # Speaker and tone by the rule of fit_polynomial(), not the first row
      expect_no_match(code, "first(", fixed = TRUE)
      expect_match(code, "speaker = pick_value(speaker, time, is.finite(f0))", fixed = TRUE)
      expect_match(code, "label = pick_value(label, time, is.finite(f0))", fixed = TRUE)
      expect_match(code, "the most frequent\n# non-empty value among the token's rows with f0",
                   fixed = TRUE)
      expect_identical(grepl("drop_rows_without_f0(dat", code, fixed = TRUE), remove)

      run <- e3_run(code)
      got <- as.data.frame(run$results)
      # The same tokens (S2_b, with no f0, is there only when its rows are kept)
      expect_setequal(got$token, app$token)
      expect_identical("S2_b" %in% app$token, !remove)
      got <- got[match(app$token, got$token), ]
      for (cn in c("c0", "c1", "c2")) {
        expect_identical(is.na(got[[cn]]), is.na(app[[cn]]), info = cn)
        expect_equal(got[[cn]], app[[cn]], tolerance = 1e-8, info = cn)
      }
      expect_identical(got$speaker, app$speaker)
      expect_identical(got$label, app$label)
      # S1_b's label is its most frequent one; its first row (no f0) has ""
      # and its first row with f0 has "T1"
      expect_identical(app$label[app$token == "S1_b"], "T2")
      # The label of the rows with f0, though more rows without f0 say T1
      expect_identical(app$label[app$token == "S1_spill"], "T2")
      # A tie goes to the label that comes first in time
      expect_identical(app$label[app$token == "S2_tie"], "T2")
      # All samples at one time point: their mean as c0, no slope
      one <- app[app$token == "S1_one", ]
      expect_equal(one$c0, 212)
      expect_true(is.na(one$c1) && is.na(one$c2))
      # S1_a keeps its row with f0 but no time out of the fit, not the token
      expect_false(is.na(app$c2[app$token == "S1_a"]))
    })
  }
})

test_that("GCA's R code keeps the fitted rows before poly() and reproduces the fit", {
  skip_if_not_installed("lme4")
  f <- start_write_csv(e3_fixture())
  for (remove in c(TRUE, FALSE)) {
    s <- e3_server()
    shiny::testServer(s$server, {
      e3_upload(session, f, remove)
      session$setInputs(gca_dataset = "uploaded", gca_token_var = "token", gca_f0_var = "f0",
                        gca_time_var = "time", gca_speaker_var = "speaker",
                        gca_tone_var = "tone", gca_item_var = "item", gca_degree = "2",
                        gca_ri_speaker = TRUE, gca_ri_item = FALSE, gca_rs_speaker = FALSE,
                        gca_rs_item = FALSE)
      suppressWarnings(suppressMessages(session$setInputs(gca_button = 1, gca_show_code = 1)))
      app  <- s$cap$gca_ui$gca_model()
      code <- e3_code(output$gca_r_code)

      expect_match(code, "t_min <- min(t[ok])", fixed = TRUE)
      expect_match(code, "t_max <- max(t[ok])", fixed = TRUE)
      expect_match(code, "mutate(time_norm = rescale01(time, is.finite(f0)))", fixed = TRUE)
      # The rows lmer() fits (speaker has a term, item does not) go before poly()
      keep <- "filter(is.finite(time_norm), is.finite(f0), !is.na(tone), !is.na(speaker))"
      expect_match(code, keep, fixed = TRUE)
      expect_no_match(code, "!is.na(item)", fixed = TRUE)
      expect_lt(e3_pos(code, "rescale01(time, is.finite(f0))"), e3_pos(code, keep))
      expect_lt(e3_pos(code, keep), e3_pos(code, "poly(dat$time_norm"))

      run <- e3_run(code)
      expect_false(anyNA(run$dat$time_norm))
      expect_false(anyNA(run$dat$f0))
      # S1_one's samples share one time point: 0.5, as the app gives them.
      # Its rows without f0 never set the span, so this holds with removal
      # unticked too.
      one <- run$dat$time_norm[run$dat$token == "S1_one"]
      expect_length(one, 3)
      expect_equal(one, rep(0.5, 3))
      expect_identical(nrow(run$model@frame), nrow(app@frame))
      expect_equal(unname(lme4::fixef(run$model)), unname(lme4::fixef(app)), tolerance = 1e-6)
    })
  }

  # The item gets a row condition only when the model has a term for it
  s <- e3_server()
  shiny::testServer(s$server, {
    e3_upload(session, f, TRUE)
    session$setInputs(gca_dataset = "uploaded", gca_token_var = "token", gca_f0_var = "f0",
                      gca_time_var = "time", gca_speaker_var = "speaker",
                      gca_tone_var = "tone", gca_item_var = "item", gca_degree = "2",
                      gca_ri_speaker = FALSE, gca_ri_item = FALSE, gca_rs_speaker = FALSE,
                      gca_rs_item = FALSE, gca_show_code = 1)
    code <- e3_code(output$gca_r_code)
    expect_match(code, "filter(is.finite(time_norm), is.finite(f0), !is.na(tone), !is.na(item))",
                 fixed = TRUE)
    expect_match(code, "(1 | item)", fixed = TRUE)
  })
})

# Compare a run of the GAMM tab's R code with the tab's fit: the rows bam()
# fits, the AR1 starts, rho, the fitted values (matched by token and time)
# and the curves of the tab's plot. `tone_col` is the tone column of the
# code's prediction grid.
e3_expect_gamm <- function(run, fit, curves, n_starts, tone_col = "tone", info = NULL) {
  m <- fit$model
  expect_identical(nrow(run$model$model), nrow(m$model), info = info)
  expect_identical(sum(run$model$model[["(AR.start)"]]), sum(m$model[["(AR.start)"]]),
                   info = info)
  expect_identical(sum(run$model$model[["(AR.start)"]]), n_starts, info = info)
  expect_equal(run$rho, fit$rho, tolerance = 1e-6, info = info)
  rows <- fit$data_for_predict
  if (!is.null(m$na.action)) rows <- rows[-as.integer(m$na.action), ]
  i <- match(paste(rows$token, rows$time), paste(run$dat$token, run$dat$time))
  expect_false(anyNA(i), info = info)
  expect_equal(as.numeric(fitted(run$model))[i], as.numeric(fitted(m)), tolerance = 1e-6,
               info = info)
  j <- match(paste(curves$tone, round(curves$time, 10)),
             paste(run$newdat[[tone_col]], round(run$newdat$time_norm, 10)))
  expect_false(anyNA(j), info = info)
  expect_identical(nrow(run$newdat), nrow(curves), info = info)
  expect_equal(run$newdat$fit[j], curves$f0_predicted, tolerance = 1e-6, info = info)
}

gamm_inputs <- list(gamm_dataset = "uploaded", gamm_token_var = "token", gamm_f0_var = "f0",
                    gamm_time_var = "time", gamm_speaker_var = "speaker",
                    gamm_tone_var = "tone", gamm_item_var = "item", gamm_dur_var = "none",
                    gamm_k = 4, gamm_bs = "tp", gamm_smooth_type = "separate",
                    gamm_ri_speaker = TRUE, gamm_ri_item = TRUE, gamm_rs_type = "none",
                    gamm_ar1 = TRUE)

test_that("GAMM's R code fits the rows bam() fits, with the AR1 start, rho and grid on them", {
  skip_if_not_installed("mgcv")
  # Shuffled rows: the AR1 start and rho need the rows in time order
  f <- start_write_csv(e3_shuffle(e3_fixture()))
  for (remove in c(TRUE, FALSE)) {
    s <- e3_server()
    shiny::testServer(s$server, {
      e3_upload(session, f, remove)
      do.call(session$setInputs, gamm_inputs)
      suppressWarnings(suppressMessages(session$setInputs(gamm_button = 1, gamm_show_code = 1)))
      fit  <- s$cap$gamm_ui$gamm_fit_obj()
      code <- e3_code(output$gamm_r_code)

      expect_match(code, "t_min <- min(t[ok])", fixed = TRUE)
      expect_match(code, "t_max <- max(t[ok])", fixed = TRUE)
      expect_match(code, "mutate(time_norm = rescale01(time, is.finite(f0)))", fixed = TRUE)
      # The fitted rows are kept before the factors, the AR1 start and the grid
      keep <- 'dat <- dat[complete.cases(dat[, c("f0", "time_norm", "tone", "speaker", "item")]), ]'
      expect_match(code, keep, fixed = TRUE)
      at <- vapply(c("rescale01(time, is.finite(f0))", keep, "# Ensure factor variables",
                     "arrange(token, time_norm)", "mutate(start_event = row_number() == 1)",
                     "rho  <- sum(", "AR.start = dat$start_event", "newdat <- expand.grid("),
                   function(x) e3_pos(code, x), numeric(1))
      expect_true(all(at > 0))
      expect_false(is.unsorted(at))
      expect_match(code, "dat holds only the rows the model fits", fixed = TRUE)
      expect_match(code, "time_norm = seq(0, 1, length.out = 200)", fixed = TRUE)

      # One AR1 start on the first fitted row of each of the 7 tokens with f0
      e3_expect_gamm(e3_run(code), fit, s$cap$gamm_ui$gamm_plot_data(), 7L,
                     info = paste("remove =", remove))
    })
  }
})

test_that("GAMM's R code with Speaker by Tone smooths fits the app's model", {
  skip_if_not_installed("mgcv")
  # fit_gamm() gives these smooths the plain tone factor, with difference
  # smooths too, so the code makes that factor as well as tone.ord
  f <- start_write_csv(e3_fixture())
  for (remove in c(TRUE, FALSE)) for (st in c("difference", "separate")) {
    s <- e3_server()
    shiny::testServer(s$server, {
      e3_upload(session, f, remove)
      do.call(session$setInputs, modifyList(gamm_inputs, list(
        gamm_smooth_type = st, gamm_ri_speaker = FALSE, gamm_rs_type = "speaker_by_tone")))
      suppressWarnings(suppressMessages(session$setInputs(gamm_button = 1, gamm_show_code = 1)))
      fit  <- s$cap$gamm_ui$gamm_fit_obj()
      code <- e3_code(output$gamm_r_code)
      info <- paste("remove =", remove, st)

      expect_match(code, 's(time_norm, speaker, by = tone, bs = "fs", m = 1, k = 4)',
                   fixed = TRUE, info = info)
      expect_no_match(code, "speaker, by = tone.ord", fixed = TRUE)
      # The formula the tab shows says the same
      expect_match(fit$formula_str, 's(time, speaker, by = tone, bs = "fs"', fixed = TRUE,
                   info = info)
      if (st == "difference") {
        expect_match(code, "dat$tone <- as.factor(dat$tone)", fixed = TRUE)
        expect_lt(e3_pos(code, "dat$tone.ord <- as.ordered(dat$tone)"),
                  e3_pos(code, "dat$tone <- as.factor(dat$tone)"))
        expect_match(code, paste0("newdat$tone <- factor(as.character(newdat$tone.ord), ",
                                  "levels = levels(dat$tone))"), fixed = TRUE)
      }
      e3_expect_gamm(e3_run(code), fit, s$cap$gamm_ui$gamm_plot_data(), 7L,
                     tone_col = if (st == "difference") "tone.ord" else "tone", info = info)
    })
  }
})

test_that("GAMM's curves come from the fitted rows: a tone, speaker or item only rows without f0 carry", {
  skip_if_not_installed("mgcv")
  # A token with no f0 whose tone (T0), speaker (A0) and item (aa) are its
  # own, and sort first. With removal unticked its rows reach fit_gamm(),
  # but bam() fits none of them.
  fx <- e3_fixture()
  fx <- rbind(data.frame(token = "A0_none", time = round(0.005 + 0.01 * (0:4), 4), f0 = NA,
                         intensity = 50, speaker = "A0", tone = "T0", item = "aa", label = "T0",
                         stringsAsFactors = FALSE), fx)
  f <- start_write_csv(fx)

  # The package: no curve for T0, and S1 (the first fitted speaker), not A0,
  # is the reference speaker, whose Speaker by Tone smooths are in the
  # curves. So the curves are those of the data without that token.
  fx0 <- zero_hz_to_na(fx)
  d   <- fx0[fx0$token != "A0_none", ]
  for (rs in c("none", "ref_diff")) {
    fit_all <- suppressMessages(fit_gamm(fx0, f0 = "f0", time = "time", token = "token",
                                         tone = "tone", speaker = "speaker", item = "item",
                                         k = 4, smooth_type = "difference",
                                         random_intercept_item = TRUE, random_smooth = rs,
                                         use_ar1 = TRUE))
    fit_f0 <- suppressMessages(fit_gamm(d, f0 = "f0", time = "time", token = "token",
                                        tone = "tone", speaker = "speaker", item = "item",
                                        k = 4, smooth_type = "difference",
                                        random_intercept_item = TRUE, random_smooth = rs,
                                        use_ar1 = TRUE))
    p_all <- expect_no_warning(predict_gamm(fit_all, n = 50))
    p_f0  <- predict_gamm(fit_f0, n = 50)
    expect_identical(unique(p_all$tone), c("T1", "T2"))
    expect_equal(p_all$f0_predicted, p_f0$f0_predicted, tolerance = 1e-6, info = rs)
  }

  for (remove in c(TRUE, FALSE)) {
    s <- e3_server()
    shiny::testServer(s$server, {
      e3_upload(session, f, remove)
      do.call(session$setInputs, modifyList(gamm_inputs, list(
        gamm_smooth_type = "difference", gamm_ri_speaker = FALSE, gamm_rs_type = "ref_diff")))
      suppressWarnings(suppressMessages(session$setInputs(gamm_button = 1, gamm_show_code = 1)))
      curves <- s$cap$gamm_ui$gamm_plot_data()
      expect_identical(unique(curves$tone), c("T1", "T2"))
      expect_false(any(vapply(s$rec$toasts, function(t) t$type == "error", logical(1))))
      e3_expect_gamm(e3_run(e3_code(output$gamm_r_code)), s$cap$gamm_ui$gamm_fit_obj(), curves,
                     7L, tone_col = "tone.ord", info = paste("remove =", remove))
    })
  }
})

test_that("an error in GAMM's prediction shows a notification and keeps the session", {
  skip_if_not_installed("mgcv")
  f <- start_write_csv(e3_fixture())
  s <- e3_server()
  s$env$predict_gamm <- function(...) stop("no grid")
  shiny::testServer(s$server, {
    e3_upload(session, f, TRUE)
    do.call(session$setInputs, gamm_inputs)
    expect_no_error(suppressWarnings(suppressMessages(session$setInputs(gamm_button = 1))))
    # An error that left the observer would have ended the session
    expect_false(session$isClosed())
    expect_false(is.null(s$cap$gamm_ui$gamm_fit_obj()))
    expect_null(s$cap$gamm_ui$gamm_plot_data())
    err <- Filter(function(t) t$type == "error", s$rec$toasts)
    expect_length(err, 1)
    expect_match(err[[1]]$text, "its curves could not be predicted: no grid", fixed = TRUE)
  })
})

test_that("GAMM's R code on a sequential axis predicts over the span the model fits", {
  skip_if_not_installed("mgcv")
  # A two-segment landmark axis made elsewhere: 0 to 1.05 over each token's
  # rows, rows without f0 included. With removal unticked, those rows widen
  # the axis but not the span bam() fits. A <tier>_tseq from Normalise
  # (normalise_time_landmarks()) never has values on rows without f0 at a
  # token's edges; this checks axes made elsewhere.
  fx <- e3_fixture()
  fx$syl_tseq <- fx$time * 10
  f <- start_write_csv(fx)
  s <- e3_server()
  shiny::testServer(s$server, {
    e3_upload(session, f, FALSE)
    session$setInputs(gamm_dataset = "uploaded", gamm_token_var = "token", gamm_f0_var = "f0",
                      gamm_time_var = "syl_tseq", gamm_speaker_var = "speaker",
                      gamm_tone_var = "tone", gamm_item_var = "item", gamm_dur_var = "none",
                      gamm_k = 4, gamm_bs = "tp", gamm_smooth_type = "separate",
                      gamm_ri_speaker = TRUE, gamm_ri_item = TRUE, gamm_rs_type = "none",
                      gamm_ar1 = FALSE)
    suppressWarnings(suppressMessages(session$setInputs(gamm_button = 1, gamm_show_code = 1)))
    fit  <- s$cap$gamm_ui$gamm_fit_obj()
    code <- e3_code(output$gamm_r_code)
    expect_match(code, "over the span the model fits", fixed = TRUE)
    expect_lt(e3_pos(code, "complete.cases("),
              e3_pos(code, "time_norm = seq(min(dat$time_norm, na.rm = TRUE)"))

    run <- e3_run(code)
    expect_identical(nrow(run$model$model), nrow(fit$model$model))
    expect_equal(range(run$newdat$time_norm), fit$time_range)
    expect_gt(min(run$newdat$time_norm), min(fx$syl_tseq, na.rm = TRUE))
    pred <- predict_gamm(fit)
    j <- match(paste(pred$tone, round(pred$time, 10)),
               paste(run$newdat$tone, round(run$newdat$time_norm, 10)))
    expect_false(anyNA(j))
    expect_equal(run$newdat$fit[j], pred$f0_predicted, tolerance = 1e-6)
  })
})

# Disyllabic tokens on an in-sequence axis from normalise_time_landmarks().
# Syllable 1 falls (T4T1) or stays level (T1T1), rows without f0 at the
# boundary (a voiceless stretch), and syllable 2 starts high: syllable 1's
# last f0 and syllable 2's first f0 share x = 1 on syllable_tseq, with an f0
# jump between them in T4T1. Rows without f0 at both edges, one of them a
# 0 Hz row, and tokens of 20 to 22 rows, so removal starts ticked.
e3_stacked <- function() {
  set.seed(21)
  g <- expand.grid(speaker = c("S1", "S2"), tone = c("T4T1", "T1T1"), item = c("w1", "w2"),
                   rep = 1:2, stringsAsFactors = FALSE)
  d <- do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    n  <- 20 + i %% 3
    t  <- round(0.005 + 0.01 * (seq_len(n) - 1), 4)
    s1 <- t < 0.1
    f0 <- if (g$tone[i] == "T4T1") ifelse(s1, 250 - 1000 * t, 240) else rep(200, n)
    f0 <- f0 + 10 * (g$speaker[i] == "S2") + stats::rnorm(n, sd = 1)
    f0[c(1, 2, n)] <- NA
    if (i == 1) f0[n] <- 0               # a 0 Hz row: no f0, as in Praat's output
    f0[t > 0.085 & t < 0.115] <- NA
    data.frame(token = paste(g$speaker[i], g$tone[i], g$item[i], g$rep[i], sep = "_"),
               time = t, f0 = f0, speaker = g$speaker[i], tone = g$tone[i], item = g$item[i],
               syllable = ifelse(s1, "a", "b"), syllable_start = ifelse(s1, 0, 0.1),
               syllable_end = ifelse(s1, 0.1, max(t) + 0.005), syllable_i = ifelse(s1, 1L, 2L),
               stringsAsFactors = FALSE)
  }))
  normalise_time_landmarks(d, "time", "syllable", f0 = "f0", token = "token")
}

test_that("the R code reproduces fits on a <tier>_tseq from normalise_time_landmarks(), ties included", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("mgcv")
  fx <- e3_stacked()
  # Every syllable runs from its first f0 to its last: i - 1 to i, so two
  # rows of each token sit at 1
  tied <- tapply(abs(fx$syllable_tseq - 1) < 1e-12, fx$token, sum, na.rm = TRUE)
  expect_true(all(tied == 2))
  # Shuffled rows: the AR1 start, rho and the order of the two rows at 1
  # come from the rows in time order (and the file's order at a tie)
  f <- start_write_csv(e3_shuffle(fx, seed = 5))
  n_tok <- length(unique(fx$token))
  for (remove in c(TRUE, FALSE)) {
    s <- e3_server()
    shiny::testServer(s$server, {
      e3_upload(session, f, remove)
      info <- paste("remove =", remove)
      expect_identical(isTRUE(start_settings()$screen$action == "remove"), remove)
      expect_identical(anyNA(dataset()$f0), !remove)

      # Model: syllable_tseq is rescaled per token from its first f0 (0) to
      # its last (2)
      session$setInputs(model_dataset = "uploaded", model_token_var = "token",
                        model_f0_var = "f0", model_time_var = "syllable_tseq",
                        model_speaker_var = "speaker", model_tone_var = "tone",
                        model_degree = "2", model_button = 1, model_show_code = 1)
      app  <- as.data.frame(s$cap$model_ui$model_result())
      code <- e3_code(output$model_r_code)
      expect_match(code, "t_min  <- min(t_raw[ok])", fixed = TRUE)
      got <- as.data.frame(e3_run(code)$results)
      got <- got[match(app$token, got$token), ]
      for (cn in c("c0", "c1", "c2"))
        expect_equal(got[[cn]], app[[cn]], tolerance = 1e-8, info = paste(info, cn))

      # GCA: the same rescale
      session$setInputs(gca_dataset = "uploaded", gca_token_var = "token", gca_f0_var = "f0",
                        gca_time_var = "syllable_tseq", gca_speaker_var = "speaker",
                        gca_tone_var = "tone", gca_item_var = "item", gca_degree = "2",
                        gca_ri_speaker = TRUE, gca_ri_item = FALSE, gca_rs_speaker = FALSE,
                        gca_rs_item = FALSE)
      suppressWarnings(suppressMessages(session$setInputs(gca_button = 1, gca_show_code = 1)))
      code <- e3_code(output$gca_r_code)
      expect_match(code, "mutate(time_norm = rescale01(syllable_tseq, is.finite(f0)))",
                   fixed = TRUE)
      run <- e3_run(code)
      gm  <- s$cap$gca_ui$gca_model()
      expect_identical(nrow(run$model@frame), nrow(gm@frame))
      expect_equal(unname(lme4::fixef(run$model)), unname(lme4::fixef(gm)), tolerance = 1e-6,
                   info = info)

      # GAMM with AR1: syllable_tseq is used as it is, on 0 to 2
      do.call(session$setInputs, modifyList(gamm_inputs, list(gamm_time_var = "syllable_tseq")))
      suppressWarnings(suppressMessages(session$setInputs(gamm_button = 1, gamm_show_code = 1)))
      fit  <- s$cap$gamm_ui$gamm_fit_obj()
      code <- e3_code(output$gamm_r_code)
      expect_match(code, "dat$time_norm <- dat$syllable_tseq", fixed = TRUE)
      expect_match(code, "keep their order in the file, as in the app", fixed = TRUE)
      expect_match(code, "dashed line where one syllable's f0 ends and the next begins",
                   fixed = TRUE)
      expect_equal(fit$time_range, c(0, 2))
      expect_identical(fit$n_segments, 2L)
      e3_expect_gamm(e3_run(code), fit, s$cap$gamm_ui$gamm_plot_data(), n_tok, info = info)

      # GAMM on syllable_tbound: also used as it is, on 0 to 2, with the gap
      # kept, so no two rows of a token share a time
      do.call(session$setInputs, modifyList(gamm_inputs, list(gamm_time_var = "syllable_tbound")))
      suppressWarnings(suppressMessages(session$setInputs(gamm_button = 2)))
      fit  <- s$cap$gamm_ui$gamm_fit_obj()
      code <- e3_code(output$gamm_r_code)
      expect_match(code, "dat$time_norm <- dat$syllable_tbound", fixed = TRUE)
      expect_match(code, "# syllable_tbound puts the syllables in sequence, gap kept: 0 is the first",
                   fixed = TRUE)
      expect_match(code, "dashed line at each TextGrid boundary (1, 2, ...)", fixed = TRUE)
      expect_no_match(code, "keep their order in the file", fixed = TRUE)
      expect_match(code, 'labs(x = "Normalised time, syllable by syllable (syllable_tbound)"',
                   fixed = TRUE)
      expect_equal(fit$time_range, c(0, 2))
      expect_identical(fit$n_segments, 2L)
      e3_expect_gamm(e3_run(code), fit, s$cap$gamm_ui$gamm_plot_data(), n_tok, info = info)
      # The plot names each syllable and marks the boundary at 1
      p <- s$cap$gamm_ui$gamm_prediction_plot(s$cap$gamm_ui$gamm_plot_data(), fit)
      expect_identical(p$labels$x, "Normalised time, syllable by syllable (syllable_tbound)")
      vl <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomVline"), NA))
      expect_equal(ggplot2::layer_data(p, vl)$xintercept, 1)
    })
  }
})

test_that("the Normalised-data note names the time-column call with its f0 and token", {
  f <- start_write_csv(e3_fixture())
  s <- e3_server()
  shiny::testServer(s$server, {
    e3_upload(session, f, TRUE)
    nd <- normalise_time_token(dataset(), "time", "token", f0 = "f0")
    nd$syllable_start <- 0; nd$syllable_end <- 1; nd$syllable_i <- 1L
    nd <- normalise_time_landmarks(nd, "time", "syllable", f0 = "f0", token = "token")
    normalised_data(nd)
    session$setInputs(model_dataset = "normalised", gca_dataset = "normalised",
                      gamm_dataset = "normalised", model_token_var = "token",
                      model_f0_var = "f0", model_speaker_var = "speaker",
                      model_tone_var = "tone", model_degree = "2",
                      gca_token_var = "token", gca_f0_var = "f0", gca_speaker_var = "speaker",
                      gca_tone_var = "tone", gca_item_var = "item", gca_degree = "2",
                      gca_ri_speaker = TRUE, gca_ri_item = FALSE, gca_rs_speaker = FALSE,
                      gca_rs_item = FALSE)
    do.call(session$setInputs, gamm_inputs[!names(gamm_inputs) %in% c("gamm_dataset", "gamm_time_var")])
    session$setInputs(model_show_code = 1, gca_show_code = 1, gamm_show_code = 1)
    for (tv in c("token_t01", "syllable_tseq", "syllable_tbound", "time")) {
      session$setInputs(model_time_var = tv, gca_time_var = tv, gamm_time_var = tv)
      for (code in list(e3_code(output$model_r_code), e3_code(output$gca_r_code),
                        e3_code(output$gamm_r_code))) {
        note <- substring(code, 1, e3_pos(code, "read_csv_utf8("))
        expect_match(note, "# Note: this tab used the Normalised data", fixed = TRUE)
        if (tv == "token_t01") {
          expect_match(note, '#   dat <- shinytone::normalise_time_token(dat, "<time>", "token",',
                       fixed = TRUE)
          expect_match(note, 'f0 = "<f0 (Hz)>")', fixed = TRUE)
        } else if (tv %in% c("syllable_tseq", "syllable_tbound")) {
          expect_match(note, paste0('#   dat <- shinytone::normalise_time_landmarks(dat, "<time>", ',
                                    '"syllable",'), fixed = TRUE)
          expect_match(note, 'f0 = "<f0 (Hz)>", token = "token")', fixed = TRUE)
        } else {
          # time was in the uploaded file: the general line
          expect_no_match(note, "dat <- shinytone::normalise_time", fixed = TRUE)
          expect_match(note, "with the f0 (Hz) and token columns that tab used (f0 =, token =)",
                       fixed = TRUE)
        }
      }
    }
  })
})

# A larger set with the tabs' own defaults: GCA with random slopes by
# speaker and intercepts by item, GAMM with difference smooths, speaker
# random smooths (Speaker + Speaker by Tone) and AR1.
e3_bigger <- function() {
  set.seed(11)
  g <- expand.grid(speaker = c("S1", "S2", "S3", "S4"), tone = c("T1", "T2", "T3"),
                   item = c("w1", "w2"), rep = 1:3, stringsAsFactors = FALSE)
  do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    n  <- 18 + i %% 3
    x  <- seq(0, 1, length.out = n)
    f0 <- switch(g$tone[i], T1 = 220 - 10 * x, T2 = 170 + 60 * x, T3 = 200 - 70 * x) +
      12 * sin(pi * x) + 5 * (g$speaker[i] == "S2") + stats::rnorm(n, sd = 2)
    f0[seq_len(1 + i %% 3)] <- NA
    f0[n - seq_len(1 + i %% 2) + 1] <- NA
    if (i %% 4 == 0) f0[8] <- NA
    data.frame(token = paste(g$speaker[i], g$tone[i], g$item[i], g$rep[i], sep = "_"),
               time = round(0.005 + 0.01 * (seq_len(n) - 1), 4), f0 = f0,
               speaker = g$speaker[i], tone = g$tone[i], item = g$item[i],
               stringsAsFactors = FALSE)
  }))
}

test_that("GCA and GAMM R code reproduce fits with the tabs' default random effects", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("mgcv")
  f <- start_write_csv(e3_bigger())
  for (remove in c(TRUE, FALSE)) {
    s <- e3_server()
    shiny::testServer(s$server, {
      e3_upload(session, f, remove)
      session$setInputs(gca_dataset = "uploaded", gca_token_var = "token", gca_f0_var = "f0",
                        gca_time_var = "time", gca_speaker_var = "speaker",
                        gca_tone_var = "tone", gca_item_var = "item", gca_degree = "2",
                        gca_ri_speaker = TRUE, gca_ri_item = TRUE, gca_rs_speaker = TRUE,
                        gca_rs_item = FALSE)
      suppressWarnings(suppressMessages(session$setInputs(gca_button = 1, gca_show_code = 1)))
      code <- e3_code(output$gca_r_code)
      expect_match(code, paste0("filter(is.finite(time_norm), is.finite(f0), !is.na(tone), ",
                                "!is.na(speaker), !is.na(item))"), fixed = TRUE)
      run <- e3_run(code)
      app <- s$cap$gca_ui$gca_model()
      expect_identical(nrow(run$model@frame), nrow(app@frame))
      expect_equal(unname(lme4::fixef(run$model)), unname(lme4::fixef(app)), tolerance = 1e-6,
                   info = paste("remove =", remove))

      session$setInputs(gamm_dataset = "uploaded", gamm_token_var = "token", gamm_f0_var = "f0",
                        gamm_time_var = "time", gamm_speaker_var = "speaker",
                        gamm_tone_var = "tone", gamm_item_var = "item", gamm_dur_var = "none",
                        gamm_k = 6, gamm_bs = "tp", gamm_smooth_type = "difference",
                        gamm_ri_speaker = FALSE, gamm_ri_item = TRUE, gamm_rs_type = "ref_diff",
                        gamm_ar1 = TRUE)
      suppressWarnings(suppressMessages(session$setInputs(gamm_button = 1, gamm_show_code = 1)))
      fit  <- s$cap$gamm_ui$gamm_fit_obj()
      code <- e3_code(output$gamm_r_code)
      run  <- e3_run(code)
      m <- fit$model
      expect_identical(nrow(run$model$model), nrow(m$model))
      expect_identical(sum(run$model$model[["(AR.start)"]]), 72L)
      expect_identical(sum(m$model[["(AR.start)"]]), 72L)
      expect_equal(run$rho, fit$rho, tolerance = 1e-6)
      rows <- fit$data_for_predict
      if (!is.null(m$na.action)) rows <- rows[-as.integer(m$na.action), ]
      i <- match(paste(rows$token, rows$time), paste(run$dat$token, run$dat$time))
      expect_equal(as.numeric(fitted(run$model))[i], as.numeric(fitted(m)), tolerance = 1e-6,
                   info = paste("remove =", remove))
    })
  }
})

test_that("GAMM's R code runs with its diagnostics block", {
  skip_on_cran()
  skip_if_not_installed("mgcv")
  skip_if_not_installed("itsadug")
  # The code attaches itsadug; leave the search path as it was
  before <- search()
  withr::defer(for (p in setdiff(search(), before))
    if (startsWith(p, "package:")) try(detach(p, character.only = TRUE), silent = TRUE))
  f <- start_write_csv(e3_fixture())
  for (ar1 in c(TRUE, FALSE)) {
    s <- e3_server()
    shiny::testServer(s$server, {
      e3_upload(session, f, FALSE)
      do.call(session$setInputs, modifyList(gamm_inputs, list(gamm_ar1 = ar1)))
      suppressWarnings(suppressMessages(session$setInputs(gamm_button = 1, gamm_show_code = 1)))
      code <- e3_code(output$gamm_r_code)
      expect_match(code, "try(concurvity(model, full = TRUE))", fixed = TRUE)
      expect_match(code, if (ar1) "acf_resid(model)   #" else "acf_resid(model, split_pred",
                   fixed = TRUE)
      run <- expect_no_error(e3_run(code, diagnostics = TRUE))
      expect_s3_class(run$model, "gam")
    })
  }
})

test_that("the R code says when the tab used other data than the uploaded file", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("mgcv")
  f <- start_write_csv(e3_fixture())
  s <- e3_server()
  shiny::testServer(s$server, {
    e3_upload(session, f, TRUE)
    session$setInputs(model_token_var = "token", model_f0_var = "f0", model_time_var = "time",
                      model_speaker_var = "speaker", model_tone_var = "tone", model_degree = "2",
                      gca_token_var = "token", gca_f0_var = "f0", gca_time_var = "time",
                      gca_speaker_var = "speaker", gca_tone_var = "tone", gca_item_var = "item",
                      gca_degree = "2", gca_ri_speaker = TRUE, gca_ri_item = FALSE,
                      gca_rs_speaker = FALSE, gca_rs_item = FALSE)
    do.call(session$setInputs, gamm_inputs[names(gamm_inputs) != "gamm_dataset"])
    session$setInputs(model_show_code = 1, gca_show_code = 1, gamm_show_code = 1)
    codes <- function() list(model = e3_code(output$model_r_code),
                             gca   = e3_code(output$gca_r_code),
                             gamm  = e3_code(output$gamm_r_code))
    libs <- c(model = "library(dplyr)\n\n", gca = "library(dplyr)\nlibrary(lme4)\n\n",
              gamm = "library(dplyr)\nlibrary(mgcv)\nlibrary(ggplot2)\n\n")
    pick <- function(src) session$setInputs(model_dataset = src, gca_dataset = src,
                                            gamm_dataset = src)

    # The uploaded data, or a source whose data does not exist yet: no note
    for (src in c("uploaded", "curated", "normalised")) {
      pick(src)
      for (tab in names(libs)) {
        code <- codes()[[tab]]
        expect_no_match(code, "# Note: this tab used", fixed = TRUE)
        expect_true(startsWith(code, paste0(libs[[tab]], "# The Start-page steps below")),
                    info = paste(tab, src))
        expect_no_match(code, "\u2014", fixed = TRUE)
      }
    }

    d  <- dataset()
    cd <- d[!d$token %in% c("S1_b", "S2_c"), ]
    cd$tone[cd$token == "S1_spill"] <- "T1"
    nd <- d
    nd$f0_z <- as.numeric(scale(nd$f0))
    normalised_data(nd)
    curated_data(cd)
    # After the flush, so the server's check of curated + normalised data
    # against the curated data (which this copy has no key for) has run
    session$flushReact()
    curated_norm_data(cd)
    cluster_data(transform(d, cluster = tone))
    first_line <- c(normalised = "# Note: this tab used the Normalised data, not the uploaded",
                    curated = "# Note: this tab used the Curated data, not the uploaded",
                    curated_norm = "# Note: this tab used the Curated + normalised data, not",
                    clustered = "# Note: this tab used the Clustered data, not the uploaded")
    says <- c(normalised = "shinytone::normalise_f0()",
              curated = "\"Curated CSV (analysis-ready)\" download",
              curated_norm = "\"Curated CSV (analysis-ready)\" download",
              clustered = "\"Download clustered CSV\" file")
    for (src in names(first_line)) {
      pick(src)
      for (tab in names(libs)) {
        code <- codes()[[tab]]
        info <- paste(tab, src)
        # The note comes first, before the lines that read the file
        expect_true(startsWith(code, paste0(libs[[tab]], first_line[[src]])), info = info)
        expect_match(code, says[[src]], fixed = TRUE, info = info)
        expect_lt(e3_pos(code, "# Note:"), e3_pos(code, "read_csv_utf8("))
        if (src != "normalised")
          expect_match(code, "leave out the Start-page steps below", fixed = TRUE, info = info)
      }
    }

    # Following the note reproduces the tab's fit on the Curated data: read
    # the Curate tab's download (written as that tab writes it) in place of
    # the uploaded file, without the Start-page steps
    pick("curated")
    session$setInputs(model_button = 1)
    app <- as.data.frame(s$cap$model_ui$model_result())
    expect_setequal(app$token, unique(cd$token))
    cf <- tempfile(fileext = ".csv")
    utils::write.csv(cd, cf, row.names = FALSE)
    code <- codes()$model
    mine <- paste0("library(dplyr)\n",
                   "dat <- shinytone::read_csv_utf8(", encodeString(cf, quote = '"'),
                   ", stringsAsFactors = FALSE)\n\n",
                   substring(code, e3_pos(code, "# Legendre polynomial basis")))
    got <- as.data.frame(e3_run(mine)$results)
    got <- got[match(app$token, got$token), ]
    expect_identical(got$tone, app$tone)
    expect_identical(app$tone[app$token == "S1_spill"], "T1")
    for (cn in c("c0", "c1", "c2")) expect_equal(got[[cn]], app[[cn]], tolerance = 1e-8)
  })
})

test_that("the Start-page lines say where to choose the token variable when none is chosen", {
  h <- start_server()$env
  st <- function(without) list(version = NA, screen = list(token = NULL, time = "time", f0 = "f0",
                                                           action = "none", without = without,
                                                           rows = 10L))
  code <- h$read_data_code("x.csv", st(3L))
  expect_true(any(code == paste("# Start page: 3 of 10 rows have no f0 in column f0; choose the token",
                                "variable under Data screening on the Start page to remove them")))
  code0 <- h$read_data_code("x.csv", st(0L))
  expect_true(any(code0 == paste("# Start page: every row has f0 in column f0 (10 rows); no token",
                                 "variable, so gaps were not checked")))
  expect_false(any(grepl("token column", c(code, code0), fixed = TRUE)))
})

test_that("the Model, GCA and GAMM tabs show no em dash", {
  app <- system.file("app", package = "shinytone")
  for (f in c("model_ui.R", "gca_ui.R", "gamm_ui.R")) {
    pd  <- utils::getParseData(parse(file.path(app, "ui", f), keep.source = TRUE,
                                     encoding = "UTF-8"))
    txt <- pd$text[pd$token == "STR_CONST"]
    expect_false(any(grepl("\u2014|&mdash;", txt)), info = f)
  }
})
