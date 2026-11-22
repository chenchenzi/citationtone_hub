# Tone order in the app's plots and R code: Chinese tone names in traditional
# order (sort_tones()), Latin labels as before. Chinese characters as \u
# escapes, so the tests read the same in any locale.

mandarin <- c("\u9634\u5e73", "\u9633\u5e73", "\u4e0a\u58f0", "\u53bb\u58f0")

# global.R's definitions and the given modules, with shiny's and ggplot2's
# exports in reach, as in the running app. No CJK font lookup.
app_env <- function(modules = character(0)) {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pkgs <- new.env(parent = globalenv())
  for (p in c("shiny", "ggplot2"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  assign("cache", "", environment(env$cjk_family))
  for (f in modules) sys.source(file.path(app, "ui", f), env, keep.source = FALSE)
  env
}

# A module function bound to its extra reactives, for shiny::testServer().
bind_module <- function(env, name, ...) {
  bound <- function(input, output, session) NULL
  body(bound) <- body(env[[name]])
  environment(bound) <- list2env(list(...), parent = env)
  bound
}

# Long-format f0: speakers x tones x items x reps, 10 frames per token.
tone_data <- function(tones, n_spk = 3, n_item = 3) {
  set.seed(2)
  g <- expand.grid(speaker = paste0("S", seq_len(n_spk)), tone = tones,
                   item = paste0("w", seq_len(n_item)), rep = 1:2, stringsAsFactors = FALSE)
  do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    t <- seq(0, 0.3, length.out = 10)
    data.frame(speaker = g$speaker[i], tone = g$tone[i], item = g$item[i],
               token = paste(g$speaker[i], match(g$tone[i], tones), g$item[i], g$rep[i], sep = "_"),
               time = t, f0 = match(g$tone[i], tones) * 25 + 100 + 60 * t + stats::rnorm(10, sd = 3),
               stringsAsFactors = FALSE)
  }))
}

colour_limits <- function(p) ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")$get_limits()

test_that("tone_factor_code() spells out the order only for Chinese tone names", {
  h <- app_env()
  expect_false(h$tone_needs_levels(c("T2", "T1", "T10")))
  expect_false(h$tone_needs_levels(NULL))
  expect_false(h$tone_needs_levels(factor(rev(mandarin))))
  expect_true(h$tone_needs_levels(rev(mandarin)))
  expect_identical(h$tone_factor_code("tone", c("T1", "T2")), "dat$tone <- as.factor(dat$tone)")
  expect_identical(h$tone_factor_code("tone", rev(mandarin)),
    "dat$tone <- factor(dat$tone, levels = shinytone::sort_tones(dat$tone, natural = FALSE))")
  expect_identical(h$tone_factor_code("tone", rev(mandarin), ordered = TRUE, to = "tone.ord"),
    "dat$tone.ord <- ordered(dat$tone, levels = shinytone::sort_tones(dat$tone, natural = FALSE))")
  expect_identical(h$tone_factor_code("tone", NULL, ordered = TRUE, to = "tone.ord"),
                   "dat$tone.ord <- as.ordered(dat$tone)")
})

test_that("Visualise orders the legend and facets of Chinese tone names", {
  env <- app_env("visualise_ui.R")
  d <- tone_data(rev(mandarin), n_spk = 2, n_item = 1)
  srv <- bind_module(env, "visualise_ui", dataset = shiny::reactiveVal(d),
                     normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL)
  shiny::testServer(srv, {
    session$setInputs(x_var = "time", y_var = "f0", tone_var = "tone", speaker_var = "speaker",
                      convert_tone_to_factor = TRUE, plot_facet = "tone", plot_button = 1)
    p <- current_plot()
    expect_identical(colour_limits(p), mandarin)
    expect_identical(as.character(ggplot2::ggplot_build(p)$layout$layout$tone), mandarin)
    session$setInputs(convert_tone_to_factor = FALSE, plot_facet = "speaker_tone")
    expect_identical(colour_limits(current_plot()), mandarin)
    # Show R code reproduces the order
    session$setInputs(show_code_button = 1)
    expect_match(paste(as.character(output$r_code_output$html), collapse = ""),
                 "factor(dat$tone, levels = shinytone::sort_tones(dat$tone, natural = FALSE))",
                 fixed = TRUE)
  })
})

test_that("Visualise keeps the old order for Latin labels", {
  env <- app_env("visualise_ui.R")
  tones <- c("T2", "T10", "T1")
  d <- tone_data(tones, n_spk = 2, n_item = 1)
  srv <- bind_module(env, "visualise_ui", dataset = shiny::reactiveVal(d),
                     normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL)
  shiny::testServer(srv, {
    session$setInputs(x_var = "time", y_var = "f0", tone_var = "tone", speaker_var = "speaker",
                      convert_tone_to_factor = TRUE, plot_facet = "none", plot_button = 1)
    expect_identical(colour_limits(current_plot()), levels(as.factor(d$tone)))
    session$setInputs(convert_tone_to_factor = FALSE)
    expect_true(is.character(current_plot()$data$tone))     # left as it was
    session$setInputs(show_code_button = 1)
    expect_no_match(paste(as.character(output$r_code_output$html), collapse = ""), "sort_tones")
  })
})

# Text labels of a grob tree, in drawing order.
grob_labels <- function(g) {
  if (inherits(g, "text")) return(as.character(g$label))
  kids <- c(if (!is.null(g$grobs)) g$grobs, if (!is.null(g$children)) as.list(g$children))
  unlist(lapply(kids, grob_labels))
}

test_that("the Summarise figure's legend lists Chinese tone names in order", {
  skip_if_not_installed("gridExtra")
  env <- app_env("summarise_plot.R")
  mc <- compute_mean_contour(tone_data(rev(mandarin), n_spk = 1, n_item = 1),
                             time_normalised = "no")
  ch <- contour_to_chao(mc)
  expect_identical(ch$tone, mandarin)                       # the table's row order
  rng <- range(mc$f0_predicted)
  mc$chao_ref <- (mc$f0_predicted - rng[1]) / diff(rng) * 4 + 1
  mc$chao_int <- (mc$f0_predicted - rng[1]) / diff(rng) * 5
  pts <- function(v) do.call(rbind, lapply(seq_len(nrow(ch)), function(i)
    data.frame(time = c(0, 1), chao_val = ch[[v]][[i]], chao_digit = "3", tone = ch$tone[i])))
  mc$tone <- sample(mc$tone)                                # order must not come from the rows
  pdf(NULL); on.exit(grDevices::dev.off())
  g <- env$plot_chao_panels(mc, pts("ref_continuous"), pts("int_continuous"), tol = 0.1)
  labs <- grob_labels(g)
  expect_identical(unname(labs[labs %in% mandarin]), mandarin)
})

test_that("GCA and GAMM prediction plots list Chinese tone names in order", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("emmeans")
  env <- app_env(c("gca_ui.R", "gamm_ui.R"))
  d <- tone_data(rev(mandarin))
  rv <- function(x = NULL) shiny::reactiveVal(x)
  gca <- bind_module(env, "gca_ui", dataset = rv(d), normalised_data = rv(), gca_pred_data = rv(),
                     curated_data = NULL, cluster_data = NULL, curated_norm_data = NULL)
  shiny::testServer(gca, {
    session$setInputs(gca_token_var = "token", gca_time_var = "time", gca_speaker_var = "speaker",
                      gca_tone_var = "tone", gca_f0_var = "f0", gca_item_var = "item",
                      gca_degree = "2", gca_ri_speaker = TRUE, gca_ri_item = FALSE,
                      gca_rs_speaker = FALSE, gca_rs_item = FALSE)
    suppressWarnings(suppressMessages(session$setInputs(gca_button = 1)))
    expect_identical(gca_tone_estimates()$Tone, mandarin)
    p <- gca_build_plot(gca_plot_data(), gca_obs_data(), "tone", "f0", overlay = TRUE)
    expect_identical(colour_limits(p), mandarin)
  })
  gamm <- bind_module(env, "gamm_ui", dataset = rv(d), normalised_data = rv(), gamm_pred_data = rv(),
                      curated_data = NULL, cluster_data = NULL, curated_norm_data = NULL)
  shiny::testServer(gamm, {
    session$setInputs(gamm_token_var = "token", gamm_time_var = "time", gamm_speaker_var = "speaker",
                      gamm_tone_var = "tone", gamm_f0_var = "f0", gamm_item_var = "item",
                      gamm_dur_var = "none", gamm_k = "4", gamm_bs = "tp",
                      gamm_smooth_type = "separate", gamm_rs_type = "none",
                      gamm_ri_speaker = TRUE, gamm_ri_item = FALSE, gamm_ar1 = FALSE)
    suppressWarnings(suppressMessages(session$setInputs(gamm_button = 1)))
    p <- gamm_prediction_plot(gamm_plot_data(), gamm_fit_obj())
    expect_identical(colour_limits(p), mandarin)
  })
})
