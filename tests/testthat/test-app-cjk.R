# The app's CJK font helpers for static plots (inst/app/global.R).

cjk_helpers <- function() {
  global <- system.file("app", "global.R", package = "shinytone")
  skip_if(!nzchar(global), "app not found")
  keep <- c(".cjk_probe", ".cjk_candidates", ".text_values", "has_cjk", ".find_cjk_family",
            "cjk_family", "cjk_font_notice", "label_family", ".plot_text", "cjk_family_theme")
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                   as.character(e[[2]]) %in% keep,
                 as.list(parse(global, keep.source = FALSE, encoding = "UTF-8")))
  expect_length(defs, length(keep))
  env <- new.env(parent = globalenv())
  env$.cjk_font_dir <- tempfile("no-cjk-font")
  for (e in defs) eval(e, env)
  env
}
set_found_family <- function(env, fam) assign("cache", fam, environment(env$cjk_family))

test_that("has_cjk() finds CJK characters by code point", {
  h <- cjk_helpers()
  expect_false(h$has_cjk(c("T1", "T2", NA, "")))
  expect_false(h$has_cjk("tɕʰ ˥˩"))          # IPA and tone letters
  expect_false(h$has_cjk(list(NULL, 1:3, character(0))))
  expect_true(h$has_cjk("阴平"))                         # simplified
  expect_true(h$has_cjk("陰平"))                         # traditional
  expect_true(h$has_cjk(factor(c("T1", "张三"))))        # levels, not codes
  expect_true(h$has_cjk(list("a", list(factor("声调")))))
})

test_that("label_family() switches only for CJK text and never errors outside a session", {
  h <- cjk_helpers()
  set_found_family(h, "Test CJK")
  expect_identical(h$label_family("T1", "S1"), "")
  expect_identical(h$label_family("T1", factor("阳平")), "Test CJK")
  set_found_family(h, "")                                        # no font found
  expect_identical(h$label_family("阳平"), "")
  expect_false(h$cjk_font_notice())
})

test_that("cjk_family_theme() leaves plots without CJK text unchanged", {
  skip_if_not_installed("ggplot2")
  h <- cjk_helpers()
  set_found_family(h, "Test CJK")
  d <- data.frame(t = rep(1:2, 4), f = 1:8, tone = rep(c("T1", "T2", "T3", "T4"), each = 2),
                  tok = "张三_1")                        # CJK only in an unshown column
  p <- ggplot2::ggplot(d, ggplot2::aes(t, f, colour = tone)) +
    ggplot2::geom_line(ggplot2::aes(group = tok))
  expect_null(h$cjk_family_theme(p))
  expect_identical(p + h$cjk_family_theme(p), p)
})

test_that("cjk_family_theme() sets the family when labels, facets or text layers have CJK", {
  skip_if_not_installed("ggplot2")
  h <- cjk_helpers()
  set_found_family(h, "Test CJK")
  d <- data.frame(t = 1:4, f = 1:4, tone = c("T1", "T2", "T3", "T4"),
                  spk = c("张三", "S2", "S2", "S2"))
  base <- ggplot2::ggplot(d, ggplot2::aes(t, f)) + ggplot2::geom_point()
  v <- "spk"
  plots <- list(
    colour = ggplot2::ggplot(d, ggplot2::aes(t, f, colour = .data[[v]])) + ggplot2::geom_point(),
    facet  = base + ggplot2::facet_grid(get(v) ~ .),
    label  = base + ggplot2::labs(x = "音节"),
    text   = base + ggplot2::annotate("text", x = 1, y = 1, label = "音节 1"))
  for (nm in names(plots)) {
    th <- h$cjk_family_theme(plots[[nm]])
    expect_identical(th$text$family, "Test CJK", info = nm)
    if (utils::packageVersion("ggplot2") >= "4.0.0")
      expect_identical(th$geom$family, "Test CJK", info = nm)
  }
})

test_that("the font lookup skips files and families without the probe glyphs", {
  skip_if_not_installed("systemfonts")
  h <- cjk_helpers()
  dir <- tempfile("cjk-font"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  latin <- system.file("app", "open-sans", "regular.ttf", package = "shinytone")
  skip_if(!nzchar(latin), "Open Sans not found")
  file.copy(latin, file.path(dir, "Latin-Regular.ttf"))
  expect_identical(h$.find_cjk_family(dir = dir, candidates = "No Such Font"), "")
  expect_identical(h$.find_cjk_family(dir = dir, candidates = "Open Sans"), "")
})

test_that("a missing CJK font gives one notification per session", {
  skip_if_not_installed("ggplot2")
  h <- cjk_helpers()
  set_found_family(h, "")
  shiny::testServer(function(input, output, session) NULL, {
    expect_identical(h$label_family("阴平"), "")
    expect_true(isTRUE(session$userData$cjk_font_notice))
    expect_false(h$cjk_font_notice())                            # already shown
  })
})
