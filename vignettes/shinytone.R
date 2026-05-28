## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 7,
  fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(shinytone)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
set.seed(1)

make_toy_corpus <- function(n_frames = 20) {
  spec <- expand.grid(
    speaker = c("S01", "S02"),
    tone    = c("T1", "T2", "T3"),
    rep     = 1:4,
    stringsAsFactors = FALSE
  )
  spec$token <- paste(spec$speaker, spec$tone, spec$rep, sep = "_")
  spec$item  <- paste0("w", spec$rep)

  do.call(rbind, lapply(seq_len(nrow(spec)), function(i) {
    r <- spec[i, ]
    t <- seq(0, 1, length.out = n_frames)
    # Speaker baseline: female speaker S01 a bit higher than S02
    base <- if (r$speaker == "S01") 220 else 130
    # Tone shape: T1 level high, T2 rising, T3 falling
    shape <- switch(r$tone,
      T1 = rep(0,  n_frames),
      T2 = 20 * t,
      T3 = -25 * t
    )
    data.frame(
      speaker = r$speaker,
      tone    = r$tone,
      item    = r$item,
      token   = r$token,
      time    = t,
      f0      = base + shape + rnorm(n_frames, sd = 3),
      stringsAsFactors = FALSE
    )
  }))
}

toy <- make_toy_corpus()
head(toy)

## -----------------------------------------------------------------------------
normed <- normalise_f0(toy,
                       f0          = "f0",
                       speaker     = "speaker",
                       tone        = "tone",
                       method      = "semitone",
                       mean_method = "weighted")
head(normed)

## -----------------------------------------------------------------------------
ggplot(normed, aes(time, f0_st, colour = tone, group = token)) +
  geom_line(alpha = 0.6) +
  facet_wrap(~ speaker) +
  labs(y = "f0 (semitones, relative to speaker mean)")

## -----------------------------------------------------------------------------
inspected <- inspect_f0(toy,
                        f0      = "f0",
                        token   = "token",
                        time    = "time",
                        speaker = "speaker",
                        tone    = "tone")
inspected |>
  distinct(token, .keep_all = TRUE) |>
  count(flagged_token)

## -----------------------------------------------------------------------------
poly_coefs <- fit_polynomial(normed,
                             f0      = "f0_st",
                             token   = "token",
                             time    = "time",
                             speaker = "speaker",
                             tone    = "tone",
                             degree  = 2)
head(poly_coefs)

## ----eval = FALSE-------------------------------------------------------------
# gca <- fit_gca(normed,
#                f0      = "f0_st",
#                time    = "time",
#                token   = "token",
#                tone    = "tone",
#                speaker = "speaker",
#                item    = "item",
#                degree  = 2,
#                random_slope_speaker = FALSE,
#                random_slope_item    = FALSE)
# 
# # Population-level per-tone curves
# preds <- predict_gca(gca, n = 60)
# ggplot(preds, aes(time, f0_predicted, colour = tone)) +
#   geom_line(linewidth = 1)

## -----------------------------------------------------------------------------
# Pre-aggregate the toy data into a per-tone mean contour
mean_contour <- compute_mean_contour(toy,
                                     token = "token", f0 = "f0",
                                     time  = "time",  tone = "tone")

chao <- contour_to_chao(mean_contour, raw_data = toy,
                        raw_token = "token", raw_f0 = "f0",
                        raw_tone  = "tone")
chao[, c("tone", "refline", "interval", "robust", "shape")]

