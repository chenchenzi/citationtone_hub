# tones.R: the order tone labels are shown in (plot legends, facets, tables).
#
# Chinese tone names sorted by code point come out as shangsheng, qusheng,
# yangping, yinping, the reverse of their usual order, so the traditional
# category names are put in the traditional order. R code stays ASCII:
# Chinese characters are written as \u escapes.

#' Sort tone labels for display
#'
#' @description
#' Returns the distinct tone labels in `x` in the order to show them, for
#' example in a plot legend, facets or a table. The traditional Chinese tone
#' category names come first, in the traditional order: ping, shang, qu, ru
#' (level, rising, departing, entering), with yin before yang within each.
#' So Mandarin's yinping, yangping, shangsheng and qusheng, written in Chinese
#' characters, keep that order instead of the reverse order that sorting by
#' code point gives. Other labels, such as `T1`, `tone2` or `55`, follow in
#' natural order.
#'
#' @details
#' A Chinese tone name is a category character (ping, shang, qu or ru),
#' optionally preceded by yin or yang and followed by sheng or diao, in
#' simplified or traditional characters (the yinping of simplified and of
#' traditional characters both count). The entering tone may also be
#' zhong, shangyin or xiayin (as in Cantonese), placed between yin and yang.
#' Within a category, a name without yin or yang comes first. Space around a
#' label is ignored when matching. Anything else, such as the neutral tone
#' qingsheng or tone numbers written in Chinese, sorts with the other labels.
#'
#' Natural order compares runs of digits as numbers, ignoring case, so
#' `T2` comes before `T10`. With `natural = FALSE` the other labels are in
#' the order [factor()] gives them: alphabetical in the current locale, or
#' numeric for a numeric `x`.
#'
#' @param x A vector of tone labels. A factor's levels are returned as they
#'   are, so a factor sets its own order.
#' @param natural If `TRUE` (default), labels that are not Chinese tone names
#'   are in natural order; if `FALSE`, in the order [factor()] gives them.
#'
#' @return A character vector of the distinct non-missing labels of `x`, or,
#'   for a factor, its levels.
#'
#' @examples
#' sort_tones(c("T10", "T2", "T1"))
#'
#' # Mandarin tone names: yinping, yangping, shangsheng, qusheng
#' x <- c("\u4e0a\u58f0", "\u53bb\u58f0", "\u9633\u5e73", "\u9634\u5e73")
#' sort(x)          # locale order, often by code point
#' sort_tones(x)
#'
#' # Use the result as factor levels, e.g. for a ggplot2 legend
#' factor(x, levels = sort_tones(x))
#'
#' @export
sort_tones <- function(x, natural = TRUE) {
  if (is.factor(x)) return(levels(x))
  x <- x[!is.na(x)]
  u <- if (isTRUE(natural)) {
    u <- unique(as.character(x))
    key <- tolower(u)
    m <- gregexpr("[0-9]+", key)
    regmatches(key, m) <- lapply(regmatches(key, m), function(d)
      formatC(as.numeric(d), width = 12, format = "d", flag = "0"))
    u[order(key)]
  } else {
    levels(factor(x))
  }
  if (!length(u)) return(character(0))
  u[order(zh_tone_rank(u), seq_along(u), na.last = TRUE)]
}

# The traditional Chinese tone category names, each with its rank: 10 per
# category (ping, shang, qu, ru), plus 0 without yin or yang, 1 for yin,
# 2 for the middle entering tone and 3 for yang. Built when the package is.
zh_tone_table <- local({
  yin  <- c("\u9634", "\u9670")                  # simplified, traditional
  yang <- c("\u9633", "\u967d")
  reg  <- list(list(p = "", r = 0), list(p = yin, r = 1), list(p = yang, r = 3))
  ru_mid <- list(p = c("\u4e2d",                  # zhong (middle) ru
                       paste0("\u4e0a", yin),     # shangyin (upper yin) ru
                       paste0("\u4e0b", yin)),    # xiayin (lower yin) ru
                 r = 2)
  cats   <- c("\u5e73", "\u4e0a", "\u53bb", "\u5165")        # ping shang qu ru
  suffix <- c("", "\u58f0", "\u8072", "\u8c03", "\u8abf")   # sheng, diao
  nm <- character(0); rk <- numeric(0)
  for (k in seq_along(cats)) {
    for (g in if (k == 4) c(reg, list(ru_mid)) else reg) {
      add <- as.vector(outer(paste0(g$p, cats[k]), suffix, paste0))
      nm <- c(nm, add); rk <- c(rk, rep(10 * k + g$r, length(add)))
    }
  }
  list(name = enc2utf8(nm), rank = rk)
})

# Rank of each label in the traditional Chinese tone order (lower first), or
# NA when it is not a traditional tone name.
zh_tone_rank <- function(x) {
  zh_tone_table$rank[match(trimws(enc2utf8(as.character(x))), zh_tone_table$name)]
}

# A factor of tone labels in display order (sort_tones() with factor()'s
# order for other labels), for model fits. A factor is returned unchanged.
tone_factor <- function(x) {
  if (is.factor(x)) x else factor(x, levels = sort_tones(x, natural = FALSE))
}
