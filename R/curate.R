# =============================================================================
# Annotation curation: re-label tone-category variants and mark tokens for
# exclusion. This is distinct from f0 *correction* (R/inspect.R and the F0
# Correction tab), which repairs mis-tracked f0 values: here the f0 is taken to
# be correct, but the category label is revised. Typical uses are splitting a
# pre-labelled tone into colloquial / literary variants (文白异读), separating
# sandhi or sociolinguistic variants, or dropping mis-elicited tokens.
# =============================================================================


#' Apply tone re-labels and exclusions to a long-format f0 table
#'
#' @description
#' Adds annotation-curation columns to a long-format f0 data frame without
#' touching the original tone labels. Tokens named in `relabel` receive a new
#' value in `tone_relabelled`; tokens named in `exclude` are marked `TRUE` in
#' `excluded`. All other tokens keep their original tone in `tone_relabelled`
#' and `FALSE` in `excluded`. Curation operates at the token level: every row
#' belonging to a token receives that token's relabel / exclusion.
#'
#' @details
#' The original `tone` column is never modified, so a curation decision is
#' always reversible and auditable. Downstream analyses can then either group
#' by `tone_relabelled` (treating a variant as its own category, e.g.
#' `T4-colloquial` vs `T4-literary`) and/or drop rows where `excluded` is
#' `TRUE`.
#'
#' This is an annotation step, not a signal-repair step: it assumes the f0
#' values are already clean (see [inspect_f0()] and the F0 Correction tab for
#' repairing mis-tracked f0). Variant patterns are often discovered visually
#' or surfaced by the token-level register check of [flag_level_outliers()],
#' which flags tokens whose overall level is unusual for their speaker and
#' tone --- a natural seed for relabelling.
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param token Column name of token ID. Default `"token"`.
#' @param tone Column name of the (original) tone category. Default `"tone"`.
#' @param relabel A named character vector mapping token IDs (names) to their
#'   new tone label (values); or `NULL` for no relabelling. Tokens not named
#'   keep their original tone.
#' @param exclude A character vector of token IDs to mark as excluded; or an
#'   empty vector for none.
#' @param note A named character vector mapping token IDs (names) to a
#'   free-text reason for the curation decision (e.g. `"literary form"`,
#'   `"sandhi form"`); or `NULL` for none. Tokens not named get `NA`.
#'
#' @return `data` with three appended columns:
#' * `tone_relabelled`: character, the curated tone label (original tone where
#'   no relabel applies).
#' * `excluded`: logical, `TRUE` for tokens in `exclude`.
#' * `curate_note`: character, the reason recorded for a token (or `NA`).
#'
#' @seealso [flag_level_outliers()] for the register check that seeds variant
#'   discovery; [inspect_f0()] for the full diagnostic pass.
#'
#' @examples
#' data(sample_f0)
#' # Relabel two tokens to a literary-reading variant, exclude one mis-elicited
#' out <- apply_relabels(sample_f0,
#'                       token   = "token",
#'                       tone    = "tone",
#'                       relabel = c("t4_03" = "T4lit", "t4_07" = "T4lit"),
#'                       exclude = c("t5_11"))
#' table(out$tone, out$tone_relabelled, useNA = "ifany")
#'
#' @export
#' @importFrom rlang .data
apply_relabels <- function(data,
                           token   = "token",
                           tone    = "tone",
                           relabel = NULL,
                           exclude = character(0),
                           note    = NULL) {
  required <- c(token, tone)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  tok <- as.character(data[[token]])

  # Relabel: start from the original tone, override named tokens.
  tone_relabelled <- as.character(data[[tone]])
  if (!is.null(relabel) && length(relabel) > 0) {
    if (is.null(names(relabel))) {
      stop("`relabel` must be a named vector (names = token IDs, ",
           "values = new tone labels).", call. = FALSE)
    }
    m <- match(tok, names(relabel))
    hit <- !is.na(m)
    tone_relabelled[hit] <- as.character(relabel)[m[hit]]
  }

  # Free-text reason per token (NA where none given).
  curate_note <- rep(NA_character_, nrow(data))
  if (!is.null(note) && length(note) > 0) {
    if (is.null(names(note))) {
      stop("`note` must be a named vector (names = token IDs, ",
           "values = reasons).", call. = FALSE)
    }
    mn <- match(tok, names(note))
    hitn <- !is.na(mn)
    curate_note[hitn] <- as.character(note)[mn[hitn]]
  }

  data$tone_relabelled <- tone_relabelled
  data$excluded        <- tok %in% as.character(exclude)
  data$curate_note     <- curate_note
  data
}
