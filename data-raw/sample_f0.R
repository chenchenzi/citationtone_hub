# ----------------------------------------------------------------------------
# Build the bundled `sample_f0` dataset.
#
# This is a subset of citation-tone f0 recordings used as a demonstration
# dataset throughout the shinytone package: vignettes, examples in @examples
# blocks, and the "Try with our sample data" button in the Shiny app.
#
# To regenerate: in R, from the project root, run
#   source("data-raw/sample_f0.R")
# The resulting data/sample_f0.rda is committed to the repository so that
# `data(sample_f0)` works after `library(shinytone)` without users needing
# any external file.
#
# Source: Xu, C. (2025). Plastic Mandarin tones: regional identity in
#   prosody. _Phonetica_, 82(5), 331-362. DOI: 10.1515/phon-2025-0001
# ----------------------------------------------------------------------------

# Read the bundled CSV — the same file the Shiny app uses for its
# "Try with our sample data" button.
sample_f0 <- utils::read.csv(
  "inst/app/www/dc21f0_test.csv",
  stringsAsFactors = FALSE,
  check.names      = FALSE,
  fileEncoding     = "UTF-8"
)

# Tidy: drop empty rows, ensure integer types where appropriate.
sample_f0 <- sample_f0[!is.na(sample_f0$token) & nzchar(sample_f0$token), ]
sample_f0$tone     <- as.integer(sample_f0$tone)
sample_f0$index    <- as.integer(sample_f0$index)
sample_f0$position <- as.integer(sample_f0$position)

cat(sprintf("sample_f0: %d rows, %d columns, %d tokens, %d speakers\n",
            nrow(sample_f0), ncol(sample_f0),
            length(unique(sample_f0$token)),
            length(unique(sample_f0$speaker))))

# Save as .rda for fast access via data(sample_f0).
# version = 3 = R >= 3.5 compression (smaller files).
usethis::use_data(sample_f0, overwrite = TRUE, compress = "xz", version = 3)
