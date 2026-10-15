library(meta3level)

file <- system.file(
  "extdata", "example_correlations.csv",
  package = "meta3level"
)
raw <- m3read(file)
dat <- m3prep(
  raw, measure = "r",
  study = "studyID", effect = "effectID",
  value = "r", n = "n"
)

result <- m3run(
  dat,
  cont = "age",
  groups = list(
    list(var = "design", ref = "cross-sectional", name = "Study design")
  ),
  level = "single",
  rho = 0.60,
  bias = FALSE,
  leave = TRUE,
  show = TRUE
)

result$main$aggregation
result$main$overall
result$main$i2
result$main$prediction

