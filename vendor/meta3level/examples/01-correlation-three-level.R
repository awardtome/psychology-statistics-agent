library(meta3level)

file <- system.file(
  "extdata", "example_correlations.csv",
  package = "meta3level"
)
raw <- m3read(file)

dat <- m3prep(
  raw,
  measure = "r",
  study = "studyID",
  effect = "effectID",
  value = "r",
  n = "n"
)

result <- m3run(
  dat,
  cont = c("age", "year"),
  groups = list(
    list(var = "design", ref = "cross-sectional", name = "Study design")
  ),
  spline = list(
    list(var = "age", df = 1:3, name = "Age")
  ),
  bias = FALSE,
  leave = TRUE,
  show = TRUE,
  keep = FALSE,
  code = FALSE
)

m3plot(result, "cont", name = "age")
m3plot(result, "spline", name = "age")
m3plot(result, "effect")
m3plot(result, "study")

