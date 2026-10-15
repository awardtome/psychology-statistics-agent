library(meta3level)

raw <- data.frame(
  study = rep(seq_len(12), each = 2),
  effect = rep(1:2, 12),
  d = seq(0.15, 0.61, length.out = 24),
  nTreatment = rep(seq(30, 52, by = 2), each = 2),
  nControl = rep(seq(32, 54, by = 2), each = 2),
  year = rep(2012:2023, each = 2),
  training = rep(c("math", "executive"), 12)
)

dat <- m3prep(
  raw,
  measure = "d",
  design = "independent",
  study = "study",
  effect = "effect",
  value = "d",
  n1 = "nTreatment",
  n2 = "nControl"
)

result <- m3run(
  dat,
  cont = "year",
  groups = list(
    list(var = "training", ref = "math", name = "Training type")
  ),
  bias = FALSE,
  leave = FALSE,
  show = TRUE
)

