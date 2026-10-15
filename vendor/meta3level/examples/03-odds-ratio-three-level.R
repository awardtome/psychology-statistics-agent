library(meta3level)

raw <- data.frame(
  study = rep(seq_len(12), each = 2),
  effect = rep(1:2, 12),
  treatmentEvents = rep(seq(8, 19), each = 2) + rep(0:1, 12),
  treatmentNonEvents = rep(seq(32, 43), each = 2),
  controlEvents = rep(seq(10, 21), each = 2),
  controlNonEvents = rep(seq(30, 41), each = 2) + rep(1:0, 12)
)

dat <- m3prep(
  raw,
  measure = "or",
  study = "study",
  effect = "effect",
  cellA = "treatmentEvents",
  cellB = "treatmentNonEvents",
  cellC = "controlEvents",
  cellD = "controlNonEvents"
)

model <- m3fit(dat)
summary(model)
m3effect(model)
m3i2(model)
m3lrt(model)

