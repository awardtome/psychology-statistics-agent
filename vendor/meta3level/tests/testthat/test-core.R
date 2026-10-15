test_that("correlations are converted to Fisher z and back to r", {
  dat <- make_example()
  expect_equal(dat$yi[1], atanh(-0.12), tolerance = 1e-10)
  expect_equal(dat$vi[1], 1 / 77, tolerance = 1e-10)
  model <- m3fit(dat)
  out <- m3effect(model)
  expect_equal(out$estimate_reported, tanh(out$estimate_analysis), tolerance = 1e-10)
})

test_that("independent-group d is corrected to Hedges g", {
  raw <- data.frame(study = 1:4, effect = 1:4,
                    d = c(0.2, 0.4, 0.5, 0.3), n1 = 20:23, n2 = 22:25)
  dat <- m3prep(raw, "d", study = "study", effect = "effect",
                value = "d", n1 = "n1", n2 = "n2")
  expect_true(all(dat$yi < raw$d))
  expect_true(all(dat$vi > 0))
  reference <- metafor::escalc(
    measure = "SMD",
    m1i = raw$d,
    m2i = rep(0, nrow(raw)),
    sd1i = rep(1, nrow(raw)),
    sd2i = rep(1, nrow(raw)),
    n1i = raw$n1,
    n2i = raw$n2,
    vtype = "LS"
  )
  expect_equal(dat$yi, as.numeric(reference$yi), tolerance = 1e-10)
  expect_equal(dat$vi, as.numeric(reference$vi), tolerance = 1e-10)
})

test_that("precomputed g with known variance does not require group sizes", {
  raw <- data.frame(study = 1:4, effect = 1:4,
                    g = c(0.2, 0.4, 0.5, 0.3), vi = rep(0.05, 4))
  dat <- m3prep(raw, "g", study = "study", effect = "effect",
                value = "g", vi = "vi")
  expect_equal(dat$yi, raw$g)
  expect_equal(dat$vi, raw$vi)
})

test_that("effect identifiers can be generated when the source has no effect column", {
  raw <- data.frame(
    study = rep(1:4, each = 2),
    g = seq(.1, .8, length.out = 8),
    vi = rep(.05, 8)
  )
  dat <- m3prep(raw, "g", study = "study", value = "g", vi = "vi")
  expect_identical(as.character(dat$effectID), paste0("effect_", 1:8))
  expect_equal(length(unique(dat$effectID)), nrow(dat))

  script <- tempfile(fileext = ".R")
  m3code(dat, print = FALSE, file = script)
  env <- new.env(parent = globalenv())
  env$raw <- raw
  expect_silent(capture.output(sys.source(script, envir = env)))
  expect_identical(as.character(env$dat$effectID), paste0("effect_", 1:8))
})

test_that("main model returns two variance components and I2", {
  dat <- make_example()
  model <- m3fit(dat)
  expect_length(model$sigma2, 2)
  i2 <- m3i2(model)
  expect_equal(nrow(i2), 3)
  expect_true(all(is.finite(i2$I2_percent)))
  expect_true(is.data.frame(m3lrt(model)))
})

test_that("two-by-two OR input matches metafor and reports OR", {
  raw <- data.frame(
    study = rep(1:4, each = 2), effect = 1:8,
    a = c(8, 10, 12, 9, 15, 7, 11, 13),
    b = c(22, 20, 28, 31, 25, 23, 29, 27),
    c = c(5, 7, 8, 6, 10, 4, 7, 9),
    d = c(25, 23, 32, 34, 30, 26, 33, 31)
  )
  dat <- m3prep(
    raw, "or", study = "study", effect = "effect",
    cellA = "a", cellB = "b", cellC = "c", cellD = "d"
  )
  reference <- metafor::escalc(
    measure = "OR", ai = raw$a, bi = raw$b, ci = raw$c, di = raw$d,
    add = 0
  )
  expect_equal(dat$yi, as.numeric(reference$yi), tolerance = 1e-12)
  expect_equal(dat$vi, as.numeric(reference$vi), tolerance = 1e-12)
  expect_identical(attr(dat, "meta3_scale"), "logor")

  model <- suppressWarnings(m3fit(dat))
  out <- m3effect(model)
  expect_equal(out$estimate_reported, exp(out$estimate_analysis), tolerance = 1e-12)
  expect_gt(out$estimate_reported, 0)
})

test_that("OR, log-OR SE, and OR confidence limits produce valid variances", {
  raw <- data.frame(
    study = rep(1:4, each = 2), effect = 1:8,
    odds = seq(1.1, 1.8, length.out = 8),
    logse = seq(.10, .17, length.out = 8)
  )
  zcrit <- stats::qnorm(.975)
  raw$lower <- exp(log(raw$odds) - zcrit * raw$logse)
  raw$upper <- exp(log(raw$odds) + zcrit * raw$logse)
  from_se <- m3prep(
    raw, "or", study = "study", effect = "effect", value = "odds",
    se = "logse"
  )
  from_ci <- m3prep(
    raw, "or", study = "study", effect = "effect", value = "odds",
    lower = "lower", upper = "upper"
  )
  expect_equal(from_se$yi, log(raw$odds), tolerance = 1e-12)
  expect_equal(from_se$vi, raw$logse^2, tolerance = 1e-12)
  expect_equal(from_ci$vi, raw$logse^2, tolerance = 1e-12)
})

test_that("OR zero cells are corrected only in affected tables", {
  raw <- data.frame(
    study = rep(1:2, each = 2), effect = 1:4,
    a = c(0, 5, 3, 4), b = c(20, 15, 17, 16),
    c = c(2, 3, 2, 5), d = c(18, 17, 18, 15)
  )
  dat <- m3prep(
    raw, "or", study = "study", effect = "effect",
    cellA = "a", cellB = "b", cellC = "c", cellD = "d"
  )
  expect_identical(dat$or_zero_corrected, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(dat$yi[1], log((.5 * 18.5) / (20.5 * 2.5)), tolerance = 1e-12)
  expect_error(
    m3prep(raw, "or", study = "study", effect = "effect",
           cellA = "a", cellB = "b", cellC = "c", cellD = "d",
           correction = 0),
    "zero cells"
  )
})
