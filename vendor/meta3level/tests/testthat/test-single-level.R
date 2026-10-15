test_that("single-level workflow pools effects before fitting", {
  dat <- make_example()
  result <- suppressWarnings(m3run(
    dat,
    cont = "age",
    groups = list(var = "design", ref = "cross-sectional"),
    spline = list(var = "age", df = 1:2),
    bias = FALSE,
    leave = TRUE,
    level = "single",
    show = FALSE
  ))

  studies <- length(unique(dat$studyID))
  expect_s3_class(result$main$model, "rma.uni")
  expect_equal(result$main$model$k, studies)
  expect_equal(nrow(result$main$aggregation), studies)
  expect_equal(sum(result$main$aggregation$pooled_effects), nrow(dat))
  expect_true(all(c("tau2", "I2_percent", "Q") %in% names(result$main$i2)))
  expect_true(all(c("pi_lb_reported", "pi_ub_reported") %in%
                    names(result$main$prediction)))
  expect_true(all(c("F", "df1", "df2", "p") %in%
                    names(result$cont[[1]]$f_test)))
  expect_equal(nrow(result$effectleave$results), nrow(dat))
  expect_equal(nrow(result$studyleave$results), studies)
})

test_that("single-level pooling preserves only consistent study moderators", {
  dat <- make_example()
  pooled <- m3study(dat, keep = c("age", "design"))
  expect_equal(nrow(pooled), length(unique(dat$studyID)))
  expect_true(all(c("age", "design", "effects") %in% names(pooled)))

  bad <- dat
  bad$age[2] <- bad$age[2] + 1
  expect_error(m3study(bad, keep = "age"), "inconsistent values")
})

test_that("single-level publication bias uses independent study estimates", {
  dat <- make_example()
  result <- suppressWarnings(m3bias(dat, level = "single", extra = FALSE))
  expect_s3_class(result$pet, "rma.uni")
  expect_s3_class(result$peese, "rma.uni")
  expect_equal(result$pet$k, length(unique(dat$studyID)))
  expect_true(all(c("F", "df1", "df2", "p") %in% names(result$pet_f)))
})

test_that("default three-level workflow remains unchanged", {
  dat <- make_example()
  model <- suppressWarnings(m3fit(dat))
  expect_s3_class(model, "rma.mv")
  expect_length(model$sigma2, 2)
})

test_that("single-level audit code exposes pooling and runs independently", {
  raw <- m3read(system.file("extdata", "example_correlations.csv",
                            package = "meta3level"))
  dat <- m3prep(raw, "r", study = "studyID", effect = "effectID",
                value = "r", n = "n")
  result <- suppressWarnings(m3run(
    dat, cont = "age", bias = FALSE, leave = FALSE,
    level = "single", show = FALSE
  ))
  code <- m3code(result, print = FALSE)
  expect_true(any(grepl("poolStudy <- function", code, fixed = TRUE)))
  expect_true(any(grepl("metafor::rma(\\(|,)", code)))
  expect_false(any(grepl("metafor::rma.mv", code, fixed = TRUE)))

  script <- tempfile(fileext = ".R")
  m3code(result, print = FALSE, file = script)
  env <- new.env(parent = globalenv())
  expect_silent(capture.output(sys.source(script, envir = env)))
  expect_equal(as.numeric(coef(env$main)[1]),
               as.numeric(coef(result$main$model)[1]), tolerance = 1e-10)
})

test_that("single-level workflow draws a study-level forest plot", {
  dat <- make_example()
  result <- suppressWarnings(m3run(
    dat, bias = FALSE, leave = FALSE, level = "single", show = FALSE
  ))
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(m3plot(result, "forest"))
})

test_that("single-level moderator pooling retains effects from partially missing rows", {
  dat <- make_example()
  target <- levels(dat$studyID)[1]
  target_rows <- which(dat$studyID == target)
  dat$age[target_rows[2]] <- NA

  result <- suppressWarnings(m3cont(dat, "age", level = "single"))
  pooled <- result$data[result$data$studyID == target, ]
  expected <- m3study(dat[dat$studyID == target, ])
  expect_equal(pooled$effects, length(target_rows))
  expect_equal(pooled$yi, expected$yi, tolerance = 1e-12)
  expect_equal(pooled$vi, expected$vi, tolerance = 1e-12)
})

test_that("pooling validates rho, keep columns, and GLS output", {
  dat <- make_example()
  expect_error(m3study(dat, rho = NA_real_), "single number")
  expect_error(m3study(dat, rho = 1), "single number")
  expect_error(m3study(dat, rho = -0.1), "single number")
  expect_error(m3study(dat, keep = "yi"), "reserved")
  expect_error(m3study(dat, keep = NA_character_), "valid column")

  for (rho in c(0, 0.5, 0.999)) {
    pooled <- m3study(dat, rho = rho)
    expect_true(all(is.finite(pooled$yi)))
    expect_true(all(is.finite(pooled$vi) & pooled$vi > 0))
  }
})

test_that("single-level analysis works when every study has one effect", {
  raw <- data.frame(
    study = 1:6, effect = 1:6,
    r = c(-.2, -.1, 0, .1, .2, .3), n = 40:45
  )
  dat <- m3prep(raw, "r", study = "study", effect = "effect",
                value = "r", n = "n")
  result <- suppressWarnings(m3run(
    dat, level = "single", bias = FALSE, leave = TRUE, show = FALSE
  ))
  expect_equal(result$main$model$k, 6)
  expect_true(all(result$main$aggregation$pooled_effects == 1))
  expect_equal(nrow(result$effectleave$results), 6)
  expect_equal(nrow(result$studyleave$results), 6)
})

test_that("leave-one-study-out fails early when reduced models are impossible", {
  raw <- data.frame(
    study = 1:3, effect = 1:3,
    g = c(.1, .2, .3), vi = c(.04, .05, .06)
  )
  dat <- m3prep(raw, "g", study = "study", effect = "effect",
                value = "g", vi = "vi")
  expect_error(m3leave(dat, "study", level = "single"), "at least 4")
})

test_that("single-level Egger validates pooled study-level standard errors", {
  raw <- data.frame(
    study = rep(1:6, each = 2), effect = 1:12,
    g = seq(.1, .6, length.out = 12), vi = rep(c(.04, .08), 6)
  )
  dat <- m3prep(raw, "g", study = "study", effect = "effect",
                value = "g", vi = "vi")
  expect_error(
    suppressWarnings(m3bias(dat, level = "single", extra = FALSE)),
    "pooled study-level"
  )
})

test_that("single-level reported scales are correct for g and OR", {
  raw_g <- data.frame(
    study = 1:6, effect = 1:6,
    g = seq(.1, .6, length.out = 6), vi = seq(.03, .08, length.out = 6)
  )
  dat_g <- m3prep(raw_g, "g", study = "study", effect = "effect",
                  value = "g", vi = "vi")
  model_g <- suppressWarnings(m3fit(dat_g, level = "single"))
  expect_equal(m3effect(model_g)$estimate_reported,
               m3effect(model_g)$estimate_analysis)

  raw_or <- transform(raw_g, odds = seq(.8, 1.8, length.out = 6), logse = .2)
  dat_or <- m3prep(raw_or, "or", study = "study", effect = "effect",
                   value = "odds", se = "logse")
  model_or <- suppressWarnings(m3fit(dat_or, level = "single"))
  effect_or <- m3effect(model_or)
  expect_equal(effect_or$estimate_reported,
               exp(effect_or$estimate_analysis), tolerance = 1e-12)
  expect_gt(effect_or$estimate_reported, 0)
})

test_that("audit code matches package numeric cleaning", {
  raw <- data.frame(
    study = 1:6, effect = 1:6,
    r = c("0.10", "0.20", "0.05", "-0.10", "0.15", "0.08"),
    n = c("1,000", "900", "800", "700", "600", "500")
  )
  dat <- m3prep(raw, "r", study = "study", effect = "effect",
                value = "r", n = "n")
  model <- suppressWarnings(m3fit(dat, level = "single"))
  script <- tempfile(fileext = ".R")
  m3code(model, print = FALSE, file = script)
  env <- new.env(parent = globalenv())
  env$raw <- raw
  expect_silent(capture.output(sys.source(script, envir = env)))
  expect_equal(as.numeric(coef(env$main)[1]), as.numeric(coef(model)[1]),
               tolerance = 1e-12)
})

test_that("non-finite moderators and invalid references fail clearly", {
  dat <- make_example()
  dat$bad <- dat$age
  dat$bad[1] <- Inf
  expect_error(m3cont(dat, "bad"), "non-finite")
  expect_error(m3spline(dat, "bad", df = 1:2), "non-finite")
  expect_error(m3group(dat, "design", NA_character_), "reference")
  expect_error(m3group(dat, "design", character()), "reference")
})

test_that("single-level audit code obeys the supplementary switch", {
  base <- list(bias = TRUE, rho = 0.60, direction = "auto")
  no_extra <- meta3level:::.audit_single_bias_code(
    c(base, list(extra = FALSE)), "g"
  )
  with_extra <- meta3level:::.audit_single_bias_code(
    c(base, list(extra = TRUE)), "g"
  )
  expect_false(any(grepl("trimfill|selmodel|puniform|zcurve", no_extra)))
  expect_true(any(grepl("trimfill", with_extra)))
  expect_true(any(grepl("selmodel", with_extra)))
  expect_true(any(grepl("puniform", with_extra)))
  expect_true(any(grepl("zcurve", with_extra)))
})

test_that("prepared independent data generate a runnable single-level audit", {
  raw <- data.frame(
    study = 1:5, effect = 1:5,
    g = seq(.1, .5, length.out = 5), vi = seq(.03, .07, length.out = 5)
  )
  dat <- m3prep(raw, "g", study = "study", effect = "effect",
                value = "g", vi = "vi")
  code <- m3code(dat, print = FALSE)
  expect_true(any(grepl("poolStudy <- function", code, fixed = TRUE)))
  expect_true(any(grepl("metafor::rma(\\(|,)", code)))
  expect_false(any(grepl("metafor::rma.mv", code, fixed = TRUE)))
})

test_that("percent moderators normalize only unambiguous mixed scales", {
  dat <- make_example()
  dat$female <- rep(c("50%", "0.50", "60%", "0.60"), length.out = nrow(dat))
  result <- suppressWarnings(m3cont(dat, "female", level = "single"))
  expect_equal(result$moderator_scale, "proportion")
  expect_true(result$normalized_mixed_scale)
  expect_true(result$center <= 1)

  dat$female <- rep(c("50%", "50", "60%", "60"), length.out = nrow(dat))
  result <- suppressWarnings(m3cont(dat, "female", level = "single"))
  expect_equal(result$moderator_scale, "percentage_points")
  expect_true(result$normalized_mixed_scale)
  expect_true(result$center > 1)

  dat$female <- rep(c("50%", "-0.20", "60%", "0.60"), length.out = nrow(dat))
  expect_error(m3cont(dat, "female"), "ambiguous scale")
  expect_error(m3spline(dat, "female", df = 1:2), "ambiguous scale")

  dat$female <- rep(c("50%", "0", "60%", "1"), length.out = nrow(dat))
  expect_error(m3cont(dat, "female"), "ambiguous scale")

  dat$female <- paste0(rep(seq(40, 75, length.out = 8), each = 2), "%")
  result <- suppressWarnings(m3cont(dat, "female", level = "single"))
  expect_true(result$percent_scale)
  expect_true(result$center > 1)
})

test_that("single-level sigma2 misuse fails clearly", {
  dat <- make_example()
  expect_error(
    m3fit(dat, level = "single", sigma2 = c(0, NA)),
    "only to level='three'"
  )
})

test_that("direct decimal-comma data are parsed without magnitude errors", {
  raw <- data.frame(
    study = 1:4, effect = 1:4,
    g = c("0,2", "-0,1", "0,4", "0,3"),
    vi = c("0,04", "0,05", "0,06", "0,07")
  )
  dat <- m3prep(raw, "g", study = "study", effect = "effect",
                value = "g", vi = "vi", decimal = ",")
  expect_equal(dat$yi, c(.2, -.1, .4, .3), tolerance = 1e-12)
  expect_equal(dat$vi, c(.04, .05, .06, .07), tolerance = 1e-12)
})

test_that("pre-pooled input is not pooled again and keeps aggregation history", {
  dat <- make_example()
  pooled <- suppressWarnings(m3study(dat, rho = 0.50, keep = c("age", "design")))
  model <- suppressWarnings(m3fit(pooled, level = "single", rho = 0.50))
  expect_true(model$meta3_preaggregated)
  expect_equal(model$k, length(unique(dat$studyID)))
  expect_equal(sum(model$meta3_aggregation$pooled_effects), nrow(dat))

  mismatched <- NULL
  expect_warning(
    mismatched <- m3fit(pooled, level = "single", rho = 0.60, warn = FALSE),
    "already pooled"
  )
  expect_equal(mismatched$meta3_rho, 0.50)

  pooled_again <- NULL
  expect_warning(
    pooled_again <- m3study(pooled, rho = 0.60, keep = c("age", "design")),
    "already pooled"
  )
  expect_equal(sum(attr(pooled_again, "meta3_aggregation")$pooled_effects),
               nrow(dat))
  expect_equal(attr(pooled_again, "meta3_rho"), 0.50)

  result <- suppressWarnings(m3run(
    pooled, level = "single", rho = 0.50,
    bias = FALSE, leave = FALSE, show = FALSE
  ))
  expect_equal(result$main$original_effects, nrow(dat))
  expect_equal(result$main$model$k, length(unique(dat$studyID)))
})

test_that("single-level models preserve factors and interaction moderators", {
  dat <- make_example()
  dat$design <- factor(ifelse(dat$design == "cross-sectional", "横向 研究", "纵向 研究"))
  dat$highage <- dat$age >= median(dat$age)
  interaction_model <- suppressWarnings(m3fit(
    dat, mods = ~ age * design,
    level = "single"
  ))
  logical_model <- suppressWarnings(m3fit(
    dat, mods = ~ highage,
    level = "single"
  ))
  expect_s3_class(interaction_model, "rma.uni")
  expect_true(any(grepl("age:design", names(coef(interaction_model)), fixed = TRUE)))
  expect_true(any(grepl("highage", names(coef(logical_model)), fixed = TRUE)))
})

test_that("moderator-model first coefficient is identified as an intercept", {
  dat <- make_example()
  model <- suppressWarnings(m3fit(dat, mods = ~ age, level = "single"))
  expect_warning(m3effect(model), "intercept")
  expect_error(meta3level:::single_level_prediction(model), "moderator values")
})
