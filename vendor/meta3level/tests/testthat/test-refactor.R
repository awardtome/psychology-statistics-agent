test_that("CR2 estimates and tests agree with independent clubSandwich calls", {
  skip_if_not_installed("clubSandwich")
  dat <- make_example()
  base <- suppressWarnings(m3fit(dat, mods = ~ age))
  actual <- suppressWarnings(m3fit(dat, mods = ~ age, inference = "CR2"))
  reference <- metafor::robust(base, cluster = dat$studyID, clubSandwich = TRUE)
  expect_equal(actual$beta, base$beta, tolerance = 1e-12)
  expect_equal(actual$sigma2, base$sigma2, tolerance = 1e-12)
  expect_equal(actual$se, reference$se, tolerance = 1e-12)
  expect_equal(actual$ci.lb, reference$ci.lb, tolerance = 1e-12)
  expect_equal(actual$ci.ub, reference$ci.ub, tolerance = 1e-12)
  wald <- clubSandwich::Wald_test(base, constraints = clubSandwich::constrain_zero(2),
    vcov = "CR2", cluster = dat$studyID, test = "HTZ")
  expect_equal(m3ftest(actual)$F, wald$Fstat, tolerance = 1e-10)
  expect_equal(m3ftest(actual)$df2, wald$df_denom, tolerance = 1e-10)
  expect_equal(m3ftest(actual)$p, wald$p_val, tolerance = 1e-10)
  expect_equal(actual$meta3_model_coefficients, stats::coef(summary(base)))
})

test_that("formula omissions leave model metadata and CR2 clusters aligned", {
  skip_if_not_installed("clubSandwich")
  dat <- make_example()
  dat$age[1] <- NA_real_
  expect_warning(model <- m3fit(dat, mods = ~ age, warn = FALSE,
    inference = "CR2"), "omitted before fitting")
  ref <- metafor::rma.mv(yi, vi, mods = ~ age,
    random = ~ 1 | studyID/effectID, tdist = TRUE, data = dat[-1, ])
  ref <- metafor::robust(ref, cluster = dat$studyID[-1], clubSandwich = TRUE)
  expect_equal(nrow(model$meta3_data), model$k)
  expect_equal(as.character(model$meta3_data$effectID), as.character(dat$effectID[-1]))
  expect_equal(model$se, ref$se, tolerance = 1e-10)
  expect_true(all(is.finite(m3lrt(model)$LRT)))
  dat$age[2] <- 0
  expect_warning(transformed <- m3fit(dat, mods = ~ log(age), warn = FALSE),
                 "omitted before fitting")
  expect_equal(nrow(transformed$meta3_data), nrow(dat) - 2L)
})

test_that("literal moderator names are not interpreted as expressions", {
  dat <- make_example()
  column <- "平均 年龄 + 原始值"
  dat[[column]] <- dat$age
  actual <- suppressWarnings(m3fit(dat, mods = column))
  ref <- suppressWarnings(m3fit(dat, mods = ~ age))
  expect_equal(actual$beta, ref$beta, ignore_attr = TRUE, tolerance = 1e-10)
  expect_equal(all.vars(actual$meta3_mods), column)
  expect_error(m3fit(dat, mods = NA_character_), "non-empty")
})

test_that("audit code preserves advanced model arguments and CR2 inference", {
  skip_if_not_installed("clubSandwich")
  dat <- make_example()
  for (inference in c("model", "CR2")) {
    model <- suppressWarnings(m3fit(dat, mods = ~ age, inference = inference,
      dfs = "contain", level = "three", control = list(rel.tol = 1e-8)))
    script <- tempfile(fileext = ".R")
    m3code(model, print = FALSE, file = script)
    env <- new.env(parent = globalenv())
    expect_silent(capture.output(sys.source(script, envir = env)))
    expect_equal(env$main$beta, model$beta, tolerance = 1e-10)
    expect_equal(env$main$se, model$se, tolerance = 1e-10)
    expect_equal(env$main$QMdf, model$QMdf, tolerance = 1e-10)
  }
})

test_that("CR2 propagates through complete and single-level audit workflows", {
  skip_if_not_installed("clubSandwich")
  dat <- make_example()
  for (level in c("three", "single")) {
    result <- suppressWarnings(m3run(dat, cont = "age",
      groups = list(var = "design", ref = "cross-sectional"),
      spline = list(var = "age", df = 1:2), level = level,
      inference = "CR2", bias = FALSE, leave = TRUE, show = FALSE))
    expect_identical(result$cont[[1]]$model$meta3_inference, "CR2")
    expect_identical(result$groups[[1]]$intercept_model$meta3_inference, "CR2")
    expect_true(all(vapply(result$spline[[1]]$models, inherits, logical(1), "robust.rma")))
    expect_true(all(is.na(result$studyleave$results$error)))
    script <- tempfile(fileext = ".R")
    m3code(result, print = FALSE, file = script)
    env <- new.env(parent = globalenv())
    expect_silent(capture.output(suppressWarnings(sys.source(script, envir = env))))
    expect_equal(env$mcont$se, result$cont[[1]]$model$se, tolerance = 1e-8)
    expect_equal(env$mgroup$QMp, result$groups[[1]]$intercept_model$QMp, tolerance = 1e-8)
    expect_equal(env$mgroupNoIntercept$se, result$groups[[1]]$no_intercept_model$se, tolerance = 1e-8)
    expect_equal(env$splineModels$splineDf2$se, result$spline[[1]]$models$spline_df2$se, tolerance = 1e-8)
    for (i in seq_along(env$effectLeave)) {
      expect_equal(as.numeric(env$effectLeave[[i]]$se[1]), result$effectleave$results$se[i], tolerance = 1e-8)
    }
    for (i in seq_along(env$studyLeave)) {
      expect_equal(as.numeric(env$studyLeave[[i]]$se[1]), result$studyleave$results$se[i], tolerance = 1e-8)
    }
  }
})

test_that("PET-PEESE selection uses the chosen inference and audits it", {
  skip_if_not_installed("clubSandwich")
  dat <- make_example()
  for (level in c("three", "single")) {
    result <- suppressWarnings(m3bias(dat, extra = FALSE, inference = "CR2", level = level))
    expect_s3_class(result$pet, "robust.rma")
    expect_identical(result$decision,
      if (result$pet$pval[1] < .05) "PEESE" else "PET")
    script <- tempfile(fileext = ".R")
    m3code(result, print = FALSE, file = script)
    env <- new.env(parent = globalenv())
    expect_silent(capture.output(sys.source(script, envir = env)))
    expect_equal(env$pet$se, result$pet$se, tolerance = 1e-10)
    expect_equal(env$peese$pval, result$peese$pval, tolerance = 1e-10)
  }
})

test_that("F extraction rejects chi-square tests and conflicting overrides", {
  dat <- make_example()
  model <- suppressWarnings(m3fit(dat, mods = ~ age, f = FALSE))
  expect_error(m3ftest(model), "not an F statistic")
  expect_error(m3fit(dat, test = "z"), "Do not override")
  expect_error(m3fit(dat, subset = rep(TRUE, nrow(dat))), "Do not override")
  expect_error(m3fit(dat, f = FALSE, inference = "CR2"), "requires f = TRUE")
})

test_that("spline complexity must contain actual positive integers", {
  dat <- make_example()
  for (df in list(1.5, c(1, NA_real_), Inf, "2", 0, numeric())) {
    expect_error(m3spline(dat, "age", df = df), "positive integers")
  }
})

test_that("audit string literals escape control characters correctly", {
  text <- "age\n\t\"\\value"
  expect_identical(eval(parse(text = meta3level:::.dq(text))), text)
})

test_that("moderator plot intervals follow the selected CR2 inference", {
  skip_if_not_installed("clubSandwich")
  dat <- make_example()
  result <- suppressWarnings(m3cont(dat, "age", inference = "CR2"))
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  actual <- m3plot(result)
  base <- suppressWarnings(m3cont(dat, "age"))
  reference <- metafor::robust(base$model, cluster = base$data$studyID,
                              clubSandwich = TRUE)
  prediction <- metafor::predict.rma(reference,
    newmods = matrix(actual$x - base$center, ncol = 1))
  expect_equal(actual$ci_lb, tanh(as.numeric(prediction$ci.lb)), tolerance = 1e-10)
  expect_equal(actual$ci_ub, tanh(as.numeric(prediction$ci.ub)), tolerance = 1e-10)
})

test_that("audit leave-one-effect-out never merges distinct identifier pairs", {
  dat <- make_example()
  dat$studyID <- as.character(dat$studyID)
  dat$effectID <- as.character(dat$effectID)
  dat$studyID[1:2] <- "a.b"
  dat$studyID[3:4] <- "a"
  dat$effectID[1] <- "c"
  dat$effectID[3] <- "b.c"
  result <- suppressWarnings(m3leave(dat, by = "effect"))
  script <- tempfile(fileext = ".R")
  m3code(result, print = FALSE, file = script)
  env <- new.env(parent = globalenv())
  expect_silent(capture.output(sys.source(script, envir = env)))
  expect_length(env$effectLeave, nrow(dat))
  expect_true(all(vapply(env$effectLeave, function(x) x$k == nrow(dat) - 1L, logical(1))))
})

test_that("variance-component tests preserve other fixed components", {
  dat <- make_example()
  model <- suppressWarnings(m3fit(dat, sigma2 = c(.02, NA)))
  actual <- m3lrt(model)
  reference <- metafor::rma.mv(yi, vi, random = ~ 1 | studyID/effectID,
    sigma2 = c(.02, 0), method = "REML", tdist = TRUE, data = dat)
  statistic <- max(0, 2 * (as.numeric(logLik(model)) - as.numeric(logLik(reference))))
  expect_true(is.na(actual$LRT[1]))
  expect_match(actual$note[1], "fixed")
  expect_equal(actual$LRT[2], statistic, tolerance = 1e-10)
  script <- tempfile(fileext = ".R")
  m3code(model, print = FALSE, file = script)
  env <- new.env(parent = globalenv())
  expect_silent(capture.output(sys.source(script, envir = env)))
  expect_null(env$reducedL3)
  expect_equal(env$reducedL2$sigma2, reference$sigma2, tolerance = 1e-10)
})

test_that("a no-intercept slope is not reported as an overall effect", {
  dat <- make_example()
  model <- suppressWarnings(m3fit(dat, mods = ~ 0 + age))
  expect_error(m3effect(model), "requires an intercept")
})

test_that("audit inference generation never rewrites literal column names", {
  skip_if_not_installed("clubSandwich")
  dat <- make_example()
  column <- "age metafor::rma.mv( value"
  dat[[column]] <- dat$age
  result <- suppressWarnings(m3cont(dat, column, inference = "CR2"))
  script <- tempfile(fileext = ".R")
  m3code(result, print = FALSE, file = script)
  env <- new.env(parent = globalenv())
  expect_silent(capture.output(sys.source(script, envir = env)))
  expect_equal(env$mcont$se, result$model$se, tolerance = 1e-10)
})
