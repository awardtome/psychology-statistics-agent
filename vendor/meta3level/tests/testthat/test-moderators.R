test_that("continuous moderator is centered and uses F inference", {
  dat <- make_example()
  result <- m3cont(dat, "age")
  expect_equal(mean(result$data$moderator_c), 0, tolerance = 1e-12)
  expect_identical(result$model$test, "t")
  expect_true(all(c("F", "df1", "df2", "p") %in% names(result$f_test)))
})

test_that("categorical moderator returns contrasts and group effects", {
  dat <- make_example()
  result <- m3group(dat, "design", "cross-sectional")
  expect_true(any(result$group_effects$term == "moderator_factorcross-sectional"))
  expect_true(any(result$group_effects$term == "moderator_factorlongitudinal"))
  expect_equal(result$reference, "cross-sectional")
  expect_equal(sum(result$counts_effects$effects), nrow(result$data))
  expect_equal(sum(result$counts_studies$studies), length(unique(result$data$studyID)))
})

test_that("single-level category counts distinguish original effects and studies", {
  dat <- make_example()
  result <- suppressWarnings(m3group(
    dat, "design", "cross-sectional", level = "single"
  ))
  expect_equal(sum(result$counts_effects$effects), nrow(dat))
  expect_equal(sum(result$counts_studies$studies), length(unique(dat$studyID)))
  expect_true(all(result$counts_effects$effects >= result$counts_studies$studies))
})

test_that("spline candidates use identical data and ML", {
  dat <- make_example()
  result <- m3spline(dat, "age", df = 1:3)
  expect_true(all(vapply(result$models, function(x) x$method == "ML", logical(1))))
  expect_true(length(unique(vapply(result$models, function(x) x$k, numeric(1)))) == 1)
  expect_true(all(c("AIC", "AICc", "BIC") %in% names(result$comparison)))
  expect_true("delta" %in% names(result$comparison))
  expect_true(result$best_model %in% result$comparison$model)
})
