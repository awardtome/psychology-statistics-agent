test_that("leave-one-effect-out and leave-one-study-out have correct row counts", {
  dat <- make_example()
  effect <- m3leave(dat, "effect")
  study <- m3leave(dat, "study")
  expect_equal(nrow(effect$results), nrow(dat))
  expect_equal(nrow(study$results), length(unique(dat$studyID)))
  expect_equal(effect$results$remaining_effects,
               rep(nrow(dat) - 1, nrow(effect$results)))
  expect_equal(study$results$remaining_effects + study$results$omitted_effects,
               rep(nrow(dat), nrow(study$results)))
})

test_that("study aggregation returns one row per study", {
  dat <- make_example()
  agg <- m3study(dat, rho = 0.6)
  expect_equal(nrow(agg), length(unique(dat$studyID)))
  expect_true(all(agg$vi > 0))
})
