test_that("file reader rejects missing files and bad headers", {
  expect_error(m3read("this-file-does-not-exist.csv"), "does not exist")
  bad <- tempfile(fileext = ".csv")
  writeLines("studyID,,r\n1,1,.2", bad)
  expect_error(m3read(bad), "Could not read|blank|missing")
})

test_that("file reader repairs duplicate headers by original column position", {
  file <- tempfile(fileext = ".csv")
  writeLines(c("study,x,x,r,n", "1,label,1,0.2,40"), file, useBytes = TRUE)
  expect_warning(raw <- m3read(file), "repaired")
  expect_identical(names(raw), c("study", "x...2", "x...3", "r", "n"))
  map <- attr(raw, "meta3_source")$name_map
  expect_identical(map$original[2:3], c("x", "x"))
  expect_identical(map$repaired[2:3], c("x...2", "x...3"))
})

test_that("file reader detects common CSV separators and decimal commas", {
  comma <- tempfile(fileext = ".csv")
  semicolon <- tempfile(fileext = ".csv")
  tabbed <- tempfile(fileext = ".csv")
  writeLines(c("study,effect,r,n", "1,1,0.2,40"), comma, useBytes = TRUE)
  writeLines(c("study;effect;r;n", "1;1;0,2;40"), semicolon, useBytes = TRUE)
  writeLines(c("study\teffect\tr\tn", "1\t1\t0.2\t40"), tabbed, useBytes = TRUE)

  expect_equal(ncol(m3read(comma)), 4)
  semicolon_data <- m3read(semicolon, decimal = ",")
  expect_equal(ncol(semicolon_data), 4)
  expect_identical(semicolon_data$r, "0,2")
  expect_equal(ncol(m3read(tabbed)), 4)
  expect_error(m3read(comma, decimal = ";"), "decimal")

  prepared <- m3prep(
    semicolon_data, "r", study = "study", effect = "effect",
    value = "r", n = "n"
  )
  expect_equal(prepared$yi, atanh(0.2), tolerance = 1e-12)
  expect_equal(prepared$vi, 1 / 37, tolerance = 1e-12)
})

test_that("delimited readers preserve leading-zero identifiers and accept overrides", {
  file <- tempfile(fileext = ".csv")
  writeLines(c("study,effect,r,n", "001,01,0.2,40", "1,1,0.3,50"),
             file, useBytes = TRUE)
  raw <- m3read(file)
  expect_identical(raw$study, c("001", "1"))
  expect_identical(raw$effect, c("01", "1"))
  dat <- m3prep(raw, "r", study = "study", effect = "effect",
                value = "r", n = "n")
  expect_setequal(as.character(dat$studyID), c("001", "1"))

  numeric_raw <- m3read(file, colClasses = c("character", "character", "numeric", "numeric"))
  expect_true(is.numeric(numeric_raw$r))
  expect_error(
    do.call(m3read, c(
      list(file = file, sheet = 1, encoding = "auto", decimal = "."),
      list("unnamed argument")
    )),
    "must be named"
  )
})

test_that("blank IDs and invalid correlations are removed with warnings", {
  raw <- data.frame(
    study = c("1", "1", "2", "2", "", "3"),
    effect = c("1", "2", "1", "2", "1", "1"),
    r = c(.1, .2, -.1, -.2, .3, 1),
    n = c(30, 30, 40, 40, 50, 50)
  )
  expect_warning(
    dat <- m3prep(raw, "r", study = "study", effect = "effect",
                  value = "r", n = "n"),
    "outside|removed"
  )
  expect_equal(nrow(dat), 4)
  expect_false(any(is.na(dat$studyID)))
})

test_that("textual missing IDs are not treated as studies or effects", {
  raw <- data.frame(
    study = c("1", "1", "NA", "N/A", ".", "2", "2"),
    effect = c("1", "2", "1", "1", "1", "1", "NULL"),
    r = c(.1, .2, .3, .4, .5, .1, .2),
    n = rep(40, 7)
  )
  expect_warning(
    dat <- m3prep(raw, "r", study = "study", effect = "effect",
                  value = "r", n = "n"),
    "removed"
  )
  expect_equal(nrow(dat), 3)
  expect_setequal(as.character(dat$studyID), c("1", "2"))
})

test_that("duplicated IDs and duplicated headers stop early", {
  raw <- data.frame(study = c(1, 1, 2, 2), effect = c(1, 1, 1, 2),
                    r = c(.1, .2, .3, .4), n = 30)
  expect_error(
    m3prep(raw, "r", study = "study", effect = "effect",
           value = "r", n = "n"),
    "must be unique"
  )
  duplicate_names <- raw
  names(duplicate_names)[3:4] <- c("x", "x")
  expect_error(
    m3prep(duplicate_names, "r", study = "study", effect = "effect",
           value = "x", n = "x"),
    "duplicated column names"
  )
})

test_that("identifier pairs are collision-safe when IDs contain separators", {
  raw <- data.frame(
    study = c("A::B", "A", "C", "C"),
    effect = c("C", "B::C", "1", "2"),
    g = c(.1, .2, .3, .4), vi = rep(.05, 4)
  )
  dat <- m3prep(raw, "g", study = "study", effect = "effect",
                value = "g", vi = "vi")
  expect_equal(nrow(dat), 4)
  leave <- suppressWarnings(m3leave(dat, "effect", level = "single"))
  expect_equal(nrow(leave$results), 4)
})

test_that("one effect per study is rejected as non-three-level data", {
  raw <- data.frame(study = 1:5, effect = 1:5,
                    r = seq(.1, .5, length.out = 5), n = 50)
  dat <- m3prep(raw, "r", study = "study", effect = "effect",
                value = "r", n = "n")
  expect_error(m3fit(dat), "No study contributes multiple")
})

test_that("three-level fitting warns when only one study informs Level 2", {
  raw <- data.frame(
    study = c(1, 1, 2, 3, 4), effect = 1:5,
    g = seq(.1, .5, length.out = 5), vi = rep(.05, 5)
  )
  dat <- m3prep(raw, "g", study = "study", effect = "effect",
                value = "g", vi = "vi")
  expect_warning(suppressWarnings(m3fit(dat, warn = FALSE)), NA)
  expect_warning(m3fit(dat, warn = FALSE), "weakly identified")
})

test_that("invalid SMD sample sizes and core model overrides fail clearly", {
  raw <- data.frame(
    study = rep(1:3, each = 2), effect = 1:6,
    d = seq(.1, .6, length.out = 6),
    n1 = c(1, 20, 21, 22, 23, 24), n2 = rep(20, 6)
  )
  expect_warning(
    dat <- m3prep(raw, "d", study = "study", effect = "effect",
                  value = "d", n1 = "n1", n2 = "n2"),
    "n1 or n2"
  )
  expect_equal(nrow(dat), 5)

  model_dat <- make_example()
  expect_error(m3fit(model_dat, V = diag(nrow(model_dat))), "Do not override")
  expect_error(m3fit(model_dat, level = "single", test = "z"), "Do not override")
  expect_error(
    do.call(m3fit, c(
      list(data = model_dat, mods = NULL, method = "REML", f = TRUE,
           sigma2 = c(NA, NA), warn = TRUE, level = "three", rho = .60),
      list(1)
    )),
    "must be named"
  )
  expect_s3_class(suppressWarnings(m3fit(model_dat, control = list(rel.tol = 1e-8))),
                  "rma.mv")
})

test_that("SMD fallback variances and unusually large g values are disclosed", {
  raw <- data.frame(
    study = rep(1:3, each = 2), effect = 1:6,
    d = c(.2, .4, .3, .5, .6, .8),
    n1 = rep(30, 6), n2 = rep(32, 6),
    vi = c(.04, NA, .05, .06, .05, .04)
  )
  expect_warning(
    dat <- m3prep(raw, "d", study = "study", effect = "effect",
                  value = "d", n1 = "n1", n2 = "n2", vi = "vi"),
    "1 independent-group d sampling variance"
  )
  expect_equal(nrow(dat), 6)

  large <- data.frame(study = 1:4, effect = 1:4,
                      g = c(.2, 3.1, -.4, -3.2), vi = rep(.05, 4))
  expect_warning(
    large_dat <- m3prep(large, "g", study = "study", effect = "effect",
                        value = "g", vi = "vi"),
    "2 standardized mean difference"
  )
  expect_equal(nrow(large_dat), 4)
})

test_that("non-numeric continuous moderators fail clearly", {
  dat <- make_example()
  dat$age_bad <- as.character(dat$age)
  dat$age_bad[1] <- "eight years"
  expect_error(m3cont(dat, "age_bad"), "non-numeric")
  expect_error(m3spline(dat, "age_bad", df = 1:2), "non-numeric")
})

test_that("declared missing strings are omitted rather than treated as text errors", {
  dat <- make_example()
  dat$age_missing <- as.character(dat$age)
  dat$age_missing[c(1, 3)] <- c("NA", "N/A")
  result <- suppressWarnings(m3cont(dat, "age_missing"))
  expect_s3_class(result, "meta3_continuous")
  expect_equal(nrow(result$data), nrow(dat) - 2)
})

test_that("workflow records optional component errors and continues", {
  dat <- make_example()
  dat$age_bad <- "not numeric"
  result <- suppressWarnings(m3run(
    dat,
    cont = list(
      list(var = "age_bad", name = "Bad age"),
      list(var = "age", name = "Good age")
    ),
    bias = FALSE,
    leave = FALSE,
    show = FALSE,
    keep = TRUE
  ))
  expect_s3_class(result$cont[["Bad age"]], "meta3_error")
  expect_s3_class(result$cont[["Good age"]], "meta3_continuous")
})

test_that("publication bias guards too few studies and equal SEs", {
  dat <- make_example()
  two_studies <- droplevels(dat[dat$studyID %in% levels(dat$studyID)[1:2], ])
  expect_error(m3bias(two_studies, extra = FALSE), "at least three")

  equal_vi <- dat
  equal_vi$vi <- 0.02
  equal_vi$sei <- sqrt(equal_vi$vi)
  expect_error(m3bias(equal_vi, extra = FALSE), "identical")
})

test_that("precomputed one-group g and vi do not require n", {
  raw <- data.frame(study = rep(1:3, each = 2), effect = 1:6,
                    g = seq(.1, .6, length.out = 6), vi = rep(.04, 6))
  dat <- m3prep(raw, "g", design = "onegroup",
                study = "study", effect = "effect",
                value = "g", vi = "vi")
  expect_equal(dat$yi, raw$g)
  expect_equal(dat$vi, raw$vi)
})

test_that("partial missing one-group variances require an explicit fallback", {
  raw <- data.frame(
    study = rep(1:3, each = 2), effect = 1:6,
    d = seq(.1, .6, length.out = 6), n = rep(30, 6),
    vi = c(.03, .03, NA, .03, .03, .03)
  )
  expect_error(
    m3prep(raw, "d", design = "onegroup", study = "study",
           effect = "effect", value = "d", n = "n", vi = "vi"),
    "missing or invalid"
  )
  expect_warning(
    dat <- m3prep(
      raw, "d", design = "onegroup", study = "study", effect = "effect",
      value = "d", n = "n", vi = "vi", variance = "approximate"
    ),
    "approximate"
  )
  expect_equal(nrow(dat), 6)
})

test_that("OR input rejects ambiguous or incomplete uncertainty information", {
  raw <- data.frame(
    study = rep(1:2, each = 2), effect = 1:4,
    odds = c(1.2, 1.3, 1.4, 1.5), vi = .04, se = .2,
    lower = c(.8, .9, 1, 1.1), upper = c(1.8, 1.9, 2, 2.1),
    a = 4, b = 16, c = 2, d = 18
  )
  expect_error(
    m3prep(raw, "or", study = "study", effect = "effect", value = "odds"),
    "exactly one uncertainty"
  )
  expect_error(
    m3prep(raw, "or", study = "study", effect = "effect", value = "odds",
           vi = "vi", se = "se"),
    "exactly one uncertainty"
  )
  expect_error(
    m3prep(raw, "or", study = "study", effect = "effect", value = "odds",
           lower = "lower"),
    "exactly one uncertainty|both"
  )
  expect_error(
    m3prep(raw, "or", study = "study", effect = "effect", value = "odds",
           vi = "vi", cellA = "a", cellB = "b", cellC = "c", cellD = "d"),
    "not both"
  )

  raw$se[1] <- -0.2
  expect_warning(
    cleaned <- m3prep(raw, "or", study = "study", effect = "effect",
                      value = "odds", se = "se"),
    "removed"
  )
  expect_equal(nrow(cleaned), 3)
})

test_that("OR tables reject non-counts and remove double-zero studies", {
  raw <- data.frame(
    study = rep(1:3, each = 2), effect = 1:6,
    a = c(0, 3, 2.5, 4, 5, 6), b = c(20, 17, 18, 16, 15, 14),
    c = c(0, 2, 2, 3, 4, 5), d = c(20, 18, 18, 17, 16, 15)
  )
  expect_warning(
    dat <- m3prep(raw, "or", study = "study", effect = "effect",
                  cellA = "a", cellB = "b", cellC = "c", cellD = "d"),
    "invalid two-by-two|removed"
  )
  expect_equal(nrow(dat), 4)
  expect_false(any(as.character(dat$effectID) %in% c("1", "3")))
})
