test_that("workflow maps replaceable specifications to public functions", {
  dat <- make_example()
  result <- m3run(
    dat,
    cont = list(list(var = "age", name = "Age")),
    groups = list(var = "design", ref = "cross-sectional",
                  name = "Design"),
    spline = list(var = "age", df = 1:2, name = "Age"),
    bias = FALSE,
    leave = FALSE,
    show = FALSE
  )
  expect_s3_class(result, "meta3_workflow")
  expect_named(result$cont, "Age")
  expect_named(result$groups, "Design")
  expect_named(result$spline, "Age")
  expect_named(
    result,
    c("main", "cont", "groups", "spline", "bias", "effectleave", "studyleave")
  )
})

test_that("workflow result names must be unique", {
  dat <- make_example()
  expect_error(
    m3run(dat, cont = NA_character_, bias = FALSE, leave = FALSE, show = FALSE),
    "non-empty 'var'"
  )
  expect_error(
    m3run(dat, groups = list(ref = "A"), bias = FALSE, leave = FALSE, show = FALSE),
    "non-empty 'var'"
  )
  expect_error(
    m3run(dat, groups = list(var = "design", ref = NA_character_),
          bias = FALSE, leave = FALSE, show = FALSE),
    "non-empty 'ref'"
  )
  expect_error(
    m3run(dat, spline = list(var = "age", linear = NA),
          bias = FALSE, leave = FALSE, show = FALSE),
    "TRUE or FALSE"
  )
  expect_error(
    m3run(dat, cont = c("age", "age"), bias = FALSE, leave = FALSE, show = FALSE),
    "Duplicate cont result name"
  )
  expect_error(
    m3run(
      dat,
      cont = list(
        list(var = "age", name = "same"),
        list(var = "age", name = "same")
      ),
      bias = FALSE, leave = FALSE, show = FALSE
    ),
    "Duplicate cont result name"
  )
  expect_error(
    m3run(
      dat,
      groups = list(
        list(var = "design", ref = "cross-sectional", name = "same"),
        list(var = "design", ref = "cross-sectional", name = "same")
      ),
      bias = FALSE, leave = FALSE, show = FALSE
    ),
    "Duplicate group result name"
  )
})

test_that("the public API is short, unique, and underscore-free", {
  api <- getNamespaceExports("meta3level")
  expect_true(all(grepl("^m3", api)))
  expect_false(any(grepl("_", api, fixed = TRUE)))
  expect_equal(length(api), length(unique(api)))
})

test_that("m3plot dispatches from direct and workflow results", {
  dat <- make_example()
  age <- suppressWarnings(m3cont(dat, "age"))
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(m3plot(age))

  result <- suppressWarnings(m3run(
    dat, cont = "age", bias = FALSE, leave = FALSE, show = FALSE
  ))
  expect_silent(m3plot(result, "cont"))
})

test_that("m3plot selects workflow moderators by name without leaking arguments", {
  dat <- make_example()
  result <- suppressWarnings(m3run(
    dat, cont = list(list(var = "age", name = "Age")),
    spline = list(var = "age", df = 1:2, name = "Age"),
    bias = FALSE, leave = FALSE, show = FALSE
  ))
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(m3plot(result, "cont", name = "Age"))
  expect_silent(m3plot(result, "spline", name = "Age"))
  expect_silent(m3plot(result, "cont", name = "age"))
  expect_silent(m3plot(result, "spline", name = "age"))
  expect_error(m3plot(result, "cont", name = "missing"), "Available names")
  expect_error(m3plot(result, "forest", name = "Age"), "only")
  expect_error(m3plot(result, "cont", index = 99), "index")
})

test_that("audit code is dependency-level, executable, and matches the model", {
  raw <- m3read(system.file("extdata", "example_correlations.csv",
                            package = "meta3level"))
  dat <- m3prep(raw, "r", study = "studyID", effect = "effectID",
                value = "r", n = "n")
  result <- suppressWarnings(m3run(
    dat, cont = "age", bias = FALSE, leave = FALSE, show = FALSE
  ))
  code <- m3code(result, print = FALSE)
  expect_true(any(grepl("metafor::rma.mv", code, fixed = TRUE)))
  expect_false(any(grepl("m3fit(", code, fixed = TRUE)))
  expect_false(any(grepl("m3run(", code, fixed = TRUE)))

  script <- tempfile(fileext = ".R")
  m3code(result, print = FALSE, file = script)
  env <- new.env(parent = globalenv())
  expect_silent(capture.output(sys.source(script, envir = env)))
  expect_equal(as.numeric(coef(env$main)[1]),
               as.numeric(coef(result$main$model)[1]), tolerance = 1e-10)
  sidecar <- file.path(
    dirname(script),
    paste0(tools::file_path_sans_ext(basename(script)), "_data.rds")
  )
  expect_true(file.exists(sidecar))
  saved_data <- readRDS(sidecar)
  expect_setequal(names(saved_data), c("studyID", "effectID", "yi", "vi", "age"))
  expect_false(any(c("r", "n", "design") %in% names(saved_data)))

  portable_dir <- tempfile("portable-audit-")
  dir.create(portable_dir)
  moved_script <- file.path(portable_dir, basename(script))
  moved_sidecar <- file.path(portable_dir, basename(sidecar))
  expect_true(file.copy(script, moved_script))
  expect_true(file.copy(sidecar, moved_sidecar))
  unlink(sidecar)
  portable_env <- new.env(parent = globalenv())
  expect_silent(capture.output(source(moved_script, local = portable_env)))
  expect_equal(as.numeric(coef(portable_env$main)[1]),
               as.numeric(coef(result$main$model)[1]), tolerance = 1e-10)
})

test_that("audit sidecar reproduces filtered data and derived moderators", {
  raw <- m3read(system.file("extdata", "example_correlations.csv",
                            package = "meta3level"))
  dat <- m3prep(raw, "r", study = "studyID", effect = "effectID",
                value = "r", n = "n")
  keep_studies <- unique(as.character(dat$studyID))[1:6]
  dat <- dat[as.character(dat$studyID) %in% keep_studies, , drop = FALSE]
  age_numeric <- as.numeric(dat$age)
  dat$ageband <- ifelse(
    age_numeric <= stats::median(age_numeric), "younger", "older"
  )
  dat$ageband[c(2, 9)] <- c("", ".")
  result <- suppressWarnings(m3run(
    dat,
    cont = "age",
    groups = list(var = "ageband", ref = "younger"),
    bias = FALSE,
    leave = FALSE,
    show = FALSE
  ))
  script <- tempfile(fileext = ".R")
  m3code(result, print = FALSE, file = script)
  env <- new.env(parent = globalenv())
  expect_silent(capture.output(sys.source(script, envir = env)))
  expect_equal(nrow(env$dat), nrow(result$main$source_data))
  expect_true("ageband" %in% names(env$dat))
  expect_setequal(
    names(env$dat),
    c("studyID", "effectID", "yi", "vi", "age", "ageband")
  )
  expect_equal(as.numeric(coef(env$main)),
               as.numeric(coef(result$main$model)), tolerance = 1e-10)
  expect_equal(as.numeric(coef(env$mcont)),
               as.numeric(coef(result$cont$age$model)), tolerance = 1e-10)
  expect_equal(as.numeric(coef(env$mgroup)),
               as.numeric(coef(result$groups$ageband$intercept)), tolerance = 1e-10)
  expect_true("age" %in% names(env$continuousModels))
  expect_true("ageband" %in% names(env$categoricalModels))
})

test_that("console reporting and full source inspection are available", {
  dat <- make_example()
  result <- suppressWarnings(m3run(
    dat, bias = FALSE, leave = FALSE, show = FALSE
  ))
  console <- capture.output(m3report(result, code = TRUE))
  expect_true(any(grepl("THREE-LEVEL META-ANALYSIS", console, fixed = TRUE)))
  expect_true(any(grepl("UNDERLYING-PACKAGE AUDIT CODE", console, fixed = TRUE)))
  expect_true(any(grepl("metafor::rma.mv", console, fixed = TRUE)))

  source <- m3source("all", print = FALSE)
  expect_true(is.function(source$m3run))
  expect_true(is.function(source$prepare_effects))
  expect_true(is.function(source$.hedges_J))

  sourceFile <- tempfile(fileext = ".R")
  expect_invisible(m3source("all", print = FALSE, file = sourceFile))
  expect_true(file.exists(sourceFile))
  sourceText <- readLines(sourceFile, warn = FALSE)
  expect_true(any(grepl("m3run <- function", sourceText, fixed = TRUE)))
  expect_true(any(grepl("prepare_effects <- function", sourceText, fixed = TRUE)))
  expect_silent(parse(sourceFile))
})

test_that("standalone fitted moderator settings appear in audit code", {
  raw <- m3read(system.file("extdata", "example_correlations.csv",
                            package = "meta3level"))
  dat <- m3prep(raw, "r", study = "studyID", effect = "effectID",
                value = "r", n = "n")
  model <- suppressWarnings(m3fit(dat, mods = ~ age, method = "ML", f = TRUE))
  code <- m3code(model, print = FALSE)
  expect_true(any(grepl("mods = ~\\s*age", code)))
  expect_true(any(grepl("method = \"ML\"", code, fixed = TRUE)))
  script <- tempfile(fileext = ".R")
  m3code(model, print = FALSE, file = script)
  env <- new.env(parent = globalenv())
  expect_silent(capture.output(sys.source(script, envir = env)))
  expect_equal(as.numeric(coef(env$main)[1]), as.numeric(coef(model)[1]),
               tolerance = 1e-10)
})
