.run_continuous_specs <- function(data, specs, continue_on_error = TRUE,
                                  level = "three", rho = 0.60, inference = "model") {
  if (is.null(specs)) return(list())
  if (is.character(specs)) {
    specs <- lapply(specs, function(column) list(column = column, label = column))
  }
  .run_specs(data, specs, moderate_continuous, "continuous", continue_on_error,
              level, rho, inference)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.run_categorical_specs <- function(data, specs, continue_on_error = TRUE,
                                   level = "three", rho = 0.60, inference = "model") {
  if (is.null(specs)) return(list())
  if (!is.list(specs) || any(!vapply(specs, is.list, logical(1)))) {
    stop("Categorical specifications must be a list of lists with column and reference.", call. = FALSE)
  }
  .run_specs(data, specs, moderate_categorical, "categorical", continue_on_error,
              level, rho, inference)
}

.run_specs <- function(data, specs, fun, type, continue_on_error,
                        level, rho, inference) {
  stats::setNames(lapply(specs, function(spec) {
    if (is.null(spec$column)) {
      stop("Each ", type, " specification requires 'column'.", call. = FALSE)
    }
    if (identical(type, "categorical") && is.null(spec$reference)) {
      stop("Each categorical specification requires 'column' and 'reference'.", call. = FALSE)
    }
    spec$moderator <- spec$column
    spec$column <- NULL
    spec$level <- level
    spec$rho <- rho
    spec$inference <- inference
    run <- function() do.call(fun, c(list(data = data), spec))
    if (continue_on_error) {
      .result_or_error(run(), paste0(type, " moderator: ", spec$label %||% spec$moderator))
    } else run()
  }), vapply(specs, function(z) z$label %||% z$column, character(1)))
}

.run_spline_specs <- function(data, specs, continue_on_error = TRUE,
                              level = "three", rho = 0.60, inference = "model") {
  if (is.null(specs)) return(list())
  if (!is.list(specs) || any(!vapply(specs, is.list, logical(1)))) {
    stop("Spline specifications must be a list of lists.", call. = FALSE)
  }
  .run_specs(data, specs, compare_splines, "spline", continue_on_error,
              level, rho, inference)
}

#' Run an automated three-level meta-analysis workflow
#'
#' @param data Prepared effect-size data.
#' @param continuous Character vector or list of specifications. Each list can
#'   contain `column`, `label`, and arguments to [moderate_continuous()].
#' @param categorical List of specifications containing at least `column` and
#'   `reference`.
#' @param splines List of spline specifications containing `column` and `dfs`.
#' @param publication Run publication-bias analyses.
#' @param leave_out Run both leave-one-effect-out and leave-one-study-out.
#' @param rho Correlation used in study-level aggregation.
#' @param level `"three"` or `"single"`.
#' @param console Print the complete report to the R console.
#' @param continue_on_error Continue other optional analyses when one moderator,
#'   spline, publication-bias, or leave-one-out component fails.
#' @return A `meta3_workflow` result.
run_meta_workflow <- function(data,
                              continuous = NULL,
                              categorical = NULL,
                              splines = NULL,
                              publication = TRUE,
                              leave_out = TRUE,
                              rho = 0.60,
                              level = c("three", "single"),
                              console = TRUE,
                              continue_on_error = TRUE,
                              inference = c("model", "CR2")) {
  level <- match.arg(level)
  inference <- match.arg(inference)
  dat <- .complete_meta_rows(data)
  main_model <- .fit_meta(dat, level = level, rho = rho, inference = inference,
                          aggregate = TRUE, warn_few_studies = TRUE)
  if (identical(level, "single")) rho <- main_model$meta3_rho
  main_data <- main_model$meta3_data
  aggregation <- main_model$meta3_aggregation
  original_effects <- if (!is.null(aggregation) && nrow(aggregation)) {
    sum(aggregation$pooled_effects)
  } else {
    nrow(dat)
  }
  main <- list(
    level = level,
    rho = rho,
    data = main_data,
    source_data = dat,
    aggregation = aggregation,
    original_effects = original_effects,
    model = main_model,
    overall = overall_effect(main_model),
    i2 = if (identical(level, "single")) single_level_heterogeneity(main_model) else multilevel_i2(main_model),
    prediction = if (identical(level, "single") && main_model$p == 1) single_level_prediction(main_model) else NULL,
    lrt = if (identical(level, "single")) single_level_lrt(main_model, main_data) else variance_lrt(main_model, dat)
  )
  class(main) <- "meta3_main"
  result <- list(
    main = main,
    continuous = .run_continuous_specs(dat, continuous, continue_on_error, level, rho, inference),
    categorical = .run_categorical_specs(dat, categorical, continue_on_error, level, rho, inference),
    splines = .run_spline_specs(dat, splines, continue_on_error, level, rho, inference),
    publication_bias = if (publication) {
      if (continue_on_error) .result_or_error(publication_bias(dat, rho = rho, level = level, inference = inference), "publication bias") else publication_bias(dat, rho = rho, level = level, inference = inference)
    } else NULL,
    leave_one_effect = if (leave_out) {
      if (continue_on_error) .result_or_error(leave_one_out(dat, "effect", level = level, rho = rho, inference = inference), "leave-one-effect-out") else leave_one_out(dat, "effect", level = level, rho = rho, inference = inference)
    } else NULL,
    leave_one_study = if (leave_out) {
      if (continue_on_error) .result_or_error(leave_one_out(dat, "study", level = level, rho = rho, inference = inference), "leave-one-study-out") else leave_one_out(dat, "study", level = level, rho = rho, inference = inference)
    } else NULL
  )
  class(result) <- "meta3_workflow"
  if (console) print(result)
  invisible(result)
}

#' Print a complete meta-analysis report
#'
#' @param x Any result produced by this package.
#' @param ... Additional print arguments.
#' @return `x`, invisibly.
print_meta_report <- function(x, ...) {
  print(x, ...)
  invisible(x)
}

#' @export
print.meta3_workflow <- function(x, ...) {
  print(x$main)
  continuous <- x$cont %||% x$continuous
  categorical <- x$groups %||% x$categorical
  splines <- x$spline %||% x$splines
  publication <- x$bias %||% x$publication_bias
  effect_leave <- x$effectleave %||% x$leave_one_effect
  study_leave <- x$studyleave %||% x$leave_one_study
  if (length(continuous)) {
    for (z in continuous) .print_safe(z)
  }
  if (length(categorical)) {
    for (z in categorical) .print_safe(z)
  }
  if (length(splines)) {
    for (z in splines) .print_safe(z)
  }
  if (!is.null(publication)) .print_safe(publication)
  if (!is.null(effect_leave)) .print_safe(effect_leave)
  if (!is.null(study_leave)) .print_safe(study_leave)
  invisible(x)
}
