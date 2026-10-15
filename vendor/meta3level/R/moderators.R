#' Analyze a centered continuous moderator
#'
#' Only an intercept model is fitted. The moderator is automatically centered
#' using the mean among rows retained in this moderator analysis.
#'
#' @param data Prepared effect-size data.
#' @param moderator Name of the continuous moderator column.
#' @param label Optional display label.
#' @param method Estimation method.
#' @param level `"three"` or `"single"`.
#' @param rho Sampling correlation used to pool effects for a single-level model.
#' @return A `meta3_continuous` result.
moderate_continuous <- function(data, moderator, label = moderator,
                                method = "REML", level = c("three", "single"),
                                rho = 0.60, inference = c("model", "CR2")) {
  level <- match.arg(level)
  inference <- match.arg(inference)
  .assert_columns(data, moderator)
  mod_data <- data
  parsed <- .continuous_values(mod_data[[moderator]], .decimal_of(data), moderator)
  mod_data$.moderator_raw <- parsed$values
  source_data <- .complete_meta_rows(mod_data)
  if (identical(level, "single")) {
    mod_data <- aggregate_by_study(source_data, rho = rho, keep = ".moderator_raw")
    rho <- attr(mod_data, "meta3_rho", exact = TRUE) %||% rho
    mod_data$.moderator_raw <- .as_numeric(mod_data$.moderator_raw, .decimal_of(data))
    mod_data <- .complete_meta_rows(mod_data, ".moderator_raw")
  } else {
    mod_data <- .complete_meta_rows(source_data, ".moderator_raw")
  }
  if (nrow(mod_data) < 3) stop("At least three complete effect sizes are required.", call. = FALSE)
  if (length(unique(mod_data$.moderator_raw)) < 2) {
    stop("The moderator must contain at least two distinct values.", call. = FALSE)
  }
  .warn_few_studies(mod_data, paste0("Continuous moderator '", label, "'"))
  center <- mean(mod_data$.moderator_raw)
  mod_data$moderator_c <- mod_data$.moderator_raw - center
  model <- .fit_meta(mod_data, mods = ~ moderator_c, method = method,
                     level = level, rho = rho, inference = inference)
  result <- list(
    level = level,
    rho = rho,
    label = label,
    moderator = moderator,
    percent_scale = identical(parsed$scale, "percentage_points"),
    moderator_scale = parsed$scale,
    normalized_mixed_scale = parsed$normalized,
    center = center,
    data = mod_data,
    source_data = source_data,
    model = model,
    coefficients = .coef_table(model),
    f_test = moderator_f_test(model),
    i2 = if (identical(level, "single")) single_level_heterogeneity(model) else multilevel_i2(model),
    lrt = if (identical(level, "single")) single_level_lrt(model, mod_data) else variance_lrt(model, mod_data)
  )
  class(result) <- "meta3_continuous"
  result
}

#' @export
print.meta3_continuous <- function(x, ...) {
  .section(paste("CONTINUOUS MODERATOR:", x$label,
                 if (identical(x$level, "single")) "(SINGLE LEVEL)" else "(THREE LEVEL)"))
  cat("Effect sizes k =", x$model$k, "\n")
  cat("Studies =", length(unique(x$data$studyID)), "\n")
  cat("Centering mean =", format(x$center, digits = 6), "\n")
  cat("Centered mean =", format(mean(x$data$moderator_c), digits = 6), "\n")
  if (identical(x$moderator_scale, "proportion")) {
    cat("Percent-marked and proportion values were normalized to the 0-1 proportion scale.\n")
  } else if (isTRUE(x$percent_scale)) {
    cat("Percent values are analysed on the 0-100 percentage-point scale.\n")
  }
  .print_inference(x$model)
  cat("\nReported-scale intercept (the slope remains on the analysis scale):\n")
  print(x$coefficients, row.names = FALSE)
  .section(if (identical(x$model$meta3_inference, "CR2")) "CR2 / HTZ F TEST" else "METAFOR F TEST")
  print(x$f_test, row.names = FALSE)
  .section("ONE-SIDED VARIANCE-COMPONENT LRT")
  print(x$lrt, row.names = FALSE)
  invisible(x)
}

#' Analyze a categorical moderator
#'
#' Fits both an intercept model (category contrasts against a reference group)
#' and a no-intercept model (a pooled effect for every category).
#'
#' @param data Prepared effect-size data.
#' @param moderator Name of the categorical moderator column.
#' @param reference Reference-category label.
#' @param label Optional display label.
#' @param method Estimation method.
#' @param level `"three"` or `"single"`.
#' @param rho Sampling correlation used to pool effects for a single-level model.
#' @return A `meta3_categorical` result.
moderate_categorical <- function(data, moderator, reference,
                                 label = moderator, method = "REML",
                                 level = c("three", "single"), rho = 0.60,
                                 inference = c("model", "CR2")) {
  level <- match.arg(level)
  inference <- match.arg(inference)
  if (length(reference) != 1 || is.na(reference) ||
      !nzchar(trimws(as.character(reference)))) {
    stop("'reference' must be one non-missing category label.", call. = FALSE)
  }
  reference <- gsub("[[:space:]]+", " ", trimws(as.character(reference)))
  .assert_columns(data, moderator)
  mod_data <- data
  raw <- trimws(as.character(mod_data[[moderator]]))
  raw[.is_declared_missing(raw)] <- NA_character_
  raw <- gsub("[[:space:]]+", " ", raw)
  mod_data$.moderator_clean <- raw
  source_data <- .complete_meta_rows(mod_data)
  if (identical(level, "single")) {
    mod_data <- aggregate_by_study(source_data, rho = rho, keep = ".moderator_clean")
    rho <- attr(mod_data, "meta3_rho", exact = TRUE) %||% rho
    raw <- trimws(as.character(mod_data$.moderator_clean))
    raw[.is_declared_missing(raw)] <- NA_character_
    mod_data$moderator_factor <- factor(gsub("[[:space:]]+", " ", raw))
    mod_data <- .complete_meta_rows(mod_data, "moderator_factor")
  } else {
    mod_data$moderator_factor <- factor(mod_data$.moderator_clean)
    mod_data <- .complete_meta_rows(mod_data, "moderator_factor")
  }
  if (!reference %in% levels(mod_data$moderator_factor)) {
    stop("Reference category not found: ", reference, call. = FALSE)
  }
  if (nlevels(mod_data$moderator_factor) < 2) {
    stop("The categorical moderator must contain at least two categories.", call. = FALSE)
  }
  category_studies <- vapply(
    split(mod_data, mod_data$moderator_factor, drop = TRUE),
    .study_count,
    numeric(1)
  )
  sparse <- category_studies < 2
  if (any(sparse)) {
    warning("Categorical moderator '", label, "' has category/categories represented by fewer than two studies: ",
            paste(names(category_studies)[sparse], collapse = ", "),
            ". Coefficients and F tests may be unstable.", call. = FALSE)
  }
  if (.study_count(mod_data) <= nlevels(mod_data$moderator_factor)) {
    warning("The number of independent studies is not larger than the number of categories; the omnibus test is likely unreliable.", call. = FALSE)
  }
  mod_data$moderator_factor <- stats::relevel(mod_data$moderator_factor, ref = reference)

  fit_fun <- .fit_meta
  fit_args <- list(method = method, level = level, rho = rho, inference = inference)
  intercept_model <- do.call(fit_fun, c(list(data = mod_data, mods = ~ moderator_factor), fit_args))
  no_intercept_model <- do.call(fit_fun, c(list(data = mod_data, mods = ~ 0 + moderator_factor), fit_args))
  category_parts <- split(mod_data, mod_data$moderator_factor, drop = TRUE)
  counts_effects <- do.call(rbind, lapply(category_parts, function(z) {
    represented <- if (identical(level, "single") && "effects" %in% names(z)) {
      sum(z$effects)
    } else {
      nrow(z)
    }
    data.frame(category = as.character(z$moderator_factor[1]),
               effects = as.integer(represented), stringsAsFactors = FALSE)
  }))
  counts_studies <- do.call(rbind, lapply(category_parts, function(z) {
    data.frame(category = as.character(z$moderator_factor[1]),
               studies = .study_count(z), stringsAsFactors = FALSE)
  }))
  result <- list(
    level = level,
    rho = rho,
    label = label,
    moderator = moderator,
    reference = reference,
    counts_effects = counts_effects,
    counts_studies = counts_studies,
    data = mod_data,
    source_data = source_data,
    intercept_model = intercept_model,
    no_intercept_model = no_intercept_model,
    contrasts = .coef_table(intercept_model),
    group_effects = .coef_table(no_intercept_model, all_are_effects = TRUE),
    f_test = moderator_f_test(intercept_model),
    i2 = if (identical(level, "single")) single_level_heterogeneity(intercept_model) else multilevel_i2(intercept_model),
    lrt = if (identical(level, "single")) single_level_lrt(intercept_model, mod_data) else variance_lrt(intercept_model, mod_data)
  )
  class(result) <- "meta3_categorical"
  result
}

#' @export
print.meta3_categorical <- function(x, ...) {
  .section(paste("CATEGORICAL MODERATOR:", x$label,
                 if (identical(x$level, "single")) "(SINGLE LEVEL)" else "(THREE LEVEL)"))
  cat("Reference category:", x$reference, "\n")
  cat("Effect sizes k =", x$intercept_model$k, "\n")
  cat("Studies =", length(unique(x$data$studyID)), "\n")
  cat("\nEffects per category:\n")
  print(x$counts_effects, row.names = FALSE)
  cat("\nStudies per category:\n")
  print(x$counts_studies, row.names = FALSE)
  .section("INTERCEPT MODEL: CONTRASTS AGAINST REFERENCE")
  .print_inference(x$intercept_model)
  .section(if (identical(x$intercept_model$meta3_inference, "CR2")) "CR2 / HTZ OMNIBUS F TEST" else "METAFOR OMNIBUS F TEST")
  print(x$f_test, row.names = FALSE)
  .section("NO-INTERCEPT MODEL: POOLED EFFECT FOR EACH CATEGORY")
  .print_inference(x$no_intercept_model)
  cat("\nReported-scale group effects:\n")
  print(x$group_effects, row.names = FALSE)
  .section("ONE-SIDED VARIANCE-COMPONENT LRT")
  print(x$lrt, row.names = FALSE)
  invisible(x)
}

#' Compare linear/natural-spline moderator models using ML
#'
#' All candidate models use exactly the same complete-case data. Model
#' comparison is based on ML AIC, AICc, and BIC.
#'
#' @param data Prepared effect-size data.
#' @param moderator Continuous moderator column.
#' @param dfs Natural-spline degrees of freedom to compare.
#' @param include_linear Include a conventional linear model.
#' @param label Optional display label.
#' @param level `"three"` or `"single"`.
#' @param rho Sampling correlation used to pool effects for a single-level model.
#' @return A `meta3_splines` result.
compare_splines <- function(data, moderator, dfs = 1:3,
                            include_linear = TRUE, label = moderator,
                            level = c("three", "single"), rho = 0.60,
                            inference = c("model", "CR2")) {
  level <- match.arg(level)
  inference <- match.arg(inference)
  .assert_columns(data, moderator)
  if (!is.numeric(dfs) || !length(dfs) || any(!is.finite(dfs)) ||
      any(dfs < 1 | dfs != floor(dfs))) {
    stop("All spline dfs must be positive integers.", call. = FALSE)
  }
  dfs <- sort(unique(as.integer(dfs)))
  mod_data <- data
  parsed <- .continuous_values(mod_data[[moderator]], .decimal_of(data), moderator)
  mod_data$.moderator_raw <- parsed$values
  source_data <- .complete_meta_rows(mod_data)
  if (identical(level, "single")) {
    mod_data <- aggregate_by_study(source_data, rho = rho, keep = ".moderator_raw")
    rho <- attr(mod_data, "meta3_rho", exact = TRUE) %||% rho
    mod_data$.moderator_raw <- .as_numeric(mod_data$.moderator_raw, .decimal_of(data))
    mod_data <- .complete_meta_rows(mod_data, ".moderator_raw")
  } else {
    mod_data <- .complete_meta_rows(source_data, ".moderator_raw")
  }
  if (length(unique(mod_data$.moderator_raw)) <= max(dfs)) {
    stop("Too few distinct moderator values for the requested spline degrees of freedom.", call. = FALSE)
  }
  if (nrow(mod_data) <= max(dfs) + 3) {
    stop("Too few complete effect sizes for the requested spline complexity; require k > max(df) + 3.", call. = FALSE)
  }
  if (.study_count(mod_data) <= max(dfs) + 1) {
    warning("The requested spline degrees of freedom are high relative to the number of independent studies; AIC may favor an unstable curve.", call. = FALSE)
  }
  center <- mean(mod_data$.moderator_raw)
  mod_data$moderator_c <- mod_data$.moderator_raw - center
  models <- list()
  bases <- list()
  fit_fun <- .fit_meta
  fit_args <- list(method = "ML", level = level, rho = rho, inference = inference)

  if (include_linear) {
    models$linear <- do.call(fit_fun, c(list(data = mod_data, mods = ~ moderator_c), fit_args))
  }
  for (df in dfs) {
    basis <- splines::ns(mod_data$moderator_c, df = df)
    basis_names <- paste0("spline", seq_len(ncol(basis)))
    spline_data <- mod_data
    spline_data[basis_names] <- basis
    formula <- stats::as.formula(paste("~", paste(basis_names, collapse = " + ")))
    model <- do.call(fit_fun, c(list(data = spline_data, mods = formula), fit_args))
    model$meta3_spline_df <- df
    model$meta3_spline_basis <- basis
    models[[paste0("spline_df", df)]] <- model
    bases[[paste0("spline_df", df)]] <- basis
  }
  comparison <- do.call(rbind, lapply(names(models), function(name) {
    cbind(model = name, .fit_stats(models[[name]]), row.names = NULL)
  }))
  comparison$complexity <- rep(NA_integer_, nrow(comparison))
  linear_rows <- comparison$model == "linear"
  comparison$complexity[linear_rows] <- 1L
  comparison$complexity[!linear_rows] <- as.integer(
    sub("spline_df", "", comparison$model[!linear_rows])
  )
  use_aicc <- any(is.finite(comparison$AICc))
  criterion <- if (use_aicc) comparison$AICc else comparison$AIC
  comparison$delta <- criterion - min(criterion, na.rm = TRUE)
  comparison <- comparison[order(comparison$delta, comparison$complexity,
                                 comparison$model != "linear"), , drop = FALSE]
  competitive <- comparison[is.finite(comparison$delta) & comparison$delta <= 2, , drop = FALSE]
  preferred <- competitive[order(competitive$complexity,
                                 competitive$model != "linear",
                                 competitive$delta), "model"][1]
  result <- list(
    level = level,
    rho = rho,
    label = label,
    moderator = moderator,
    center = center,
    moderator_scale = parsed$scale,
    normalized_mixed_scale = parsed$normalized,
    data = mod_data,
    source_data = source_data,
    models = models,
    bases = bases,
    comparison = comparison,
    minimum_ic_model = comparison$model[1],
    best_model = preferred,
    selection_criterion = if (use_aicc) "AICc" else "AIC"
  )
  class(result) <- "meta3_splines"
  result
}

#' @export
print.meta3_splines <- function(x, ...) {
  .section(paste("ML SPLINE COMPARISON:", x$label))
  cat("All models use k =", nrow(x$data), "effect sizes from",
      length(unique(x$data$studyID)), "studies.\n")
  cat("Centering mean =", format(x$center, digits = 6), "\n")
  print(x$comparison, row.names = FALSE)
  cat("\nMinimum", x$selection_criterion, "model:", x$minimum_ic_model, "\n")
  cat("Preferred parsimonious model (Delta <= 2):", x$best_model, "\n")
  invisible(x)
}
