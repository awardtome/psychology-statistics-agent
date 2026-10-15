#' Fit a three-level meta-analysis model
#'
#' @param data Prepared data from [prepare_effects()].
#' @param mods Moderator formula or character vector.
#' @param method Estimation method, normally `"REML"`.
#' @param tdist Use t/F inference. Defaults to `TRUE`.
#' @param sigma2 Optional fixed/free variance-component specification.
#' @param ... Additional arguments passed to [metafor::rma.mv()].
#' @return A fitted `rma.mv` object with metadata used by this package.
fit_three_level <- function(data, mods = NULL, method = "REML",
                            tdist = TRUE, sigma2 = c(NA, NA),
                            warn_few_studies = TRUE,
                            inference = c("model", "CR2"), ...) {
  inference <- match.arg(inference)
  mods <- .mods_formula(mods)
  .assert_columns(data, c("studyID", "effectID", "yi", "vi"))
  data <- .model_rows(data, mods)
  duplicate_ids <- duplicated(data.frame(
    studyID = as.character(data$studyID),
    effectID = as.character(data$effectID),
    stringsAsFactors = FALSE
  ))
  if (any(duplicate_ids)) {
    stop("Each studyID/effectID combination must be unique before fitting the model.", call. = FALSE)
  }
  if (nrow(data) < 4) {
    stop("At least four valid effect sizes are required for a three-level model.", call. = FALSE)
  }
  studies <- .study_count(data)
  if (studies < 2) {
    stop("A three-level meta-analysis requires at least two independent studies.", call. = FALSE)
  }
  effects_per_study <- table(data$studyID)
  if (!any(effects_per_study > 1)) {
    stop("No study contributes multiple effect sizes, so Level 2 and Level 3 variance cannot be separated. Use a conventional two-level meta-analysis instead.", call. = FALSE)
  }
  repeated_studies <- sum(effects_per_study > 1)
  if (repeated_studies < 2) {
    warning("Only one independent study contributes multiple effect sizes; the Level 2 variance component is weakly identified and should not be interpreted as stable.",
            call. = FALSE)
  }
  if (warn_few_studies) .warn_few_studies(data, "The three-level model")
  args <- list(
    yi = data$yi,
    V = data$vi,
    random = stats::as.formula("~ 1 | studyID/effectID"),
    method = method,
    tdist = tdist,
    sigma2 = sigma2,
    data = data
  )
  if (!is.null(mods)) args$mods <- mods
  args <- c(args, list(...))
  model <- do.call(metafor::rma.mv, args)
  model$meta3_scale <- .scale_of(data)
  model$meta3_data <- data
  model$meta3_mods <- mods
  model$meta3_fit_args <- list(...)
  model$meta3_sigma2 <- sigma2
  model$meta3_level <- "three"
  .apply_inference(model, inference)
}

#' Fit a conventional random-effects meta-analysis
#'
#' Multiple effects from the same study are first pooled by generalized least
#' squares under the specified sampling correlation.
fit_single_level <- function(data, mods = NULL, method = "REML",
                             tdist = TRUE, rho = 0.60,
                             aggregate = TRUE, warn_few_studies = TRUE,
                             inference = c("model", "CR2"), ...) {
  inference <- match.arg(inference)
  .assert_columns(data, c("studyID", "effectID", "yi", "vi"))
  input_prepooled <- inherits(data, "meta3_study_data")
  stored_rho <- attr(data, "meta3_rho", exact = TRUE)
  stored_aggregation <- attr(data, "meta3_aggregation", exact = TRUE)
  source_data <- .complete_meta_rows(data)
  mods <- .mods_formula(mods)
  moderator_columns <- if (is.null(mods)) character() else all.vars(mods)
  if (aggregate) {
    already_pooled <- input_prepooled &&
      !anyDuplicated(as.character(source_data$studyID))
    if (already_pooled) {
      if (!is.null(stored_rho) && is.finite(stored_rho) &&
          !isTRUE(all.equal(as.numeric(rho), as.numeric(stored_rho)))) {
        warning("The input was already pooled with rho = ", stored_rho,
                "; the requested rho = ", rho,
                " cannot be applied without the original effects. The stored rho is used.",
                call. = FALSE)
        rho <- stored_rho
      }
      model_data <- .complete_meta_rows(source_data, moderator_columns)
      attr(model_data, "meta3_rho") <- stored_rho %||% rho
      attr(model_data, "meta3_aggregation") <- stored_aggregation
      class(model_data) <- unique(c("meta3_study_data", "meta3_data", class(model_data)))
    } else {
      model_data <- aggregate_by_study(source_data, rho = rho,
                                       keep = moderator_columns)
    }
    if (warn_few_studies && !already_pooled && any(table(source_data$studyID) > 1)) {
      warning("The single-level model pools dependent effects using rho = ", rho,
              ". This correlation is assumed rather than estimated; repeat the analysis with plausible rho values as a sensitivity check.",
              call. = FALSE)
    }
  } else {
    model_data <- source_data
    if (anyDuplicated(as.character(model_data$studyID))) {
      stop("Single-level model data must contain one independent effect per study.",
           call. = FALSE)
    }
  }
  model_data <- .model_rows(model_data, mods)
  if (nrow(model_data) < 3) {
    stop("A conventional random-effects meta-analysis requires at least three independent studies.",
         call. = FALSE)
  }
  if (warn_few_studies) .warn_few_studies(model_data, "The single-level model")
  args <- list(
    yi = model_data$yi,
    vi = model_data$vi,
    method = method,
    test = if (tdist) "knha" else "z",
    data = model_data
  )
  if (!is.null(mods)) args$mods <- mods
  args <- c(args, list(...))
  model <- do.call(metafor::rma.uni, args)
  model$meta3_scale <- .scale_of(data)
  model$meta3_data <- model_data
  model$meta3_source_data <- source_data
  model$meta3_mods <- mods
  model$meta3_level <- "single"
  model$meta3_rho <- rho
  model$meta3_aggregation <- attr(model_data, "meta3_aggregation", exact = TRUE)
  model$meta3_preaggregated <- input_prepooled
  model$meta3_fit_args <- list(...)
  .apply_inference(model, inference)
}

#' Extract the overall effect on analysis and reported scales
overall_effect <- function(model, transform = TRUE) {
  tab <- .coef_table(model, transform = transform)
  tab[1, , drop = FALSE]
}

#' Extract metafor's omnibus moderator F test
moderator_f_test <- function(model) {
  if (is.null(model$QM) || !length(model$QM)) {
    stop("This model has no moderator omnibus test.", call. = FALSE)
  }
  if (!model$test %in% c("t", "knha", "adhoc")) {
    stop("The model was not fitted with t/F inference; QM is not an F statistic. Refit with f = TRUE.", call. = FALSE)
  }
  df1 <- if (length(model$QMdf)) model$QMdf[1] else model$m
  df2 <- if (length(model$QMdf) > 1) model$QMdf[2] else model$ddf
  data.frame(F = as.numeric(model$QM), df1 = as.numeric(df1),
             df2 = as.numeric(df2), p = as.numeric(model$QMp))
}

.typical_vi <- function(vi) {
  w <- 1 / vi
  denominator <- sum(w)^2 - sum(w^2)
  if (length(vi) < 2 || denominator <= 0) return(NA_real_)
  ((length(vi) - 1) * sum(w)) / denominator
}

#' Decompose multilevel I-squared
multilevel_i2 <- function(model) {
  typical_vi <- .typical_vi(model$vi)
  sigma <- as.numeric(model$sigma2)
  if (length(sigma) < 2) sigma <- c(sigma, NA_real_)
  denom <- sum(sigma[1:2], na.rm = TRUE) + typical_vi
  data.frame(
    level = c("Between studies (Level 3)", "Within studies (Level 2)", "Total I2"),
    variance = c(sigma[1], sigma[2], sum(sigma[1:2], na.rm = TRUE)),
    I2_percent = 100 * c(sigma[1], sigma[2], sum(sigma[1:2], na.rm = TRUE)) / denom
  )
}

#' Summarize conventional random-effects heterogeneity
single_level_heterogeneity <- function(model) {
  q_df <- if (length(model$QEdf)) model$QEdf else model$k - model$p
  data.frame(
    k = as.numeric(model$k),
    tau2 = as.numeric(model$tau2),
    tau = sqrt(as.numeric(model$tau2)),
    I2_percent = as.numeric(model$I2),
    H2 = as.numeric(model$H2),
    Q = as.numeric(model$QE),
    Q_df = as.numeric(q_df),
    Q_p = as.numeric(model$QEp)
  )
}

#' Prediction interval for a conventional random-effects model
single_level_prediction <- function(model) {
  if (model$p > 1) {
    stop("A moderator model requires specified moderator values for prediction; no unconditional prediction interval is reported.",
         call. = FALSE)
  }
  pred <- metafor::predict.rma(model)
  scale <- .scale_of(model)
  data.frame(
    estimate_analysis = as.numeric(pred$pred),
    ci_lb_analysis = as.numeric(pred$ci.lb),
    ci_ub_analysis = as.numeric(pred$ci.ub),
    pi_lb_analysis = as.numeric(pred$pi.lb),
    pi_ub_analysis = as.numeric(pred$pi.ub),
    estimate_reported = .transform_value(as.numeric(pred$pred), scale),
    ci_lb_reported = .transform_value(as.numeric(pred$ci.lb), scale),
    ci_ub_reported = .transform_value(as.numeric(pred$ci.ub), scale),
    pi_lb_reported = .transform_value(as.numeric(pred$pi.lb), scale),
    pi_ub_reported = .transform_value(as.numeric(pred$pi.ub), scale)
  )
}

#' One-sided likelihood-ratio test for tau-squared
single_level_lrt <- function(model, data = model$meta3_data) {
  if (is.null(data)) stop("The study-level model data are required.", call. = FALSE)
  full <- .safe(do.call(fit_single_level, c(list(
    data = data, mods = model$meta3_mods, method = "ML", tdist = FALSE,
    rho = model$meta3_rho %||% 0.60, aggregate = FALSE,
    warn_few_studies = FALSE
  ), model$meta3_fit_args)))
  reduced <- .safe(do.call(fit_single_level, c(list(
    data = data, mods = model$meta3_mods, method = "FE", tdist = FALSE,
    rho = model$meta3_rho %||% 0.60, aggregate = FALSE,
    warn_few_studies = FALSE
  ), model$meta3_fit_args)))
  if (inherits(full, "meta3_error") || inherits(reduced, "meta3_error")) {
    message <- if (inherits(full, "meta3_error")) full$message else reduced$message
    return(data.frame(component = "Between-study variance", LRT = NA_real_,
                      df = 1, p_one_sided = NA_real_, note = message))
  }
  statistic <- max(0, 2 * (as.numeric(stats::logLik(full)) -
                             as.numeric(stats::logLik(reduced))))
  data.frame(
    component = "Between-study variance",
    LRT = statistic,
    df = 1,
    p_one_sided = 0.5 * stats::pchisq(statistic, 1, lower.tail = FALSE),
    note = "ML boundary test: 0.5*chi-square(0) + 0.5*chi-square(1)"
  )
}

#' One-sided likelihood-ratio tests for variance components
variance_lrt <- function(model, data = model$meta3_data) {
  if (is.null(data)) stop("The original model data are required.", call. = FALSE)
  specification <- model$meta3_sigma2 %||% c(NA_real_, NA_real_)
  fit_reduced <- function(sigma) {
    do.call(fit_three_level, c(list(data = data, mods = model$meta3_mods,
      method = model$method, tdist = identical(model$test, "t"), sigma2 = sigma,
      warn_few_studies = FALSE), model$meta3_fit_args))
  }
  reduced_l3 <- if (is.na(specification[1])) {
    .safe(fit_reduced(replace(specification, 1, 0)))
  } else NULL
  reduced_l2 <- if (is.na(specification[2])) {
    .safe(fit_reduced(replace(specification, 2, 0)))
  } else NULL
  one <- function(reduced, component) {
    if (is.null(reduced)) {
      return(data.frame(component = component, LRT = NA_real_, df = 1,
        p_one_sided = NA_real_, note = "Component fixed in the fitted model; no variance-component test is defined."))
    }
    if (inherits(reduced, "meta3_error")) {
      return(data.frame(component = component, LRT = NA_real_, df = 1,
                        p_one_sided = NA_real_, note = reduced$message))
    }
    statistic <- max(0, 2 * (as.numeric(stats::logLik(model)) -
                               as.numeric(stats::logLik(reduced))))
    data.frame(component = component, LRT = statistic, df = 1,
               p_one_sided = 0.5 * stats::pchisq(statistic, 1, lower.tail = FALSE),
               note = "0.5*chi-square(0) + 0.5*chi-square(1) boundary reference")
  }
  rbind(one(reduced_l3, "Between-study variance (Level 3)"),
        one(reduced_l2, "Within-study variance (Level 2)"))
}

#' @export
print.meta3_main <- function(x, ...) {
  level <- x$level %||% "three"
  .section(if (identical(level, "single")) "CONVENTIONAL RANDOM-EFFECTS META-ANALYSIS" else "THREE-LEVEL META-ANALYSIS")
  if (identical(level, "single")) {
    cat("Pooled study-level effect sizes k =", x$model$k, "\n")
    cat("Original effects represented =", x$original_effects, "\n")
    cat("Within-study pooling rho =", x$rho, "\n")
    cat("\nEffects pooled within each study:\n")
    print(x$aggregation, row.names = FALSE)
  } else {
    cat("Effect sizes k =", x$model$k, "\n")
    cat("Studies =", length(unique(x$data$studyID)), "\n")
  }
  .print_inference(x$model)
  .section(if (x$model$p > 1) "REPORTED-SCALE MODEL INTERCEPT" else "REPORTED-SCALE OVERALL EFFECT")
  print(x$overall, row.names = FALSE)
  .section(if (identical(level, "single")) "HETEROGENEITY" else "MULTILEVEL I2")
  print(x$i2, row.names = FALSE)
  if (identical(level, "single")) {
    .section("PREDICTION INTERVAL")
    print(x$prediction, row.names = FALSE)
  }
  .section("ONE-SIDED VARIANCE-COMPONENT LRT")
  print(x$lrt, row.names = FALSE)
  invisible(x)
}
