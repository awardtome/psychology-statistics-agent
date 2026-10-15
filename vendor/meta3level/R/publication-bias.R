.selection_table <- function(base_model, moderate, severe, scale = "custom") {
  extract <- function(model, label) {
    if (inherits(model, "meta3_error")) {
      return(data.frame(model = label, estimate = NA, se = NA,
                        ci.lb = NA, ci.ub = NA, p = NA,
                        note = model$message))
    }
    data.frame(
      model = label,
      estimate = as.numeric(model$beta[1]),
      se = as.numeric(model$se[1]),
      ci.lb = as.numeric(model$ci.lb[1]),
      ci.ub = as.numeric(model$ci.ub[1]),
      p = as.numeric(model$pval[1]),
      note = ""
    )
  }
  out <- rbind(extract(base_model, "Unadjusted ML"),
               extract(moderate, "Moderate selection"),
               extract(severe, "Severe selection"))
  out$estimate_reported <- .transform_value(out$estimate, scale)
  out$ci_lb_reported <- .transform_value(out$ci.lb, scale)
  out$ci_ub_reported <- .transform_value(out$ci.ub, scale)
  out
}

#' Run publication-bias and small-study-effect analyses
#'
#' Multilevel Egger/PET and PEESE retain all dependent effect sizes. Optional
#' selection-based methods are run on one GLS-aggregated estimate per study.
#'
#' @param data Prepared effect-size data.
#' @param rho Assumed within-study correlation for study-level aggregation.
#' @param alternative Selection direction: `"auto"` uses the sign of the
#'   study-level pooled effect; other options are `"greater"`, `"less"`, and
#'   `"two.sided"`.
#' @param supplementary Run Begg, 3PSM, Vevea-Woods, Copas, p-uniform, and
#'   Rosenthal fail-safe N when their packages are available.
#' @param level `"three"` retains dependent effects for multilevel PET-PEESE;
#'   `"single"` uses one GLS-pooled estimate per study.
#' @return A `meta3_publication_bias` result.
publication_bias <- function(data, rho = 0.60,
                             supplementary = TRUE,
                             alternative = c("auto", "greater", "less", "two.sided"),
                             level = c("three", "single"),
                             inference = c("model", "CR2")) {
  alternative <- match.arg(alternative)
  level <- match.arg(level)
  inference <- match.arg(inference)
  dat <- .complete_meta_rows(data)
  studies <- .study_count(dat)
  if (studies < 3) {
    stop("Publication-bias regression requires at least three independent studies; ten or more are strongly recommended.", call. = FALSE)
  }
  if (studies < 10) {
    warning("Publication-bias tests use only ", studies,
            " independent studies and are severely underpowered; interpret them as exploratory.", call. = FALSE)
  }
  dat$sei <- sqrt(dat$vi)
  dat$variance <- dat$vi
  study_data <- aggregate_by_study(dat, rho = rho)
  rho <- attr(study_data, "meta3_rho", exact = TRUE) %||% rho
  study_data$sei <- sqrt(study_data$vi)
  study_data$variance <- study_data$vi
  regression_data <- if (identical(level, "single")) study_data else dat
  if (length(unique(round(sqrt(regression_data$vi), 12))) < 2) {
    unit <- if (identical(level, "single")) "pooled study-level" else "effect-level"
    stop("Egger/PET-PEESE cannot be fitted because all ", unit,
         " sampling standard errors are identical.", call. = FALSE)
  }
  pet <- .fit_meta(regression_data, mods = ~ sei, level = level, rho = rho,
                    inference = inference)
  peese <- .fit_meta(regression_data, mods = ~ variance, level = level, rho = rho,
                      inference = inference)
  pet_coef <- .coef_table(pet)
  peese_coef <- .coef_table(peese)
  pet_intercept_p <- pet_coef$pval[pet_coef$term == "intrcpt"]
  decision <- if (length(pet_intercept_p) && pet_intercept_p < 0.05) "PEESE" else "PET"
  selected_coef <- if (decision == "PEESE") peese_coef else pet_coef
  selected_effect <- selected_coef[selected_coef$term == "intrcpt", , drop = FALSE]

  base_reml <- metafor::rma(yi = study_data$yi, vi = study_data$vi,
                            method = "REML", test = "knha")
  base_ml <- metafor::rma(yi = study_data$yi, vi = study_data$vi, method = "ML")
  base_reml$meta3_scale <- .scale_of(data)
  base_ml$meta3_scale <- .scale_of(data)
  selection_alternative <- alternative
  if (alternative == "auto") {
    selection_alternative <- if (as.numeric(base_ml$beta[1]) >= 0) "greater" else "less"
  }
  selection_step <- if (selection_alternative == "two.sided") 0.05 else 0.025

  result <- list(
    level = level,
    data = dat,
    study_data = study_data,
    rho = rho,
    alternative = selection_alternative,
    pet = pet,
    peese = peese,
    pet_coefficients = pet_coef,
    peese_coefficients = peese_coef,
    pet_f = moderator_f_test(pet),
    peese_f = moderator_f_test(peese),
    decision = decision,
    selected_effect = selected_effect,
    smd_correlation_warning = .scale_of(data) == "g",
    base_study_reml = base_reml,
    base_study_ml = base_ml
  )

  if (supplementary) {
    report_scale <- .scale_of(data)
    egger_mod <- matrix(study_data$sei, ncol = 1,
                        dimnames = list(NULL, "sei"))
    result$single_egger <- .safe(metafor::rma(
      yi = study_data$yi, vi = study_data$vi,
      mods = egger_mod,
      method = "REML", test = "knha"
    ))
    result$trimfill <- .safe(metafor::trimfill(base_reml))
    result$begg <- .safe(metafor::ranktest(base_reml))
    result$fail_safe_n <- .safe(metafor::fsn(
      x = study_data$yi, vi = study_data$vi, type = "Rosenthal"
    ))
    result$three_psm <- .safe(metafor::selmodel(
      base_ml, type = "stepfun", alternative = selection_alternative,
      steps = selection_step, control = list(optimizer = "nlminb")
    ))

    vw_steps <- c(0.005, 0.01, 0.05, 0.10, 0.25, 0.35, 0.50,
                  0.65, 0.75, 0.90, 0.95, 0.99, 0.995, 1)
    if (selection_alternative == "two.sided") {
      vw_moderate <- c(1, 0.99, 0.95, 0.90, 0.80, 0.75, 0.60,
                       0.60, 0.75, 0.80, 0.90, 0.95, 0.99, 1.00)
      vw_severe <- c(1, 0.99, 0.90, 0.75, 0.60, 0.50, 0.25,
                     0.25, 0.50, 0.60, 0.75, 0.90, 0.99, 1.00)
    } else {
      vw_moderate <- c(1, 0.99, 0.95, 0.80, 0.75, 0.65, 0.60,
                       0.55, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50)
      vw_severe <- c(1, 0.99, 0.90, 0.75, 0.60, 0.50, 0.40,
                     0.35, 0.30, 0.25, 0.10, 0.10, 0.10, 0.10)
    }
    result$vevea_moderate <- .safe(metafor::selmodel(
      base_ml, type = "stepfun", alternative = selection_alternative,
      steps = vw_steps, delta = vw_moderate
    ))
    result$vevea_severe <- .safe(metafor::selmodel(
      base_ml, type = "stepfun", alternative = selection_alternative,
      steps = vw_steps, delta = vw_severe
    ))
    result$vevea_table <- .selection_table(
      base_ml, result$vevea_moderate, result$vevea_severe,
      scale = .scale_of(data)
    )

    if (.require_optional("meta", "Copas selection analysis") &&
        .require_optional("metasens", "Copas selection analysis")) {
      meta_measure <- switch(
        report_scale,
        logor = "OR",
        z = "ZCOR",
        g = "SMD",
        "SMD"
      )
      meta_object <- meta::metagen(
        TE = study_data$yi, seTE = study_data$sei,
        studlab = study_data$studyID, sm = meta_measure,
        common = FALSE, random = TRUE, method.tau = "ML",
        method.random.ci = "HK"
      )
      copas_run <- .safe_capture_warnings(metasens::copas(meta_object, silent = TRUE))
      result$copas <- copas_run$value
      result$copas_warnings <- copas_run$warnings
    } else {
      result$copas <- NULL
    }

    if (selection_alternative != "two.sided" &&
        .require_optional("puniform", "p-uniform analysis")) {
      puniform_side <- if (selection_alternative == "greater") "right" else "left"
      result$puniform <- .safe(puniform::puniform(
        yi = study_data$yi, vi = study_data$vi,
        side = puniform_side, method = "P", plot = FALSE
      ))
    } else {
      result$puniform <- NULL
    }

    study_data$zval <- study_data$yi / study_data$sei
    study_data$p_two <- 2 * stats::pnorm(abs(study_data$zval), lower.tail = FALSE)
    significant_direction <- if (selection_alternative == "less") {
      study_data$yi < 0 & study_data$p_two < 0.05
    } else if (selection_alternative == "two.sided") {
      study_data$p_two < 0.05
    } else {
      study_data$yi > 0 & study_data$p_two < 0.05
    }
    significant_p <- study_data$p_two[significant_direction]
    result$pcurve <- if (length(significant_p) >= 2) {
      conditional_p <- significant_p / 0.05
      statistic <- -2 * sum(log(conditional_p))
      data.frame(
        significant_results = length(significant_p),
        chi_square = statistic,
        df = 2 * length(significant_p),
        p_right_skew = stats::pchisq(statistic, 2 * length(significant_p),
                                     lower.tail = FALSE)
      )
    } else {
      data.frame(significant_results = length(significant_p),
                 chi_square = NA, df = NA, p_right_skew = NA)
    }
    result$zcurve <- NULL
    result$zcurve_note <- NULL
    significant_z <- abs(study_data$zval[significant_direction])
    if (length(significant_z) >= 10 && .require_optional("zcurve", "z-curve analysis")) {
      result$zcurve <- .safe(zcurve::zcurve(
        z = significant_z, method = "EM", bootstrap = FALSE
      ))
    } else {
      result$zcurve_note <- paste0(
        "z-curve not estimated: ", length(significant_z),
        " significant study-level z statistics; at least 10 are required."
      )
    }

    result$study_reported_effects <- do.call(rbind, list(
      .reported_model_effect(base_reml, report_scale, "Unadjusted study-level REML"),
      .reported_model_effect(result$single_egger, report_scale, "Single-level Egger intercept"),
      .reported_model_effect(result$trimfill, report_scale, "Trim-and-fill"),
      .reported_model_effect(result$three_psm, report_scale, "Three-parameter selection model"),
      .reported_model_effect(result$vevea_moderate, report_scale, "Vevea-Woods moderate"),
      .reported_model_effect(result$vevea_severe, report_scale, "Vevea-Woods severe")
    ))
  }

  class(result) <- "meta3_publication_bias"
  result
}

#' @export
print.meta3_publication_bias <- function(x, ...) {
  .section(if (identical(x$level, "single")) "SINGLE-LEVEL EGGER / PET" else "MULTILEVEL EGGER / PET")
  .print_inference(x$pet)
  cat("\nMetafor moderator F test:\n")
  print(x$pet_f, row.names = FALSE)
  .section(if (identical(x$level, "single")) "SINGLE-LEVEL PEESE" else "MULTILEVEL PEESE")
  .print_inference(x$peese)
  cat("\nMetafor moderator F test:\n")
  print(x$peese_f, row.names = FALSE)
  .section("PET-PEESE CORRECTED EFFECTS")
  cat("Decision rule selects:", x$decision, "\n\n")
  cat("Selected corrected effect on the reported scale:\n")
  print(x$selected_effect, row.names = FALSE)
  if (isTRUE(x$smd_correlation_warning)) {
    cat("\nCaution: for standardized mean differences, g is mechanically related to its sampling SE/variance; Egger and PET-PEESE slopes can partly reflect this mathematical association.\n")
  }
  cat("PET coefficients:\n")
  print(x$pet_coefficients, row.names = FALSE)
  cat("\nPEESE coefficients:\n")
  print(x$peese_coefficients, row.names = FALSE)
  .section("STUDY-LEVEL SUPPLEMENTARY ANALYSES")
  cat("Independent studies =", nrow(x$study_data),
      "; assumed rho =", x$rho,
      "; selection direction =", x$alternative, "\n")
  if (!is.null(x$study_reported_effects)) {
    cat("\nStudy-level pooled/intercept effects on the reported scale:\n")
    print(x$study_reported_effects, row.names = FALSE)
  }
  if (!is.null(x$single_egger)) {
    cat("\nSingle-level study-level Egger test:\n")
    .print_safe(x$single_egger)
  }
  if (!is.null(x$trimfill)) {
    cat("\nTrim-and-fill (supplementary):\n")
    .print_safe(x$trimfill)
  }
  if (!is.null(x$begg)) {
    cat("\nBegg-Mazumdar test:\n")
    .print_safe(x$begg)
  }
  if (!is.null(x$three_psm)) {
    cat("\nThree-parameter selection model:\n")
    .print_safe(x$three_psm)
  }
  if (!is.null(x$vevea_table)) {
    cat("\nVevea-Woods sensitivity table:\n")
    print(x$vevea_table, row.names = FALSE)
  }
  if (!is.null(x$copas)) {
    cat("\nCopas selection model:\n")
    .print_safe(x$copas)
    if (length(x$copas_warnings)) {
      cat("Copas numerical warnings captured:", length(x$copas_warnings), "\n")
    }
  }
  if (!is.null(x$puniform)) {
    cat("\np-uniform (exploratory under heterogeneity):\n")
    .print_safe(x$puniform)
  }
  if (!is.null(x$fail_safe_n)) {
    cat("\nRosenthal fail-safe N (supplementary only):\n")
    .print_safe(x$fail_safe_n)
  }
  if (!is.null(x$pcurve)) {
    cat("\np-curve right-skew test (exploratory):\n")
    print(x$pcurve, row.names = FALSE)
  }
  if (!is.null(x$zcurve)) {
    cat("\nz-curve (exploratory):\n")
    .print_safe(x$zcurve)
  } else if (!is.null(x$zcurve_note)) {
    cat("\n", x$zcurve_note, "\n", sep = "")
  }
  invisible(x)
}
