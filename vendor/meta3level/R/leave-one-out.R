.loo_row <- function(model, omitted_id, omitted_effects,
                     remaining_effects, remaining_studies, scale) {
  tab <- as.data.frame(stats::coef(summary(model)))[1, , drop = FALSE]
  data.frame(
    omitted = as.character(omitted_id),
    omitted_effects = omitted_effects,
    remaining_effects = remaining_effects,
    remaining_studies = remaining_studies,
    estimate_analysis = tab$estimate,
    se = tab$se,
    statistic = if ("tval" %in% names(tab)) tab$tval else tab$zval,
    df = if ("df" %in% names(tab)) tab$df else NA_real_,
    p = tab$pval,
    ci_lb_analysis = tab$ci.lb,
    ci_ub_analysis = tab$ci.ub,
    estimate_reported = .transform_value(tab$estimate, scale),
    ci_lb_reported = .transform_value(tab$ci.lb, scale),
    ci_ub_reported = .transform_value(tab$ci.ub, scale),
    error = NA_character_,
    stringsAsFactors = FALSE
  )
}

#' Leave-one-effect-out or leave-one-study-out sensitivity analysis
#'
#' @param data Prepared effect-size data.
#' @param unit `"effect"` or `"study"`.
#' @param method Model estimation method.
#' @param level `"three"` or `"single"`.
#' @param rho Sampling correlation used for single-level pooling.
#' @return A `meta3_leave_one_out` result with a complete results table.
leave_one_out <- function(data, unit = c("effect", "study"), method = "REML",
                          level = c("three", "single"), rho = 0.60,
                          inference = c("model", "CR2")) {
  unit <- match.arg(unit)
  level <- match.arg(level)
  inference <- match.arg(inference)
  dat <- .complete_meta_rows(data)
  scale <- .scale_of(dat)
  if (unit == "effect" && inherits(data, "meta3_study_data")) {
    warning("The input is already pooled to one row per study; leave-one-effect-out now omits pooled study rows, not the original within-study effects.",
            call. = FALSE)
  }
  studies <- .study_count(dat)
  if (unit == "study") {
    minimum <- if (identical(level, "single")) 4 else 3
    if (studies < minimum) {
      stop("Leave-one-study-out with level='", level, "' requires at least ",
           minimum, " independent studies so every reduced model remains identifiable.",
           call. = FALSE)
    }
  }
  fit_model <- function(z) {
    .fit_meta(z, method = method, level = level, rho = rho,
               inference = inference, aggregate = TRUE)
  }
  full <- fit_model(dat)
  if (identical(level, "single")) rho <- full$meta3_rho
  .warn_few_studies(dat, paste0("Leave-one-", unit, "-out analysis"))
  ids <- if (unit == "effect") {
    seq_len(nrow(dat))
  } else {
    unique(as.character(dat$studyID))
  }
  ids <- unique(ids)

  rows <- lapply(ids, function(id) {
    omit <- if (unit == "effect") {
      seq_len(nrow(dat)) == id
    } else {
      as.character(dat$studyID) == id
    }
    display_id <- if (unit == "effect") {
      paste(as.character(dat$studyID[id]), as.character(dat$effectID[id]), sep = "::")
    } else {
      id
    }
    reduced <- droplevels(dat[!omit, , drop = FALSE])
    attr(reduced, "meta3_scale") <- scale
    fitted <- .safe(fit_model(reduced))
    if (inherits(fitted, "meta3_error")) {
      return(data.frame(
        omitted = display_id,
        omitted_effects = sum(omit),
        remaining_effects = nrow(reduced),
        remaining_studies = length(unique(reduced$studyID)),
        estimate_analysis = NA, se = NA, statistic = NA, df = NA,
        p = NA, ci_lb_analysis = NA, ci_ub_analysis = NA,
        estimate_reported = NA, ci_lb_reported = NA, ci_ub_reported = NA,
        error = fitted$message, stringsAsFactors = FALSE
      ))
    }
    .loo_row(
      fitted, display_id, sum(omit), nrow(reduced),
      length(unique(reduced$studyID)), scale
    )
  })
  result <- list(
    level = level,
    rho = rho,
    unit = unit,
    data = dat,
    full_model = full,
    full_effect = overall_effect(full),
    results = do.call(rbind, rows)
  )
  failures <- sum(!is.na(result$results$error))
  if (failures > 0) {
    warning(failures, " leave-one-", unit,
            "-out model(s) could not be fitted. Inspect result$results$error.",
            call. = FALSE)
  }
  class(result) <- "meta3_leave_one_out"
  result
}

#' @export
print.meta3_leave_one_out <- function(x, ...) {
  .section(paste("LEAVE-ONE-", toupper(x$unit), "-OUT", sep = ""))
  cat("Full model: k =", x$full_model$k,
      "; studies =", length(unique(x$data$studyID)), "\n")
  failures <- sum(!is.na(x$results$error))
  if (failures > 0) cat("Failed reduced models =", failures, "\n")
  print(x$results, row.names = FALSE)
  invisible(x)
}
