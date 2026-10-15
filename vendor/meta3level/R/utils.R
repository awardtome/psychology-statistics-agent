.assert_columns <- function(data, columns) {
  columns <- unique(columns[!is.na(columns) & nzchar(columns)])
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

.validate_column_names <- function(data) {
  nms <- names(data)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(trimws(nms)))) {
    stop("The data contain blank or missing column names. Repair the header row before analysis.", call. = FALSE)
  }
  duplicated_names <- unique(nms[duplicated(nms)])
  if (length(duplicated_names)) {
    stop("The data contain duplicated column names: ",
         paste(duplicated_names, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.repair_imported_names <- function(data) {
  nms <- names(data)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(trimws(nms)))) {
    stop("The data contain blank or missing column names. Repair the header row before analysis.",
         call. = FALSE)
  }
  duplicated_all <- duplicated(nms) | duplicated(nms, fromLast = TRUE)
  if (!any(duplicated_all)) {
    attr(data, "meta3_name_map") <- data.frame(
      index = seq_along(nms), original = nms, repaired = nms,
      stringsAsFactors = FALSE
    )
    return(data)
  }
  repaired <- nms
  repaired[duplicated_all] <- paste0(nms[duplicated_all], "...",
                                      which(duplicated_all))
  repaired <- make.unique(repaired, sep = "...")
  mapping <- data.frame(
    index = seq_along(nms), original = nms, repaired = repaired,
    stringsAsFactors = FALSE
  )
  names(data) <- repaired
  attr(data, "meta3_name_map") <- mapping
  changed <- mapping[mapping$original != mapping$repaired, , drop = FALSE]
  warning(
    "Duplicated imported column names were repaired using original column positions: ",
    paste0("'", changed$original, "' -> '", changed$repaired, "'",
           collapse = "; "),
    ". Use the repaired names in m3prep() and moderator specifications.",
    call. = FALSE
  )
  data
}

.study_count <- function(data) length(unique(as.character(data$studyID)))

.warn_few_studies <- function(data, context, threshold = 10) {
  studies <- .study_count(data)
  if (studies < threshold) {
    warning(context, " uses only ", studies,
            " independent studies; F-test degrees of freedom and variance components may be unstable.",
            call. = FALSE)
  }
  invisible(studies)
}

.as_numeric <- function(x, decimal = ".") {
  if (!is.character(decimal) || length(decimal) != 1 ||
      !decimal %in% c(".", ",")) {
    stop("'decimal' must be '.' or ','.", call. = FALSE)
  }
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x <- gsub("%", "", x, fixed = TRUE)
  if (decimal == ",") {
    x <- gsub(".", "", x, fixed = TRUE)
    x <- gsub(",", ".", x, fixed = TRUE)
  } else {
    x <- gsub(",", "", x, fixed = TRUE)
  }
  x[x %in% c("", "NA", "N/A", "NaN", "NULL")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

.decimal_of <- function(x) {
  attr(x, "meta3_decimal", exact = TRUE) %||% "."
}

.continuous_values <- function(x, decimal = ".", label = "moderator") {
  original <- trimws(as.character(x))
  present <- !.is_declared_missing(original)
  percent <- present & grepl("%", original, fixed = TRUE)
  plain <- present & !percent
  values <- .as_numeric(x, decimal)
  conversion_failures <- present & is.na(values)
  if (any(conversion_failures)) {
    stop("Continuous moderator '", label, "' contains ",
         sum(conversion_failures), " non-numeric value(s), for example: ",
         paste(utils::head(unique(original[conversion_failures]), 3), collapse = ", "),
         call. = FALSE)
  }
  non_finite <- present & !is.finite(values)
  if (any(non_finite)) {
    stop("Continuous moderator '", label, "' contains ", sum(non_finite),
         " non-finite value(s).", call. = FALSE)
  }

  scale <- "plain"
  normalized <- FALSE
  if (any(percent) && any(plain)) {
    percent_values <- values[percent]
    plain_values <- values[plain]
    percent_valid <- all(percent_values >= 0 & percent_values <= 100)
    plain_proportion <- all(plain_values >= 0 & plain_values <= 1) &&
      any(plain_values > 0 & plain_values < 1)
    plain_percentage <- all(plain_values >= 0 & plain_values <= 100) &&
      any(plain_values > 1)
    if (percent_valid && plain_proportion) {
      values[percent] <- values[percent] / 100
      scale <- "proportion"
      normalized <- TRUE
    } else if (percent_valid && plain_percentage) {
      scale <- "percentage_points"
      normalized <- TRUE
    } else {
      stop("Continuous moderator '", label,
           "' mixes percent-marked and unmarked values on an ambiguous scale. ",
           "Use either proportions from 0 to 1 or percentage points from 0 to 100 consistently.",
           call. = FALSE)
    }
  } else if (any(percent)) {
    scale <- "percentage_points"
  }
  list(values = values, scale = scale, normalized = normalized,
       percent_rows = percent, plain_rows = plain)
}

.is_declared_missing <- function(x) {
  normalized <- toupper(trimws(as.character(x)))
  is.na(x) | normalized %in% c("", "NA", "N/A", "NAN", "NULL", ".")
}

.hedges_J <- function(df) {
  df <- .as_numeric(df)
  out <- rep(NA_real_, length(df))
  valid <- is.finite(df) & df > 1
  out[valid] <- exp(
    lgamma(df[valid] / 2) -
      0.5 * log(df[valid] / 2) -
      lgamma((df[valid] - 1) / 2)
  )
  out
}

.data_signature <- function(data) {
  core <- lapply(as.data.frame(data, stringsAsFactors = FALSE), function(x) {
    if (is.factor(x)) as.character(x) else x
  })
  raw <- serialize(
    list(names = names(data), rows = nrow(data), data = core),
    connection = NULL,
    version = 2
  )
  bytes <- as.double(as.integer(raw))
  index <- seq_along(bytes)
  c(
    bytes = length(bytes),
    sum = sum(bytes) %% 2147483647,
    weighted = sum((index %% 65521) * bytes) %% 2147483647
  )
}

.data_was_modified <- function(data) {
  original <- attr(data, "meta3_prep_signature", exact = TRUE)
  is.null(original) || !isTRUE(all.equal(
    as.numeric(original), as.numeric(.data_signature(data)),
    tolerance = 0, check.attributes = FALSE
  ))
}

.complete_meta_rows <- function(data, extra = character()) {
  special_classes <- intersect(class(data), c("meta3_study_data", "meta3_data"))
  scale <- .scale_of(data)
  measure <- attr(data, "meta3_measure", exact = TRUE)
  prep <- attr(data, "meta3_prep", exact = TRUE)
  source <- attr(data, "meta3_source", exact = TRUE)
  decimal <- attr(data, "meta3_decimal", exact = TRUE)
  rho <- attr(data, "meta3_rho", exact = TRUE)
  aggregation <- attr(data, "meta3_aggregation", exact = TRUE)
  prep_signature <- attr(data, "meta3_prep_signature", exact = TRUE)
  required <- c("studyID", "effectID", "yi", "vi", extra)
  .assert_columns(data, required)
  study_text <- trimws(as.character(data$studyID))
  effect_text <- trimws(as.character(data$effectID))
  keep <- stats::complete.cases(data[, required, drop = FALSE]) &
    !.is_declared_missing(study_text) & !.is_declared_missing(effect_text) &
    is.finite(data$yi) & is.finite(data$vi) & data$vi > 0
  out <- droplevels(data[keep, , drop = FALSE])
  attr(out, "meta3_scale") <- scale
  attr(out, "meta3_measure") <- measure
  attr(out, "meta3_prep") <- prep
  attr(out, "meta3_source") <- source
  attr(out, "meta3_decimal") <- decimal
  attr(out, "meta3_rho") <- rho
  attr(out, "meta3_aggregation") <- aggregation
  attr(out, "meta3_prep_signature") <- prep_signature
  class(out) <- unique(c(special_classes, class(out)))
  out
}

.scale_of <- function(x) {
  scale <- attr(x, "meta3_scale", exact = TRUE)
  if (is.null(scale) && inherits(x, "rma")) scale <- x$meta3_scale
  if (is.null(scale)) scale <- "custom"
  scale
}

.transform_value <- function(x, scale) {
  if (identical(scale, "z")) return(tanh(x))
  if (identical(scale, "logor")) return(exp(x))
  x
}

.scale_label <- function(scale, pooled = FALSE) {
  if (identical(scale, "z")) return(if (pooled) "Pooled correlation (r)" else "Correlation (r)")
  if (identical(scale, "g")) return(if (pooled) "Pooled Hedges' g" else "Hedges' g")
  if (identical(scale, "logor")) return(if (pooled) "Pooled odds ratio (OR)" else "Odds ratio (OR)")
  if (pooled) "Pooled effect" else "Effect size"
}

.coef_table <- function(model, transform = TRUE, all_are_effects = FALSE) {
  tab <- as.data.frame(stats::coef(summary(model)))
  tab$term <- rownames(tab)
  rownames(tab) <- NULL
  scale <- .scale_of(model)
  tab$estimate_analysis <- tab$estimate
  tab$ci_lb_analysis <- tab$ci.lb
  tab$ci_ub_analysis <- tab$ci.ub
  if (transform) {
    exact_effect <- if (all_are_effects) rep(TRUE, nrow(tab)) else tab$term == "intrcpt"
    if (scale %in% c("z", "logor")) {
      tab$estimate_reported <- ifelse(exact_effect, .transform_value(tab$estimate, scale), NA_real_)
      tab$ci_lb_reported <- ifelse(exact_effect, .transform_value(tab$ci.lb, scale), NA_real_)
      tab$ci_ub_reported <- ifelse(exact_effect, .transform_value(tab$ci.ub, scale), NA_real_)
    } else {
      tab$estimate_reported <- tab$estimate
      tab$ci_lb_reported <- tab$ci.lb
      tab$ci_ub_reported <- tab$ci.ub
    }
  }
  tab
}

.reported_model_effect <- function(model, scale, label) {
  if (inherits(model, "meta3_error") || is.null(model)) {
    note <- if (inherits(model, "meta3_error")) model$message else "Not estimated"
    return(data.frame(
      method = label, estimate_analysis = NA_real_, ci_lb_analysis = NA_real_,
      ci_ub_analysis = NA_real_, estimate_reported = NA_real_,
      ci_lb_reported = NA_real_, ci_ub_reported = NA_real_, note = note
    ))
  }
  estimate <- as.numeric(model$beta[1])
  lower <- as.numeric(model$ci.lb[1])
  upper <- as.numeric(model$ci.ub[1])
  data.frame(
    method = label,
    estimate_analysis = estimate,
    ci_lb_analysis = lower,
    ci_ub_analysis = upper,
    estimate_reported = .transform_value(estimate, scale),
    ci_lb_reported = .transform_value(lower, scale),
    ci_ub_reported = .transform_value(upper, scale),
    note = ""
  )
}

.fit_stats <- function(model) {
  ll <- stats::logLik(model)
  p <- attr(ll, "df")
  k <- model$k
  aic <- stats::AIC(model)
  aicc <- if (is.finite(k - p - 1) && k > p + 1) {
    aic + 2 * p * (p + 1) / (k - p - 1)
  } else {
    NA_real_
  }
  data.frame(
    logLik = as.numeric(ll),
    deviance = -2 * as.numeric(ll),
    AIC = aic,
    BIC = stats::BIC(model),
    AICc = aicc,
    parameters = p,
    k = k
  )
}

.safe <- function(expr) {
  tryCatch(expr, error = function(e) {
    structure(list(message = conditionMessage(e)), class = "meta3_error")
  })
}

.safe_capture_warnings <- function(expr) {
  collected <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        collected <<- c(collected, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) structure(list(message = conditionMessage(e)), class = "meta3_error")
  )
  list(value = value, warnings = unique(collected))
}

.print_safe <- function(x) {
  if (inherits(x, "meta3_error")) {
    cat("Not estimated:", x$message, "\n")
  } else if (is.null(x)) {
    cat("Not requested or dependency unavailable.\n")
  } else {
    print(x)
  }
}

.result_or_error <- function(expr, context) {
  tryCatch(
    expr,
    error = function(e) structure(
      list(context = context, message = conditionMessage(e)),
      class = "meta3_error"
    )
  )
}

.section <- function(title) {
  cat("\n", paste(rep("=", 72), collapse = ""), "\n", sep = "")
  cat(title, "\n")
  cat(paste(rep("=", 72), collapse = ""), "\n", sep = "")
}

.require_optional <- function(package, feature) {
  if (!requireNamespace(package, quietly = TRUE)) {
    warning(feature, " requires the optional package '", package, "'.", call. = FALSE)
    return(FALSE)
  }
  TRUE
}

.mods_formula <- function(mods) {
  if (is.null(mods)) return(NULL)
  if (inherits(mods, "formula")) return(mods)
  if (is.character(mods)) {
    if (!length(mods)) return(NULL)
    if (anyNA(mods) || any(!nzchar(mods))) {
      stop("Moderator column names must be non-empty and non-missing.", call. = FALSE)
    }
    # Building a language object preserves spaces, Chinese text, and operators
    # in column names without interpreting them as R expressions.
    terms <- lapply(mods, as.name)
    rhs <- Reduce(function(a, b) call("+", a, b), terms)
    return(stats::as.formula(call("~", rhs), env = parent.frame()))
  }
  stop("'mods' must be NULL, a formula, or a character vector.", call. = FALSE)
}

.validate_model_dots <- function(dots, protected) {
  if (!length(dots)) return(invisible(TRUE))
  dot_names <- names(dots)
  if (is.null(dot_names) || anyNA(dot_names) || any(!nzchar(dot_names))) {
    stop("Additional model arguments in '...' must be named.", call. = FALSE)
  }
  if (anyDuplicated(dot_names)) {
    stop("Additional model arguments must have unique names.", call. = FALSE)
  }
  conflicts <- intersect(dot_names, protected)
  if (length(conflicts)) {
    stop("Do not override core model argument(s) through '...': ",
         paste(unique(conflicts), collapse = ", "),
         ". Use the corresponding m3fit() argument instead.", call. = FALSE)
  }
  invisible(TRUE)
}
