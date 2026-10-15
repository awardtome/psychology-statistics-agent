#' Prepare effect sizes for a three-level meta-analysis
#'
#' Converts correlations to Fisher's z, Cohen's d to Hedges' g, odds ratios to
#' log odds ratios, or accepts custom effect sizes and sampling variances.
#'
#' @param data Input data frame.
#' @param measure One of `"r"`, `"d"`, `"g"`, `"or"`, or `"custom"`.
#' @param study Column identifying independent studies or samples.
#' @param effect Optional effect-size identifier column. If omitted, IDs are
#'   generated from the original row numbers.
#' @param effect_size Column containing r, d, g, OR, or a custom effect. It can
#'   be omitted for ORs calculated from a two-by-two table.
#' @param n Total sample-size column for correlations or one-group effects.
#' @param n1,n2 Group sample-size columns for independent-groups SMDs.
#' @param vi Optional known sampling-variance column.
#' @param se Optional standard-error column for log(OR).
#' @param lower,upper Optional lower and upper OR confidence-limit columns.
#' @param cell_a,cell_b,cell_c,cell_d Optional two-by-two table columns where
#'   a/b are event/non-event counts in group 1 and c/d in group 2.
#' @param conf Confidence level of `lower` and `upper`.
#' @param correction Continuity correction added to all four cells of a table
#'   containing at least one zero. Use zero only when all cells are positive.
#' @param design `"independent_groups"` or `"one_group"` for d/g.
#' @param variance_method Variance method for one-group d. The legacy
#'   approximation must be selected explicitly when no known variance exists.
#' @param decimal Decimal mark used in text-form numeric cells.
#' @return A `meta3_data` data frame containing studyID, effectID, yi, vi, and sei.
prepare_effects <- function(data,
                            measure = c("r", "d", "g", "or", "custom"),
                            study,
                            effect = NULL,
                            effect_size = NULL,
                            n = NULL,
                            n1 = NULL,
                            n2 = NULL,
                            vi = NULL,
                            se = NULL,
                            lower = NULL,
                            upper = NULL,
                            cell_a = NULL,
                            cell_b = NULL,
                            cell_c = NULL,
                            cell_d = NULL,
                            conf = 0.95,
                            correction = 0.5,
                            design = c("independent_groups", "one_group"),
                            variance_method = c("known", "legacy_one_group"),
                            decimal = ".") {
  source_info <- attr(data, "meta3_source", exact = TRUE)
  if (length(measure) == 1) measure <- tolower(measure)
  measure <- match.arg(measure)
  design <- match.arg(design)
  variance_method <- match.arg(variance_method)
  if (!is.character(decimal) || length(decimal) != 1 ||
      !decimal %in% c(".", ",")) {
    stop("'decimal' must be '.' or ','.", call. = FALSE)
  }
  if (!is.data.frame(data)) data <- as.data.frame(data, stringsAsFactors = FALSE)
  .validate_column_names(data)

  required <- c(study, effect, effect_size, n, n1, n2, vi, se, lower, upper,
                cell_a, cell_b, cell_c, cell_d)
  .assert_columns(data, required)

  out <- as.data.frame(data, stringsAsFactors = FALSE)
  study_id <- trimws(as.character(out[[study]]))
  effect_id <- if (is.null(effect)) {
    paste0("effect_", seq_len(nrow(out)))
  } else {
    trimws(as.character(out[[effect]]))
  }
  study_id[.is_declared_missing(study_id)] <- NA_character_
  effect_id[.is_declared_missing(effect_id)] <- NA_character_
  out$studyID <- factor(study_id)
  out$effectID <- factor(effect_id)
  raw_effect <- if (!is.null(effect_size)) .as_numeric(out[[effect_size]], decimal) else
    rep(NA_real_, nrow(out))
  if (measure != "or" && is.null(effect_size)) {
    stop("Effect measures r, d, g, and custom require 'effect_size'.", call. = FALSE)
  }
  known_vi <- if (!is.null(vi)) .as_numeric(out[[vi]], decimal) else rep(NA_real_, nrow(out))

  if (measure == "r") {
    if (is.null(n)) stop("Correlation effects require the total sample-size column 'n'.", call. = FALSE)
    ni <- .as_numeric(out[[n]], decimal)
    invalid_r <- is.finite(raw_effect) & abs(raw_effect) >= 1
    invalid_n <- is.finite(ni) & ni <= 3
    if (any(invalid_r)) warning(sum(invalid_r), " correlation(s) outside (-1, 1) were removed.", call. = FALSE)
    if (any(invalid_n)) warning(sum(invalid_n), " correlation effect(s) with n <= 3 were removed.", call. = FALSE)
    valid <- abs(raw_effect) < 1 & ni > 3
    out$yi <- ifelse(valid, atanh(raw_effect), NA_real_)
    out$vi <- ifelse(valid, 1 / (ni - 3), NA_real_)
    scale <- "z"
  } else if (measure == "or") {
    has_value <- !is.null(effect_size)
    has_table <- all(!vapply(list(cell_a, cell_b, cell_c, cell_d), is.null, logical(1)))
    has_any_table <- any(!vapply(list(cell_a, cell_b, cell_c, cell_d), is.null, logical(1)))
    has_vi <- !is.null(vi)
    has_se <- !is.null(se)
    has_ci <- !is.null(lower) || !is.null(upper)
    if (has_any_table && !has_table) {
      stop("OR two-by-two input requires all of 'cell_a', 'cell_b', 'cell_c', and 'cell_d'.", call. = FALSE)
    }
    if (has_value == has_table) {
      stop("For OR, supply either 'effect_size' or all four table cells, but not both.", call. = FALSE)
    }
    if (has_table && (has_vi || has_se || has_ci)) {
      stop("For a two-by-two OR table, do not also supply vi, se, or confidence limits.", call. = FALSE)
    }
    if (has_value && sum(c(has_vi, has_se, has_ci)) != 1) {
      stop("An OR column requires exactly one uncertainty source: vi, se, or both lower and upper confidence limits.", call. = FALSE)
    }
    if (has_ci && (is.null(lower) || is.null(upper))) {
      stop("OR confidence-interval input requires both 'lower' and 'upper'.", call. = FALSE)
    }
    if (!is.numeric(conf) || length(conf) != 1 || !is.finite(conf) || conf <= 0 || conf >= 1) {
      stop("'conf' must be a single confidence level between 0 and 1.", call. = FALSE)
    }
    if (!is.numeric(correction) || length(correction) != 1 || !is.finite(correction) || correction < 0) {
      stop("'correction' must be a single non-negative number.", call. = FALSE)
    }
    if (has_table) {
      cells <- data.frame(
        a = .as_numeric(out[[cell_a]], decimal), b = .as_numeric(out[[cell_b]], decimal),
        c = .as_numeric(out[[cell_c]], decimal), d = .as_numeric(out[[cell_d]], decimal)
      )
      invalid_cells <- apply(cells, 1, function(z) {
        any(!is.finite(z)) || any(z < 0) || any(abs(z - round(z)) > sqrt(.Machine$double.eps)) ||
          sum(z[1:2]) <= 0 || sum(z[3:4]) <= 0
      })
      uninformative <- !invalid_cells &
        ((cells$a == 0 & cells$c == 0) | (cells$b == 0 & cells$d == 0))
      invalid_cells <- invalid_cells | uninformative
      zero_table <- !invalid_cells & apply(cells, 1, function(z) any(z == 0))
      if (any(zero_table) && correction == 0) {
        stop("A two-by-two table contains zero cells; use a positive continuity correction.", call. = FALSE)
      }
      adjusted <- cells
      adjusted[invalid_cells, ] <- NA_real_
      adjusted[zero_table, ] <- adjusted[zero_table, , drop = FALSE] + correction
      out$yi <- with(adjusted, log((a * d) / (b * c)))
      out$vi <- with(adjusted, 1 / a + 1 / b + 1 / c + 1 / d)
      out$or_raw <- exp(out$yi)
      out$or_zero_corrected <- zero_table
      if (any(invalid_cells)) {
        warning(sum(invalid_cells), " invalid two-by-two table(s) were removed.", call. = FALSE)
      }
    } else {
      invalid_or <- is.finite(raw_effect) & raw_effect <= 0
      if (any(invalid_or)) warning(sum(invalid_or), " non-positive OR value(s) were removed.", call. = FALSE)
      out$yi <- ifelse(raw_effect > 0, log(raw_effect), NA_real_)
      if (has_vi) {
        out$vi <- known_vi
      } else if (has_se) {
        log_se <- .as_numeric(out[[se]], decimal)
        out$vi <- ifelse(is.finite(log_se) & log_se > 0, log_se^2, NA_real_)
      } else {
        ci_lower <- .as_numeric(out[[lower]], decimal)
        ci_upper <- .as_numeric(out[[upper]], decimal)
        zcrit <- stats::qnorm(1 - (1 - conf) / 2)
        valid_ci <- ci_lower > 0 & ci_lower <= raw_effect &
          ci_upper >= raw_effect & ci_upper > ci_lower
        out$vi <- ifelse(valid_ci,
                         ((log(ci_upper) - log(ci_lower)) / (2 * zcrit))^2,
                         NA_real_)
      }
      out$or_raw <- raw_effect
      out$or_zero_corrected <- FALSE
    }
    scale <- "logor"
  } else if (measure %in% c("d", "g") && design == "independent_groups") {
    if (measure == "g" && !is.null(vi)) {
      out$yi <- raw_effect
      out$vi <- known_vi
      out$hedges_J <- NA_real_
      scale <- "g"
    } else {
    if (is.null(n1) || is.null(n2)) {
      stop("Independent-groups d/g requires both 'n1' and 'n2'.", call. = FALSE)
    }
    n1i <- .as_numeric(out[[n1]], decimal)
    n2i <- .as_numeric(out[[n2]], decimal)
    invalid_n <- (is.finite(n1i) & n1i <= 1) | (is.finite(n2i) & n2i <= 1)
    if (any(invalid_n)) {
      warning(sum(invalid_n), " independent-group effect(s) with n1 or n2 <= 1 were removed.",
              call. = FALSE)
      n1i[invalid_n] <- NA_real_
      n2i[invalid_n] <- NA_real_
    }
    df <- n1i + n2i - 2
    J <- .hedges_J(df)
    out$yi <- if (measure == "d") J * raw_effect else raw_effect
    converted_known_vi <- if (measure == "d") J^2 * known_vi else known_vi
    fallback_known_vi <- !is.finite(converted_known_vi) | converted_known_vi <= 0
    fallback_eligible <- fallback_known_vi & is.finite(out$yi) &
      is.finite(n1i) & n1i > 1 & is.finite(n2i) & n2i > 1
    if (!is.null(vi) && any(fallback_eligible)) {
      warning(
        sum(fallback_eligible),
        " independent-group d sampling variance(s) were missing or invalid; ",
        "the sample-size SMD variance formula was used for those rows.",
        call. = FALSE
      )
    }
    out$vi <- ifelse(
      is.finite(converted_known_vi) & converted_known_vi > 0,
      converted_known_vi,
      (n1i + n2i) / (n1i * n2i) + out$yi^2 / (2 * (n1i + n2i))
    )
    out$hedges_J <- J
    scale <- "g"
    }
  } else if (measure %in% c("d", "g") && design == "one_group") {
    if (measure == "g" && !is.null(vi)) {
      out$yi <- raw_effect
      out$vi <- known_vi
      out$hedges_J <- NA_real_
      scale <- "g"
    } else {
    if (is.null(n)) stop("One-group d/g requires 'n' unless precomputed g and vi are supplied.", call. = FALSE)
    ni <- .as_numeric(out[[n]], decimal)
    invalid_n <- is.finite(ni) & ni <= 2
    if (any(invalid_n)) {
      warning(sum(invalid_n), " one-group effect(s) with n <= 2 were removed.",
              call. = FALSE)
      ni[invalid_n] <- NA_real_
    }
    df <- ni - 1
    J <- .hedges_J(df)
    out$yi <- if (measure == "d") J * raw_effect else raw_effect
    converted_known_vi <- if (measure == "d") J^2 * known_vi else known_vi
    missing_known <- !is.finite(converted_known_vi) | converted_known_vi <= 0
    if (any(missing_known)) {
      if (variance_method != "legacy_one_group") {
        stop("One-group d/g has missing or invalid sampling variances. Supply valid vi for every retained effect, or explicitly set variance_method='legacy_one_group'.", call. = FALSE)
      }
      warning("Using an approximate one-group variance where vi is unavailable. A pre-post effect normally requires the pre-post correlation or a design-specific variance formula.", call. = FALSE)
      var_d <- 1 / ni + raw_effect^2 / (2 * ni)
      approximate_vi <- if (measure == "d") J^2 * var_d else var_d
      out$vi <- ifelse(missing_known, approximate_vi, converted_known_vi)
    } else {
      out$vi <- converted_known_vi
    }
    out$hedges_J <- J
    scale <- "g"
    }
  } else {
    if (is.null(vi)) stop("Custom effects require a sampling-variance column 'vi'.", call. = FALSE)
    out$yi <- raw_effect
    out$vi <- known_vi
    scale <- "custom"
  }

  if (identical(scale, "g")) {
    unusually_large <- is.finite(out$yi) & abs(out$yi) > 3
    if (any(unusually_large)) {
      warning(
        sum(unusually_large),
        " standardized mean difference(s) have |g| > 3. ",
        "Verify the effect-size definition, group direction, and that SDs rather than SEs were used.",
        call. = FALSE
      )
    }
  }

  out$sei <- sqrt(out$vi)
  input_rows <- nrow(out)
  out <- .complete_meta_rows(out)
  if (!nrow(out)) stop("No valid effect sizes remain after preparation.", call. = FALSE)
  removed <- input_rows - nrow(out)
  if (removed > 0) warning(removed, " row(s) with missing/invalid IDs, effects, or variances were removed.", call. = FALSE)
  id_pairs <- data.frame(
    studyID = as.character(out$studyID),
    effectID = as.character(out$effectID),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(id_pairs)) {
    stop("Each studyID/effectID combination must be unique.", call. = FALSE)
  }
  attr(out, "meta3_scale") <- scale
  attr(out, "meta3_measure") <- measure
  attr(out, "meta3_rows_removed") <- removed
  attr(out, "meta3_source") <- source_info
  attr(out, "meta3_decimal") <- decimal
  class(out) <- c("meta3_data", class(out))
  out
}
