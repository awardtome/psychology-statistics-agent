#' Aggregate dependent effect sizes to one estimate per study
#'
#' Uses generalized least squares under an assumed common sampling correlation.
#' This is intended for supplementary publication-bias methods that require
#' independent study-level estimates.
#'
#' @param data Prepared effect-size data.
#' @param rho Assumed sampling correlation among effects from the same study.
#' @param keep Study-level moderator columns to retain. A retained variable must
#'   have no more than one non-missing value within each study.
#' @return A study-level data frame.
aggregate_by_study <- function(data, rho = 0.60, keep = character()) {
  if (!is.numeric(rho) || length(rho) != 1 || !is.finite(rho) || rho < 0 || rho >= 1) {
    stop("'rho' must be a single number in [0, 1).", call. = FALSE)
  }
  if (is.null(keep)) keep <- character()
  if (!is.character(keep) || anyNA(keep) || any(!nzchar(trimws(keep)))) {
    stop("'keep' must contain valid column names.", call. = FALSE)
  }
  keep <- unique(keep)
  reserved <- intersect(keep, c("studyID", "effectID", "yi", "vi", "sei", "effects"))
  if (length(reserved)) {
    stop("'keep' cannot include reserved analysis columns: ",
         paste(reserved, collapse = ", "), call. = FALSE)
  }
  input_prepooled <- inherits(data, "meta3_study_data")
  stored_rho <- attr(data, "meta3_rho", exact = TRUE)
  stored_aggregation <- attr(data, "meta3_aggregation", exact = TRUE)
  .assert_columns(data, c("studyID", "effectID", "yi", "vi", keep))
  data <- .complete_meta_rows(data)
  already_pooled <- input_prepooled &&
    !anyDuplicated(as.character(data$studyID))
  if (already_pooled) {
    if (!is.null(stored_rho) && is.finite(stored_rho) &&
        !isTRUE(all.equal(as.numeric(rho), as.numeric(stored_rho)))) {
      warning("The input was already pooled with rho = ", stored_rho,
              "; the requested rho = ", rho,
                " cannot be applied without the original effects. The stored rho is retained.",
                call. = FALSE)
    }
    attr(data, "meta3_rho") <- stored_rho %||% rho
    attr(data, "meta3_aggregation") <- stored_aggregation
    class(data) <- unique(c("meta3_study_data", "meta3_data", class(data)))
    return(data)
  }
  pieces <- split(data, data$studyID, drop = TRUE)
  rows <- lapply(pieces, function(z) {
    V <- outer(sqrt(z$vi), sqrt(z$vi)) * rho
    diag(V) <- z$vi
    Vinv <- tryCatch(solve(V), error = function(e) qr.solve(V))
    one <- rep(1, nrow(z))
    denominator <- as.numeric(crossprod(one, Vinv %*% one))
    if (length(denominator) != 1 || !is.finite(denominator) || denominator <= 0) {
      stop("Study '", as.character(z$studyID[1]),
           "' produced an invalid GLS pooling denominator. Check vi and rho.",
           call. = FALSE)
    }
    estimate <- as.numeric(crossprod(one, Vinv %*% z$yi) / denominator)
    variance <- 1 / denominator
    row <- data.frame(
      studyID = as.character(z$studyID[1]),
      effectID = paste0("study_", as.character(z$studyID[1])),
      yi = estimate,
      vi = variance,
      sei = sqrt(variance),
      effects = nrow(z),
      stringsAsFactors = FALSE
    )
    for (column in keep) {
      values <- z[[column]]
      text <- trimws(as.character(values))
      present <- !.is_declared_missing(text)
      normalized <- gsub("[[:space:]]+", " ", text[present])
      distinct <- unique(normalized)
      if (length(distinct) > 1) {
        numeric_distinct <- .as_numeric(distinct, .decimal_of(data))
        tolerance <- sqrt(.Machine$double.eps) * max(1, abs(numeric_distinct))
        equivalent_numeric <- all(is.finite(numeric_distinct)) &&
          max(abs(numeric_distinct - numeric_distinct[1])) <= max(tolerance)
        if (!equivalent_numeric) {
          stop("Study '", as.character(z$studyID[1]), "' has inconsistent values for '",
               column, "'. A single-level moderator must be study-level before effects are pooled.",
               call. = FALSE)
        }
      }
      row[[column]] <- if (length(distinct)) values[which(present)[1]] else NA
    }
    row
  })
  out <- do.call(rbind, rows)
  out$studyID <- factor(out$studyID)
  out$effectID <- factor(out$effectID)
  attr(out, "meta3_scale") <- .scale_of(data)
  attr(out, "meta3_measure") <- attr(data, "meta3_measure", exact = TRUE)
  attr(out, "meta3_prep") <- attr(data, "meta3_prep", exact = TRUE)
  attr(out, "meta3_source") <- attr(data, "meta3_source", exact = TRUE)
  attr(out, "meta3_decimal") <- attr(data, "meta3_decimal", exact = TRUE)
  attr(out, "meta3_rho") <- rho
  attr(out, "meta3_aggregation") <- data.frame(
    studyID = vapply(pieces, function(z) as.character(z$studyID[1]), character(1)),
    pooled_effects = vapply(pieces, nrow, integer(1)),
    stringsAsFactors = FALSE
  )
  class(out) <- unique(c("meta3_study_data", "meta3_data", class(out)))
  out
}
