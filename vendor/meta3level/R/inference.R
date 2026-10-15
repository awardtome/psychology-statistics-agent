# Shared model preparation and inference -----------------------------------

.model_rows <- function(data, mods = NULL) {
  data <- .complete_meta_rows(data)
  if (is.null(mods)) return(data)
  .assert_columns(data, all.vars(mods))
  frame <- stats::model.frame(mods, data = data, na.action = stats::na.pass)
  keep <- stats::complete.cases(frame)
  for (column in frame) {
    if (is.numeric(column)) {
      finite <- is.finite(column)
      if (is.matrix(finite)) finite <- rowSums(!finite) == 0
      keep <- keep & finite
    }
  }
  if (any(!keep)) {
    warning(sum(!keep), " row(s) with missing or non-finite moderator values were omitted before fitting.",
            call. = FALSE)
  }
  .complete_meta_rows(data[keep, , drop = FALSE])
}

.apply_inference <- function(model, inference = c("model", "CR2")) {
  inference <- match.arg(inference)
  model$meta3_inference <- inference
  if (identical(inference, "model")) return(model)
  if (!requireNamespace("clubSandwich", quietly = TRUE)) {
    stop("inference = 'CR2' requires the optional package 'clubSandwich'.",
         call. = FALSE)
  }
  cluster <- model$meta3_data$studyID
  if (length(cluster) != model$k) {
    stop("Model data and CR2 clusters are not aligned.", call. = FALSE)
  }
  # metafor's public adapter updates coefficient tests, CIs, omnibus F tests,
  # and predictions together. Keep the ordinary table for matched comparison.
  model$meta3_model_coefficients <- stats::coef(summary(model))
  model$meta3_model_f <- list(QM = model$QM, QMdf = model$QMdf, QMp = model$QMp)
  model <- metafor::robust(model, cluster = cluster, clubSandwich = TRUE,
                           vcov = "CR2", coef_test = "Satterthwaite",
                           conf_test = "Satterthwaite", wald_test = "HTZ")
  if (any(!is.finite(model$ddf) | model$ddf <= 0) ||
      any(!is.finite(model$pval))) {
    stop("CR2 coefficient inference is not estimable for this study/design configuration.",
         call. = FALSE)
  }
  if (length(model$QMdf) < 2 || !is.finite(model$QMdf[2]) ||
      model$QMdf[2] <= 0 || !is.finite(model$QMp)) {
    stop("CR2 omnibus F inference is not estimable for this study/design configuration.",
         call. = FALSE)
  }
  if (any(model$ddf < 2) || model$QMdf[2] < 2) {
    warning("CR2 has fewer than two denominator degrees of freedom; inference is highly unstable.",
            call. = FALSE)
  }
  model
}

.fit_meta <- function(data, mods = NULL, method = "REML", tdist = TRUE,
                      level = c("three", "single"), rho = 0.60,
                      inference = c("model", "CR2"),
                      warn_few_studies = FALSE, aggregate = FALSE, ...) {
  level <- match.arg(level)
  inference <- match.arg(inference)
  args <- c(list(data = data, mods = mods, method = method, tdist = tdist,
                 inference = inference, warn_few_studies = warn_few_studies),
            list(...))
  if (identical(level, "single")) {
    args$rho <- rho
    args$aggregate <- aggregate
    return(do.call(fit_single_level, args))
  }
  do.call(fit_three_level, args)
}

.print_inference <- function(model) {
  cat("Inference:", if (identical(model$meta3_inference, "CR2")) {
    "CR2 by study; Satterthwaite t/CI and HTZ omnibus F"
  } else {
    paste("model-based", model$test, "tests")
  }, "\n")
  print(.coef_table(model), row.names = FALSE)
}
