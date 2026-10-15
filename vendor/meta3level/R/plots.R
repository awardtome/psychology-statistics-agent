.prediction_scale <- function(pred, scale) {
  list(
    pred = .transform_value(as.numeric(pred$pred), scale),
    ci.lb = .transform_value(as.numeric(pred$ci.lb), scale),
    ci.ub = .transform_value(as.numeric(pred$ci.ub), scale)
  )
}

#' Draw a forest plot for the main model
#'
#' @param x A main workflow result or fitted metafor model.
#' @param main Plot title.
#' @param ... Additional arguments passed to [metafor::forest()].
#' @return The fitted model, invisibly.
plot_main_forest <- function(x, main = NULL, ...) {
  model <- if (inherits(x, "meta3_main")) x$model else x
  if (!inherits(model, "rma")) stop("'x' must contain a fitted metafor model.", call. = FALSE)
  scale <- .scale_of(model)
  dots <- list(...)
  args <- list(x = model, main = main %||% "Meta-analysis forest plot")
  model_data <- model$meta3_data
  if (is.null(dots$slab) && !is.null(model_data) &&
      "studyID" %in% names(model_data) && nrow(model_data) == model$k) {
    args$slab <- if (identical(model$meta3_level, "single") ||
                     !"effectID" %in% names(model_data)) {
      as.character(model_data$studyID)
    } else {
      paste(as.character(model_data$studyID),
            as.character(model_data$effectID), sep = "::")
    }
  }
  if (scale == "z" && is.null(dots$atransf)) {
    args$atransf <- tanh
    if (is.null(dots$xlab)) args$xlab <- "Correlation (r)"
  } else if (scale == "logor" && is.null(dots$atransf)) {
    args$atransf <- exp
    if (is.null(dots$xlab)) args$xlab <- "Odds ratio (OR)"
  } else if (scale == "g" && is.null(dots$xlab)) {
    args$xlab <- "Hedges' g"
  }
  do.call(metafor::forest, c(args, dots))
  invisible(model)
}

#' Plot a linear continuous moderator
#'
#' @param x A `meta3_continuous` result.
#' @param points Draw observed effects.
#' @param point_col,line_col,ci_col Plot colors.
#' @param xlab,ylab,main Axis labels and title.
#' @param ... Additional graphical parameters passed to [graphics::plot()].
#' @return Prediction data, invisibly.
plot_linear_moderator <- function(x, points = TRUE,
                                  point_col = "gray70", line_col = "black",
                                  ci_col = "gray45", xlab = x$label,
                                  ylab = NULL,
                                  main = paste(x$label, "moderator (linear)"), ...) {
  if (!inherits(x, "meta3_continuous")) stop("'x' must be a meta3_continuous result.", call. = FALSE)
  raw <- x$data$.moderator_raw
  grid <- seq(min(raw), max(raw), length.out = 200)
  newmods <- matrix(grid - x$center, ncol = 1,
                    dimnames = list(NULL, "moderator_c"))
  pred <- metafor::predict.rma(x$model, newmods = newmods)
  scale <- .scale_of(x$model)
  transformed <- .prediction_scale(pred, scale)
  observed <- .transform_value(x$data$yi, scale)
  if (is.null(ylab)) ylab <- .scale_label(scale)
  ylim <- range(c(observed, transformed$ci.lb, transformed$ci.ub), finite = TRUE)
  graphics::plot(raw, observed, type = "n", xlab = xlab, ylab = ylab,
                 main = main, ylim = ylim, ...)
  if (points) graphics::points(raw, observed, pch = 19, col = point_col)
  graphics::lines(grid, transformed$ci.lb, lty = 3, col = ci_col)
  graphics::lines(grid, transformed$ci.ub, lty = 3, col = ci_col)
  graphics::lines(grid, transformed$pred, lwd = 2, col = line_col)
  invisible(data.frame(x = grid, estimate = transformed$pred,
                       ci_lb = transformed$ci.lb, ci_ub = transformed$ci.ub))
}

#' Plot linear and spline moderator fits
#'
#' @param x A `meta3_splines` result.
#' @param models Model names to draw. Defaults to all fitted models.
#' @param colors,line_types Line styles.
#' @param show_ci Name of one model for which confidence bounds are drawn.
#' @param xlab,ylab,main Labels and title.
#' @param ... Additional parameters passed to [graphics::plot()].
#' @return A named list of prediction data, invisibly.
plot_spline_models <- function(x, models = names(x$models),
                               colors = rep("black", length(models)),
                               line_types = seq_along(models),
                               show_ci = x$best_model,
                               xlab = x$label, ylab = NULL,
                               main = paste(x$label, "moderator: model comparison"), ...) {
  if (!inherits(x, "meta3_splines")) stop("'x' must be a meta3_splines result.", call. = FALSE)
  if (!length(models)) stop("At least one fitted model must be selected for plotting.", call. = FALSE)
  unknown <- setdiff(models, names(x$models))
  if (length(unknown)) stop("Unknown model(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  raw <- x$data$.moderator_raw
  colors <- rep_len(colors, length(models))
  line_types <- rep_len(line_types, length(models))
  grid <- seq(min(raw), max(raw), length.out = 300)
  centered_grid <- grid - x$center
  scale <- .scale_of(x$data)
  predictions <- list()
  for (name in models) {
    model <- x$models[[name]]
    if (name == "linear") {
      newmods <- matrix(centered_grid, ncol = 1,
                        dimnames = list(NULL, "moderator_c"))
    } else {
      basis <- x$bases[[name]]
      newmods <- stats::predict(basis, newx = centered_grid)
      colnames(newmods) <- paste0("spline", seq_len(ncol(newmods)))
    }
    pred <- metafor::predict.rma(model, newmods = newmods)
    values <- .prediction_scale(pred, scale)
    predictions[[name]] <- data.frame(x = grid, estimate = values$pred,
                                      ci_lb = values$ci.lb, ci_ub = values$ci.ub)
  }
  observed <- .transform_value(x$data$yi, scale)
  all_y <- c(observed, unlist(lapply(predictions, function(z) c(z$ci_lb, z$ci_ub))))
  if (is.null(ylab)) ylab <- .scale_label(scale)
  graphics::plot(raw, observed, pch = 19, col = "gray75", xlab = xlab,
                 ylab = ylab, main = main, ylim = range(all_y, finite = TRUE), ...)
  if (!is.null(show_ci) && show_ci %in% names(predictions)) {
    graphics::lines(predictions[[show_ci]]$x, predictions[[show_ci]]$ci_lb,
                    lty = 3, col = "gray50")
    graphics::lines(predictions[[show_ci]]$x, predictions[[show_ci]]$ci_ub,
                    lty = 3, col = "gray50")
  }
  for (i in seq_along(models)) {
    z <- predictions[[models[i]]]
    graphics::lines(z$x, z$estimate, col = colors[i], lty = line_types[i], lwd = 2)
  }
  graphics::legend("topright", legend = models, col = colors,
                   lty = line_types, lwd = 2, bty = "n")
  invisible(predictions)
}

#' Plot leave-one-out estimates as a connected line
#'
#' @param x A `meta3_leave_one_out` result.
#' @param ylab,xlab,main Labels and title.
#' @param ... Additional parameters passed to [graphics::plot()].
#' @return The plotted table, invisibly.
plot_leave_one_out <- function(x, ylab = NULL,
                               xlab = paste("Left-out", x$unit, "index"),
                               main = paste0("Leave-one-", x$unit, "-out sensitivity analysis"), ...) {
  if (!inherits(x, "meta3_leave_one_out")) stop("'x' must be a meta3_leave_one_out result.", call. = FALSE)
  tab <- x$results
  if (!any(is.finite(tab$estimate_reported))) {
    stop("No leave-one-out model produced a finite estimate; inspect x$results$error.", call. = FALSE)
  }
  scale <- .scale_of(x$data)
  if (is.null(ylab)) ylab <- .scale_label(scale, pooled = TRUE)
  full <- x$full_effect$estimate_reported[1]
  old <- graphics::par(mar = c(5.5, 6.5, 4.5, 2.0))
  on.exit(graphics::par(old), add = TRUE)
  graphics::plot(seq_len(nrow(tab)), tab$estimate_reported,
                 type = "o", pch = 19, lwd = 1, col = "black",
                 xlab = xlab, ylab = ylab, main = main, ...)
  graphics::abline(h = full, col = "red", lwd = 2)
  invisible(tab)
}

#' Draw a study-level contour-enhanced funnel plot
#'
#' @param x A `meta3_publication_bias` result or prepared effect-size data.
#' @param rho Within-study correlation used when `x` is data.
#' @param main Plot title.
#' @param ... Additional arguments passed to [metafor::funnel()].
#' @return The study-level model, invisibly.
plot_contour_funnel <- function(x, rho = 0.60,
                                main = "Contour-enhanced funnel plot", ...) {
  if (inherits(x, "meta3_publication_bias")) {
    model <- x$base_study_reml
  } else {
    study_data <- aggregate_by_study(x, rho = rho)
    model <- metafor::rma(yi = study_data$yi, vi = study_data$vi,
                          method = "REML")
    model$meta3_scale <- .scale_of(x)
  }
  dots <- list(...)
  scale <- .scale_of(model)
  reference <- 0
  args <- list(
    x = model, level = c(90, 95, 99),
    shade = c("gray92", "gray80", "gray65"),
    pch = 19, col = "black", refline = reference, main = main
  )
  if (scale == "z" && is.null(dots$atransf)) {
    args$atransf <- tanh
    if (is.null(dots$xlab)) args$xlab <- "Correlation (r)"
  } else if (scale == "g" && is.null(dots$xlab)) {
    args$xlab <- "Hedges' g"
  } else if (scale == "logor" && is.null(dots$atransf)) {
    args$atransf <- exp
    if (is.null(dots$xlab)) args$xlab <- "Odds ratio (OR)"
  }
  do.call(metafor::funnel, c(args, dots))
  invisible(model)
}
