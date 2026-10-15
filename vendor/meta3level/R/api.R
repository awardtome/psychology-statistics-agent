# Short, package-specific public API ---------------------------------------

#' Read a meta-analysis data file
#'
#' @param file Path to a CSV, TSV, XLSX, or XLS file.
#' @param sheet Excel sheet name or index.
#' @param encoding Delimited-file encoding; `"auto"` also tries common
#'   Chinese encodings.
#' @param decimal Decimal mark for delimited files.
#' @param ... Additional arguments passed to the file reader.
#' @return A data frame.
m3read <- function(file, sheet = 1, encoding = "auto", decimal = ".", ...) {
  out <- read_meta_data(path = file, sheet = sheet, encoding = encoding,
                        decimal = decimal, ...)
  attr(out, "meta3_source") <- list(
    file = file,
    sheet = sheet,
    encoding = attr(out, "meta3_encoding", exact = TRUE) %||% encoding,
    separator = attr(out, "meta3_separator", exact = TRUE),
    decimal = decimal,
    name_map = attr(out, "meta3_name_map", exact = TRUE)
  )
  out
}

#' Prepare effect sizes
#'
#' @param data Input data frame.
#' @param measure Effect measure: `"r"`, `"d"`, `"g"`, `"or"`, or `"custom"`.
#' @param study Study identifier column.
#' @param effect Optional effect identifier column. When omitted, stable unique
#'   IDs are generated from the original row numbers.
#' @param value Effect-size column; omit it when OR is calculated from cells.
#' @param n, n1, n2 Sample-size columns.
#' @param vi Known sampling-variance column.
#' @param se Standard-error column for log(OR).
#' @param lower,upper OR confidence-limit columns.
#' @param cellA,cellB,cellC,cellD Two-by-two table columns: event/non-event in
#'   group 1 followed by event/non-event in group 2.
#' @param conf Confidence level for OR limits.
#' @param correction Continuity correction for a table containing a zero cell.
#' @param design `"independent"` or `"onegroup"`.
#' @param variance `"known"` or the explicit one-group fallback
#'   `"approximate"`.
#' @param decimal Decimal mark used in text-form numeric cells.
#' @return Prepared effect-size data.
m3prep <- function(data,
                   measure = c("r", "d", "g", "or", "custom"),
                   study,
                   effect = NULL,
                   value = NULL,
                   n = NULL,
                   n1 = NULL,
                   n2 = NULL,
                   vi = NULL,
                   se = NULL,
                   lower = NULL,
                   upper = NULL,
                   cellA = NULL,
                   cellB = NULL,
                   cellC = NULL,
                   cellD = NULL,
                   conf = 0.95,
                   correction = 0.5,
                   design = c("independent", "onegroup"),
                   variance = c("known", "approximate"),
                   decimal = NULL) {
  if (length(measure) == 1) measure <- tolower(measure)
  measure <- match.arg(measure)
  design <- match.arg(design)
  variance <- match.arg(variance)
  if (is.null(decimal)) {
    source <- attr(data, "meta3_source", exact = TRUE)
    decimal <- source$decimal %||% "."
  }
  out <- prepare_effects(
    data = data,
    measure = measure,
    study = study,
    effect = effect,
    effect_size = value,
    n = n,
    n1 = n1,
    n2 = n2,
    vi = vi,
    se = se,
    lower = lower,
    upper = upper,
    cell_a = cellA,
    cell_b = cellB,
    cell_c = cellC,
    cell_d = cellD,
    conf = conf,
    correction = correction,
    design = if (identical(design, "independent")) "independent_groups" else "one_group",
    variance_method = if (identical(variance, "known")) "known" else "legacy_one_group",
    decimal = decimal
  )
  attr(out, "meta3_prep") <- list(
    measure = measure, study = study, effect = effect, value = value,
    n = n, n1 = n1, n2 = n2, vi = vi, se = se,
    lower = lower, upper = upper,
    cellA = cellA, cellB = cellB, cellC = cellC, cellD = cellD,
    conf = conf, correction = correction,
    design = design, variance = variance, decimal = decimal,
    source = attr(data, "meta3_source", exact = TRUE)
  )
  attr(out, "meta3_prep_signature") <- .data_signature(out)
  out
}

#' Fit the three-level model
#'
#' @param data Prepared effect-size data.
#' @param mods Optional moderator formula or character vector.
#' @param method Estimation method.
#' @param f Use metafor t/F inference.
#' @param sigma2 Fixed/free variance-component vector.
#' @param warn Warn when fewer than ten studies are available.
#' @param level `"three"` or `"single"`.
#' @param inference Model-based inference or study-clustered CR2 (Satterthwaite t/CI and HTZ F).
#' @param rho Sampling correlation used to pool dependent effects in a
#'   single-level model.
#' @param ... Additional arguments passed to `metafor::rma.mv()`.
#' @return A fitted `rma.mv` model.
m3fit <- function(data, mods = NULL, method = "REML", f = TRUE,
                  sigma2 = c(NA, NA), warn = TRUE,
                  level = c("three", "single"), rho = 0.60, ...,
                  inference = c("model", "CR2")) {
  level <- match.arg(level)
  inference <- match.arg(inference)
  if (identical(inference, "CR2") && !isTRUE(f)) {
    stop("inference = 'CR2' requires f = TRUE.", call. = FALSE)
  }
  dots <- list(...)
  protected <- if (identical(level, "single")) {
    c("yi", "vi", "mods", "method", "test", "data", "rho", "aggregate", "subset", "inference")
  } else {
    c("yi", "V", "vi", "mods", "random", "method", "tdist", "test", "sigma2", "data", "subset", "inference")
  }
  .validate_model_dots(dots, protected)
  out <- if (identical(level, "single")) {
    if (length(sigma2) != 2 || any(!is.na(sigma2))) {
      stop("'sigma2' applies only to level='three'.", call. = FALSE)
    }
    do.call(fit_single_level, c(list(
      data = data, mods = mods, method = method, tdist = f,
      rho = rho, warn_few_studies = warn, inference = inference
    ), dots))
  } else {
    do.call(fit_three_level, c(list(
      data = data, mods = mods, method = method, tdist = f,
      sigma2 = sigma2, warn_few_studies = warn, inference = inference
    ), dots))
  }
  attr(out, "meta3_audit") <- list(type = "fit", mods = mods, method = method,
                                    f = f, sigma2 = sigma2,
                                    level = level, inference = inference, dots = dots,
                                    rho = if (identical(level, "single")) out$meta3_rho else rho)
  out
}

#' Extract the overall effect
#' @param model A fitted three-level model.
#' @param back Transform Fisher's z back to r.
#' @return Overall-effect table.
m3effect <- function(model, back = TRUE) {
  if (!is.null(model$meta3_mods) && !"intrcpt" %in% rownames(model$beta)) {
    stop("m3effect() requires an intercept. Use the coefficients or group_effects for a no-intercept model.",
         call. = FALSE)
  }
  if (!is.null(model$p) && model$p > 1) {
    warning("The returned first coefficient is the model intercept, not an unconditional overall effect.",
            call. = FALSE)
  }
  overall_effect(model, transform = back)
}

#' Extract the moderator F test
#' @param model A fitted moderator model.
#' @return F-test table.
m3ftest <- function(model) moderator_f_test(model)

#' Calculate multilevel I-squared
#' @param model A fitted three-level model.
#' @return Multilevel heterogeneity table.
m3i2 <- function(model) {
  if (identical(model$meta3_level, "single") || inherits(model, "rma.uni")) {
    single_level_heterogeneity(model)
  } else {
    multilevel_i2(model)
  }
}

#' Run one-sided variance likelihood-ratio tests
#' @param model A fitted three-level model.
#' @param data Prepared data used by the model.
#' @return Likelihood-ratio test table.
m3lrt <- function(model, data = model$meta3_data) {
  if (identical(model$meta3_level, "single") || inherits(model, "rma.uni")) {
    single_level_lrt(model, data)
  } else {
    variance_lrt(model, data)
  }
}

#' Analyse a continuous moderator
#' @param data Prepared effect-size data.
#' @param var Moderator column.
#' @param name Display name.
#' @param method Estimation method.
#' @param level `"three"` or `"single"`.
#' @param inference Model-based inference or study-clustered CR2 (Satterthwaite t/CI and HTZ F).
#' @param rho Sampling correlation used for single-level pooling.
#' @return Continuous-moderator result.
m3cont <- function(data, var, name = var, method = "REML",
                   level = c("three", "single"), rho = 0.60,
                   inference = c("model", "CR2")) {
  level <- match.arg(level)
  inference <- match.arg(inference)
  out <- moderate_continuous(data, moderator = var, label = name,
                             method = method, level = level, rho = rho,
                             inference = inference)
  attr(out, "meta3_audit") <- list(
    type = "cont", cont = list(list(column = var, label = name,
                                     method = method)),
    level = level, rho = out$rho, inference = inference
  )
  out
}

#' Analyse a categorical moderator
#' @param data Prepared effect-size data.
#' @param var Moderator column.
#' @param ref Reference category.
#' @param name Display name.
#' @param method Estimation method.
#' @param level `"three"` or `"single"`.
#' @param inference Model-based inference or study-clustered CR2 (Satterthwaite t/CI and HTZ F).
#' @param rho Sampling correlation used for single-level pooling.
#' @return Categorical-moderator result with intercept and no-intercept models.
m3group <- function(data, var, ref, name = var, method = "REML",
                    level = c("three", "single"), rho = 0.60,
                    inference = c("model", "CR2")) {
  level <- match.arg(level)
  inference <- match.arg(inference)
  out <- moderate_categorical(data, moderator = var, reference = ref, label = name,
                              method = method, level = level, rho = rho,
                              inference = inference)
  attr(out, "meta3_audit") <- list(
    type = "group", groups = list(list(column = var, reference = ref,
                                        label = name, method = method)),
    level = level, rho = out$rho, inference = inference
  )
  out
}

#' Compare linear and spline moderators
#' @param data Prepared effect-size data.
#' @param var Moderator column.
#' @param df Natural-spline degrees of freedom.
#' @param linear Include a conventional linear model.
#' @param name Display name.
#' @param level `"three"` or `"single"`.
#' @param inference Model-based inference or study-clustered CR2 (Satterthwaite t/CI and HTZ F).
#' @param rho Sampling correlation used for single-level pooling.
#' @return ML spline-comparison result.
m3spline <- function(data, var, df = 1:3, linear = TRUE, name = var,
                     level = c("three", "single"), rho = 0.60,
                     inference = c("model", "CR2")) {
  level <- match.arg(level)
  inference <- match.arg(inference)
  out <- compare_splines(data, moderator = var, dfs = df,
                         include_linear = linear, label = name,
                         level = level, rho = rho, inference = inference)
  attr(out, "meta3_audit") <- list(
    type = "spline", spline = list(list(column = var, dfs = df,
                                         include_linear = linear, label = name)),
    level = level, rho = out$rho, inference = inference
  )
  out
}

#' Aggregate dependent effects by study
#' @param data Prepared effect-size data.
#' @param rho Assumed within-study sampling correlation.
#' @param keep Study-level columns to preserve during pooling.
#' @return One row per study.
m3study <- function(data, rho = 0.60, keep = character()) {
  if (!is.numeric(rho) || length(rho) != 1 || !is.finite(rho) ||
      rho < 0 || rho >= 1) {
    stop("'rho' must be a single number in [0, 1).", call. = FALSE)
  }
  if (any(table(.complete_meta_rows(data)$studyID) > 1)) {
    warning("Effects are pooled using assumed rho = ", rho,
            ". Use this only for effects targeting the same construct/comparison, and repeat plausible rho values as a sensitivity check.",
            call. = FALSE)
  }
  aggregate_by_study(data, rho = rho, keep = keep)
}

#' Run publication-bias diagnostics
#' @param data Prepared effect-size data.
#' @param rho Assumed within-study sampling correlation.
#' @param extra Run supplementary study-level methods.
#' @param direction Selection direction.
#' @param level `"three"` or `"single"`.
#' @param inference Model-based inference or study-clustered CR2 (Satterthwaite t/CI and HTZ F).
#' @return Publication-bias result.
m3bias <- function(data, rho = 0.60, extra = TRUE,
                   direction = c("auto", "greater", "less", "two.sided"),
                   level = c("three", "single"), inference = c("model", "CR2")) {
  direction <- match.arg(direction)
  level <- match.arg(level)
  inference <- match.arg(inference)
  out <- publication_bias(
    data, rho = rho, supplementary = extra,
    alternative = direction, level = level, inference = inference
  )
  attr(out, "meta3_audit") <- list(
    type = "bias", bias = TRUE, rho = out$rho,
    extra = extra, direction = direction, level = level, inference = inference
  )
  out
}

#' Run a leave-one-out analysis
#' @param data Prepared effect-size data.
#' @param by Omit one effect or one study at a time.
#' @param method Estimation method.
#' @param level `"three"` or `"single"`.
#' @param inference Model-based inference or study-clustered CR2 (Satterthwaite t/CI and HTZ F).
#' @param rho Sampling correlation used for single-level pooling.
#' @return Leave-one-out result.
m3leave <- function(data, by = c("effect", "study"), method = "REML",
                    level = c("three", "single"), rho = 0.60,
                    inference = c("model", "CR2")) {
  by <- match.arg(by)
  level <- match.arg(level)
  inference <- match.arg(inference)
  out <- leave_one_out(data, unit = by, method = method,
                       level = level, rho = rho, inference = inference)
  attr(out, "meta3_audit") <- list(
    type = "leave", leave = TRUE, leaveby = by, method = method,
    level = level, rho = out$rho, inference = inference
  )
  out
}

.m3specs <- function(x, type) {
  if (is.null(x)) return(NULL)
  if (is.character(x)) {
    if (!identical(type, "cont")) {
      stop("Group and spline specifications must include their settings.", call. = FALSE)
    }
    x <- lapply(x, function(var) list(column = var, label = var))
  }
  if (!is.list(x)) stop("Specifications must be column names or lists.", call. = FALSE)

  keys <- c("var", "column", "ref", "reference", "df", "dfs")
  if (!is.null(names(x)) && any(names(x) %in% keys)) x <- list(x)
  if (any(!vapply(x, is.list, logical(1)))) {
    stop("Each specification must be a list.", call. = FALSE)
  }

  out <- lapply(x, function(spec) {
    column <- spec$column %||% spec$var
    if (!is.character(column) || length(column) != 1 || is.na(column) ||
        !nzchar(trimws(column))) {
      stop("Every ", type, " specification requires one non-empty 'var'.",
           call. = FALSE)
    }
    spec$column <- trimws(column)
    label <- spec$label %||% spec$name %||% spec$column
    if (!is.character(label) || length(label) != 1 || is.na(label) ||
        !nzchar(trimws(label))) {
      stop("Every ", type, " specification requires one non-empty display name.",
           call. = FALSE)
    }
    spec$label <- trimws(label)
    spec$var <- NULL
    spec$name <- NULL
    if (identical(type, "group")) {
      reference <- spec$reference %||% spec$ref
      if (length(reference) != 1 || is.na(reference) ||
          !nzchar(trimws(as.character(reference)))) {
        stop("Every group specification requires one non-empty 'ref'.",
             call. = FALSE)
      }
      spec$reference <- gsub(
        "[[:space:]]+", " ", trimws(as.character(reference))
      )
      spec$ref <- NULL
    }
    if (identical(type, "spline")) {
      spec$dfs <- spec$dfs %||% spec$df %||% 1:3
      spec$include_linear <- spec$include_linear %||% spec$linear %||% TRUE
      if (!is.logical(spec$include_linear) || length(spec$include_linear) != 1 ||
          is.na(spec$include_linear)) {
        stop("Every spline specification requires 'linear' to be TRUE or FALSE.",
             call. = FALSE)
      }
      spec$df <- NULL
      spec$linear <- NULL
    }
    spec
  })
  labels <- vapply(out, function(spec) spec$label %||% "", character(1))
  if (anyNA(labels) || any(!nzchar(trimws(labels)))) {
    stop("Every ", type, " specification requires a non-empty variable/display name.",
         call. = FALSE)
  }
  duplicates <- unique(labels[duplicated(labels)])
  if (length(duplicates)) {
    stop("Duplicate ", type, " result name(s): ",
         paste(duplicates, collapse = ", "),
         ". Give each specification a unique 'name'.", call. = FALSE)
  }
  out
}

#' Run the complete workflow
#'
#' `cont` can be a character vector. Use compact specifications such as
#' `list(var = "design", ref = "cross-sectional")` for groups and
#' `list(var = "age", df = 1:3)` for splines.
#'
#' @param data Prepared effect-size data.
#' @param cont Continuous moderator names or specifications.
#' @param groups Categorical moderator specifications.
#' @param spline Spline specifications.
#' @param bias Run publication-bias analyses.
#' @param leave Run both leave-one-out analyses.
#' @param rho Assumed within-study sampling correlation.
#' @param level `"three"` or `"single"`.
#' @param inference Model-based inference or study-clustered CR2 (Satterthwaite t/CI and HTZ F).
#' @param show Print the complete report.
#' @param keep Continue optional analyses when one component fails.
#' @param code Print underlying-package audit code after the results.
#' @return Complete workflow result.
m3run <- function(data, cont = NULL, groups = NULL, spline = NULL,
                  bias = TRUE, leave = TRUE, rho = 0.60,
                  level = c("three", "single"),
                  show = TRUE, keep = TRUE, code = FALSE,
                  inference = c("model", "CR2")) {
  level <- match.arg(level)
  inference <- match.arg(inference)
  contspec <- .m3specs(cont, "cont")
  groupspec <- .m3specs(groups, "group")
  splinespec <- .m3specs(spline, "spline")
  out <- run_meta_workflow(
    data,
    continuous = contspec,
    categorical = groupspec,
    splines = splinespec,
    publication = bias,
    leave_out = leave,
    rho = rho,
    level = level,
    console = FALSE,
    continue_on_error = keep, inference = inference
  )
  actualrho <- out$main$rho
  out <- list(
    main = out$main,
    cont = out$continuous,
    groups = out$categorical,
    spline = out$splines,
    bias = out$publication_bias,
    effectleave = out$leave_one_effect,
    studyleave = out$leave_one_study
  )
  attr(out, "meta3_audit") <- list(
    type = "workflow", cont = contspec, groups = groupspec, spline = splinespec,
    bias = bias, leave = leave, leaveby = "both", rho = actualrho, keep = keep,
    extra = TRUE, direction = "auto", level = level, inference = inference
  )
  class(out) <- "meta3_workflow"
  if (show) print(out)
  if (isTRUE(code)) m3code(out)
  invisible(out)
}

#' Plot a package result
#'
#' When `x` is a complete workflow, use `what = "forest"`, `"cont"`,
#' `"spline"`, `"bias"`, `"effect"`, or `"study"`.
#'
#' @param x A moderator, spline, publication-bias, leave-one-out, or workflow
#'   result.
#' @param what Component to draw from a complete workflow.
#' @param index Component index when several moderators were analysed.
#' @param name Display name or original variable name when several continuous
#'   or spline moderators were analysed. This is an alternative to `index`.
#' @param ... Plot options passed to the corresponding plotting function.
#' @return Plot data or model, invisibly.
m3plot <- function(x, what = NULL, index = 1, name = NULL, ...) {
  if (inherits(x, "meta3_workflow")) {
    if (is.null(what)) {
      stop("For a complete workflow, choose what = 'forest', 'cont', 'spline', 'bias', 'effect', or 'study'.",
           call. = FALSE)
    }
    what <- match.arg(what, c("forest", "cont", "spline", "bias", "effect", "study"))
    if (!is.null(name) && !what %in% c("cont", "spline")) {
      stop("'name' can be used only with what = 'cont' or 'spline'.", call. = FALSE)
    }
    select_component <- function(components, type) {
      if (!length(components)) {
        stop("The workflow contains no ", type, " results.", call. = FALSE)
      }
      if (!is.null(name)) {
        if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
          stop("'name' must be one non-empty result name.", call. = FALSE)
        }
        if (name %in% names(components)) {
          return(components[[name]])
        }
        variables <- vapply(components, function(component) {
          if (is.list(component) && length(component$moderator) == 1 &&
              is.character(component$moderator) && !is.na(component$moderator)) {
            component$moderator
          } else {
            ""
          }
        }, character(1))
        matches <- which(variables == name)
        if (length(matches) == 1) return(components[[matches]])
        if (length(matches) > 1) {
          stop("The variable name '", name, "' matches several ", type,
               " results; select a display name instead: ",
               paste(names(components)[matches], collapse = ", "),
               call. = FALSE)
        }
        available <- names(components)
        aliases <- unique(variables[nzchar(variables) & !variables %in% available])
        if (length(aliases)) available <- c(available, aliases)
        stop("Unknown ", type, " result '", name, "'. Available names: ",
             paste(available, collapse = ", "), call. = FALSE)
      }
      if (!is.numeric(index) || length(index) != 1 || !is.finite(index) ||
          index < 1 || index != as.integer(index) || index > length(components)) {
        stop("'index' must select one available ", type, " result (1-",
             length(components), ").", call. = FALSE)
      }
      components[[as.integer(index)]]
    }
    x <- switch(
      what,
      forest = x$main,
      cont = select_component(x$cont %||% x$continuous, "continuous moderator"),
      spline = select_component(x$spline %||% x$splines, "spline moderator"),
      bias = x$bias %||% x$publication_bias,
      effect = x$effectleave %||% x$leave_one_effect,
      study = x$studyleave %||% x$leave_one_study
    )
  }
  if (inherits(x, "meta3_main") || inherits(x, "rma")) return(plot_main_forest(x, ...))
  if (inherits(x, "meta3_continuous")) return(plot_linear_moderator(x, ...))
  if (inherits(x, "meta3_splines")) return(plot_spline_models(x, ...))
  if (inherits(x, "meta3_publication_bias")) return(plot_contour_funnel(x, ...))
  if (inherits(x, "meta3_leave_one_out")) return(plot_leave_one_out(x, ...))
  if (inherits(x, "meta3_error")) stop(x$message, call. = FALSE)
  stop("m3plot() does not recognise this result object.", call. = FALSE)
}

#' Print a complete package report
#' @param x A result produced by this package.
#' @param code Also print the underlying-package audit code.
#' @param ... Additional print arguments.
#' @return `x`, invisibly.
m3report <- function(x, code = FALSE, ...) {
  print_meta_report(x, ...)
  if (isTRUE(code)) m3code(x)
  invisible(x)
}
