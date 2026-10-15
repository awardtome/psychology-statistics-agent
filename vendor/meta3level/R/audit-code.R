.dq <- function(x) {
  if (is.null(x)) return("NULL")
  encodeString(as.character(x), quote = '"')
}

.audit_extra_code <- function(audit) {
  literal <- utils::capture.output(dput(audit$dots %||% list()))
  c("# Preserve the exact additional arguments of the fitted model.",
    paste0("auditExtraArgs <- ", paste(literal, collapse = "\n")))
}

.audit_inference_code <- function(audit) {
  if (!identical(audit$inference, "CR2")) {
    return("applyInference <- function(model, cluster) model")
  }
  c(
    "if (!requireNamespace(\"clubSandwich\", quietly = TRUE)) stop(\"Install clubSandwich for CR2\")",
    "applyInference <- function(model, cluster) {",
    "  metafor::robust(model, cluster = cluster, clubSandwich = TRUE,",
    "    vcov = \"CR2\", coef_test = \"Satterthwaite\",",
    "    conf_test = \"Satterthwaite\", wald_test = \"HTZ\")",
    "}",
    "fitThree <- function(yi, V, ..., data) {",
    "  y <- eval(substitute(yi), data, parent.frame())",
    "  v <- eval(substitute(V), data, parent.frame())",
    "  fit <- metafor::rma.mv(yi = y, V = v, ..., data = data)",
    "  applyInference(fit, data$studyID)",
    "}",
    "fitSingle <- function(yi, vi, ..., data) {",
    "  y <- eval(substitute(yi), data, parent.frame())",
    "  v <- eval(substitute(vi), data, parent.frame())",
    "  fit <- metafor::rma(yi = y, vi = v, ..., data = data)",
    "  applyInference(fit, data$studyID)",
    "}"
  )
}

.audit_fit_name <- function(audit, single = FALSE) {
  if (identical(audit$inference, "CR2")) {
    if (single) "fitSingle" else "fitThree"
  } else {
    if (single) "metafor::rma" else "metafor::rma.mv"
  }
}

.audit_header <- function(title) {
  c("", paste0("# ", paste(rep("=", 68), collapse = "")),
    paste0("# ", title),
    paste0("# ", paste(rep("=", 68), collapse = "")))
}

.audit_prep_code <- function(data) {
  prep <- attr(data, "meta3_prep", exact = TRUE)
  if (is.null(prep)) {
    return(c(
      "# Prepared data were not created with m3prep(), so the original column mapping",
      "# is unavailable. The audit continues from data, which must contain:",
      "# studyID, effectID, yi (analysis-scale effect), and vi (sampling variance).",
      "dat <- data"
    ))
  }
  source <- prep$source
  read <- if (is.null(source)) {
    c("# raw must be the original data frame used in m3prep().", "raw <- raw")
  } else if (grepl("\\.xlsx?$", source$file, ignore.case = TRUE)) {
    c("raw <- readxl::read_excel(",
      paste0("  ", .dq(source$file), ", sheet = ", deparse(source$sheet),
             ", .name_repair = \"minimal\""),
      ")", "raw <- as.data.frame(raw, stringsAsFactors = FALSE)")
  } else if (grepl("\\.csv$", source$file, ignore.case = TRUE)) {
    c("raw <- utils::read.table(",
      paste0("  ", .dq(source$file), ", header = TRUE, sep = ",
             .dq(source$separator %||% ","), ","),
      paste0("  fileEncoding = ", .dq(source$encoding), ", dec = ",
             .dq(source$decimal %||% "."), ", quote = \"\\\"\","),
      "  comment.char = \"\", check.names = FALSE, stringsAsFactors = FALSE,", 
      "  colClasses = \"character\"", ")")
  } else {
    c("raw <- utils::read.delim(",
      paste0("  ", .dq(source$file), ", fileEncoding = ", .dq(source$encoding), ","),
      paste0("  dec = ", .dq(source$decimal %||% "."),
             ", check.names = FALSE, stringsAsFactors = FALSE,"),
      "  colClasses = \"character\"", ")")
  }
  effect_code <- if (is.null(prep$effect)) {
    "effectText <- paste0(\"effect_\", seq_len(nrow(dat)))"
  } else {
    paste0("effectText <- trimws(as.character(dat[[", .dq(prep$effect), "]]))")
  }
  repair_code <- c(
    "duplicateNames <- duplicated(names(raw)) | duplicated(names(raw), fromLast = TRUE)",
    "if (any(duplicateNames)) {",
    "  repairedNames <- names(raw)",
    "  repairedNames[duplicateNames] <- paste0(repairedNames[duplicateNames], \"...\", which(duplicateNames))",
    "  names(raw) <- make.unique(repairedNames, sep = \"...\")",
    "}"
  )
  common <- c(
    repair_code,
    "dat <- raw",
    paste0("studyText <- trimws(as.character(dat[[", .dq(prep$study), "]]))"),
    effect_code,
    "studyText[missingText(studyText)] <- NA_character_",
    "effectText[missingText(effectText)] <- NA_character_",
    "dat$studyID <- factor(studyText)",
    "dat$effectID <- factor(effectText)"
  )
  measure <- prep$measure
  conversion <- switch(
    measure,
    r = c(
      paste0("ri <- num(dat[[", .dq(prep$value), "]])"),
      paste0("ni <- num(dat[[", .dq(prep$n), "]])"),
      "valid <- is.finite(ri) & abs(ri) < 1 & is.finite(ni) & ni > 3",
      "dat$yi <- ifelse(valid, atanh(ri), NA_real_)  # Fisher's z",
      "dat$vi <- ifelse(valid, 1 / (ni - 3), NA_real_)"
    ),
    d = {
      if (identical(prep$design, "independent")) {
        c(
          paste0("di <- num(dat[[", .dq(prep$value), "]])"),
          paste0("n1i <- num(dat[[", .dq(prep$n1), "]])"),
          paste0("n2i <- num(dat[[", .dq(prep$n2), "]])"),
          "df <- n1i + n2i - 2",
          "J <- exp(lgamma(df / 2) - 0.5 * log(df / 2) - lgamma((df - 1) / 2))",
          "dat$yi <- J * di                       # Hedges' g",
          if (is.null(prep$vi))
            "dat$vi <- (n1i + n2i) / (n1i * n2i) + dat$yi^2 / (2 * (n1i + n2i))"
          else c(
            paste0("knownVi <- J^2 * num(dat[[", .dq(prep$vi), "]])"),
            "fallbackVi <- (n1i + n2i) / (n1i * n2i) + dat$yi^2 / (2 * (n1i + n2i))",
            "dat$vi <- ifelse(is.finite(knownVi) & knownVi > 0, knownVi, fallbackVi)"
          )
        )
      } else {
        c(
          paste0("di <- num(dat[[", .dq(prep$value), "]])"),
          paste0("ni <- num(dat[[", .dq(prep$n), "]])"),
          "df <- ni - 1",
          "J <- exp(lgamma(df / 2) - 0.5 * log(df / 2) - lgamma((df - 1) / 2))",
          "dat$yi <- J * di                       # Hedges' g",
          if (!is.null(prep$vi) && identical(prep$variance, "known"))
            paste0("dat$vi <- J^2 * num(dat[[", .dq(prep$vi), "]])")
          else if (!is.null(prep$vi)) c(
            paste0("knownVi <- J^2 * num(dat[[", .dq(prep$vi), "]])"),
            "approxVi <- J^2 * (1 / ni + di^2 / (2 * ni))",
            "dat$vi <- ifelse(is.finite(knownVi) & knownVi > 0, knownVi, approxVi)"
          ) else c(
            "# Explicit approximation requested; a design-specific variance is preferable.",
            "dat$vi <- J^2 * (1 / ni + di^2 / (2 * ni))"
          )
        )
      }
    },
    g = c(
      paste0("dat$yi <- num(dat[[", .dq(prep$value), "]])"),
      if (!is.null(prep$vi)) paste0("dat$vi <- num(dat[[", .dq(prep$vi), "]])") else {
        if (identical(prep$design, "independent")) c(
          paste0("n1i <- num(dat[[", .dq(prep$n1), "]])"),
          paste0("n2i <- num(dat[[", .dq(prep$n2), "]])"),
          "dat$vi <- (n1i + n2i) / (n1i * n2i) + dat$yi^2 / (2 * (n1i + n2i))"
        ) else c(
          paste0("ni <- num(dat[[", .dq(prep$n), "]])"),
          "dat$vi <- 1 / ni + dat$yi^2 / (2 * ni)"
        )
      }
    ),
    or = {
      if (!is.null(prep$cellA)) {
        c(
          paste0("a <- num(dat[[", .dq(prep$cellA), "]])"),
          paste0("b <- num(dat[[", .dq(prep$cellB), "]])"),
          paste0("c <- num(dat[[", .dq(prep$cellC), "]])"),
          paste0("d <- num(dat[[", .dq(prep$cellD), "]])"),
          "invalid <- !is.finite(a) | !is.finite(b) | !is.finite(c) | !is.finite(d) |",
          "  a < 0 | b < 0 | c < 0 | d < 0 |",
          "  abs(a - round(a)) > sqrt(.Machine$double.eps) |",
          "  abs(b - round(b)) > sqrt(.Machine$double.eps) |",
          "  abs(c - round(c)) > sqrt(.Machine$double.eps) |",
          "  abs(d - round(d)) > sqrt(.Machine$double.eps) |",
          "  (a + b) <= 0 | (c + d) <= 0 | (a == 0 & c == 0) | (b == 0 & d == 0)",
          "a[invalid] <- b[invalid] <- c[invalid] <- d[invalid] <- NA_real_",
          "zero <- !invalid & (a == 0 | b == 0 | c == 0 | d == 0)",
          paste0("a[zero] <- a[zero] + ", prep$correction),
          paste0("b[zero] <- b[zero] + ", prep$correction),
          paste0("c[zero] <- c[zero] + ", prep$correction),
          paste0("d[zero] <- d[zero] + ", prep$correction),
          "dat$yi <- log((a * d) / (b * c))       # log(OR)",
          "dat$vi <- 1 / a + 1 / b + 1 / c + 1 / d"
        )
      } else if (!is.null(prep$vi)) {
        c(paste0("ORi <- num(dat[[", .dq(prep$value), "]])"),
          "dat$yi <- log(ORi)",
          paste0("dat$vi <- num(dat[[", .dq(prep$vi), "]])  # variance of log(OR)"))
      } else if (!is.null(prep$se)) {
        c(paste0("ORi <- num(dat[[", .dq(prep$value), "]])"),
          paste0("sei <- num(dat[[", .dq(prep$se), "]])     # SE of log(OR)"),
          "dat$yi <- ifelse(ORi > 0, log(ORi), NA_real_)",
          "dat$vi <- ifelse(is.finite(sei) & sei > 0, sei^2, NA_real_)")
      } else {
        c(paste0("ORi <- num(dat[[", .dq(prep$value), "]])"),
          paste0("lower <- num(dat[[", .dq(prep$lower), "]])"),
          paste0("upper <- num(dat[[", .dq(prep$upper), "]])"),
          paste0("zcrit <- qnorm(1 - (1 - ", prep$conf, ") / 2)"),
          "validCi <- lower > 0 & lower <= ORi & upper >= ORi & upper > lower",
          "dat$yi <- ifelse(ORi > 0, log(ORi), NA_real_)",
          "dat$vi <- ifelse(validCi, ((log(upper) - log(lower)) / (2 * zcrit))^2, NA_real_)")
      }
    },
    custom = c(
      paste0("dat$yi <- num(dat[[", .dq(prep$value), "]])"),
      paste0("dat$vi <- num(dat[[", .dq(prep$vi), "]])")
    )
  )
  c(read, "", common, conversion,
    "dat <- dat[complete.cases(dat[, c(\"studyID\", \"effectID\", \"yi\", \"vi\")]) &",
    "           is.finite(dat$yi) & is.finite(dat$vi) & dat$vi > 0, ]",
    "dat$studyID <- droplevels(dat$studyID)",
    "dat$effectID <- droplevels(dat$effectID)",
    "if (anyDuplicated(data.frame(studyID = as.character(dat$studyID),",
    "                             effectID = as.character(dat$effectID))))",
    "  stop(\"Each studyID/effectID combination must be unique\")",
    "dat$sei <- sqrt(dat$vi)")
}

.audit_main_code <- function(scale, audit = list()) {
  back <- switch(scale, z = "tanh", logor = "exp", "identity")
  method <- audit$method %||% "REML"
  useF <- audit$f %||% TRUE
  sigma2 <- audit$sigma2 %||% c(NA, NA)
  mods <- audit$mods
  modsLine <- if (is.null(mods)) NULL else {
    form <- paste(deparse(.mods_formula(mods)), collapse = " ")
    paste0("  mods = ", form, ",")
  }
  reducedMethod <- .dq(method)
  reducedF <- useF
  c(
    .audit_extra_code(audit),
    "main <- do.call(metafor::rma.mv, c(list(",
    "  yi = dat$yi, V = dat$vi,",
    modsLine,
    "  random = ~ 1 | studyID/effectID,",
    paste0("  method = ", .dq(method), ", tdist = ", useF,
           ", sigma2 = c(", paste(sigma2, collapse = ", "), "), data = dat"),
    "), auditExtraArgs))",
    "main <- applyInference(main, dat$studyID)",
    "print(summary(main))",
    "mainCoef <- as.data.frame(coef(summary(main)))[1, ]",
    paste0("mainReported <- data.frame(estimate = ", back, "(mainCoef$estimate),"),
    paste0("                           lower = ", back, "(mainCoef$ci.lb),"),
    paste0("                           upper = ", back, "(mainCoef$ci.ub))"),
    "print(mainReported)",
    "",
    "# Multilevel I-squared decomposition",
    "w <- 1 / dat$vi",
    "typicalVi <- ((nrow(dat) - 1) * sum(w)) / (sum(w)^2 - sum(w^2))",
    "i2den <- sum(main$sigma2) + typicalVi",
    "I2 <- data.frame(level = c(\"Level 3\", \"Level 2\", \"Total\"),",
    "                 percent = 100 * c(main$sigma2, sum(main$sigma2)) / i2den)",
    "print(I2)",
    "",
    "# One-sided boundary LRT for each variance component",
    paste0("mainSigma <- c(", paste(sigma2, collapse = ", "), ")"),
    "reducedL3 <- if (is.na(mainSigma[1])) do.call(metafor::rma.mv, c(list(yi = dat$yi, V = dat$vi,",
    modsLine,
    "  random = ~ 1 | studyID/effectID,",
    paste0("  sigma2 = replace(mainSigma, 1, 0), method = ", reducedMethod,
           ", tdist = ", reducedF, ", data = dat), auditExtraArgs)) else NULL"),
    "reducedL2 <- if (is.na(mainSigma[2])) do.call(metafor::rma.mv, c(list(yi = dat$yi, V = dat$vi,",
    modsLine,
    "  random = ~ 1 | studyID/effectID,",
    paste0("  sigma2 = replace(mainSigma, 2, 0), method = ", reducedMethod,
           ", tdist = ", reducedF, ", data = dat), auditExtraArgs)) else NULL"),
    "lrt <- function(full, reduced) {",
    "  if (is.null(reduced)) return(c(LRT = NA_real_, pOneSided = NA_real_))",
    "  value <- max(0, 2 * (as.numeric(logLik(full)) - as.numeric(logLik(reduced))))",
    "  c(LRT = value, pOneSided = 0.5 * pchisq(value, 1, lower.tail = FALSE))",
    "}",
    "print(rbind(level3 = lrt(main, reducedL3), level2 = lrt(main, reducedL2)))"
  )
}

.audit_single_setup_code <- function() {
  c(
    "poolStudy <- function(dat, rho = .60, keep = character()) {",
    "  if (!is.numeric(rho) || length(rho) != 1 || !is.finite(rho) || rho < 0 || rho >= 1)",
    "    stop(\"rho must be one number in [0, 1)\")",
    "  parts <- split(dat, dat$studyID, drop = TRUE)",
    "  out <- do.call(rbind, lapply(parts, function(z) {",
    "    V <- outer(sqrt(z$vi), sqrt(z$vi)) * rho",
    "    diag(V) <- z$vi",
    "    W <- tryCatch(solve(V), error = function(e) qr.solve(V)); one <- rep(1, nrow(z))",
    "    den <- drop(t(one) %*% W %*% one)",
    "    if (!is.finite(den) || den <= 0) stop(\"Invalid GLS pooling denominator\")",
    "    row <- data.frame(studyID = as.character(z$studyID[1]),",
    "      effectID = paste0(\"study_\", z$studyID[1]),",
    "      yi = drop(t(one) %*% W %*% z$yi) / den,",
    "      vi = 1 / den, effects = nrow(z), stringsAsFactors = FALSE)",
    "    for (column in keep) {",
    "      text <- trimws(as.character(z[[column]]))",
    "      present <- !is.na(text) & !toupper(text) %in% c(\"\", \"NA\", \"N/A\", \"NAN\", \"NULL\", \".\")",
    "      values <- unique(gsub(\"[[:space:]]+\", \" \", trimws(as.character(z[[column]][present]))))",
    "      numericValues <- suppressWarnings(as.numeric(values))",
    "      equivalentNumeric <- length(values) > 1 && all(is.finite(numericValues)) &&",
    "        max(abs(numericValues - numericValues[1])) <= sqrt(.Machine$double.eps) * max(1, abs(numericValues))",
    "      if (length(values) > 1 && !equivalentNumeric) stop(paste(\"Inconsistent within-study values for\", column))",
    "      row[[column]] <- if (any(present)) z[[column]][which(present)[1]] else NA",
    "    }",
    "    row",
    "  }))",
    "  out$studyID <- factor(out$studyID)",
    "  out$effectID <- factor(out$effectID)",
    "  out",
    "}"
  )
}

.audit_single_main_code <- function(scale, audit = list()) {
  back <- switch(scale, z = "tanh", logor = "exp", "identity")
  method <- audit$method %||% "REML"
  test <- if (isTRUE(audit$f %||% TRUE)) "knha" else "z"
  mods <- audit$mods
  modsLine <- if (is.null(mods)) NULL else {
    form <- paste(deparse(.mods_formula(mods)), collapse = " ")
    paste0("  mods = ", form, ",")
  }
  keep <- if (is.null(mods)) "character()" else {
    paste0("c(", paste(.dq(all.vars(.mods_formula(mods))), collapse = ", "), ")")
  }
  prediction <- if (is.null(mods)) c(
    "prediction <- predict(main)",
    paste0("print(data.frame(piLower = ", back, "(prediction$pi.lb),"),
    paste0("                 piUpper = ", back, "(prediction$pi.ub)))")
  ) else character()
  c(
    .audit_extra_code(audit),
    paste0("rho <- ", audit$rho %||% 0.60),
    paste0("studyDat <- poolStudy(dat, rho, keep = ", keep, ")"),
    "print(studyDat[, c(\"studyID\", \"effects\")])",
    "main <- do.call(metafor::rma, c(list(",
    "  yi = studyDat$yi, vi = studyDat$vi,",
    modsLine,
    paste0("  method = ", .dq(method), ", test = ", .dq(test), ", data = studyDat"),
    "), auditExtraArgs))",
    "main <- applyInference(main, studyDat$studyID)",
    "print(summary(main))",
    "mainCoef <- as.data.frame(coef(summary(main)))[1, ]",
    paste0("mainReported <- data.frame(estimate = ", back, "(mainCoef$estimate),"),
    paste0("                           lower = ", back, "(mainCoef$ci.lb),"),
    paste0("                           upper = ", back, "(mainCoef$ci.ub))"),
    "print(mainReported)",
    "print(data.frame(tau2 = main$tau2, tau = sqrt(main$tau2),",
    "  I2 = main$I2, H2 = main$H2, Q = main$QE, Qdf = main$k - main$p, Qp = main$QEp))",
    prediction,
    "mainML <- do.call(metafor::rma, c(list(yi = studyDat$yi, vi = studyDat$vi,",
    modsLine,
    "  method = \"ML\", data = studyDat), auditExtraArgs))",
    "mainFE <- do.call(metafor::rma, c(list(yi = studyDat$yi, vi = studyDat$vi,",
    modsLine,
    "  method = \"FE\", data = studyDat), auditExtraArgs))",
    "lrtValue <- max(0, 2 * (as.numeric(logLik(mainML)) - as.numeric(logLik(mainFE))))",
    "print(c(LRT = lrtValue, pOneSided = 0.5 * pchisq(lrtValue, 1, lower.tail = FALSE)))"
  )
}

.audit_single_moderator_code <- function(audit) {
  out <- character()
  fit <- .audit_fit_name(audit, single = TRUE)
  rho <- audit$rho %||% 0.60
  if (length(audit$cont)) out <- c(out, "continuousModels <- list()")
  for (z in audit$cont) {
    method <- z$method %||% "REML"
    out <- c(out, .audit_header(paste0("SINGLE-LEVEL CONTINUOUS MODERATOR: ", z$label)),
      paste0("dat$moderatorRaw <- moderatorNum(dat[[", .dq(z$column), "]])"),
      paste0("studyDat <- poolStudy(dat, ", rho, ", keep = \"moderatorRaw\")"),
      "studyDat$moderatorc <- studyDat$moderatorRaw - mean(studyDat$moderatorRaw, na.rm = TRUE)",
      "dmod <- studyDat[complete.cases(studyDat[, c(\"yi\", \"vi\", \"moderatorc\")]), ]",
      paste0("mcont <- ", fit, "(yi, vi, mods = ~ moderatorc,"),
      paste0("  method = ", .dq(method), ", test = \"knha\", data = dmod)"),
      paste0("continuousModels[[", .dq(z$label), "]] <- mcont"),
      "print(summary(mcont))",
      "print(c(F = mcont$QM, df1 = mcont$QMdf[1], df2 = mcont$QMdf[2], p = mcont$QMp))")
  }
  if (length(audit$groups)) out <- c(out, "categoricalModels <- list()")
  for (z in audit$groups) {
    method <- z$method %||% "REML"
    out <- c(out, .audit_header(paste0("SINGLE-LEVEL CATEGORICAL MODERATOR: ", z$label)),
      paste0("dat$moderatorClean <- gsub(\"[[:space:]]+\", \" \", trimws(as.character(dat[[", .dq(z$column), "]])))"),
      "dat$moderatorClean[toupper(dat$moderatorClean) %in% c(\"\", \"NA\", \"N/A\", \"NAN\", \"NULL\", \".\")] <- NA_character_",
      paste0("studyDat <- poolStudy(dat, ", rho, ", keep = \"moderatorClean\")"),
      paste0("studyDat$group <- relevel(factor(studyDat$moderatorClean), ref = ", .dq(z$reference), ")"),
      "dgroup <- studyDat[!is.na(studyDat$group), ]",
      paste0("mgroup <- ", fit, "(yi, vi, mods = ~ group,"),
      paste0("  method = ", .dq(method), ", test = \"knha\", data = dgroup)"),
      paste0("mgroupNoIntercept <- ", fit, "(yi, vi, mods = ~ 0 + group,"),
      paste0("  method = ", .dq(method), ", test = \"knha\", data = dgroup)"),
      paste0("categoricalModels[[", .dq(z$label), "]] <- list(",
             "intercept = mgroup, nointercept = mgroupNoIntercept)"),
      "print(summary(mgroup)); print(summary(mgroupNoIntercept))",
      "print(c(F = mgroup$QM, df1 = mgroup$QMdf[1], df2 = mgroup$QMdf[2], p = mgroup$QMp))")
  }
  for (z in audit$spline) {
    dfs <- paste(z$dfs, collapse = ", ")
    out <- c(out, .audit_header(paste0("SINGLE-LEVEL ML SPLINE COMPARISON: ", z$label)),
      paste0("dat$moderatorRaw <- moderatorNum(dat[[", .dq(z$column), "]])"),
      paste0("studyDat <- poolStudy(dat, ", rho, ", keep = \"moderatorRaw\")"),
      "x <- studyDat$moderatorRaw",
      "dspline <- transform(studyDat, xcenter = x - mean(x, na.rm = TRUE))",
      "dspline <- dspline[complete.cases(dspline[, c(\"yi\", \"vi\", \"xcenter\")]), ]",
      paste0("dfs <- c(", dfs, ")"),
      if (isTRUE(z$include_linear))
        paste0("splineModels <- list(linear = ", fit, "(yi, vi, mods = ~ xcenter, method = \"ML\", test = \"knha\", data = dspline))")
      else "splineModels <- list()",
      "for (df in dfs) {",
      "  basis <- splines::ns(dspline$xcenter, df = df)",
      "  colnames(basis) <- paste0(\"spline\", seq_len(ncol(basis)))",
      "  dfit <- cbind(dspline, basis)",
      "  form <- as.formula(paste(\"~\", paste(colnames(basis), collapse = \" + \")))",
      paste0("  splineModels[[paste0(\"splineDf\", df)]] <- ", fit, "("),
      "    yi, vi, mods = form, method = \"ML\", test = \"knha\", data = dfit)",
      "}",
      "fitTable <- do.call(rbind, lapply(names(splineModels), function(nm) {",
      "  m <- splineModels[[nm]]; ll <- logLik(m); p <- attr(ll, \"df\"); k <- m$k",
      "  aic <- AIC(m); aicc <- aic + 2 * p * (p + 1) / (k - p - 1)",
      "  data.frame(model = nm, logLik = as.numeric(ll), AIC = aic, AICc = aicc, BIC = BIC(m))",
      "}))",
      "print(fitTable)")
  }
  out
}

.audit_moderator_code <- function(audit) {
  out <- character()
  fit <- .audit_fit_name(audit)
  if (length(audit$cont)) out <- c(out, "continuousModels <- list()")
  for (i in seq_along(audit$cont)) {
    z <- audit$cont[[i]]
    method <- z$method %||% "REML"
    out <- c(out, .audit_header(paste0("CONTINUOUS MODERATOR: ", z$label)),
      paste0("mod <- moderatorNum(dat[[", .dq(z$column), "]])"),
      "modc <- mod - mean(mod, na.rm = TRUE)",
      "dmod <- transform(dat, moderatorc = modc)",
      "dmod <- dmod[complete.cases(dmod[, c(\"yi\", \"vi\", \"moderatorc\")]), ]",
      paste0("mcont <- ", fit, "(yi, vi, mods = ~ moderatorc,"),
      paste0("  random = ~ 1 | studyID/effectID, method = ", .dq(method), ","),
      "  tdist = TRUE, data = dmod)",
      paste0("continuousModels[[", .dq(z$label), "]] <- mcont"),
      "print(summary(mcont))                 # omnibus test is F when tdist=TRUE",
      "print(c(F = mcont$QM, df1 = mcont$QMdf[1], df2 = mcont$QMdf[2], p = mcont$QMp))")
  }
  if (length(audit$groups)) out <- c(out, "categoricalModels <- list()")
  for (i in seq_along(audit$groups)) {
    z <- audit$groups[[i]]
    method <- z$method %||% "REML"
    out <- c(out, .audit_header(paste0("CATEGORICAL MODERATOR: ", z$label)),
      paste0("groupRaw <- trimws(as.character(dat[[", .dq(z$column), "]]))"),
      "groupRaw[missingText(groupRaw)] <- NA_character_",
      "group <- factor(gsub(\"[[:space:]]+\", \" \", groupRaw))",
      paste0("group <- relevel(group, ref = ", .dq(z$reference), ")"),
      "dgroup <- transform(dat, group = group)",
      "dgroup <- dgroup[!is.na(dgroup$group), ]",
      paste0("mgroup <- ", fit, "(yi, vi, mods = ~ group,"),
      paste0("  random = ~ 1 | studyID/effectID, method = ", .dq(method), ","),
      "  tdist = TRUE, data = dgroup)",
      paste0("mgroupNoIntercept <- ", fit, "(yi, vi, mods = ~ 0 + group,"),
      paste0("  random = ~ 1 | studyID/effectID, method = ", .dq(method), ","),
      "  tdist = TRUE, data = dgroup)",
      paste0("categoricalModels[[", .dq(z$label), "]] <- list(",
             "intercept = mgroup, nointercept = mgroupNoIntercept)"),
      "print(summary(mgroup))",
      "print(summary(mgroupNoIntercept))",
      "print(c(F = mgroup$QM, df1 = mgroup$QMdf[1], df2 = mgroup$QMdf[2], p = mgroup$QMp))")
  }
  for (i in seq_along(audit$spline)) {
    z <- audit$spline[[i]]
    dfs <- paste(z$dfs, collapse = ", ")
    out <- c(out, .audit_header(paste0("ML SPLINE COMPARISON: ", z$label)),
      paste0("x <- moderatorNum(dat[[", .dq(z$column), "]])"),
      "dspline <- transform(dat, xcenter = x - mean(x, na.rm = TRUE))",
      "dspline <- dspline[complete.cases(dspline[, c(\"yi\", \"vi\", \"xcenter\")]), ]",
      paste0("dfs <- c(", dfs, ")"),
      if (isTRUE(z$include_linear))
        paste0("splineModels <- list(linear = ", fit, "(yi, vi, mods = ~ xcenter, random = ~ 1 | studyID/effectID, method = \"ML\", tdist = TRUE, data = dspline))")
      else "splineModels <- list()",
      "for (df in dfs) {",
      "  basis <- splines::ns(dspline$xcenter, df = df)",
      "  colnames(basis) <- paste0(\"spline\", seq_len(ncol(basis)))",
      "  dfit <- cbind(dspline, basis)",
      "  form <- as.formula(paste(\"~\", paste(colnames(basis), collapse = \" + \")))",
      paste0("  splineModels[[paste0(\"splineDf\", df)]] <- ", fit, "("),
      "    yi, vi, mods = form, random = ~ 1 | studyID/effectID,",
      "    method = \"ML\", tdist = TRUE, data = dfit)",
      "}",
      "fitTable <- do.call(rbind, lapply(names(splineModels), function(nm) {",
      "  m <- splineModels[[nm]]; ll <- logLik(m); p <- attr(ll, \"df\"); k <- m$k",
      "  aic <- AIC(m); aicc <- aic + 2 * p * (p + 1) / (k - p - 1)",
      "  data.frame(model = nm, logLik = as.numeric(ll), AIC = aic, AICc = aicc, BIC = BIC(m))",
      "}))",
      "print(fitTable)")
  }
  out
}

.audit_bias_code <- function(audit, scale = "custom") {
  fit <- .audit_fit_name(audit)
  if (!isTRUE(audit$bias)) return(character())
  rho <- audit$rho
  extra <- audit$extra %||% TRUE
  directionSetting <- audit$direction %||% "auto"
  metaMeasure <- switch(scale, logor = "OR", z = "ZCOR", g = "SMD", "SMD")
  supplementary <- if (!isTRUE(extra)) character() else c(
    "",
    "# GLS aggregation to one independent effect per study",
    paste0("rho <- ", rho),
    "parts <- split(dat, dat$studyID)",
    "studyDat <- do.call(rbind, lapply(parts, function(z) {",
    "  V <- outer(sqrt(z$vi), sqrt(z$vi)) * rho; diag(V) <- z$vi",
    "  W <- solve(V); one <- rep(1, nrow(z)); den <- drop(t(one) %*% W %*% one)",
    "  data.frame(studyID = as.character(z$studyID[1]),",
    "    yi = drop(t(one) %*% W %*% z$yi) / den, vi = 1 / den)",
    "}))",
    "studyModel <- metafor::rma(yi, vi, method = \"REML\", test = \"knha\", data = studyDat)",
    "singleEgger <- metafor::rma(yi, vi, mods = ~ I(sqrt(vi)), method = \"REML\", test = \"knha\", data = studyDat)",
    "trimfillModel <- metafor::trimfill(studyModel)",
    "print(summary(singleEgger)); print(trimfillModel)",
    "print(metafor::ranktest(studyModel))",
    "print(metafor::fsn(studyDat$yi, studyDat$vi, type = \"Rosenthal\"))",
    "",
    "# Selection models used by the package",
    "studyML <- metafor::rma(yi, vi, method = \"ML\", data = studyDat)",
    if (identical(directionSetting, "auto"))
      "direction <- if (coef(studyML)[1] >= 0) \"greater\" else \"less\""
    else paste0("direction <- ", .dq(directionSetting)),
    "step <- if (direction == \"two.sided\") .05 else .025",
    "threePSM <- try(metafor::selmodel(studyML, type = \"stepfun\",",
    "  alternative = direction, steps = step, control = list(optimizer = \"nlminb\")), silent = TRUE)",
    "print(threePSM)",
    "vwSteps <- c(.005,.01,.05,.10,.25,.35,.50,.65,.75,.90,.95,.99,.995,1)",
    "if (direction == \"two.sided\") {",
    "  vwModerate <- c(1,.99,.95,.90,.80,.75,.60,.60,.75,.80,.90,.95,.99,1)",
    "  vwSevere <- c(1,.99,.90,.75,.60,.50,.25,.25,.50,.60,.75,.90,.99,1)",
    "} else {",
    "  vwModerate <- c(1,.99,.95,.80,.75,.65,.60,.55,.50,.50,.50,.50,.50,.50)",
    "  vwSevere <- c(1,.99,.90,.75,.60,.50,.40,.35,.30,.25,.10,.10,.10,.10)",
    "}",
    "veveaModerate <- try(metafor::selmodel(studyML, type = \"stepfun\",",
    "  alternative = direction, steps = vwSteps, delta = vwModerate), silent = TRUE)",
    "veveaSevere <- try(metafor::selmodel(studyML, type = \"stepfun\",",
    "  alternative = direction, steps = vwSteps, delta = vwSevere), silent = TRUE)",
    "print(veveaModerate); print(veveaSevere)",
    "",
    "# Copas is optional and is skipped when meta/metasens are unavailable",
    "if (requireNamespace(\"meta\", quietly = TRUE) && requireNamespace(\"metasens\", quietly = TRUE)) {",
    "  metaObject <- meta::metagen(TE = studyDat$yi, seTE = sqrt(studyDat$vi),",
    "    studlab = studyDat$studyID, common = FALSE, random = TRUE,",
    paste0("    sm = ", .dq(metaMeasure),
           ", method.tau = \"ML\", method.random.ci = \"HK\")"),
    "  print(try(metasens::copas(metaObject, silent = TRUE), silent = TRUE))",
    "}",
    "if (direction != \"two.sided\" && requireNamespace(\"puniform\", quietly = TRUE)) {",
    "  pside <- if (direction == \"greater\") \"right\" else \"left\"",
    "  print(try(puniform::puniform(yi = studyDat$yi, vi = studyDat$vi, side = pside,",
    "    method = \"P\", plot = FALSE), silent = TRUE))",
    "}",
    "zval <- studyDat$yi / sqrt(studyDat$vi)",
    "ptwo <- 2 * pnorm(abs(zval), lower.tail = FALSE)",
    "sig <- if (direction == \"greater\") studyDat$yi > 0 & ptwo < .05 else if (direction == \"less\") studyDat$yi < 0 & ptwo < .05 else ptwo < .05",
    "psig <- ptwo[sig]",
    "if (length(psig) >= 2) {",
    "  pcurveStatistic <- -2 * sum(log(psig / .05))",
    "  print(c(chiSquare = pcurveStatistic, df = 2 * length(psig),",
    "          pRightSkew = pchisq(pcurveStatistic, 2 * length(psig), lower.tail = FALSE)))",
    "}",
    "if (length(zval[sig]) >= 10 && requireNamespace(\"zcurve\", quietly = TRUE)) {",
    "  print(try(zcurve::zcurve(abs(zval[sig]), method = \"EM\", bootstrap = FALSE), silent = TRUE))",
    "}"
  )
  c(.audit_header("PUBLICATION BIAS: MULTILEVEL EGGER / PET-PEESE"),
    "dbias <- transform(dat, sei = sqrt(vi), variance = vi)",
    paste0("pet <- ", fit, "(yi, vi, mods = ~ sei,"),
    "  random = ~ 1 | studyID/effectID, method = \"REML\",",
    "  tdist = TRUE, data = dbias)",
    paste0("peese <- ", fit, "(yi, vi, mods = ~ variance,"),
    "  random = ~ 1 | studyID/effectID, method = \"REML\",",
    "  tdist = TRUE, data = dbias)",
    "print(summary(pet)); print(summary(peese))",
    "print(c(F = pet$QM, df1 = pet$QMdf[1], df2 = pet$QMdf[2], p = pet$QMp))",
    "print(c(F = peese$QM, df1 = peese$QMdf[1], df2 = peese$QMdf[2], p = peese$QMp))",
    "selected <- if (coef(summary(pet))[\"intrcpt\", \"pval\"] < .05) peese else pet",
    "print(coef(summary(selected))[\"intrcpt\", , drop = FALSE])",
    supplementary)
}

.audit_leave_code <- function(audit) {
  if (!isTRUE(audit$leave)) return(character())
  fit <- .audit_fit_name(audit)
  by <- audit$leaveby %||% "both"
  method <- audit$method %||% "REML"
  effectCode <- if (by %in% c("both", "effect")) c(
    "effectKey <- seq_len(nrow(dat))",
    "effectLeave <- lapply(effectKey, function(id) {",
    "  d <- dat[effectKey != id, ]",
    paste0("  ", fit, "(yi, vi, random = ~ 1 | studyID/effectID,"),
    paste0("    method = ", .dq(method), ", tdist = TRUE, data = d)"),
    "})") else character()
  studyCode <- if (by %in% c("both", "study")) c(
    "studyLeave <- lapply(unique(as.character(dat$studyID)), function(id) {",
    "  d <- droplevels(dat[dat$studyID != id, ])",
    paste0("  ", fit, "(yi, vi, random = ~ 1 | studyID/effectID,"),
    paste0("    method = ", .dq(method), ", tdist = TRUE, data = d)"),
    "})") else character()
  c(.audit_header("LEAVE-ONE-OUT SENSITIVITY ANALYSIS"), effectCode, studyCode)
}

.audit_single_bias_code <- function(audit, scale = "custom") {
  if (!isTRUE(audit$bias)) return(character())
  fit <- .audit_fit_name(audit, single = TRUE)
  rho <- audit$rho %||% 0.60
  directionSetting <- audit$direction %||% "auto"
  extra <- audit$extra %||% TRUE
  metaMeasure <- switch(scale, logor = "OR", z = "ZCOR", g = "SMD", "SMD")
  supplementary <- if (!isTRUE(extra)) character() else c(
    "studyModel <- metafor::rma(yi, vi, method = \"REML\", test = \"knha\", data = studyDat)",
    "singleEgger <- metafor::rma(yi, vi, mods = ~ I(sqrt(vi)), method = \"REML\", test = \"knha\", data = studyDat)",
    "trimfillModel <- try(metafor::trimfill(studyModel), silent = TRUE)",
    "print(summary(singleEgger)); print(trimfillModel)",
    "print(metafor::ranktest(studyModel))",
    "print(metafor::fsn(studyDat$yi, studyDat$vi, type = \"Rosenthal\"))",
    "studyML <- metafor::rma(yi, vi, method = \"ML\", data = studyDat)",
    if (identical(directionSetting, "auto"))
      "direction <- if (coef(studyML)[1] >= 0) \"greater\" else \"less\""
    else paste0("direction <- ", .dq(directionSetting)),
    "step <- if (direction == \"two.sided\") .05 else .025",
    "threePSM <- try(metafor::selmodel(studyML, type = \"stepfun\",",
    "  alternative = direction, steps = step, control = list(optimizer = \"nlminb\")), silent = TRUE)",
    "print(threePSM)",
    "vwSteps <- c(.005,.01,.05,.10,.25,.35,.50,.65,.75,.90,.95,.99,.995,1)",
    "if (direction == \"two.sided\") {",
    "  vwModerate <- c(1,.99,.95,.90,.80,.75,.60,.60,.75,.80,.90,.95,.99,1)",
    "  vwSevere <- c(1,.99,.90,.75,.60,.50,.25,.25,.50,.60,.75,.90,.99,1)",
    "} else {",
    "  vwModerate <- c(1,.99,.95,.80,.75,.65,.60,.55,.50,.50,.50,.50,.50,.50)",
    "  vwSevere <- c(1,.99,.90,.75,.60,.50,.40,.35,.30,.25,.10,.10,.10,.10)",
    "}",
    "veveaModerate <- try(metafor::selmodel(studyML, type = \"stepfun\",",
    "  alternative = direction, steps = vwSteps, delta = vwModerate), silent = TRUE)",
    "veveaSevere <- try(metafor::selmodel(studyML, type = \"stepfun\",",
    "  alternative = direction, steps = vwSteps, delta = vwSevere), silent = TRUE)",
    "print(veveaModerate); print(veveaSevere)",
    "if (requireNamespace(\"meta\", quietly = TRUE) && requireNamespace(\"metasens\", quietly = TRUE)) {",
    "  metaObject <- meta::metagen(TE = studyDat$yi, seTE = sqrt(studyDat$vi),",
    "    studlab = studyDat$studyID, common = FALSE, random = TRUE,",
    paste0("    sm = ", .dq(metaMeasure),
           ", method.tau = \"ML\", method.random.ci = \"HK\")"),
    "  print(try(metasens::copas(metaObject, silent = TRUE), silent = TRUE))",
    "}",
    "if (direction != \"two.sided\" && requireNamespace(\"puniform\", quietly = TRUE)) {",
    "  pside <- if (direction == \"greater\") \"right\" else \"left\"",
    "  print(try(puniform::puniform(yi = studyDat$yi, vi = studyDat$vi, side = pside,",
    "    method = \"P\", plot = FALSE), silent = TRUE))",
    "}",
    "zval <- studyDat$yi / sqrt(studyDat$vi)",
    "ptwo <- 2 * pnorm(abs(zval), lower.tail = FALSE)",
    "sig <- if (direction == \"greater\") studyDat$yi > 0 & ptwo < .05 else if (direction == \"less\") studyDat$yi < 0 & ptwo < .05 else ptwo < .05",
    "psig <- ptwo[sig]",
    "if (length(psig) >= 2) {",
    "  pcurveStatistic <- -2 * sum(log(psig / .05))",
    "  print(c(chiSquare = pcurveStatistic, df = 2 * length(psig),",
    "          pRightSkew = pchisq(pcurveStatistic, 2 * length(psig), lower.tail = FALSE)))",
    "}",
    "if (length(zval[sig]) >= 10 && requireNamespace(\"zcurve\", quietly = TRUE)) {",
    "  print(try(zcurve::zcurve(abs(zval[sig]), method = \"EM\", bootstrap = FALSE), silent = TRUE))",
    "}"
  )
  c(.audit_header("SINGLE-LEVEL PUBLICATION BIAS / PET-PEESE"),
    paste0("studyDat <- poolStudy(dat, ", rho, ")"),
    "studyDat$sei <- sqrt(studyDat$vi); studyDat$variance <- studyDat$vi",
    paste0("pet <- ", fit, "(yi, vi, mods = ~ sei, method = \"REML\", test = \"knha\", data = studyDat)"),
    paste0("peese <- ", fit, "(yi, vi, mods = ~ variance, method = \"REML\", test = \"knha\", data = studyDat)"),
    "print(summary(pet)); print(summary(peese))",
    "print(c(F = pet$QM, df1 = pet$QMdf[1], df2 = pet$QMdf[2], p = pet$QMp))",
    "print(c(F = peese$QM, df1 = peese$QMdf[1], df2 = peese$QMdf[2], p = peese$QMp))",
    "selected <- if (coef(summary(pet))[\"intrcpt\", \"pval\"] < .05) peese else pet",
    "print(coef(summary(selected))[\"intrcpt\", , drop = FALSE])",
    supplementary)
}

.audit_single_leave_code <- function(audit) {
  if (!isTRUE(audit$leave)) return(character())
  fit <- .audit_fit_name(audit, single = TRUE)
  by <- audit$leaveby %||% "both"
  method <- audit$method %||% "REML"
  rho <- audit$rho %||% 0.60
  effectCode <- if (by %in% c("both", "effect")) c(
    "effectKey <- seq_len(nrow(dat))",
    "effectLeave <- lapply(effectKey, function(id) {",
    "  d <- dat[effectKey != id, ]",
    paste0("  pooled <- poolStudy(d, ", rho, ")"),
    paste0("  ", fit, "(yi, vi, method = ", .dq(method), ", test = \"knha\", data = pooled)"),
    "})") else character()
  studyCode <- if (by %in% c("both", "study")) c(
    "studyLeave <- lapply(unique(as.character(dat$studyID)), function(id) {",
    "  d <- droplevels(dat[dat$studyID != id, ])",
    paste0("  pooled <- poolStudy(d, ", rho, ")"),
    paste0("  ", fit, "(yi, vi, method = ", .dq(method), ", test = \"knha\", data = pooled)"),
    "})") else character()
  c(.audit_header("SINGLE-LEVEL LEAVE-ONE-OUT SENSITIVITY ANALYSIS"),
    effectCode, studyCode)
}

#' Print or return the underlying-package audit code
#'
#' The generated script calls `metafor`, `splines`, and base R
#' directly. It contains no calls to meta3level analysis wrappers.
#'
#' @param x A complete workflow, prepared data, fitted model, moderator,
#'   publication-bias, or leave-one-out result. Prepared data generate the
#'   preparation plus main-model audit script.
#' @param print Print the code in the R console.
#' @param file Optional path for writing the generated `.R` script. An exact
#'   prepared-data `.rds` sidecar is written beside the script so filtered data
#'   and derived moderator columns can be reproduced.
#' @return A character vector containing auditable R code, invisibly when
#'   printed.
m3code <- function(x, print = TRUE, file = NULL) {
  if (inherits(x, "meta3_workflow")) {
    data <- x$main$source_data %||% x$main$data
    audit <- attr(x, "meta3_audit", exact = TRUE)
  } else if (inherits(x, "meta3_data")) {
    data <- x
    inferred_level <- if (any(table(data$studyID) > 1)) "three" else "single"
    audit <- list(cont = NULL, groups = NULL, spline = NULL,
                  bias = FALSE, leave = FALSE, rho = 0.60,
                  level = inferred_level)
  } else if (inherits(x, "meta3_continuous")) {
    data <- x$source_data %||% x$data
    audit <- attr(x, "meta3_audit", exact = TRUE)
  } else if (inherits(x, "meta3_categorical")) {
    data <- x$source_data %||% x$data
    audit <- attr(x, "meta3_audit", exact = TRUE)
  } else if (inherits(x, "meta3_splines")) {
    data <- x$source_data %||% x$data
    audit <- attr(x, "meta3_audit", exact = TRUE)
  } else if (inherits(x, "meta3_publication_bias")) {
    data <- x$data
    audit <- attr(x, "meta3_audit", exact = TRUE)
  } else if (inherits(x, "meta3_leave_one_out")) {
    data <- x$data
    audit <- attr(x, "meta3_audit", exact = TRUE)
  } else if (inherits(x, "rma") && !is.null(x$meta3_data)) {
    data <- x$meta3_source_data %||% x$meta3_data
    audit <- attr(x, "meta3_audit", exact = TRUE) %||%
      list(type = "fit", cont = NULL, groups = NULL, spline = NULL,
           bias = FALSE, leave = FALSE, rho = 0.60)
  } else {
    stop("m3code() requires an m3run(), m3prep(), m3fit(), moderator, bias, or leave-one-out result.", call. = FALSE)
  }
  if (is.null(audit)) {
    stop("This result does not contain audit settings. Re-run it with the current meta3level version.", call. = FALSE)
  }
  audit <- utils::modifyList(list(cont = NULL, groups = NULL, spline = NULL,
                                  bias = FALSE, leave = FALSE, rho = 0.60,
                                  level = "three"), audit)
  spec_columns <- function(specs) {
    if (!length(specs)) return(character())
    unique(vapply(specs, function(spec) spec$column %||% "", character(1)))
  }
  moderator_columns <- unique(c(
    spec_columns(audit$cont),
    spec_columns(audit$groups),
    spec_columns(audit$spline),
    if (!is.null(audit$mods)) all.vars(.mods_formula(audit$mods)) else character()
  ))
  moderator_columns <- moderator_columns[nzchar(moderator_columns)]
  audit_data_columns <- unique(c("studyID", "effectID", "yi", "vi", moderator_columns))
  audit_data_columns <- intersect(audit_data_columns, names(data))
  audit_data <- data[, audit_data_columns, drop = FALSE]
  for (attribute in c(
    "meta3_scale", "meta3_measure", "meta3_prep", "meta3_source",
    "meta3_decimal", "meta3_rho", "meta3_aggregation",
    "meta3_prep_signature"
  )) {
    attr(audit_data, attribute) <- attr(data, attribute, exact = TRUE)
  }
  class(audit_data) <- unique(c(
    intersect(class(data), c("meta3_study_data", "meta3_data")),
    class(audit_data)
  ))
  single <- identical(audit$level, "single")
  mainHeader <- if (single) {
    "SINGLE-LEVEL MAIN MODEL, POOLED EFFECT, HETEROGENEITY, AND VARIANCE LRT"
  } else {
    "THREE-LEVEL MAIN MODEL, I-SQUARED, AND VARIANCE LRT"
  }
  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1 || !nzchar(file)) {
      stop("'file' must be one non-empty path.", call. = FALSE)
    }
    output_dir <- dirname(file)
    if (!dir.exists(output_dir)) {
      stop("The audit-code output directory does not exist: ", output_dir,
           call. = FALSE)
    }
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
    file <- file.path(output_dir, basename(file))
    snapshot_file <- file.path(
      output_dir,
      paste0(tools::file_path_sans_ext(basename(file)), "_data.rds")
    )
    snapshot_name <- basename(snapshot_file)
    saveRDS(audit_data, snapshot_file, version = 2)
  } else {
    snapshot_file <- NULL
    snapshot_name <- NULL
  }
  original_prep <- .audit_prep_code(data)
  review_prep <- c(
    "",
    "# Original import and effect-size conversion retained for method review.",
    "# It is not executed because the exact analyzed data are loaded above.",
    "if (FALSE) {",
    paste0("  ", original_prep),
    "}"
  )
  prep_code <- if (!is.null(snapshot_file)) {
    c(
      "# Exact prepared/filtered data used by the fitted analysis.",
      paste0("auditDataName <- ", .dq(snapshot_name)),
      "auditFrameFiles <- vapply(sys.frames(), function(frame) {",
      "  candidate <- frame$ofile",
      "  if (is.null(candidate) || !length(candidate)) NA_character_ else as.character(candidate[1])",
      "}, character(1))",
      "auditFrameFiles <- auditFrameFiles[!is.na(auditFrameFiles) & nzchar(auditFrameFiles)]",
      "auditScriptFile <- if (length(auditFrameFiles)) tail(auditFrameFiles, 1) else NULL",
      "if (is.null(auditScriptFile) || !length(auditScriptFile)) {",
      "  fileArgument <- grep(\"^--file=\", commandArgs(trailingOnly = FALSE), value = TRUE)",
      "  auditScriptFile <- if (length(fileArgument)) sub(\"^--file=\", \"\", fileArgument[1]) else NULL",
      "}",
      "auditCandidates <- c(",
      "  if (!is.null(auditScriptFile) && length(auditScriptFile)) file.path(dirname(auditScriptFile), auditDataName),",
      "  file.path(getwd(), auditDataName),",
      paste0("  ", .dq(snapshot_file)),
      ")",
      "auditCandidates <- unique(auditCandidates[file.exists(auditCandidates)])",
      "if (!length(auditCandidates)) stop(\"Audit data sidecar is missing: \" , auditDataName)",
      "auditDataFile <- auditCandidates[1]",
      "dat <- readRDS(auditDataFile)",
      review_prep
    )
  } else if (.data_was_modified(data)) {
    c(
      "# This analysis used filtered rows or derived columns after m3prep().",
      "# Assign the exact analyzed meta3_data object to 'data' before running this console code.",
      "dat <- data",
      review_prep
    )
  } else {
    original_prep
  }
  moderator_code <- if (single) .audit_single_moderator_code(audit) else .audit_moderator_code(audit)
  bias_code <- if (single) .audit_single_bias_code(audit, .scale_of(data)) else .audit_bias_code(audit, .scale_of(data))
  leave_code <- if (single) .audit_single_leave_code(audit) else .audit_leave_code(audit)
  component_code <- c(moderator_code, bias_code, leave_code)
  code <- c(
    "# Reproducible underlying-package code generated by meta3level",
    paste0("# meta3level version: ", as.character(utils::packageVersion("meta3level"))),
    paste0("# generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "# The analysis below calls dependency packages directly.",
    "",
    "if (!requireNamespace(\"metafor\", quietly = TRUE)) stop(\"Install metafor\")",
    paste0("decimalMark <- ", .dq(.decimal_of(data))),
    "num <- function(x) {",
    "  if (is.numeric(x)) return(as.numeric(x))",
    "  x <- trimws(as.character(x))",
    "  x <- gsub(\"%\", \"\", x, fixed = TRUE)",
    "  if (decimalMark == \",\") {",
    "    x <- gsub(\".\", \"\", x, fixed = TRUE)",
    "    x <- gsub(\",\", \".\", x, fixed = TRUE)",
    "  } else {",
    "    x <- gsub(\",\", \"\", x, fixed = TRUE)",
    "  }",
    "  x[toupper(x) %in% c(\"\", \"NA\", \"N/A\", \"NAN\", \"NULL\", \".\")] <- NA_character_",
    "  suppressWarnings(as.numeric(x))",
    "}",
    "missingText <- function(x) {",
    "  normalized <- toupper(trimws(as.character(x)))",
    "  is.na(x) | normalized %in% c(\"\", \"NA\", \"N/A\", \"NAN\", \"NULL\", \".\")",
    "}",
    "moderatorNum <- function(x, label = \"moderator\") {",
    "  original <- trimws(as.character(x))",
    "  present <- !missingText(original)",
    "  percent <- present & grepl(\"%\", original, fixed = TRUE)",
    "  plain <- present & !percent",
    "  values <- num(x)",
    "  if (any(present & !is.finite(values))) {",
    "    stop(\"Continuous moderator contains non-numeric or non-finite values: \" , label)",
    "  }",
    "  if (any(percent) && any(plain)) {",
    "    percentValues <- values[percent]",
    "    plainValues <- values[plain]",
    "    percentValid <- all(percentValues >= 0 & percentValues <= 100)",
    "    plainProportion <- all(plainValues >= 0 & plainValues <= 1) && any(plainValues > 0 & plainValues < 1)",
    "    plainPercentage <- all(plainValues >= 0 & plainValues <= 100) && any(plainValues > 1)",
    "    if (percentValid && plainProportion) {",
    "      values[percent] <- values[percent] / 100",
    "    } else if (!(percentValid && plainPercentage)) {",
    "      stop(\"Percent-marked and unmarked moderator values use an ambiguous scale: \" , label)",
    "    }",
    "  }",
    "  values",
    "}",
    .audit_header("DATA AND EFFECT-SIZE PREPARATION"),
    prep_code,
    .audit_inference_code(audit),
    if (single) .audit_single_setup_code() else character(),
    .audit_header(mainHeader),
    if (single) .audit_single_main_code(.scale_of(data), audit) else .audit_main_code(.scale_of(data), audit),
    component_code,
    .audit_header("SOFTWARE ENVIRONMENT"),
    "print(sessionInfo())"
  )
  class(code) <- c("meta3_code", "character")
  if (!is.null(file)) {
    writeLines(unclass(code), con = file, useBytes = TRUE)
  }
  if (isTRUE(print)) {
    .section("UNDERLYING-PACKAGE AUDIT CODE")
    cat(code, sep = "\n")
    cat("\n")
    return(invisible(code))
  }
  if (!is.null(file)) return(invisible(unclass(code)))
  unclass(code)
}

#' Show the installed source code of meta3level functions
#'
#' @param fun Function name, a vector of names, or `"all"`.
#' @param print Print source in the R console.
#' @param file Optional path for writing all requested function definitions.
#' @return A named list of function definitions.
m3source <- function(fun = "all", print = TRUE, file = NULL) {
  ns <- asNamespace("meta3level")
  allNames <- ls(ns, all.names = TRUE)
  available <- sort(allNames[vapply(allNames, function(name) {
    is.function(get(name, envir = ns, inherits = FALSE))
  }, logical(1))])
  if (length(fun) == 1 && identical(fun, "all")) fun <- available
  missing <- setdiff(fun, available)
  if (length(missing)) stop("Unknown function(s): ", paste(missing, collapse = ", "), call. = FALSE)
  out <- stats::setNames(lapply(fun, function(name) get(name, envir = ns, inherits = FALSE)), fun)
  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1 || !nzchar(file)) {
      stop("'file' must be one non-empty path.", call. = FALSE)
    }
    sourceLines <- unlist(lapply(names(out), function(name) {
      safeName <- if (make.names(name) == name) name else paste0("`", name, "`")
      body <- deparse(out[[name]], width.cutoff = 500)
      c(paste0("# ---- ", name, " ----"),
        paste0(safeName, " <- ", body[1]), body[-1], "")
    }), use.names = FALSE)
    writeLines(sourceLines, con = file, useBytes = TRUE)
  }
  if (isTRUE(print)) {
    for (name in names(out)) {
      .section(paste("SOURCE:", name))
      print(out[[name]])
    }
    return(invisible(out))
  }
  if (!is.null(file)) return(invisible(out))
  out
}
