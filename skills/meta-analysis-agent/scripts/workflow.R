# Source-only helper: no installation, model fit or export on load.
ma_value <- function(x, key, default = NULL) if (is.null(x[[key]])) default else x[[key]]
ma_package <- function(inference = "model") {
  if (!requireNamespace("meta3level", quietly = TRUE)) stop("请先安装 meta3level 元分析包。原始数据尚未改动。", call. = FALSE)
  if (inference == "CR2" && (!"inference" %in% names(formals(meta3level::m3fit)) || !requireNamespace("clubSandwich", quietly = TRUE)))
    stop("CR2 需要支持 inference 参数的 meta3level，以及 clubSandwich。", call. = FALSE)
}
ma_inspect <- function(file = NULL, sheet = NULL, data = NULL) {
  sheets <- NULL
  if (is.null(data)) {
    ma_package()
    if (is.null(file) || !file.exists(file)) stop("请提供实际存在的数据文件。", call. = FALSE)
    if (tolower(tools::file_ext(file)) %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) stop("读取 Excel 需要 readxl。", call. = FALSE)
      sheets <- readxl::excel_sheets(file)
      if (is.null(sheet) && length(sheets) > 1) return(list(status = "choose_sheet", sheets = sheets))
      if (is.null(sheet)) sheet <- sheets[1]
    }
    data <- meta3level::m3read(file, sheet = if (is.null(sheet)) 1 else sheet)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (anyDuplicated(names(data)) || any(!nzchar(trimws(names(data))))) stop("列名重复或为空，请先修正表头。", call. = FALSE)
  columns <- data.frame(column = names(data), type = vapply(data, function(x) class(x)[1], character(1)),
    missing = vapply(data, function(x) sum(is.na(x) | trimws(as.character(x)) == ""), integer(1)),
    unique = vapply(data, function(x) length(unique(x[!is.na(x)])), integer(1)), row.names = NULL)
  list(status = "ready", data = data, rows = nrow(data), columns = columns, preview = utils::head(data, 6), sheet = sheet, sheets = sheets)
}
ma_plan <- function(raw, config) {
  inspection <- ma_inspect(data = raw)
  raw <- inspection$data
  if (!nrow(raw)) stop("表格没有数据行。", call. = FALSE)
  if (!is.list(config)) stop("分析设置必须是 list。", call. = FALSE)
  pending <- function(message, status = "needs_input") list(status = status, questions = message, inspection = inspection)
  if (is.null(config$measure) || is.null(config$study)) return(pending("请先选择效应量类型和独立样本编号列。"))
  config$measure <- match.arg(config$measure, c("r", "d", "g", "or", "custom"))
  if (!isTRUE(config$study_is_independent_sample)) return(pending(paste0("请确认“", config$study, "”代表独立样本。同一样本的多行共用编号，多个独立样本分别编号。")))
  dependence <- match.arg(ma_value(config, "dependence", "nested"), c("nested", "independent", "complex"))
  if (dependence == "complex") return(pending("共享对照、重叠样本或重复测量需要设计特定的方差/协方差信息；请先补充设计。", "needs_native_model"))
  needed <- switch(config$measure, r = c("value", "n"), d = c("value", "n1", "n2"), g = c("value", "vi"), custom = c("value", "vi"), or = character())
  if (config$measure == "d") {
    if (!ma_value(config, "design", "independent_groups") %in% c("independent", "independent_groups")) return(pending("配对/单组 d 需要已确认的定义与抽样方差，不使用独立组公式。", "needs_native_model"))
    config$design <- "independent"
  }
  absent <- needed[vapply(needed, function(k) is.null(config[[k]]), logical(1))]
  if (config$measure == "or") {
    cells <- all(vapply(c("cellA", "cellB", "cellC", "cellD"), function(k) !is.null(config[[k]]), logical(1)))
    precision <- !is.null(config$vi) || !is.null(config$se) || (!is.null(config$lower) && !is.null(config$upper))
    if (!cells && (is.null(config$value) || !precision)) absent <- c(absent, "OR 与 log(OR) 的精度，或完整四格表")
  }
  if (length(absent)) return(pending(paste("还需要：", paste(absent, collapse = "、"))))
  config$inference <- match.arg(ma_value(config, "inference", "model"), c("model", "CR2"))
  ma_package(config$inference)
  prep_keys <- intersect(names(config), setdiff(names(formals(meta3level::m3prep)), "data"))
  columns <- unlist(config[intersect(prep_keys, c("study", "effect", "value", "n", "n1", "n2", "vi", "se", "lower", "upper", "cellA", "cellB", "cellC", "cellD"))], use.names = FALSE)
  absent_columns <- setdiff(columns, names(raw))
  if (length(absent_columns)) stop("找不到指定列：", paste(absent_columns, collapse = "、"), call. = FALSE)
  if (".agent_source_row" %in% names(raw)) stop(".agent_source_row 是保留列名，请先改名。", call. = FALSE)
  raw$.agent_source_row <- seq_len(nrow(raw))
  warnings <- character()
  data <- withCallingHandlers(do.call(meta3level::m3prep, c(list(data = raw), config[prep_keys])),
    warning = function(w) warnings <<- c(warnings, conditionMessage(w)))
  counts <- table(as.character(data$studyID)); repeated <- any(counts > 1)
  if (dependence == "independent" && repeated) stop("样本编号重复，与“每行独立”设置不一致。请核对独立性。", call. = FALSE)
  config$level <- match.arg(ma_value(config, "level", "auto"), c("auto", "three", "single"))
  if (config$level == "auto") config$level <- if (repeated) "three" else "single"
  if (config$level == "three" && (!repeated || length(counts) < 2 || nrow(data) < 4)) stop("有效数据不支持三水平模型；需至少两个样本、四个效应且有重复效应。", call. = FALSE)
  if (config$level == "single" && length(counts) < 3) stop("普通模型至少需要三个独立样本。", call. = FALSE)
  config$rho <- ma_value(config, "rho", 0.6)
  if (length(config$rho) != 1 || !is.finite(config$rho) || config$rho < 0 || config$rho >= 1) stop("rho 应为 0 到小于 1 的单个数。", call. = FALSE)
  config$bias <- isTRUE(config$bias); config$leave <- isTRUE(ma_value(config, "leave", TRUE))
  config$cont <- ma_value(config, "cont", character()); config$groups <- ma_value(config, "groups", list())
  moderator_names <- c(config$cont, vapply(config$groups, function(x) x$var, character(1)))
  if (length(setdiff(moderator_names, names(data)))) stop("指定调节列不存在。", call. = FALSE)
  for (g in config$groups) if (is.null(g$ref) || !g$ref %in% as.character(data[[g$var]])) stop("分类调节的参照组不在实际数据中：", g$var, call. = FALSE)
  removed <- raw[!raw$.agent_source_row %in% data$.agent_source_row, , drop = FALSE]
  removed$.agent_reason <- rep("缺失/无效编号、效应量或方差；具体原因结合原行与准备阶段警告复核", nrow(removed))
  if (nrow(removed) && config$measure == "r") {
    num <- function(k) suppressWarnings(as.numeric(as.character(removed[[k]])))
    r <- num(config$value); n <- num(config$n)
    removed$.agent_reason[!is.finite(r) | abs(r) >= 1] <- "相关系数缺失或不在 (-1, 1) 内"
    removed$.agent_reason[is.finite(r) & abs(r) < 1 & (!is.finite(n) | n <= 3)] <- "样本量缺失或不大于 3"
  }
  assumptions <- if (config$level == "three") "三水平模型采用对角抽样方差 V = vi，不自动构造共享抽样误差的协方差。" else "普通随机效应模型。"
  if (config$level == "single" && repeated) assumptions <- c(assumptions, paste0("先 GLS 合并至样本层，rho = ", config$rho, " 是假定值，应做敏感性检查。"))
  if (length(counts) < 10) assumptions <- c(assumptions, "独立样本少于 10，推断可能不稳定。")
  list(status = "ready", config = config, data = data, raw = raw, removed = removed, warnings = unique(warnings),
    assumptions = assumptions, effects = nrow(data), studies = length(counts), rows = nrow(raw), counts = counts)
}
ma_run <- function(raw = NULL, config = NULL, plan = NULL) {
  if (is.null(plan)) plan <- ma_plan(raw, config)
  if (plan$status != "ready") stop(plan$questions, call. = FALSE)
  cfg <- plan$config; warnings <- plan$warnings
  result <- withCallingHandlers(meta3level::m3run(plan$data, cont = cfg$cont, groups = cfg$groups,
    spline = cfg$spline, bias = cfg$bias, leave = cfg$leave, level = cfg$level, rho = cfg$rho,
    inference = cfg$inference, show = FALSE, keep = TRUE), warning = function(w) warnings <<- c(warnings, conditionMessage(w)))
  audit <- list()
  for (type in c("cont", "groups", "spline")) for (name in names(result[[type]])) {
    component <- result[[type]][[name]]
    if (inherits(component, "meta3_error")) audit[[length(audit) + 1L]] <- data.frame(component = paste(type, name), effects = NA, studies = NA, status = component$message)
    else {
      model <- if (type == "spline") component$models[[1]] else if (type == "groups") component$intercept_model else component$model
      audit[[length(audit) + 1L]] <- data.frame(component = paste(type, name), effects = model$k, studies = length(unique(model$meta3_data$studyID)), status = "已拟合")
    }
  }
  structure(list(result = result, plan = plan, warnings = unique(warnings),
    component_audit = if (length(audit)) do.call(rbind, audit) else data.frame(), session = capture.output(sessionInfo())), class = "meta_agent_result")
}
ma_script <- function(config, file = "coding.csv", sheet = 1) {
  prep_keys <- intersect(names(config), setdiff(names(formals(meta3level::m3prep)), "data"))
  code <- function(x) paste(capture.output(dput(x)), collapse = "\n")
  c("# 完整复现脚本：将 input_file 改为本机编码表路径，核对样本独立性与方向。",
    "if (!requireNamespace('meta3level', quietly = TRUE)) stop('Please install meta3level.')",
    paste0("input_file <- ", encodeString(file, quote = '"')), paste0("config <- ", code(config)),
    paste0("raw <- meta3level::m3read(input_file, sheet = ", code(sheet), ")"),
    "raw$.agent_source_row <- seq_len(nrow(raw))", paste0("prep_args <- config[", code(prep_keys), "]"),
    "dat <- do.call(meta3level::m3prep, c(list(data = raw), prep_args))",
    "omitted <- raw[!raw$.agent_source_row %in% dat$.agent_source_row, , drop = FALSE]",
    "cat('Source:', nrow(raw), 'Effects:', nrow(dat), 'Independent samples:', length(unique(dat$studyID)), '\\n')",
    "print(omitted)",
    "result <- meta3level::m3run(dat, cont = config$cont, groups = config$groups, spline = config$spline,",
    "  bias = config$bias, leave = config$leave, level = config$level, rho = config$rho, inference = config$inference)",
    "# 原生底层代码：meta3level::m3code(result)", "sessionInfo()")
}
print.meta_agent_result <- function(x, ...) {
  model <- x$result$main$model
  cat("主模型已执行：", length(unique(model$meta3_data$studyID)), " 个独立样本，", model$k, " 个建模效应。\n", sep = "")
  cat("合并前有效效应：", x$plan$effects, "；推断：", x$plan$config$inference, "\n", sep = "")
  print(x$result$main$overall, row.names = FALSE)
  if (is.null(x$plan$config$direction) || !nzchar(x$plan$config$direction)) cat("构念/比较方向尚未确认，暂不作“更好/更差”的含义解释。\n")
  cat("假设：\n", paste(x$plan$assumptions, collapse = "\n"), "\n", sep = "")
  cat("移除原始行数：", nrow(x$plan$removed), "\n", sep = "")
  if (nrow(x$plan$removed)) print(x$plan$removed[, c(".agent_source_row", ".agent_reason"), drop = FALSE], row.names = FALSE)
  if (nrow(x$component_audit)) print(x$component_audit, row.names = FALSE)
  if (length(x$warnings)) cat("完整警告：\n", paste(x$warnings, collapse = "\n"), "\n", sep = "")
  cat("完整表格：meta3level::m3report(object$result)\n")
  invisible(x)
}
