options(warn = 1)
args <- commandArgs(trailingOnly = TRUE)
worker_dir <- normalizePath(args[1], mustWork = TRUE)
if (!requireNamespace("meta3level", quietly = TRUE)) pkgload::load_all(file.path(worker_dir, "meta3level"), quiet = TRUE, helpers = FALSE, export_all = FALSE)
source(file.path(worker_dir, "workflow.R"), encoding = "UTF-8")
for (name in c("questionnaire.R","analyses.R","reports.R","plotting.R","intervention.R","research-data.R")) source(file.path(worker_dir,name),encoding="UTF-8")
psych <- q_init()
work <- new.env(parent = globalenv())
work$raw <- NULL; work$plan <- NULL; work$result <- NULL; work$file <- NULL; work$sheet <- 1
work$output <- tempfile("meta-agent-"); dir.create(work$output)
emit <- function(x) {cat("@@META@@", jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "rows", null = "null", na = "null", digits = 12, force = TRUE), "\n", sep = ""); flush(stdout())}
snapshot <- function() {
  if (is.null(work$result)) return(NULL)
  x <- work$result; model <- x$result$main$model
  moderators <- list()
  for (type in c("cont", "groups")) for (name in names(x$result[[type]])) {
    item <- x$result[[type]][[name]]
    moderators[[paste(type, name)]] <- if (inherits(item, "meta3_error")) list(error = item$message) else item$f_test
  }
  list(overall = x$result$main$overall, measure = x$plan$config$measure, inference = x$plan$config$inference,
    model = x$plan$config$level, effects = model$k, studies = length(unique(model$meta3_data$studyID)),
    original_effects = x$plan$effects, source_rows = x$plan$rows, removed = x$plan$removed[, c(".agent_source_row", ".agent_reason"), drop = FALSE],
    i2 = x$result$main$i2, moderators = moderators, component_audit = x$component_audit, warnings = x$warnings,
    assumptions = x$plan$assumptions, direction = ma_value(x$plan$config, "direction", ""), config = x$plan$config)
}
dispatch <- function(method, params) {
  if (method == "q_read_rules") {
    need(tolower(tools::file_ext(params$file))=="docx", "请选择 DOCX 筛选规则。")
    info<-utils::unzip(params$file,list=TRUE)
    need(sum(info$Length)<20*1024*1024,"DOCX 解压内容过大，请改用 TXT。")
    doc<-officer::read_docx(params$file)
    text<-paste(officer::docx_summary(doc)$text,collapse="\n")
    need(nchar(text)<=30000,"筛选规则超过 30,000 字，请精简；未截断内容。")
    return(list(text=text))
  }
  if (method == "q_plot") return(q_research_figure(psych,params$spec,tempfile("research-figure-",tmpdir=work$output)))
  if (method == "plot") {
    need(!is.null(work$result),"请先完成元分析。")
    if(is.null(work$plotq)){
      work$plotq<-q_init();q_load(work$plotq,work$raw,basename(work$file))
      work$plotq$results$meta<-c(list(method="元分析",n=snapshot()$studies,revision=work$plotq$revision),snapshot())
      work$plotq$fits$meta<-work$result$result
    }
    params$spec$module<-"meta"
    return(q_research_figure(work$plotq,params$spec,tempfile("meta-figure-",tmpdir=work$output)))
  }
  if (method == "q_demo") return(q_load(psych,q_demo(),"合成问卷示例"))
  if (method == "q_load") {
    x<-ma_inspect(params$file,sheet=params$sheet)
    if(x$status=="choose_sheet"){
      psych<<-q_init()
      return(list(status="choose_sheet",name=basename(params$file),sheets=x$sheets))
    }
    return(q_load(psych,x$data,basename(params$file)))
  }
  if (method == "q_research_freeze") return(q_research_freeze(psych,work$output))
  if (method == "q_research_profile") return(q_research_profile(params$file,params$columns))
  if (method == "q_research_correlations") return(q_research_correlations(params$file,params$columns,pv(params,"method","spearman")))
  if (method == "q_research_export") return(q_research_report(params$file,params$research,params$directory))
  if (method == "q_state") return(q_snapshot(psych))
  if (method == "q_screen") return(q_screen(psych,pv(params,"config",list()),isTRUE(params$skip)))
  if (method == "q_review") return(list(review=q_review(psych),suitability=q_suitability(psych)))
  if (method == "q_prepare") {review<-q_validate_spec(psych,params$module,params$spec);psych$specs[[params$module]]<-params$spec;return(review)}
  if (method == "q_run") return(q_run(psych,params$module,params$spec,file.path(work$output,paste0(params$module,"-",format(Sys.time(),"%Y%m%d%H%M%OS6")))))
  if (method == "q_branch") return(q_branch(psych,pv(params,"mode","all"),params$class))
  if (method == "q_status") return(q_intervention_status(psych))
  if (method == "q_cs") {
    f<-psych$fits$network;need(!is.null(f)&&f$revision==psych$revision,"请先完成当前数据版本的网络。")
    cs<-params$value;need(is.numeric(cs)&&length(cs)==1&&is.finite(cs)&&cs>=0&&cs<=1,"CS 为 0 到 1 的数。")
    need(identical(params$metric,"expectedInfluence"),"干预门槛当前采用 EI 的 CS，不能拿其他指标替代。")
    f$cs<-list(expectedInfluence=cs);f$cs_origin<-"使用者报告，软件未核实外部计算";psych$fits$network<-f
    psych$results$network$cs<-f$cs;psych$results$network$cs_origin<-f$cs_origin
    # Only the most recent fit is active. Never overwrite earlier networks or node sets.
    for(i in rev(seq_along(psych$archive)))if(psych$archive[[i]]$module=="network"&&psych$archive[[i]]$summary$revision==psych$revision){psych$archive[[i]]$summary$cs<-f$cs;psych$archive[[i]]$summary$cs_origin<-f$cs_origin;break}
    return(q_intervention_status(psych))
  }
  if (method == "q_export") return(q_write_reports(psych,params$directory,research=params$research,ruleAttachments=params$ruleAttachments))
  if (method == "q_script") return(q_external_script(psych,params$method,params$directory,isTRUE(params$confirmed),params$spec))
  if (method == "q_import_result") {
    x<-ma_inspect(params$file,sheet=params$sheet);need(x$status!="choose_sheet","请导入只有一个结果工作表的文件或 CSV。")
    result<-list(method="外部干预结果",n=NULL,result_records=nrow(x$data),source=basename(params$file),table=x$data,revision=NULL,imported_into_revision=psych$revision,
      limitations=c("这是使用者返回的外部计算结果，软件没有验证原始计算执行。","来源数据版本、节点与样本量未经核实；结果表行数不是受访者数，当前工作区样本量不能替代外部样本量。","模拟节点操作不能解释为真实干预因果证据。"))
    psych$results$intervention<-result;psych$archive[[length(psych$archive)+1]]<-list(module="intervention",summary=result)
    return(q_snapshot(psych))
  }
  if (method == "q_import_image"){
    need(!is.null(psych$results$intervention),"请先导入对应的实际结果表。")
    file<-file.path(work$output,paste0("external-",format(Sys.time(),"%Y%m%d%H%M%S"),".",tools::file_ext(params$file)));need(file.copy(params$file,file),"无法复制结果图。")
    psych$results$intervention$image<-file
    for(i in rev(seq_along(psych$archive)))if(psych$archive[[i]]$module=="intervention"){psych$archive[[i]]$image<-file;break}
    return(q_snapshot(psych))
  }
  if (method == "inspect") {
    work$plotq<-NULL
    work$raw <- NULL; work$plan <- NULL; work$result <- NULL
    inspected <- ma_inspect(params$file, sheet = params$sheet)
    work$file <- params$file; work$sheet <- ma_value(inspected, "sheet", 1)
    if (inspected$status == "choose_sheet") return(list(status = "choose_sheet", sheets = inspected$sheets))
    work$raw <- inspected$data
    return(list(status = "ready", name = basename(params$file), rows = inspected$rows, columns = inspected$columns, preview = inspected$preview))
  }
  if (method == "invalidate") {work$plotq<-NULL;work$plan <- NULL; work$result <- NULL; return(list(status = "cleared"))}
  if (method == "plan") {
    work$plotq<-NULL
    if (is.null(work$raw)) stop("请先选择实际编码表或加载示例。", call. = FALSE)
    work$result <- NULL; work$plan <- ma_plan(work$raw, params$config)
    p <- work$plan
    if (p$status != "ready") return(list(status = p$status, questions = p$questions))
    return(list(status = "ready", config = p$config, effects = p$effects, studies = p$studies, rows = p$rows,
      removed_count = nrow(p$removed), assumptions = p$assumptions, warnings = p$warnings))
  }
  if (method == "run") {
    if (is.null(work$plan) || work$plan$status != "ready") stop("请先生成有效方案并确认独立样本。", call. = FALSE)
    work$result <- ma_run(plan = work$plan)
    image <- file.path(work$output, "forest.png")
    grDevices::png(image, width = 1300, height = min(4000, max(700, 300 + 18 * work$result$result$main$model$k)), res = 120)
    tryCatch(meta3level::m3plot(work$result$result, what = "forest"), finally = grDevices::dev.off())
    return(list(summary = snapshot(), image = image))
  }
  if (method == "results") return(snapshot())
  if (method == "export") {
    if (is.null(work$result)) stop("请先完成分析。", call. = FALSE)
    dir.create(params$directory, recursive = TRUE, showWarnings = FALSE)
    code <- ma_script(work$plan$config, basename(work$file), work$sheet)
    writeLines(enc2utf8(code), file.path(params$directory, "meta-analysis.R"), useBytes = TRUE)
    report <- c(capture.output({print(work$result); meta3level::m3report(work$result$result)}), "\n执行环境：", work$result$session)
    writeLines(enc2utf8(report), file.path(params$directory, "meta-analysis-report.txt"), useBytes = TRUE)
    jsonlite::write_json(snapshot(), file.path(params$directory, "analysis-audit.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
    file.copy(file.path(work$output, "forest.png"), file.path(params$directory, "forest.png"), overwrite = TRUE)
    report_q<-q_init();q_load(report_q,work$raw,basename(work$file));report_q$audit<-list(removed=snapshot()$removed,config=work$plan$config)
    report_q$figures<-work$plotq$figures
    report_q$archive<-list(list(module="meta",summary=c(list(method="元分析",n=snapshot()$studies,revision=report_q$revision),snapshot()),image=file.path(work$output,"forest.png")))
    q_write_reports(report_q,params$directory,research=params$research)
    return(list(directory = params$directory, files = list.files(params$directory)))
  }
  stop("未知计算操作。", call. = FALSE)
}
emit(list(event = "ready", version = as.character(utils::packageVersion("meta3level"))))
connection <- file("stdin", open = "r")
repeat {
  line <- readLines(connection, n = 1, warn = FALSE)
  if (!length(line)) break
  request <- NULL
  tryCatch({
    request <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    if (!is.null(request$params$config$cont)) request$params$config$cont <- unlist(request$params$config$cont, use.names = FALSE)
    value <- dispatch(request$method, ma_value(request, "params", list()))
    emit(list(id = request$id, ok = TRUE, value = value))
  }, error = function(e) emit(list(id = request$id, ok = FALSE, error = conditionMessage(e))))
}
close(connection)
