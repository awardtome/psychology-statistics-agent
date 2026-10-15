# Declarative questionnaire pipeline. Raw records are immutable; all rules are explicit.
pv <- function(x, key, default = NULL) if (is.null(x[[key]])) default else x[[key]]
vec <- function(x) unlist(x, use.names = FALSE)
need <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
q_parse_columns <- function(data) {
  # CSV readers preserve text for IDs; parse only unambiguous numeric columns in a working copy.
  conversions<-list()
  for(name in names(data))if(is.character(data[[name]])){
    raw<-data[[name]];x<-trimws(raw);observed<-!is.na(x)&nzchar(x);values<-x[observed]
    numeric_text<-length(values)>0&&all(grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$",values))
    identifiers<-any(grepl("^[+-]?0[0-9]",values))||any(grepl("^[+-]?[0-9]{16,}$",values))
    if(numeric_text&&!identifiers){
      parsed<-suppressWarnings(as.numeric(x));need(all(is.finite(parsed[observed])),paste0("数值列 ",name," 超出可表示范围，请核对来源。"))
      data[[name]]<-parsed;conversions[[length(conversions)+1L]]<-list(column=name,operation="numeric_text_parsing",count=sum(observed),note="工作副本将明确数值文本解析为数值，原表文本不变；前导零/长整数编号保留文本。")
    }
  }
  list(data=data,conversions=conversions)
}
q_columns <- function(data) lapply(names(data), function(n) {
  x <- data[[n]]; numeric <- is.numeric(x)
  list(column = n, type = class(x)[1], missing = sum(is.na(x)), unique = length(unique(x[!is.na(x)])),
    min = if (numeric && any(!is.na(x))) min(x, na.rm = TRUE) else NULL,
    max = if (numeric && any(!is.na(x))) max(x, na.rm = TRUE) else NULL,
    examples = head(unique(x[!is.na(x)]), 5))
})
q_snapshot <- function(q) list(status = if (is.null(q$raw)) "no_data" else "ready", name = q$name,
  rows = if (is.null(q$current)) 0 else nrow(q$current), raw_rows = if (is.null(q$raw)) 0 else nrow(q$raw),
  columns = if (is.null(q$current)) list() else q_columns(q$current), preview = head(q$current, 6),
  config = q$config, audit = q$audit, review = q$review, screening = q$screening, revision = q$revision,
  branch=q$branch, branch_options=if(is.null(q$branch_pool))NULL else list(revision=q$branch_pool$revision,method=q$branch_pool$summary$method,groups=pv(q$branch_pool$summary,"sizes",q$branch_pool$summary$groups)), results = q$results, figures = lapply(q$figures,function(f)f[setdiff(names(f),c("files","table"))]), specifications = q$specs, suitability = if (is.null(q$current)) NULL else q_suitability(q))
q_init <- function() {
  q <- new.env(parent = globalenv()); q$raw <- q$current <- NULL; q$config <- list(orientation = "rows_people", scales = list(), rules = list(), labels = list());
  q$audit <- list(); q$review <- list(); q$screening <- "pending"; q$revision <- 0; q$results <- list(); q$fits <- list(); q$specs <- list(); q$name <- NULL; q$archive <- list(); q$figures<-list(); q$branch <- NULL; q
}
q_load <- function(q, data, name) {
  need(!anyDuplicated(names(data)), "存在重复列名，请先明确区分。")
  need(!any(grepl("^\\.agent_", names(data))), "数据中含软件保留的 .agent_ 列名，请先重命名。")
  q$figures<-list()
  parsed<-q_parse_columns(as.data.frame(data,check.names=FALSE))
  q$raw <- as.data.frame(data, check.names = FALSE); q$current <- parsed$data; q$name <- name; q$config <- list(orientation = "rows_people", scales = list(), rules = list(), labels = list());
  q$audit <- list(import=parsed$conversions); q$review <- q_review(q); q$screening <- "pending"; q$revision <- q$revision + 1; q$results <- q$fits <- q$specs <- list(); q$source <- seq_len(nrow(data)); q$archive<-list(); q$branch<-NULL;q$branch_pool<-NULL; q$members<-q$predictions<-NULL; q$screened<-q$current; q$screened_source<-q$source; q_snapshot(q)
}
q_review <- function(q) {
  d <- q$current; if (is.null(d)) return(list())
  out <- list(); add <- function(code, message, columns = NULL) out[[length(out) + 1]] <<- list(code = code, message = message, columns = columns, optional = TRUE)
  if (!length(q$config$labels)) add("meaning", "尚未说明各列的构念、量表和题号。可继续补充，也可跳过；软件不会猜测。")
  if (!length(q$config$scales)) add("scoring", "尚未定义题项归属、反向题及计分范围；如果已是总分，可以说明后跳过计分。")
  if (anyNA(d)) add("missing", "存在缺失值：请考虑缺失代码、缺失比例及计分最少答题数。分析会单独报告完整案例数量。", names(d)[vapply(d, anyNA, logical(1))])
  constants <- names(d)[vapply(d, function(x) length(unique(x[!is.na(x)])) < 2, logical(1))]
  if (length(constants)) add("constant", "这些列没有可分析的变化。", constants)
  if (any(duplicated(d))) add("duplicate", sprintf("发现 %d 条完全重复记录；只提示，不自动删除。", sum(duplicated(d))))
  if (!nzchar(pv(q$config, "id", ""))) add("id", "尚未指定受访者编号；重复受访、纵向与聚类记录需要明确处理。")
  add("design", "请考虑知情同意、纳入条件、作答时间、注意力题、同质作答，以及重复测量/抽样设计。未指定的规则不会自动启用。")
  out
}
q_screen <- function(q, config = list(), skipped = FALSE) {
  need(!is.null(q$raw), "请先导入问卷数据。")
  cfg <- modifyList(q$config, config, keep.null = TRUE)
  for (k in c("scales", "rules", "missingCodes", "labels")) if (!is.null(config[[k]])) cfg[[k]] <- config[[k]]
  # Skipping the entire step keeps orientation/labels, but applies no old transformations.
  if (skipped) for (k in c("scales", "rules", "missingCodes")) cfg[[k]] <- list()
  need(pv(cfg, "orientation", "rows_people") %in% c("rows_people", "columns_people"), "请选择一行一个人或一列一个人。")
  d <- q$raw; provenance <- seq_len(nrow(d))
  if (cfg$orientation == "columns_people") {
    key <- pv(cfg, "variableColumn", ""); need(key %in% names(d), "一列一个人时，需要明确存放变量/题号的那一列。")
    nms <- as.character(d[[key]]); need(!anyNA(nms) && !anyDuplicated(nms) && all(nzchar(nms)), "变量所在行必须有非空且唯一的题号/变量名。")
    people <- setdiff(names(d), key); need(length(people) > 1, "至少需要两名受访者。")
    d <- as.data.frame(t(as.matrix(d[people])), stringsAsFactors = FALSE, check.names = FALSE); names(d) <- nms
    provenance <- people
  }
  parsed<-q_parse_columns(d);d<-parsed$data
  transformed <- parsed$conversions; reasons <- rep("", nrow(d)); mark <- function(hit, reason) {
    hit[is.na(hit)] <- FALSE; reasons[hit] <<- ifelse(nzchar(reasons[hit]), paste(reasons[hit], reason, sep = "; "), reason)
  }
  cols <- function(xs) {xs <- as.character(vec(xs)); need(length(xs) > 0 && all(xs %in% names(d)), "规则/计分引用了不存在的列，请按实际列名指定。") ; xs}
  for (code in pv(cfg, "missingCodes", list())) {
    cs <- cols(code$columns); for (n in cs) {hit <- !is.na(d[[n]]) & as.character(d[[n]]) %in% as.character(vec(code$values)); d[[n]][hit] <- NA; transformed[[length(transformed)+1]] <- list(column=n,operation="missing_code",count=sum(hit))}
  }
  recoded<-q_parse_columns(d);d<-recoded$data;transformed<-c(transformed,recoded$conversions)
  for (rule in pv(cfg, "rules", list())) {
    kind <- rule$type; label <- pv(rule, "label", kind)
    if (kind == "range") {n <- cols(rule$columns); need(length(n)==1 && is.numeric(d[[n]]), "范围规则需要一个数值列。")
      x <- d[[n]]; hit <- (!is.na(x) & (x < pv(rule,"min",-Inf) | x > pv(rule,"max",Inf))); if (isTRUE(rule$excludeMissing)) hit <- hit | is.na(x); mark(hit,label)
    } else if (kind == "attention") {n <- cols(rule$columns); need(length(n)==1, "注意力规则需要一个列。") ; mark(is.na(d[[n]]) | !as.character(d[[n]]) %in% as.character(vec(rule$allowed)),label)
    } else if (kind == "missing_rate") {cs <- cols(rule$columns); threshold <- rule$max; need(is.numeric(threshold)&&threshold>=0&&threshold<=1,"缺失比例范围为 0 到 1。") ; mark(rowMeans(is.na(d[cs])) > threshold,label)
    } else if (kind == "duplicate_id") {cs <- cols(rule$columns); need(length(cs)==1,"重复编号规则需要一个列。"); need(pv(rule,"keep","first") %in% c("first","last","none"),"重复记录保留选项无效。"); x <- d[[cs]]; mode <- pv(rule,"keep","first"); hit <- duplicated(x,fromLast=mode=="last"); if(mode=="none")hit<-duplicated(x)|duplicated(x,fromLast=TRUE); mark(hit & !is.na(x),label)
    } else if (kind == "straightline") {cs <- cols(rule$columns); need(length(cs)>=3,"同质作答至少需要三个指定题项。"); mark(apply(d[cs],1,function(x) all(!is.na(x)) && length(unique(x))==1),label)
    } else if (kind == "exclude_records") {mark(as.character(provenance) %in% as.character(vec(rule$records)),label)
    } else stop("不支持的筛选规则：",kind,call.=FALSE)
  }
  original_items <- d
  scale_names <- character();item_coding<-list()
  for (s in pv(cfg,"scales",list())) {
    cs <- cols(s$items); name <- s$name; need(is.character(name)&&length(name)==1&&nzchar(name)&&!name %in% c(names(q$raw),scale_names),"新量表名必须唯一，不能覆盖原始列。")
    need(all(vapply(d[cs],is.numeric,logical(1))),"量表题项需要数值；不能默默把文字变成分数。")
    reversed <- as.character(vec(s$reverse)); need(all(reversed %in% cs),"反向题必须属于该量表。")
    x <- original_items[cs]; lo <- pv(s,"min",NULL); hi <- pv(s,"max",NULL)
    if(length(reversed)) {need(is.numeric(lo)&&is.numeric(hi)&&lo<hi,"反向计分需要明确的最小值和最大值。"); for(n in reversed)x[[n]]<-lo+hi-x[[n]]}
    for(n in cs){
      if(!is.null(item_coding[[n]]))need(isTRUE(all.equal(item_coding[[n]],x[[n]])),paste0("题项 ",n," 在不同量表中有冲突的反向规则；请先说明要保留的编码。"))
      item_coding[[n]]<-x[[n]];d[[n]]<-x[[n]]
    }
    invalid<-rep(FALSE,nrow(d))
    if(!is.null(lo)&&!is.null(hi))for(n in cs)invalid<-invalid|(!is.na(original_items[[n]])&(original_items[[n]]<lo|original_items[[n]]>hi))
    answered <- rowSums(!is.na(x)); minimum <- pv(s,"minAnswered",length(cs)); need(minimum>=1&&minimum<=length(cs),"计分最少答题数不正确。")
    ag <- pv(s,"aggregation","mean"); need(ag %in% c("mean","sum","prorated_sum"),"请选择均分、原始和分或按题数折算和分。")
    score <- switch(ag,mean=rowMeans(x,na.rm=TRUE),sum=rowSums(x,na.rm=TRUE),prorated_sum=rowMeans(x,na.rm=TRUE)*length(cs)); score[answered<minimum | !is.finite(score)] <- NA
    score[invalid]<-NA
    d[[name]] <- score; scale_names <- c(scale_names,name); transformed[[length(transformed)+1]] <- list(scale=name,items=cs,reverse=reversed,min=lo,max=hi,aggregation=ag,minAnswered=minimum,out_of_range_records=provenance[invalid],note="越界题项只使本量表得分缺失，不擅自排除受访者。")
  }
  keep <- !nzchar(reasons); need(any(keep),"规则排除了所有记录。请修改规则；此前的数据和结果已保留。")
  audit <- list(raw_rows=nrow(q$raw),oriented_rows=nrow(d),retained=sum(keep),excluded=sum(!keep),
    excluded_records=data.frame(source_record=as.character(provenance[!keep]),reason=reasons[!keep]),transformations=transformed,config=cfg,
    note="每次从原表重新应用整套规则；多条排除原因保留，原表不改。")
  q$current <- d[keep,,drop=FALSE]; q$source <- provenance[keep]; q$config <- cfg; q$audit <- audit; q$revision <- q$revision+1; q$results<-q$fits<-q$specs<-list();q$members<-q$predictions<-NULL
  q$screening <- if(skipped) "skipped" else "screened"; q$review <- q_review(q); q$screened<-q$current; q$screened_source<-q$source; q$branch<-NULL;q$branch_pool<-NULL; q_snapshot(q)
}
q_suitability <- function(q) {
  d <- q$current; numeric <- names(d)[vapply(d,is.numeric,logical(1))]; varying <- names(d)[vapply(d,function(x)length(unique(x[!is.na(x)]))>1,logical(1))]
  binary <- varying[vapply(d[varying],function(x) all(na.omit(x) %in% c(0,1)),logical(1))]; categorical <- varying[vapply(d[varying],function(x)length(unique(x[!is.na(x)]))<=10,logical(1))]
  scales <- vapply(q$config$scales,function(s)s$name,character(1)); items <- unique(as.character(vec(lapply(q$config$scales,function(s)s$items))))
  list(n=nrow(d),numeric=numeric,binary01=binary,categorical_candidates=categorical,known_scales=scales,known_items=items,
    choices=list(lpa=list(candidate=length(intersect(numeric,varying))>=2,reason="连续指标，可比较不同剖面数；低类别题项不应直接当连续量。"),
      lca=list(candidate=length(categorical)>=2,reason="分类指标；编码及局部独立假设需确认。"),
      networktree=list(candidate=length(numeric)>=2,reason="需要另外指定分组/分裂变量；标准网络树检验相关/协方差异质性。"),
      network=list(items=items,scales=scales,reason="题项、分量表或总分层次由研究问题和测量映射决定，不能只看列名；不要同时放总分和其组成题项。")),
    reminders=c("候选不代表适合：需要研究问题、独立记录、分布、测量和有效样本量。","网络比较还需要实际分组变量、每组样本量与相同测量。"))
}
q_data <- function(q, columns, numeric = TRUE, extras = character()) {
  cs <- unique(as.character(vec(columns))); need(length(cs)>0 && all(c(cs,extras) %in% names(q$current)),"请指定存在于当前数据的分析变量。")
  d <- q$current[unique(c(cs,extras))]; if(numeric)need(all(vapply(d[cs],is.numeric,logical(1))),"该方法需要数值变量；请先明确编码。")
  need(!any(vapply(d,function(x)is.numeric(x)&&any(!is.finite(x)&!is.na(x)),logical(1))),"分析变量含 Inf/-Inf 等非有限值，请先明确缺失编码或修正来源。")
  keep <- complete.cases(d); need(sum(keep)>=20,"当前分析的完整案例少于 20，请先核对变量与缺失；本版不进行这种小样本拟合。")
  need(all(vapply(d[cs],function(x)length(unique(x[keep]))>1,logical(1))),"分析变量中有常量列。")
  list(data=d[keep,,drop=FALSE],columns=cs,n=sum(keep),removed=sum(!keep),records=q$source[keep],keep=keep)
}
q_demo <- function() {
  set.seed(20261003); n<-240; X<-rnorm(n); W<-rnorm(n); M<-.6*X+rnorm(n); Y<-.25*X+.55*M+.3*X*W+rnorm(n)
  d<-data.frame(ID=sprintf("S%03d",1:n),X=X,M=M,Y=Y,W=W,Group=rep(c("A","B"),each=n/2),Duration=sample(120:600,n,TRUE))
  for(i in 1:6)d[[paste0("Q",i)]]<-pmin(5,pmax(1,round(3+.7*X+rnorm(n))))
  for(i in 1:5)d[[paste0("B",i)]]<-as.integer(.7*X+rnorm(n)>0)
  d$Duration[1:3]<-30; d$Q2[4:6]<-NA; d
}
