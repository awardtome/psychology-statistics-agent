# Read-only research inputs are frozen only after the questionnaire screening action.
# These functions never access the active raw table when inspecting an existing snapshot.
q_research_freeze <- function(q, directory) {
  need(identical(q$screening,"screened") && !is.null(q$screened),"请先应用问卷筛选/计分规则；原始数据和跳过整个筛选的数据不能加入科研构思。")
  d<-q$screened
  cfg<-q$config;cfg$rules<-lapply(cfg$rules,function(r){if(!is.null(r$records)){r$recordCount<-length(vec(r$records));r$records<-NULL};r})
  provenance<-list(origin="questionnaire_screening",name=q$name,revision=q$revision,rows=nrow(d),raw_rows=q$audit$raw_rows,
    excluded=q$audit$excluded,created=format(Sys.time(),"%Y-%m-%dT%H:%M:%S%z"),config=cfg,
    note="由问卷筛选输出保存；只包含保留记录及实际计分列，不包含排除记录或后续分组。")
  frozen<-list(format="psych-screened-v1",data=d,source=q$screened_source,provenance=provenance)
  file<-tempfile("screened-research-",tmpdir=directory,fileext=".rds");saveRDS(frozen,file,version=3)
  list(file=file,provenance=provenance,columns=lapply(q_columns(d),function(x)x[setdiff(names(x),"examples")]))
}
q_research_read <- function(file) {
  x<-readRDS(file)
  need(identical(x$format,"psych-screened-v1")&&identical(x$provenance$origin,"questionnaire_screening")&&is.data.frame(x$data)&&nrow(x$data)>0,"这不是由问卷筛选输出保存的数据快照。")
  need(nrow(x$data)==x$provenance$rows,"筛选后数据快照的记录数不一致。")
  x
}
q_research_profile <- function(file, columns=NULL) {
  x<-q_research_read(file);d<-x$data
  if(length(vec(columns))){cs<-as.character(vec(columns));need(length(cs)<=40&&all(cs %in% names(d)),"请选择这份筛选后数据中的至多 40 个变量。") ;d<-d[cs]}
  summaries<-lapply(names(d),function(n){
    v<-d[[n]];num<-is.numeric(v);finite<-if(num)v[is.finite(v)] else NULL
    list(column=n,type=class(v)[1],n=sum(!is.na(v)),missing=sum(is.na(v)),unique=length(unique(v[!is.na(v)])),
      mean=if(num&&length(finite))mean(finite) else NULL,sd=if(num&&length(finite)>1)sd(finite) else NULL,
      min=if(num&&length(finite))min(finite) else NULL,max=if(num&&length(finite))max(finite) else NULL,
      nonfinite=if(num)sum(!is.finite(v)&!is.na(v)) else 0,
      frequencies=if(length(unique(v[!is.na(v)]))<=10)as.data.frame(table(v,useNA="ifany"),stringsAsFactors=FALSE) else NULL)
  })
  local<-q_init();local$current<-x$data;local$config<-x$provenance$config
  list(provenance=x$provenance,rows=nrow(x$data),total_columns=ncol(x$data),variables=summaries,
    suitability=q_suitability(local),labels=local$config$labels,scales=local$config$scales,
    limitations=c("这些描述统计仅来自所选筛选后快照。","没有声明的变量含义、研究设计与测量属性需要使用者补充，不能按列名猜测。","相关探索不能作为验证性结果或因果证据；方法可行性仍须结合设计。"))
}
q_research_correlations <- function(file, columns, method="spearman") {
  x<-q_research_read(file);cs<-unique(as.character(vec(columns)))
  need(length(cs)>=2&&length(cs)<=20&&all(cs %in% names(x$data)),"请选择所选数据中 2 至 20 个变量。")
  need(method %in% c("pearson","spearman"),"相关工具支持 Pearson 或 Spearman。")
  need(all(vapply(x$data[cs],is.numeric,logical(1))),"相关工具只接受已有的数值变量，不自动编码文字。")
  pairs<-combn(cs,2,simplify=FALSE)
  tab<-do.call(rbind,lapply(pairs,function(p){a<-x$data[[p[1]]];b<-x$data[[p[2]]];ok<-is.finite(a)&is.finite(b);n<-sum(ok)
    r<-if(n>=3&&sd(a[ok])>0&&sd(b[ok])>0)suppressWarnings(cor(a[ok],b[ok],method=method)) else NA_real_
    data.frame(from=p[1],to=p[2],n=n,correlation=r,method=method,stringsAsFactors=FALSE)
  }))
  list(rows=nrow(x$data),table=tab,note="逐对完整案例的探索性相关；没有按显著性筛选，不输出验证性 p 值，不等于因果路径。")
}
q_research_report <- function(file, research, directory) {
  x<-q_research_read(file);q<-q_init();q$raw<-q$current<-x$data;q$source<-x$source;q$name<-paste0(x$provenance$name," / 筛选后科研构思");q$revision<-x$provenance$revision;q$screening<-"screened";q$config<-x$provenance$config
  q$audit<-list(snapshot=x$provenance,note="本报告只使用所选筛选后快照；不含原始/排除记录。科研构思属于建议，未自动执行所提议的分析。")
  q$reportInputLabel<-"所选筛选后快照";q$reportTitle<-"科研构思完整报告"
  q$reportDescription<-paste0("所选数据：",q$name,"，快照版本 ",q$revision,"，保留记录 ",nrow(q$current),"。只使用此筛选后快照；未导出原表或排除记录。任务状态：",research$status,"。以下是检索与研究方案，提议的分析尚未执行。")
  q$archive<-list(list(module="科研构思",summary=list(method="多智能体科研构思（API）",n=nrow(x$data),revision=q$revision,
    task=list(datasetId=research$datasetId,keywords=research$keywords,status=research$status,api_calls=research$calls),profile=q_research_profile(file),artifacts=research$artifacts,reader_digests=research$digests,queries=research$queries,tool_audit=research$toolAudit,
    limitations=c("采用论文的角色分工与专业工具思想，没有使用或训练论文的模型权重。","新颖性只在实际检索证据范围内审阅，不能保证研究是首创。"))))
  q_write_reports(q,directory,research=research)
}
