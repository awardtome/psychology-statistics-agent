# External simulation tasks are declarative: retain the complete fitted network,
# and restrict perturbations/reporting to the user's explicit targets.
q_intervention_spec <- function(q,s,gate=FALSE){
  need(s$method%in%c("nirapost","ggmnira"),"请选择 NIRApost 或 GGMNIRA。")
  f<-q$fits$network;need(!is.null(f)&&identical(f$revision,q$revision),"请先完成当前数据版本的网络。")
  expected<-if(s$method=="nirapost")"ising" else "ggm"
  need(identical(f$method,expected),paste("该方法需要",expected,"网络。"))
  targets<-unique(as.character(vec(s$targets)));need(length(targets)>0&&all(targets%in%f$columns),"请明确选择当前网络中的干预变量。")
  scope<-s$outcomeScope;need(scope%in%c("whole_network","selected_nodes"),"请选择评估整个网络还是指定结果变量。")
  outcomes<-if(scope=="whole_network")f$columns else unique(as.character(vec(s$outcomes)))
  need(length(outcomes)>0&&all(outcomes%in%f$columns),"请明确选择当前网络中的结果变量。")
  intensity<-s$intensity;need(is.numeric(intensity)&&length(intensity)==1&&is.finite(intensity)&&intensity>0,"请指定大于 0 的操作强度。")
  repetitions<-pv(s,"repetitions",1000);need(is.numeric(repetitions)&&length(repetitions)==1&&is.finite(repetitions)&&repetitions==floor(repetitions)&&repetitions>=1&&repetitions<=5000,"重复次数为 1 到 5000 的整数；少量重复仅适合调试。")
  if(s$method=="nirapost")need(s$direction%in%c("alleviating","aggravating"),"请选择降低或升高 Ising 阈值。")
  if(s$method=="ggmnira")need(intensity==floor(intensity),"当前 GGMNIRA 稳定性接口要求正整数操作强度。")
  if(s$method=="ggmnira"&&scope=="selected_nodes")need(!any(targets%in%outcomes),"GGMNIRA 指定结果集合时，干预变量与结果变量不能重叠；如评估整体影响请选择整个网络。")
  if(gate){status<-q_intervention_status(q);need(if(s$method=="nirapost")status$nira else status$ggmnira,"需要同一数据版本与节点的 EI CS > 0.25；请先完成对应网络稳定性分析。")}
  list(method=s$method,targets=targets,outcomeScope=scope,outcomes=outcomes,intensity=intensity,direction=if(s$method=="nirapost")s$direction else NULL,repetitions=as.integer(repetitions),revision=q$revision)
}

q_external_script <- function(q,method,directory,confirmed=FALSE,spec=NULL){
  need(!is.null(spec),"请先指定干预变量、结果范围与操作强度。")
  need(identical(method,spec$method),"导出方法与当前干预方案不一致，请先选择相应方法。")
  s<-q_intervention_spec(q,spec,TRUE)
  if(method=="ggmnira")need(confirmed,"请由使用者确认已了解 GGMNIRA 的预印本方法状态。")
  f<-q$fits$network;status<-q_intervention_status(q)
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  write.csv(f$data,file.path(directory,"network-data.csv"),row.names=FALSE,fileEncoding="UTF-8")
  saveRDS(f$network,file.path(directory,"network-fit.rds"))
  manifest<-list(method=method,revision=q$revision,n=nrow(f$data),nodes=f$columns,cs=status$cs,cs_origin=pv(f,"cs_origin","本软件计算"),metric=status$metric,source=q$name,specification=s,
    correlation=pv(q$results$network$specification,"correlation","cor"),note="保留完整网络，每次只操作一个指定变量；评估所选结果集合。模拟或 KL 不证明真实干预因果。")
  jsonlite::write_json(manifest,file.path(directory,"task-manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  for(name in c("external-intervention.R","external-simulation.R","reports.R","report-template.docx"))need(file.copy(file.path(worker_dir,name),file.path(directory,if(name=="external-intervention.R")paste0(method,".R") else name),overwrite=TRUE),"任务文件复制失败。")
  q$specs$intervention<-s
  list(directory=directory,files=list.files(directory),specification=s,message="请在 RStudio 用 Source 运行 .R 主脚本。results/ 将生成完整 Excel、Word、PNG/PDF 和 statistics.csv，导回 statistics.csv 与图片供本 Agent 解读。长时计算不逐轮请求 AI，避免耗费 token。")
}
