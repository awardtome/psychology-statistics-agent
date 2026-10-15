# Analysis registry. Individual methods share the same validated data pipeline.
for (module_file in c("graphs.R","mediation.R","heterogeneity.R","network.R","dag.R","machine-learning.R")) {
  source(file.path(worker_dir,"analyses",module_file),encoding="UTF-8")
}

q_validate_spec <- function(q,module,spec){
  if(module=="intervention"){q_intervention_spec(q,spec);return(list(status="ready",missing=character(),optional=c("目标和结果含义与操作方向","强度、重复次数与网络假设"),note="方案已核对；EI CS 与方法状态将在导出时再次检查。"))}
  required<-switch(module,mediation="paths",heterogeneity=c("method","variables"),network=c("variables","method","level"),dag="variables",ida=c("exposure","outcome"),ml=c("target","features","task"),character())
  if(module=="heterogeneity"&&identical(spec$method,"networktree"))required<-c(required,"splitVariables")
  missing<-required[vapply(required,function(n)is.null(spec[[n]])||!length(vec(spec[[n]]))||identical(spec[[n]],""),logical(1))]
  if(module=="mediation"&&!length(missing))q_mediation_check(q,spec)
  optional<-switch(module,mediation=c("测量时间顺序与理论依据","分类编码、协变量及缺失","交互项、间接路径与重抽样区间"),heterogeneity=c("指标尺度、标准化及类别数范围","类别大小、局部独立与验证","网络树的分裂变量"),network=c("题项/分量表/总分与构念映射","相关类型及稳定性重抽样次数","比较分组与相同测量"),dag=c("数据类型与时间先后约束","潜在混淆、测量误差","算法与稳健性比较"),ida=c("线性高斯、无未测混淆、无环与忠实性","CPDAG 来源及暴露/结局意义"),ml=c("预测时可获得的特征","独立受访者/机构分组划分","训练验证与保留测试集"),character())
  list(status=if(length(missing))"needs_input" else "ready",missing=missing,optional=optional,note="可跳过补充问答；实际计算仍需指定变量/路径，不能替使用者编造。")
}
q_run <- function(q,module,spec,output){
  need(!is.null(q$current),"请先导入数据。");check<-q_validate_spec(q,module,spec);need(check$status=="ready",paste("还需指定：",paste(check$missing,collapse="、")));dir.create(output,recursive=TRUE,showWarnings=FALSE);warnings<-character()
  fn<-switch(module,mediation=q_mediation,heterogeneity=q_heterogeneity,network=q_network,dag=q_dag,ida=q_ida,ml=q_ml);need(!is.null(fn),"未知分析入口。")
  out<-withCallingHandlers(fn(q,spec,output),warning=function(w){warnings<<-unique(c(warnings,conditionMessage(w)));invokeRestart("muffleWarning")})
  out$summary$warnings<-warnings;out$summary$revision<-q$revision;out$summary$specification<-spec;out$summary$image<-out$image
  # A replaced upstream fit cannot leave an old downstream result active.
  downstream<-switch(module,network=c("dag","ida","intervention"),dag="ida",character())
  for(key in downstream){q$results[[key]]<-NULL;q$fits[[key]]<-NULL;q$specs[[key]]<-NULL}
  q$results[[module]]<-out$summary;q$fits[[module]]<-out$fit;q$specs[[module]]<-spec
  q$archive[[length(q$archive)+1]]<-list(module=module,summary=out$summary,image=out$image,members=out$members,predictions=out$predictions,data=q$current,audit=q$audit,source=q$source)
  if(!is.null(out$members)){q$members<-out$members;q$branch_pool<-list(data=q$current,source=q$source,members=out$members,summary=out$summary,revision=q$revision)};if(!is.null(out$predictions))q$predictions<-out$predictions
  list(summary=out$summary,image=out$image,state=q_snapshot(q))
}
