# Source this exported main script in RStudio. No model API is called.
source_files<-unlist(lapply(sys.frames(),function(frame)if(!is.null(frame$ofile))as.character(frame$ofile)else NULL),use.names=FALSE)
script<-if(length(source_files))tail(source_files,1)else NULL
if(is.null(script)){args<-commandArgs(FALSE);script<-sub("^--file=","",args[grepl("^--file=",args)])[1]}
if(is.null(script)||is.na(script))stop("请在 RStudio 使用 Source 执行主脚本。")
worker_dir<-dirname(normalizePath(script));setwd(worker_dir)
options(repos=c(CRAN="https://cloud.r-project.org"))
for(pkg in c("jsonlite","remotes","openxlsx","officer","xml2","zip"))if(!requireNamespace(pkg,quietly=TRUE))install.packages(pkg)
manifest<-jsonlite::read_json("task-manifest.json",simplifyVector=TRUE);s<-manifest$specification
data<-read.csv("network-data.csv",check.names=FALSE);net<-readRDS("network-fit.rds")
stopifnot(identical(colnames(data),as.character(manifest$nodes)),nrow(data)==manifest$n,all(s$targets%in%colnames(data)),all(s$outcomes%in%colnames(data)))
result_dir<-file.path("results",paste0("run-",format(Sys.time(),"%Y%m%d-%H%M%OS6")))
dir.create(result_dir,recursive=TRUE,showWarnings=FALSE)
output_file<-function(name)file.path(result_dir,name)
set.seed(20261004)
source("external-simulation.R",encoding="UTF-8")
limitations<-c("每次只操作一个指定节点，完整网络保留；不是同时干预多变量。","节点模拟、KL 和横断面关联不证明真实干预效果。","EI CS > 0.25 只是进入模拟的最低门槛；不保证模型和研究设计适用。")
extra<-list()
pv<-function(x,key,default=NULL)if(is.null(x[[key]]))default else x[[key]]
vec<-function(x)unlist(x,use.names=FALSE)
need<-function(ok,message)if(!isTRUE(ok))stop(message,call.=FALSE)
source("reports.R",encoding="UTF-8")
save_intervention_report<-function(tab,extra,limitations,image=NULL,status="completed"){
summary<-c(list(method=paste(toupper(s$method),status),n=nrow(data),revision=manifest$revision,table=tab,specification=s,limitations=limitations,cs_origin=manifest$cs_origin),extra)
q<-list(raw=data,current=data,source=seq_len(nrow(data)),name=manifest$source,revision=manifest$revision,screening="external",audit=list(task=manifest),figures=list(),archive=list(list(module="intervention",summary=summary,image=image,data=data,source=seq_len(nrow(data)),audit=list(task=manifest))))
q_write_reports(q,result_dir)
}

if(s$method=="nirapost"){
  pin<-"6231832736df7c693b630820729f73f64c30ec92"
  if(!requireNamespace("NIRApost",quietly=TRUE)||!identical(utils::packageDescription("NIRApost")$RemoteSha,pin))remotes::install_github(paste0("kingfly51/NIRA_post@",pin),upgrade="never")
  moderation<-NIRApost::runMgmmAnalysis(data,plotResults=FALSE,nB=s$repetitions)
  saveRDS(moderation,output_file("moderation.rds"))
  if(length(moderation$significant_moderators)){
    diagnostic<-do.call(rbind,lapply(moderation$significant_moderators,function(x)data.frame(moderator=colnames(data)[x$node],significant_edges=paste(x$significant_edges,collapse=", "),status="blocked_before_simulation")))
    write.csv(diagnostic,output_file("precondition-status.csv"),row.names=FALSE,fileEncoding="UTF-8")
    save_intervention_report(diagnostic,list(moderation=lapply(moderation$significant_moderators,function(x)x$interaction_details)),c(limitations,"检测到调节证据；未进行 NIRA 模拟。本报告仅是前提诊断，不是干预结果。"),status="前提检查未通过")
    stop("存在调节证据，固定边权前提不满足。已保存 Excel、Word、precondition-status.csv 与 moderation.rds；请返回 Agent 讨论。")
  }
  first<-simulate_selected_nira(net$graph,net$intercepts,s$targets,s$outcomes,s$direction,s$intensity)
  result<-NIRApost::permutationNIRAtest(first$long,method="bonferroni")
  tab<-cbind(target=s$targets[match(rownames(result$stat),first$mapping$condition)],result$stat)
  stopifnot(!anyNA(tab$target),setequal(tab$target,s$targets))
  means<-matrix(NA_real_,nrow=s$repetitions,ncol=length(first$means),dimnames=list(NULL,names(first$means)))
  set.seed(20261004)
  for(i in seq_len(s$repetitions)){means[i,]<-simulate_selected_nira(net$graph,net$intercepts,s$targets,s$outcomes,s$direction,s$intensity)$means;if(i%%10==0)message("已完成重复 ",i," / ",s$repetitions)}
  changes<-sweep(means[,-1,drop=FALSE],1,means[,1],"-")
  stability<-data.frame(target=s$targets,mean_change=colMeans(changes),mc_sd=apply(changes,2,sd),repetitions=s$repetitions)
  extra<-list(monte_carlo_stability=stability,threshold_shift=first$shift,simulated_per_condition=5000)
  saveRDS(list(result=result,means=means,mapping=first$mapping),output_file("nirapost.rds"))
  values<-tab$effect;labels<-tab$target;ylabel<-"Change in selected outcome sum"
  limitations<-c(limitations,"按所选结果变量求和；目标与结果重叠时包含目标节点自身变化。","置换 p 值、区间和重复稳定性基于拟合参数固定时的模拟样本，只描述 Monte Carlo 不确定性，不是原始样本抽样推断。未使用全节点稳定性函数冒充所选目标的稳定性。")
}else if(s$method=="ggmnira"){
  pin<-"9b3583a2e0f7af0422ea9291fd7060e2749264cb"
  if(!requireNamespace("GGMNIRA",quietly=TRUE)||!identical(utils::packageDescription("GGMNIRA")$RemoteSha,pin))remotes::install_github(paste0("ywu6888/GGMNIRA@",pin),upgrade="never")
  partial<-s$outcomeScope=="selected_nodes"
  types<-if(partial)ifelse(colnames(data)%in%s$outcomes,"Selected outcomes","Remaining nodes") else NULL
  result_type<-if(partial)"cross_marginal" else "node_joint"
  result<-GGMNIRA::GGM_NIRA(net=net,type=types,manipulation_I=s$intensity,result_type=result_type)
  tab<-GGMNIRA::GGMNIRA_result(result,result_type=result_type,target_construct=if(partial)"Selected outcomes" else NULL)
  tab<-tab[as.character(tab$manipulated_node)%in%s$targets,,drop=FALSE]
  stopifnot(nrow(tab)==length(s$targets),setequal(as.character(tab$manipulated_node),s$targets))
  saveRDS(result,output_file("ggmnira-full-network.rds"))
  boot<-GGMNIRA::bootnet_KL(data=data,type=types,result_type=result_type,manipulation_I=s$intensity,target_construct=if(partial)"Selected outcomes" else NULL,nBoots=s$repetitions,nCores=2,use_cor_auto=identical(manifest$correlation,"cor_auto"),seed=20261004)
  saveRDS(boot,output_file("kl-bootstrap.rds"))
  cs<-GGMNIRA::corStability_KL(boot);capture.output(print(cs),file=output_file("kl-stability.txt"))
  extra<-list(kl_stability=capture.output(print(cs)))
  values<-tab$KL;labels<-as.character(tab$manipulated_node);ylabel<-"Projected KL divergence"
  limitations<-c(limitations,"GGMNIRA 方法目前按预印本状态使用：https://arxiv.org/abs/2607.00915；使用时请再次核对发表状态。","KL 衡量分布变化幅度，没有改善/恶化方向。操作强度是条件均值操作单位，不是 Ising 阈值 SD。","指定结果集合是使用者选择的分析集合，不代表软件认定这些变量属于同一量表。KL CS 针对该结果范围所有候选节点，不能冒充 EI CS 或所选子集稳定性。")
}else stop("未知方法")
if(s$repetitions<1000)limitations<-c(limitations,"当前重复少于 1000；仅供探索或验证流程，正式报告需评估稳定性与精度。")
plot_result<-function(){par(mar=c(6,5,3,1));barplot(values,names.arg=labels,las=2,col="#397C65",border=NA,ylab=ylabel,main=toupper(s$method));abline(h=0,col="#888888")}
png(output_file("intervention.png"),width=2400,height=1800,res=300);plot_result();dev.off()
grDevices::cairo_pdf(output_file("intervention.pdf"),width=8,height=6,family="Microsoft YaHei");plot_result();dev.off()
write.csv(tab,output_file("statistics.csv"),row.names=FALSE,fileEncoding="UTF-8")
file.copy("task-manifest.json",output_file("task-manifest.json"),overwrite=TRUE)
capture.output(sessionInfo(),file=output_file("session-info.txt"))
save_intervention_report(tab,extra,limitations,output_file("intervention.png"))
message("完成：",normalizePath(result_dir)," 包含完整 Excel、Word、PNG/PDF、statistics.csv。每次运行独立保存，不混用旧结果。")
