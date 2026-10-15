q_dag <- function(q,s,output){
  cs<-as.character(vec(s$variables));need(length(cs)>=3&&length(cs)<=30,"DAG 需要 3 到 30 个变量。");method<-pv(s,"algorithm","hc");need(method%in%c("hc","tabu","mmhc","pc","ges","fci"),"未知算法。");kind<-pv(s,"dataType","continuous");need(kind%in%c("continuous","categorical"),"请明确连续或分类数据类型。")
  z<-q_data(q,cs,numeric=kind=="continuous");d<-z$data;if(kind=="categorical")d[]<-lapply(d,factor);aliases<-paste0("v",seq_along(cs));names(d)<-aliases;set.seed(20261003);alpha<-pv(s,"alpha",.05);need(alpha>0&&alpha<1,"alpha 应在 0 到 1 之间。")
  if(method%in%c("pc","ges","fci"))need(kind=="continuous","PC/GES/FCI 当前为高斯检验实现；分类数据请选 HC/Tabu/MMHC。")
  if(kind=="continuous")need(qr(as.matrix(scale(d)))$rank==length(cs),"变量线性依赖，检查总分与题项重叠。")
  score<-if(kind=="continuous")"bic-g" else "bic";test<-if(kind=="continuous")"cor" else "mi";graph_type<-"CPDAG";dag<-NULL
  if(method%in%c("hc","tabu","mmhc")){
    blacklist<-NULL;if(length(s$forbidden))blacklist<-do.call(rbind,lapply(s$forbidden,function(p){need(all(c(p$from,p$to)%in%cs),"禁止边引用了不存在节点。");c(aliases[match(p$from,cs)],aliases[match(p$to,cs)])}))
    if(method=="hc")dag<-bnlearn::hc(d,score=score,restart=5,blacklist=blacklist)
    if(method=="tabu")dag<-bnlearn::tabu(d,score=score,blacklist=blacklist)
    if(method=="mmhc")dag<-bnlearn::mmhc(d,blacklist=blacklist,restrict.args=list(test=test,alpha=alpha),maximize.args=list(score=score))
    eq<-bnlearn::cpdag(dag);a<-bnlearn::amat(eq)[aliases,aliases];graph<-bnlearn::as.graphNEL(eq)
  }else if(method=="ges"){
    need(!length(s$forbidden),"GES 本版未支持方向禁止约束。");sc<-methods::new("GaussL0penObsScore",as.matrix(d));f<-pcalg::ges(sc);graph<-as(f$essgraph,"graphNEL");a<-as(graph,"matrix")[aliases,aliases]
  }else{
    need(!length(s$forbidden),"PC/FCI 本版未支持方向禁止约束。");fn<-if(method=="pc")pcalg::pc else pcalg::fci;f<-fn(suffStat=list(C=cor(d),n=nrow(d)),indepTest=pcalg::gaussCItest,alpha=alpha,labels=aliases,verbose=FALSE)
    if(method=="pc"){graph<-f@graph;a<-as(graph,"matrix")[aliases,aliases]}else{a<-f@amat;graph<-NULL;graph_type<-"PAG"}
  }
  file<-file.path(output,"dag.png");q_graph(a,cs,file,paste(toupper(method),graph_type),TRUE,pag=graph_type=="PAG");prior<-q$fits$dag;comparison<-NULL
  if(!is.null(prior)&&identical(prior$columns,cs)&&prior$revision==q$revision){old<-prior$matrix;out<-list();for(i in seq_len(length(cs)-1))for(j in (i+1):length(cs))if(any(old[c(i,j),c(i,j)]!=a[c(i,j),c(i,j)]))out[[length(out)+1]]<-list(from=cs[i],to=cs[j],previous=c(old[i,j],old[j,i]),current=c(a[i,j],a[j,i]));comparison<-list(previous_algorithm=prior$algorithm,changed_edges=out,note="相同数据和节点下比较邻接、方向与端点；PAG 数字不能当作 CPDAG 方向解释。")}
  ready<-graph_type=="CPDAG"&&kind=="continuous"&&!is.null(graph)
  list(summary=list(method=toupper(method),n=z$n,complete_case_excluded=z$removed,nodes=cs,graph_type=graph_type,edges=q_edges(a,cs,pag=graph_type=="PAG"),matrix=structure(a,dimnames=list(cs,cs)),selected_dag=if(is.null(dag))NULL else q_edges(bnlearn::amat(dag)[aliases,aliases],cs),ida_candidate=ready,comparison=comparison,limitations=c("爬山法优化 BIC，选定 DAG 不代表方向已被因果识别。","HC/Tabu/MMHC 同时报告代表 DAG 与 CPDAG 未定向边。","PC 依赖独立性检验，GES 依赖评分，FCI 允许潜在混淆并返回 PAG，不能直接用于标准 IDA。","算法差异不以多数投票定因果；仍需理论、时间顺序、混淆与稳定性审查。")),fit=list(matrix=a,graph=graph,data=d,columns=cs,algorithm=method,graph_type=graph_type,ida_candidate=ready,revision=q$revision),image=file)
}
q_ida <- function(q,s,output){
  f<-q$fits$dag;need(!is.null(f)&&f$revision==q$revision&&isTRUE(f$ida_candidate),"请先以连续原始数据计算 HC/Tabu/MMHC/PC/GES 的 CPDAG；PAG、分类及旧数据不可直接用于本版 IDA。");need(isTRUE(s$assumptionsConfirmed),"请由使用者在界面确认 IDA 识别假设。")
  x<-match(s$exposure,f$columns);y<-match(s$outcome,f$columns);need(!is.na(x)&&!is.na(y)&&x!=y,"暴露与结局必须是 CPDAG 中不同节点。")
  gn<-graph::nodes(f$graph);ex<-names(f$data)[x];oy<-names(f$data)[y]
  effects<-as.numeric(pcalg::ida(x.pos=match(ex,gn),y.pos=match(oy,gn),mcov=cov(f$data[gn]),graphEst=f$graph,method="local",type="cpdag"));need(length(effects)>0&&all(is.finite(effects)),"IDA 没有有效候选效应。")
  file<-file.path(output,"ida.png");grDevices::png(file,width=1200,height=800,res=120);tryCatch(plot(seq_along(effects),effects,pch=19,xlab="Compatible parent sets",ylab="Possible total effect",main="IDA possible effects"),finally=grDevices::dev.off())
  list(summary=list(method="IDA",exposure=s$exposure,outcome=s$outcome,n=nrow(f$data),effects=effects,min=min(effects),max=max(effects),source_algorithm=f$algorithm,limitations=c("可能总效应集合的范围不是 95% 置信区间。","依赖线性高斯、无未测混淆、无环与忠实性，数据不能自动证明。","横断面结果为探索性候选效应，不等于真实干预有效。")),fit=effects,image=file)
}
