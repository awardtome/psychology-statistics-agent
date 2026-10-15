q_network <- function(q,s,output){
  cs<-as.character(vec(s$variables));need(length(cs)>=3&&length(cs)<=40,"网络需要 3 到 40 个指定节点。");method<-s$method;need(method%in%c("ggm","ising"),"请选择 GGM 或 Ising。");need(s$level%in%c("items","subscales","totals"),"请指定节点层次。")
  for(scale in q$config$scales)need(!(scale$name%in%cs&&any(as.character(vec(scale$items))%in%cs)),"不能将总分与组成题项放入同一网络。")
  group<-pv(s,"group","");z<-q_data(q,cs,extras=if(nzchar(group))group else character());d<-z$data[cs]
  if(method=="ising")need(all(vapply(d,function(v)all(v%in%c(0,1)),logical(1))),"Ising 仅接受 0/1 节点，不会擅自二分。")
  if(method=="ggm")need(!all(vapply(d,function(v)all(v%in%c(0,1)),logical(1))),"所有节点是 0/1，请选 Ising。")
  estimate<-if(method=="ising")function(x)bootnet::estimateNetwork(x,default="IsingFit",verbose=FALSE) else function(x)bootnet::estimateNetwork(x,default="EBICglasso",corMethod=pv(s,"correlation","cor"),verbose=FALSE)
  fit<-estimate(d);a<-fit$graph;file<-file.path(output,"network.png");q_graph(a,cs,file,paste(toupper(method),"network"));coef<-NULL
  B<-as.integer(pv(s,"bootstrap",0));need(B>=0&&B<=2500,"CS 重抽样次数为 0 到 2500。")
  if(B>0){need(B>=50,"CS 至少使用 50 次重抽样；正式报告建议 1000 以上。");set.seed(20261003);bt<-bootnet::bootnet(fit,nBoots=B,type="case",statistics=c("strength","expectedInfluence"),nCores=1,caseN=5,verbose=FALSE);coef<-as.list(bootnet::corStability(bt,statistics=c("strength","expectedInfluence"),verbose=FALSE))}
  comparison<-NULL
  if(nzchar(group)){
    groups<-unique(as.character(z$data[[group]]));need(length(groups)==2,"比较需要恰好两个组。");split<-lapply(groups,function(g)d[as.character(z$data[[group]])==g,,drop=FALSE]);need(all(vapply(split,nrow,integer(1))>=30),"每组完整案例至少 30，仍需评估充分性。")
    a1<-estimate(split[[1]])$graph;a2<-estimate(split[[2]])$graph;up<-upper.tri(a1);obs<-c(structure=max(abs(a1-a2)),strength=abs(sum(abs(a1[up]))-sum(abs(a2[up]))));P<-as.integer(pv(s,"permutations",100));need(P>=20&&P<=1000,"比较置换次数为 20 到 1000。");set.seed(20261003);null<-matrix(NA,P,2);failed<-0L
    for(b in seq_len(P))tryCatch({idx<-sample(nrow(d));g1<-estimate(d[idx[seq_len(nrow(split[[1]]))],,drop=FALSE])$graph;g2<-estimate(d[idx[-seq_len(nrow(split[[1]]))],,drop=FALSE])$graph;null[b,]<-c(max(abs(g1-g2)),abs(sum(abs(g1[up]))-sum(abs(g2[up]))))},error=function(e)failed<<-failed+1L)
    need(sum(complete.cases(null))>=.9*P,"超过 10% 置换失败，不报告可能有偏的 p 值。");comparison<-list(groups=groups,n=vapply(split,nrow,integer(1)),statistics=as.list(obs),p=as.list((1+colSums(sweep(null,2,obs,function(x,y)x>=y),na.rm=TRUE))/(1+sum(complete.cases(null)))),permutations=P,failed=failed,note="独立组标签置换，比较最大边差与总体绝对边强度；不能用于配对/聚类数据，未检验测量等值。")
  }
  list(summary=list(method=toupper(method),n=z$n,complete_case_excluded=z$removed,nodes=cs,level=s$level,edges=q_edges(a,cs),centrality=data.frame(node=cs,strength=rowSums(abs(a)),expectedInfluence=rowSums(a)),cs=coef,bootstrap=B,comparison=comparison,limitations=c("网络是条件关联，不能自动证明因果。","CS 必须匹配指标；>0.25 为最低门槛，更希望 >0.50。","无 case-dropping 结果则不报告 CS；低次数仅供初步检查。")),fit=list(network=fit,data=d,method=method,columns=cs,cs=coef,revision=q$revision),image=file)
}
