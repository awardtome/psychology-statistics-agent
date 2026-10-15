# Threshold shift follows nodeIdentifyR::simulateResponses. The full joint
# network is retained; selected outcomes affect aggregation only.
simulate_selected_nira <- function(weights,thresholds,targets,outcomes,direction,intensity,samples=5000){
  stopifnot(all(targets%in%colnames(weights)),all(outcomes%in%colnames(weights)),length(thresholds)==ncol(weights))
  shift<-intensity*stats::sd(thresholds)*if(direction=="alleviating")-1 else 1
  if(!is.finite(shift)||shift==0)stop("阈值没有可用的标准差，不能按 SD 定义扰动。")
  indices<-match(outcomes,colnames(weights));conditions<-c("original",paste0("target_",seq_along(targets)))
  means<-setNames(numeric(length(conditions)),conditions);long<-vector("list",length(conditions))
  for(i in seq_along(conditions)){
    perturbed<-thresholds
    if(i>1){j<-match(targets[i-1],colnames(weights));perturbed[j]<-perturbed[j]+shift}
    responses<-IsingSampler::IsingSampler(n=samples,graph=weights,thresholds=perturbed,responses=c(0L,1L))
    scores<-rowSums(responses[,indices,drop=FALSE]);means[i]<-mean(scores)
    long[[i]]<-data.frame(sample=conditions[i],sumscore=scores)
  }
  list(long=do.call(rbind,long),means=means,mapping=data.frame(condition=conditions[-1],target=targets),shift=shift)
}
