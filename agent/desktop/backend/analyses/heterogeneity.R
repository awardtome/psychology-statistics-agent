q_heterogeneity <- function(q,s,output){
  method<-s$method;need(method%in%c("lpa","lca","networktree"),"请选择 LPA、LCA 或网络树。")
  vars<-as.character(vec(s$variables));splits<-as.character(vec(pv(s,"splitVariables",list())));z<-q_data(q,vars,numeric=method!="lca",extras=if(method=="networktree")splits else character());d<-z$data
  kmax<-as.integer(pv(s,"maxClasses",4));need(kmax>=1&&kmax<=8,"类别数为 1 到 8。");set.seed(20261003);file<-file.path(output,"heterogeneity.png")
  if(method=="networktree"){
    need(length(vars)>=2&&length(splits)>0,"网络树需要至少两个节点与一个指定分裂变量。");need(!any(splits%in%vars),"分裂变量不能同时作节点。")
    split_data<-d[splits];split_data[]<-lapply(split_data,function(v)if(is.character(v)||is.logical(v))factor(v) else v)
    fit<-networktree::networktree(nodevars=as.matrix(d[vars]),splitvars=split_data,method="mob",model="correlation",transform="pcor",minsize=as.integer(pv(s,"minSize",max(30,2*length(vars)+choose(length(vars),2)))),alpha=pv(s,"alpha",.05),bonferroni=TRUE,maxdepth=3)
    groups<-as.integer(predict(fit,type="node"));grDevices::png(file,width=1400,height=1000,res=120);tryCatch(plot(fit),finally=grDevices::dev.off())
    return(list(summary=list(method="网络树",n=z$n,complete_case_excluded=z$removed,groups=as.data.frame(table(groups)),splitVariables=splits,limitations=c("标准网络树分割相关/协方差结构，不能称为 Ising 参数异质性。","分组发现需要独立验证并考虑选择偏差。")),fit=fit,image=file,members=data.frame(source_record=z$records,class=groups)))
  }
  need(length(vars)>=2,"至少需要两个指标。");candidates<-list();fits<-list();failures<-list()
  if(method=="lpa"){
    suppressPackageStartupMessages(library(mclust));x<-as.matrix(d[vars]);if(isTRUE(pv(s,"standardize",TRUE)))x<-scale(x)
    for(k in seq_len(kmax))tryCatch({f<-mclust::Mclust(x,G=k,modelNames=pv(s,"covariance","EEI"),verbose=FALSE);need(!is.null(f$bic)&&is.finite(f$bic),"无有效 BIC。");fits[[as.character(k)]]<-f;candidates[[length(candidates)+1]]<-data.frame(classes=k,BIC=-f$bic,logLik=f$loglik)},error=function(e)failures[[as.character(k)]]<<-conditionMessage(e))
  }else{
    category_labels<-lapply(d[vars],function(v)levels(factor(v)))
    x<-as.data.frame(lapply(d[vars],function(v)as.integer(factor(v))));names(x)<-paste0("v",seq_along(vars));need(all(vapply(x,function(v)length(unique(v))<=10,logical(1))),"LCA 指标类别过多，请说明分类规则或选 LPA。");form<-as.formula(paste0("cbind(",paste(names(x),collapse=","),") ~ 1"))
    for(k in seq_len(kmax))tryCatch({f<-poLCA::poLCA(form,x,nclass=k,nrep=3,maxiter=1500,verbose=FALSE,calc.se=FALSE);need(is.finite(f$bic),"无有效 BIC。");fits[[as.character(k)]]<-f;candidates[[length(candidates)+1]]<-data.frame(classes=k,BIC=f$bic,logLik=f$llik)},error=function(e)failures[[as.character(k)]]<<-conditionMessage(e))
  }
  need(length(candidates)>0,"所有类别模型拟合失败。");tab<-do.call(rbind,candidates);best<-tab$classes[which.min(tab$BIC)];selected<-as.integer(pv(s,"classes",best));need(as.character(selected)%in%names(fits),"所选类别无有效模型。");fit<-fits[[as.character(selected)]]
  posterior<-if(method=="lpa")fit$z else fit$posterior;groups<-max.col(posterior);entropy<-if(selected==1)NA_real_ else 1+sum(posterior*log(pmax(posterior,1e-15)))/(nrow(posterior)*log(selected))
  if(method=="lpa"){
    fitted_means<-matrix(fit$parameters$mean,nrow=length(vars),ncol=selected)
    centers<-if(isTRUE(pv(s,"standardize",TRUE)))attr(x,"scaled:center") else rep(0,length(vars))
    scales<-if(isTRUE(pv(s,"standardize",TRUE)))attr(x,"scaled:scale") else rep(1,length(vars))
    original_means<-sweep(sweep(fitted_means,1,scales,"*"),1,centers,"+")
    profiles<-do.call(rbind,lapply(seq_len(selected),function(k)data.frame(class=k,variable=vars,model_mean=original_means[,k],analysis_mean=fitted_means[,k],observed_mean=vapply(d[vars],function(v)if(any(groups==k))mean(v[groups==k]) else NA_real_,numeric(1)),observed_sd=vapply(d[vars],function(v)if(sum(groups==k)>1)sd(v[groups==k]) else NA_real_,numeric(1)))))
    proportions<-data.frame(class=seq_len(selected),model_proportion=fit$parameters$pro)
    grDevices::png(file,width=1300,height=900,res=120)
    tryCatch({par(mar=c(6,4,4,2));matplot(seq_along(vars),fitted_means,type="b",xaxt="n",col=seq_len(selected),pch=seq_len(selected),lty=1,xlab="Indicator",ylab=if(isTRUE(pv(s,"standardize",TRUE)))"Estimated mean (standardized scale)" else "Estimated mean (original scale)",main="LPA profiles");axis(1,at=seq_along(vars),labels=vars,las=2);legend("topright",legend=paste("Class",seq_len(selected)),col=seq_len(selected),pch=seq_len(selected),lty=1,bty="n")},finally=grDevices::dev.off())
  }else{
    profiles<-do.call(rbind,lapply(seq_along(vars),function(j)do.call(rbind,lapply(seq_len(selected),function(k)data.frame(variable=vars[j],class=k,category=category_labels[[j]],probability=as.numeric(fit$probs[[j]][k,]))))))
    proportions<-data.frame(class=seq_len(selected),model_proportion=fit$P)
    heat<-do.call(rbind,lapply(fit$probs,t));labs<-unlist(lapply(seq_along(vars),function(j)paste(vars[j],category_labels[[j]],sep=": ")))
    grDevices::png(file,width=1400,height=max(900,min(3600,24*nrow(heat)+400)),res=120)
    tryCatch({par(mar=c(4,10,4,2));image(seq_len(selected),seq_len(nrow(heat)),t(heat),zlim=c(0,1),col=colorRampPalette(c("#ffffff","#146c5d"))(100),axes=FALSE,xlab="Class",ylab="",main="LCA conditional probabilities (0 white / 1 green)");axis(1,at=seq_len(selected),labels=seq_len(selected));axis(2,at=seq_len(nrow(heat)),labels=labs,las=2,cex.axis=max(.3,min(.85,35/nrow(heat))))},finally=grDevices::dev.off())
  }
  list(summary=list(method=toupper(method),n=z$n,complete_case_excluded=z$removed,criteria=tab,selected=selected,bic_candidate=best,sizes=as.data.frame(table(groups)),proportions=proportions,profiles=profiles,entropy=entropy,failures=failures,limitations=c("BIC 仅辅助选择，还需理论、类别大小、局部独立和样本外验证。",if(method=="lpa")"默认标准化及等方差对角协方差 EEI；同时报告模型原量尺均值、分析量尺均值及最大后验分组的描述均值/SD。" else "观测类别重编码为 1…K，以标称类别拟合；条件概率按原始类别标签报告，依赖局部独立。")),fit=fit,image=file,members=data.frame(source_record=z$records,class=groups,max_probability=apply(posterior,1,max)))
}
