q_mediation_check <- function(q,s){
  paths<-pv(s,"paths",list());interactions<-pv(s,"interactions",list())
  scalar<-function(x)is.character(x)&&length(x)==1L&&!is.na(x)&&nzchar(x)
  need(length(paths)>0,"请在路径画布中添加路径，例如 X→M、M→Y。")
  need(all(vapply(paths,function(p)scalar(p$from)&&scalar(p$to)&&p$from!=p$to,logical(1))),"路径需要两个不同的实际变量。")
  vars<-unique(as.character(vec(lapply(paths,function(p)c(p$from,p$to)))))
  key<-function(from,to)paste(match(from,vars),match(to,vars),sep=":")
  keys<-vapply(paths,function(p)key(p$from,p$to),character(1));need(!anyDuplicated(keys),"同一条回归路径不能重复。")
  for(i in interactions){need(scalar(i$x)&&scalar(i$w)&&scalar(i$to)&&length(unique(c(i$x,i$w,i$to)))==3L,"调节关系需要三个不同的实际变量。");need(any(vapply(paths,function(p)p$from==i$x&&p$to==i$to,logical(1))),"调节关系必须绑定已声明的回归路径。");vars<-unique(c(vars,i$w))}
  need(!anyDuplicated(vapply(interactions,function(i)paste(match(c(i$x,i$w,i$to),vars),collapse=":"),character(1))),"同一条调节关系不能重复。")
  need(!is.null(q$current)&&all(vars%in%names(q$current)),"请使用当前数据中实际存在的分析变量。")
  if(!is.null(s$variables)){selected<-as.character(vec(s$variables));need(!anyDuplicated(selected)&&all(selected%in%names(q$current))&&all(vars%in%selected),"路径包含未选择的变量，请重新确认分析变量。")}
  a<-matrix(0,length(vars),length(vars),dimnames=list(vars,vars));for(p in paths)a[p$from,p$to]<-1;for(i in interactions)a[i$w,i$to]<-1
  need(igraph::is_dag(igraph::graph_from_adjacency_matrix(a,mode="directed")),"路径有环（包含调节变量的主效应）；本入口支持无环递归模型。")
  chain_ok<-function(chain){cs<-as.character(vec(chain));length(cs)>=3L&&!anyDuplicated(cs)&&all(cs%in%vars)&&all(vapply(seq_len(length(cs)-1L),function(j)any(vapply(paths,function(p)p$from==cs[j]&&p$to==cs[j+1L],logical(1))),logical(1)))}
  for(chain in pv(s,"indirectPaths",list()))need(chain_ok(chain),"间接效应必须沿已声明的完整路径估计。")
  for(ci in pv(s,"conditionalIndirect",list())){need(chain_ok(ci$path)&&scalar(ci$moderator),"条件间接效应需指定完整路径与调节变量。");cs<-as.character(vec(ci$path));need(any(vapply(interactions,function(i)i$w==ci$moderator&&any(vapply(seq_len(length(cs)-1L),function(j)i$x==cs[j]&&i$to==cs[j+1L],logical(1))),logical(1))),"条件间接效应需要沿该路径已有的调节关系。")}
  invisible(vars)
}
q_mediation <- function(q,s,output){
  q_mediation_check(q,s)
  paths<-s$paths;need(length(paths)>0,"请描述路径，例如 X→M、M→Y、X→Y。")
  vars<-unique(as.character(vec(lapply(paths,function(p)c(p$from,p$to)))));interactions<-pv(s,"interactions",list());vars<-unique(c(vars,as.character(vec(lapply(interactions,function(i)c(i$x,i$w,i$to))))))
  z<-q_data(q,vars);d<-z$data;aliases<-setNames(paste0("v",seq_along(vars)),vars);names(d)<-aliases[names(d)]
  a<-matrix(0,length(vars),length(vars),dimnames=list(vars,vars));labels<-list();terms<-list()
  for(i in seq_along(paths)){p<-paths[[i]];need(p$from!=p$to,"路径不能指向自身。");a[p$from,p$to]<-1;lbl<-paste0("b",i);labels[[paste(aliases[p$from],aliases[p$to],sep="|")]]<-lbl;terms[[p$to]]<-c(terms[[p$to]],paste0(lbl,"*",aliases[p$from]))}
  need(igraph::is_dag(igraph::graph_from_adjacency_matrix(a,mode="directed")),"路径有环；本入口支持无环递归路径模型。")
  interaction_labels<-list()
  for(i in seq_along(interactions)){it<-interactions[[i]];nm<-paste0("int",i);vx<-d[[aliases[[it$x]]]];vw<-d[[aliases[[it$w]]]];d[[nm]]<-(vx-mean(vx))*(vw-mean(vw));a[it$w,it$to]<-1;interaction_labels[[nm]]<-list(predictor=it$x,moderator=it$w,outcome=it$to,label=paste(it$x,"×",it$w,"(centered)"));terms[[it$to]]<-unique(c(terms[[it$to]],aliases[it$x],aliases[it$w],paste0("j",i,"*",nm)))}
  syntax<-vapply(names(terms),function(to)paste0(aliases[to]," ~ ",paste(terms[[to]],collapse=" + ")),character(1));defs<-list()
  for(chain in pv(s,"indirectPaths",list())){cs<-as.character(vec(chain));need(length(cs)>=3&&all(cs%in%vars),"间接路径需要至少三个已声明变量。");keys<-vapply(seq_len(length(cs)-1),function(j)paste(aliases[cs[j]],aliases[cs[j+1]],sep="|"),character(1));need(all(keys%in%names(labels)),"间接路径中的边需要列入路径模型。");nm<-paste0("indirect",length(defs)+1);syntax<-c(syntax,paste0(nm," := ",paste(vec(labels[keys]),collapse=" * ")));defs[[nm]]<-paste(cs,collapse=" → ")}
  conditional<-list()
  for(ci in pv(s,"conditionalIndirect",list())){
    chain<-as.character(vec(ci$path));w<-ci$moderator;need(length(chain)>=3&&all(chain%in%vars)&&w%in%vars,"条件间接效应需要声明路径和调节变量。")
    meanw<-mean(z$data[[w]]);sdw<-sd(z$data[[w]]);values<-as.numeric(vec(pv(ci,"values",c(meanw-sdw,meanw,meanw+sdw))));need(length(values)>0&&length(values)<=10&&all(is.finite(values)),"调节值须为最多十个有限数值。")
    for(value in values){
      factors<-character()
      for(j in seq_len(length(chain)-1)){from<-chain[j];to<-chain[j+1];key<-paste(aliases[from],aliases[to],sep="|");need(key%in%names(labels),"条件间接效应的路径边需要列入模型。");termslope<-labels[[key]]
        for(k in seq_along(interactions)){it<-interactions[[k]];if(it$to==to&&((it$x==from&&it$w==w)||(it$w==from&&it$x==w)))termslope<-paste0("(",termslope," + j",k," * ",format(value-meanw,scientific=FALSE,digits=12,trim=TRUE),")")}
        factors<-c(factors,termslope)
      }
      nm<-paste0("conditional",length(conditional)+1);syntax<-c(syntax,paste0(nm," := ",paste(factors,collapse=" * ")));conditional[[nm]]<-list(path=paste(chain,collapse=" → "),moderator=w,raw_value=value,centered_value=value-meanw,note="交互项中的变量均值中心化；调节值在原始量尺上指定。")
    }
  }
  B<-as.integer(pv(s,"bootstrap",500));need(B>=0&&B<=5000,"Bootstrap 次数应为 0 到 5000。");set.seed(20261003)
  fit<-lavaan::sem(paste(syntax,collapse="\n"),data=d,se=if(B>0)"bootstrap" else "standard",bootstrap=max(B,1),fixed.x=FALSE)
  need(lavaan::lavInspect(fit,"converged"),"路径模型未收敛，请调整模型。")
  tab<-lavaan::parameterEstimates(fit,ci=TRUE,standardized=TRUE);tab<-tab[tab$op%in%c("~",":="),c("lhs","op","rhs","label","est","se","z","pvalue","ci.lower","ci.upper","std.all")]
  tab$pvalue[is.finite(tab$z)]<-2*pnorm(-abs(tab$z[is.finite(tab$z)]))
  decode<-function(xs){rev<-c(setNames(names(aliases),aliases),vapply(interaction_labels,function(i)i$label,character(1)));ifelse(xs%in%names(rev),rev[xs],xs)};tab$lhs<-decode(tab$lhs);tab$rhs<-decode(tab$rhs)
  file<-file.path(output,"paths.png");q_graph(a,vars,file,"Regression paths; interaction terms in tables",TRUE)
  list(summary=list(method="中介调节路径模型",n=z$n,complete_case_excluded=z$removed,table=tab,indirect_labels=defs,conditional_labels=conditional,interaction_labels=interaction_labels,fit=as.list(lavaan::fitMeasures(fit,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))),bootstrap=B,limitations=c("横断面路径及间接效应是关联分解，不能确认因果中介。","条件间接效应需要声明 moderator 与 path；默认取原量尺均值及均值 ±1 SD。","路径图展示回归主效应；交互项与对应调节关系见参数表。","饱和模型的 df=0 与完美拟合指标不能证明模型正确。")),fit=fit,image=file)
}
