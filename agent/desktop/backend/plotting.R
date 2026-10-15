# Fixed publication templates use computed results only. No model-supplied code/data.
q_figure_options <- function(s) {
  number<-function(key,default,lo,hi){v<-pv(s,key,default);need(is.numeric(v)&&length(v)==1&&is.finite(v)&&v>=lo&&v<=hi,paste0(key," 超出允许范围 ",lo,"–",hi));v}
  text<-function(key,default=""){v<-pv(s,key,default);need(is.character(v)&&length(v)==1&&nchar(v)<=180,paste0(key," 必须为不超过 180 字的文本。"));v}
  palette<-text("palette","accessible");need(palette%in%c("accessible","mono","journal"),"请选择 accessible、mono 或 journal 配色。")
  list(width=number("width",7,4,12),height=number("height",5,3,12),dpi=number("dpi",300,300,600),fontSize=number("fontSize",11,8,18),
    title=text("title"),xLabel=text("xLabel"),yLabel=text("yLabel"),palette=palette,
    colors=switch(palette,accessible=c("#0072B2","#D55E00","#009E73","#CC79A7","#E69F00","#56B4E9","#332288","#666666"),mono=gray.colors(8,start=.15,end=.72),journal=c("#245A81","#B44C43","#367C67","#80649C","#AA7733","#4B8A99","#74534A","#696969")))
}
q_figure_label <- function(xs,labels){
  vapply(as.character(xs),function(x){value<-labels[[x]];if(is.null(value))x else{need(is.character(value)&&length(value)==1&&nchar(value)<=60,"变量标签最多 60 字。");value}},character(1),USE.NAMES=FALSE)
}
q_figure_theme <- function(o) {
  ggplot2::theme_classic(base_size=o$fontSize,base_family="Microsoft YaHei")+
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold",margin=ggplot2::margin(b=12)),
      axis.title=ggplot2::element_text(face="plain"),axis.text=ggplot2::element_text(color="#333333"),
      axis.line=ggplot2::element_line(linewidth=.35),axis.ticks=ggplot2::element_line(linewidth=.3),
      legend.position="bottom",legend.title=ggplot2::element_blank(),
      plot.caption=ggplot2::element_text(hjust=0,color="#555555",size=8),
      plot.margin=ggplot2::margin(16,20,12,16),panel.grid.major.y=ggplot2::element_line(color="#ECECEC",linewidth=.25))
}
q_research_figure <- function(q,s,output) {
  module<-pv(s,"module","");need(module%in%c("mediation","heterogeneity","network","dag","ida","ml","meta"),"请选择已完成的分析入口。")
  result<-q$results[[module]];need(!is.null(result),"请先完成当前数据的对应分析，再调用科研绘图。")
  kind<-pv(s,"type","auto")
  if(kind=="auto")kind<-switch(module,mediation="coefficients",heterogeneity=if(result$method=="网络树")"tree" else "profile",network="network",dag="dag",ida="ida",ml="importance",meta="forest")
  allowed<-switch(module,mediation="coefficients",heterogeneity=c("profile","tree"),network=c("network","centrality"),dag="dag",ida="ida",ml="importance",meta="forest")
  need(kind%in%allowed,"该绘图模板与当前结果不匹配。")
  o<-q_figure_options(s);labels<-pv(s,"labels",list());need(is.list(labels),"labels 必须为实际变量名到显示标签的映射。")
  caption<-sprintf("N = %s  |  Data version %s  |  %s",pv(result,"n","NA"),pv(result,"revision","NA"),result$method)
  subtitle<-NULL;p<-NULL;tab<-NULL;draw<-NULL
  if(kind=="coefficients") {
    tab<-result$table;need(is.data.frame(tab)&&nrow(tab)>0&&nrow(tab)<=60,"路径参数图支持 1–60 行实际参数。")
    tab$term<-ifelse(tab$op==":=",tab$lhs,paste(q_figure_label(tab$rhs,labels),"→",q_figure_label(tab$lhs,labels)));tab$term<-q_figure_label(tab$term,labels);tab$position<-rev(seq_len(nrow(tab)))
    p<-ggplot2::ggplot(tab,ggplot2::aes(x=est,y=position))+ggplot2::geom_vline(xintercept=0,linetype=2,color="#999999",linewidth=.35)+
      ggplot2::geom_segment(ggplot2::aes(x=ci.lower,xend=ci.upper,yend=position),color=o$colors[1],linewidth=.7)+
      ggplot2::geom_point(color=o$colors[1],size=2.5)+ggplot2::scale_y_continuous(breaks=tab$position,labels=tab$term)+ggplot2::labs(x="Unstandardized estimate (95% CI)",y=NULL)
    subtitle<-"Path estimates and indirect effects"
  } else if(kind=="profile") {
    tab<-result$profiles;need(is.data.frame(tab),"当前异质性结果没有剖面/类别概率，请选择网络树模板。")
    tab$display<-q_figure_label(tab$variable,labels);tab$class<-factor(tab$class)
    if(result$method=="LPA") {
      scale<-pv(s,"scale","analysis");need(scale%in%c("analysis","original"),"scale 必须为 analysis 或 original。");tab$value<-if(scale=="original")tab$model_mean else tab$analysis_mean
      tab$display<-factor(tab$display,levels=unique(tab$display))
      p<-ggplot2::ggplot(tab,ggplot2::aes(x=display,y=value,group=class,color=class,shape=class))+ggplot2::geom_line(linewidth=.7)+ggplot2::geom_point(size=2.5)+ggplot2::scale_shape_manual(values=c(16,17,15,18,0,1,2,5))+ggplot2::labs(x=NULL,y=if(scale=="original")"Estimated mean (original scale)" else "Estimated mean (analysis scale)")
      subtitle<-"Latent profile estimates"
    } else {
      p<-ggplot2::ggplot(tab,ggplot2::aes(x=category,y=probability,group=class,color=class,shape=class))+ggplot2::geom_line(linewidth=.7)+ggplot2::geom_point(size=2.4)+ggplot2::facet_wrap(~display,scales="free_x")+ggplot2::scale_y_continuous(limits=c(0,1))+ggplot2::scale_shape_manual(values=c(16,17,15,18,0,1,2,5))+ggplot2::labs(x="Response category",y="Conditional probability")
      subtitle<-"Latent class response probabilities"
    }
    p<-p+ggplot2::scale_color_manual(values=o$colors)
  } else if(kind%in%c("network","dag")) {
    f<-q$fits[[module]];need(!is.null(f),"当前图的拟合对象不可用，请重新分析。")
    a<-if(kind=="network")f$network$graph else f$matrix;nodes<-f$columns;n<-length(nodes)
    adjacency<-(abs(a)+abs(t(a)))>0;diag(adjacency)<-FALSE
    g<-igraph::graph_from_adjacency_matrix(adjacency,mode="undirected",diag=FALSE);set.seed(20261003)
    xy<-if(igraph::ecount(g)>0)igraph::layout_with_fr(g) else igraph::layout_in_circle(g);xy<-igraph::norm_coords(xy,xmin=-1,xmax=1,ymin=-1,ymax=1)
    vertices<-data.frame(node=nodes,label=q_figure_label(nodes,labels),x=xy[,1],y=xy[,2]);edge<-list();pag<-kind=="dag"&&f$graph_type=="PAG"
    # Reserve space for the full label box before drawing arrowheads/circle marks.
    units_per_point<-2.86/(72*max(1,min(o$width-1.2,o$height-1.8)))
    half_width<-(nchar(vertices$label,type="width")*o$fontSize*.25+o$fontSize*.35)*units_per_point
    half_height<-o$fontSize*.95*units_per_point
    need(max(half_width)<=.40,"节点标签过长或图幅过小，可能超出图片边界；请缩短显示标签、减小字体或增大图片尺寸。")
    for(i in seq_len(n-1))for(j in (i+1):n)if(a[i,j]!=0||a[j,i]!=0){
      v<-xy[j,]-xy[i,];distance<-max(sqrt(sum(v^2)),.001);v<-v/distance
      boundary<-function(k)min(half_width[k]/max(abs(v[1]),1e-6),half_height/max(abs(v[2]),1e-6))+.035
      from_gap<-boundary(i);to_gap<-boundary(j)
      need(from_gap+to_gap<distance,"节点标签过长或图幅过小，可能遮住边；请缩短显示标签、减小字体或增大图片尺寸。")
      u<-xy[i,]+from_gap*v;w<-xy[j,]-to_gap*v
      ends<-if(kind=="network")"none" else if(pag){if(a[j,i]==2&&a[i,j]==2)"both" else if(a[i,j]==2)"last" else if(a[j,i]==2)"first" else "none"}else if(a[i,j]!=0&&a[j,i]==0)"last" else if(a[j,i]!=0&&a[i,j]==0)"first" else "none"
      edge[[length(edge)+1]]<-data.frame(from=nodes[i],to=nodes[j],x=u[1],y=u[2],xend=w[1],yend=w[2],ends=ends,weight=if(kind=="network")a[i,j] else 1,mark_from=if(pag)a[j,i] else 0,mark_to=if(pag)a[i,j] else 0)
    }
    tab<-if(length(edge))do.call(rbind,edge) else data.frame(from=character(),to=character(),weight=numeric())
    p<-ggplot2::ggplot()
    if(nrow(tab)) {
      tab$sign<-ifelse(tab$weight<0,"Negative","Positive")
      for(end in unique(tab$ends))p<-p+ggplot2::geom_segment(data=tab[tab$ends==end,,drop=FALSE],ggplot2::aes(x=x,y=y,xend=xend,yend=yend,color=sign,linewidth=abs(weight),linetype=sign),arrow=if(end=="none")NULL else grid::arrow(length=grid::unit(2,"mm"),ends=end,type="closed"),show.legend=kind=="network")
      if(pag){circles<-rbind(data.frame(x=tab$x[tab$mark_from==1],y=tab$y[tab$mark_from==1]),data.frame(x=tab$xend[tab$mark_to==1],y=tab$yend[tab$mark_to==1]));if(nrow(circles))p<-p+ggplot2::geom_point(data=circles,ggplot2::aes(x=x,y=y),shape=21,fill="white",size=2,stroke=.5)}
      p<-p+ggplot2::scale_color_manual(values=c(Positive=o$colors[1],Negative=o$colors[2]))+ggplot2::scale_linetype_manual(values=c(Positive=1,Negative=2))+ggplot2::scale_linewidth_continuous(range=c(.4,1.5),guide="none")
    }
    p<-p+ggplot2::geom_label(data=vertices,ggplot2::aes(x=x,y=y,label=label),family="Microsoft YaHei",size=o$fontSize/3.2,fill="white",color=o$colors[1],linewidth=.35,label.padding=grid::unit(.2,"lines"))+ggplot2::coord_equal(xlim=c(-1.3,1.3),ylim=c(-1.3,1.3),clip="off")
    subtitle<-if(kind=="network")paste(result$method,"conditional association network") else paste(result$method,result$graph_type)
    caption<-paste(caption,if(kind=="network")"Edges are associations, not causal effects." else if(pag)"Circles indicate unresolved endpoints; PAG is not an IDA input." else "Undirected edges indicate orientation uncertainty.",sep="\n")
  } else if(kind%in%c("centrality","importance","ida")) {
    if(kind=="centrality"){tab<-result$centrality;tab$label<-q_figure_label(tab$node,labels);tab$value<-tab$expectedInfluence;subtitle<-"Expected influence"}
    if(kind=="importance"){tab<-result$importance;tab$label<-q_figure_label(tab$feature,labels);tab$value<-tab$importance;subtitle<-"Training importance / coefficients"}
    if(kind=="ida"){tab<-data.frame(label=paste("Set",seq_along(result$effects)),value=as.numeric(result$effects));subtitle<-"IDA possible total effects";caption<-paste(caption,"The range is not a confidence interval.",sep="\n")}
    need(nrow(tab)>0&&nrow(tab)<=100,"此图支持 1–100 个实际结果。")
    tab$position<-rev(seq_len(nrow(tab)));p<-ggplot2::ggplot(tab,ggplot2::aes(x=value,y=position))+ggplot2::geom_vline(xintercept=0,color="#999999",linetype=2,linewidth=.3)+ggplot2::geom_segment(ggplot2::aes(x=0,xend=value,yend=position),color=o$colors[1],linewidth=.6)+ggplot2::geom_point(color=o$colors[1],size=2.5)+ggplot2::scale_y_continuous(breaks=tab$position,labels=tab$label)+ggplot2::labs(x=subtitle,y=NULL)
  } else if(kind=="tree") {
    need(identical(result$method,"网络树"),"网络树模板仅用于实际网络树结果。")
    fit<-q$fits$heterogeneity;tab<-result$groups;draw<-function(){par(family="Microsoft YaHei",cex=o$fontSize/12);plot(fit)};subtitle<-"Network tree"
  } else if(kind=="forest") {
    tab<-result$overall;need(is.data.frame(tab),"当前元分析摘要不可用。");fit<-q$fits$meta
    draw<-function(){par(family="Microsoft YaHei",cex=o$fontSize/12);args<-list(x=fit,what="forest",main=if(nzchar(o$title))o$title else "Meta-analysis forest plot",col=o$colors[1]);if(nzchar(o$xLabel))args$xlab<-o$xLabel;do.call(meta3level::m3plot,args)};subtitle<-"Meta-analysis forest plot"
  }
  title<-if(nzchar(o$title))o$title else subtitle
  if(!is.null(p)) {
    p<-p+q_figure_theme(o)+ggplot2::labs(title=title,caption=caption)
    if(nzchar(o$xLabel))p<-p+ggplot2::labs(x=o$xLabel)
    if(nzchar(o$yLabel))p<-p+ggplot2::labs(y=o$yLabel)
    if(kind%in%c("network","dag"))p<-p+ggplot2::theme(axis.title=ggplot2::element_blank(),axis.text=ggplot2::element_blank(),axis.ticks=ggplot2::element_blank(),axis.line=ggplot2::element_blank(),panel.grid=ggplot2::element_blank(),panel.grid.major.y=ggplot2::element_blank())
    draw<-function()print(p)
  }
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  files<-setNames(file.path(output,paste0("research-figure.",c("png","svg","pdf"))),c("png","svg","pdf"))
  warnings<-character()
  for(format in names(files)){
    if(format=="png")grDevices::png(files[[format]],width=o$width,height=o$height,units="in",res=o$dpi,type="cairo",bg="white")
    if(format=="svg")grDevices::svg(files[[format]],width=o$width,height=o$height,family="Microsoft YaHei",bg="white")
    if(format=="pdf")grDevices::cairo_pdf(files[[format]],width=o$width,height=o$height,family="Microsoft YaHei",bg="white")
    tryCatch(withCallingHandlers(draw(),warning=function(w){warnings<<-unique(c(warnings,conditionMessage(w)));invokeRestart("muffleWarning")}),finally=grDevices::dev.off())
  }
  figure<-list(id=length(q$figures)+1L,module=module,type=kind,title=title,revision=result$revision,n=result$n,options=o[setdiff(names(o),"colors")],specification=s,table=tab,caption=caption,files=files,warnings=warnings,
    note="固定模板绘制实际结果；PNG 为指定印刷分辨率，SVG/PDF 为矢量。投稿前需按目标期刊规范核对尺寸和字体。")
  if(kind=="tree")figure$note<-paste(figure$note,"网络树保留方法包原生树结构与节点图；变量标签和配色沿用拟合对象，标题保存于报告。")
  q$figures[[length(q$figures)+1L]]<-figure
  list(figure=figure,image=files[["png"]],state=q_snapshot(q))
}
