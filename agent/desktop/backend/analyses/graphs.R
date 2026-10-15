q_graph <- function(a, labels, file, title, directed=FALSE,pag=FALSE) {
  grDevices::png(file,width=1400,height=1100,res=120);on.exit(grDevices::dev.off())
  par(mar=c(2,2,4,2));p<-length(labels);t<-seq(0,2*pi,length.out=p+1)[-1];xy<-cbind(cos(t),sin(t));plot(xy,type="n",xlim=c(-1.4,1.4),ylim=c(-1.3,1.3),axes=FALSE,xlab="",ylab="",main=title)
  for(i in seq_len(p))for(j in seq_len(p))if(i!=j&&a[i,j]!=0&&(directed||i<j)){
    v<-(xy[j,]-xy[i,])/sqrt(sum((xy[j,]-xy[i,])^2));col<-if(a[i,j]>0)"#24796c" else "#b45560"
    if(pag&&i<j){
      u<-xy[i,]+.15*v;w<-xy[j,]-.15*v;segments(u[1],u[2],w[1],w[2],col=col,lwd=2)
      if(a[i,j]==2)arrows(u[1],u[2],w[1],w[2],length=.08,col=col)
      if(a[j,i]==2)arrows(w[1],w[2],u[1],u[2],length=.08,col=col)
      if(a[i,j]==1)points(w[1],w[2],pch=21,bg="white",col=col,cex=1)
      if(a[j,i]==1)points(u[1],u[2],pch=21,bg="white",col=col,cex=1)
    }else if(pag)next
    else if(directed&&a[j,i]==0)arrows(xy[i,1]+.12*v[1],xy[i,2]+.12*v[2],xy[j,1]-.13*v[1],xy[j,2]-.13*v[2],length=.09,col=col,lwd=2)
    else if(!directed||i<j)segments(xy[i,1],xy[i,2],xy[j,1],xy[j,2],col=col,lwd=2)
  }
  points(xy,pch=21,bg="#eef6f2",col="#24796c",cex=6);text(xy,labels=labels,cex=max(.45,min(.85,7/p)))
}
q_edges <- function(a,labels,pag=FALSE){
  marks<-c("circle","arrowhead","tail");out<-list();for(i in seq_len(nrow(a)))for(j in seq_len(ncol(a)))if(i<j&&(a[i,j]!=0||a[j,i]!=0))out[[length(out)+1]]<-list(from=labels[i],to=labels[j],forward=a[i,j],backward=a[j,i],status=if(pag)paste(marks[a[j,i]],marks[a[i,j]],sep=" / ") else if(a[i,j]!=0&&a[j,i]!=0)"undirected" else if(a[i,j]!=0)"forward" else "reverse");out
}
