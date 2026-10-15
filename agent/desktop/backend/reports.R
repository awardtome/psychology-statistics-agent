q_tables <- function(x,prefix="结果"){
  out<-list();walk<-function(z,p){
    if(is.matrix(z)){
      cells<-expand.grid(row=if(is.null(rownames(z)))seq_len(nrow(z)) else rownames(z),column=if(is.null(colnames(z)))seq_len(ncol(z)) else colnames(z),stringsAsFactors=FALSE)
      cells$value<-as.vector(z);out[[p]]<<-cells;return()
    }
    if(is.data.frame(z)){out[[p]]<<-z;return()}
    if(is.list(z)&&length(z)&&all(vapply(z,function(a)is.list(a)&&!is.null(names(a))&&all(vapply(a,function(v)!is.list(v)||length(v)==0,logical(1))),logical(1)))){
      keys<-unique(vec(lapply(z,names)));out[[p]]<<-do.call(rbind,lapply(z,function(a){v<-lapply(keys,function(k)paste(vec(a[[k]]),collapse=", "));names(v)<-keys;as.data.frame(v,stringsAsFactors=FALSE)}));return()
    }
    if(is.list(z)){
      scalar<-which(vapply(z,function(v)is.atomic(v)&&length(v)==1,logical(1)))
      if(length(scalar))out[[paste(p,"摘要")]]<<-data.frame(item=if(is.null(names(z)))scalar else names(z)[scalar],value=vapply(z[scalar],as.character,character(1)))
      for(i in setdiff(seq_along(z),scalar))walk(z[[i]],paste(p,if(is.null(names(z)))as.character(i) else names(z)[i],sep=" / "));return()
    }
    if(length(z))out[[p]]<<-data.frame(item=if(is.null(names(z)))seq_along(z) else names(z),value=as.character(z))
  };walk(x,prefix);out
}
q_validate_xlsx <- function(file){
  temp<-tempfile("xlsx-verify-");dir.create(temp);utils::unzip(file,exdir=temp)
  ns<-c(r="http://schemas.openxmlformats.org/package/2006/relationships",s="http://schemas.openxmlformats.org/spreadsheetml/2006/main")
  for(rel in list.files(temp,pattern="\\.rels$",recursive=TRUE,full.names=TRUE,all.files=TRUE)){
    doc<-xml2::read_xml(rel);owner<-dirname(dirname(rel))
    for(node in xml2::xml_find_all(doc,"//r:Relationship",ns)){
      target<-xml2::xml_attr(node,"Target");external<-identical(xml2::xml_attr(node,"TargetMode"),"External")
      location<-if(startsWith(target,"/"))file.path(temp,substring(target,2)) else file.path(owner,target)
      if(!external&&!file.exists(location))xml2::xml_remove(node)
    }
    xml2::write_xml(doc,rel)
  }
  for(sheet in list.files(file.path(temp,"xl/worksheets"),pattern="^sheet[0-9]+\\.xml$",full.names=TRUE)){
    doc<-xml2::read_xml(sheet);refs<-xml2::xml_attr(xml2::xml_find_all(doc,"//s:c",ns),"r")
    if(length(refs)){rows<-as.integer(gsub("[A-Z]","",refs));cols<-openxlsx::col2int(gsub("[0-9]","",refs));dim<-xml2::xml_find_first(doc,"//s:dimension",ns);if(!inherits(dim,"xml_missing"))xml2::xml_attr(dim,"ref")<-paste0("A1:",openxlsx::int2col(max(cols)),max(rows))}
    xml2::write_xml(doc,sheet)
  }
  packaged<-tempfile(fileext=".xlsx");zip::zipr(packaged,files=list.files(temp,recursive=TRUE,all.files=TRUE),root=temp,mode="mirror")
  need(file.copy(packaged,file,overwrite=TRUE),"无法保存验证后的 Excel。")
}
q_word_table <- function(doc,tab){
  if(!is.data.frame(tab)||!ncol(tab))return(doc)
  ids<-which(names(tab)%in%c("lhs","op","rhs","node","target","manipulated_node","moderator","condition","from","to","feature","scale","source_record"))
  # Long explanations get a separate table instead of an unreadably narrow column.
  notes<-which(names(tab)%in%c("note","limitations","reason")&vapply(tab,function(v)any(nchar(as.character(v))>45,na.rm=TRUE),logical(1)))
  remaining<-setdiff(seq_len(ncol(tab)),c(ids,notes))
  if(length(ids)>5)ids<-ids[seq_len(5)]
  remaining<-setdiff(seq_len(ncol(tab)),c(ids,notes))
  batches<-lapply(split(remaining,ceiling(seq_along(remaining)/max(1,6-length(ids)))),function(cols)c(ids,cols))
  if(!length(batches)&&length(ids))batches<-list(ids)
  if(length(notes))batches<-c(batches,lapply(notes,function(col)c(if(length(ids))ids[1] else integer(),col)))
  for(b in seq_along(batches)){
    # Word merges adjacent tables; separate column batches so each keeps its own header.
    if(b>1)doc<-officer::body_add_par(doc,"")
    cols<-batches[[b]]
    formatted<-tab[,cols,drop=FALSE]
    formatted[]<-lapply(formatted,function(v)if(is.numeric(v))format(v,digits=6,trim=TRUE)else as.character(v))
    doc<-officer::body_add_table(doc,formatted,style="Table Grid")
  }
  doc
}
q_write_reports <- function(q,directory,module=NULL,research=NULL,ruleAttachments=NULL){
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  wb<-openxlsx::createWorkbook();sheet_index<-0L
  add_sheet<-function(label,tab){if(!is.data.frame(tab)||!ncol(tab))return();sheet_index<<-sheet_index+1L;safe<-label;for(ch in c("\\","/",":","*","?","[","]"))safe<-gsub(ch,"_",safe,fixed=TRUE);name<-paste0(sprintf("%02d",sheet_index),"_",substr(safe,1,24));openxlsx::addWorksheet(wb,name);openxlsx::writeData(wb,name,tab,headerStyle=openxlsx::createStyle(fontColour="#FFFFFF",fgFill="#146C5D",textDecoration="bold"));openxlsx::freezePane(wb,name,firstRow=TRUE);openxlsx::setColWidths(wb,name,cols=seq_len(ncol(tab)),widths=pmin(38,pmax(12,nchar(names(tab))+3)))}
  add_sheet(pv(q,"reportInputLabel","原始数据"),q$raw);add_sheet("当前分析数据",cbind(source_record=q$source,q$current));add_sheet("排除记录",q$audit$excluded_records)
  audit<-q_tables(q$audit,"数据审计");for(n in names(audit))add_sheet(n,audit[[n]])
  template<-file.path(worker_dir,"report-template.docx")
  doc<-officer::read_docx(template);doc<-officer::body_add_par(doc,pv(q,"reportTitle","心理学统计分析报告"),style="Title")
  doc<-officer::body_add_par(doc,pv(q,"reportDescription",sprintf("本报告保存实际计算、数据处理与方法限制。当前数据为 %s，版本 %d，当前记录 %d。原始数据、完整结果与排除审计另存于同目录 Excel 文件。",q$name,q$revision,nrow(q$current))))
  records<-if(is.null(module))q$archive else Filter(function(r)identical(r$module,module),q$archive)
  exported_versions<-character()
  if(!length(records))doc<-officer::body_add_par(doc,"尚未完成分析。本报告仅记录筛选与计分配置。")
  for(i in seq_along(records)){
    r<-records[[i]];s<-r$summary;doc<-officer::body_add_par(doc,paste(s$method,"分析",i),style="heading 1")
    doc<-officer::body_add_par(doc,sprintf("数据版本 %s，实际有效样本 %s，分析完整案例排除 %s。",pv(s,"revision","未核实"),pv(s,"n","未报告"),pv(s,"complete_case_excluded","未报告")))
    version<-as.character(s$revision)
    if(length(version)&&!version%in%exported_versions&&!is.null(r$data)){
      add_sheet(paste0("v",version,"_分析输入"),cbind(source_record=r$source,r$data))
      version_audit<-q_tables(r$audit,paste0("v",version,"_处理审计"))
      for(n in names(version_audit))add_sheet(n,version_audit[[n]])
      exported_versions<-c(exported_versions,version)
    }
    if(!is.null(s$cs_origin))doc<-officer::body_add_par(doc,paste("CS 来源：",s$cs_origin))
    tables<-q_tables(s[setdiff(names(s),c("image","limitations","warnings","specification"))],paste0(i,"_",r$module))
    for(n in names(tables)){
      t<-tables[[n]];add_sheet(n,t);if(!nrow(t))next
      doc<-officer::body_add_par(doc,n,style="heading 2")
      doc<-q_word_table(doc,t)
    }
    if(!is.null(r$members))add_sheet(paste0(i,"_分组编号"),r$members)
    if(!is.null(r$predictions))add_sheet(paste0(i,"_测试集预测"),r$predictions)
    if(!is.null(r$image)&&file.exists(r$image)){
      target<-file.path(directory,paste0(sprintf("%02d",i),"_",r$module,".png"));file.copy(r$image,target,overwrite=TRUE);doc<-officer::body_add_par(doc,"分析图",style="heading 2");doc<-officer::body_add_img(doc,target,width=6,height=4.5)
    }
    doc<-officer::body_add_par(doc,"方法限制和警告",style="heading 2");for(t in c(vec(s$limitations),vec(s$warnings)))doc<-officer::body_add_par(doc,as.character(t))
    doc<-officer::body_add_par(doc,"实际分析配置",style="heading 2");for(n in names(q_tables(s$specification,"方案"))){tab<-q_tables(s$specification,"方案")[[n]];add_sheet(paste0(i,"_",n),tab);doc<-officer::body_add_par(doc,n);doc<-q_word_table(doc,tab)}
  }
  for(f in q$figures){
    doc<-officer::body_add_break(doc)
    doc<-officer::body_add_par(doc,paste("科研绘图",f$id,f$title),style="heading 1")
    doc<-officer::body_add_par(doc,f$caption)
    for(format in names(f$files))need(file.copy(f$files[[format]],file.path(directory,paste0("科研图_",f$id,".",format)),overwrite=TRUE),"科研图片复制失败。")
    ratio<-min(6/f$options$width,7/f$options$height)
    doc<-officer::body_add_img(doc,f$files[["png"]],width=f$options$width*ratio,height=f$options$height*ratio)
    doc<-officer::body_add_par(doc,f$note)
    doc<-officer::body_add_par(doc,sprintf("绘图设置：%s × %s 英寸，%s DPI，字体 %s pt，配色 %s。完整绘图数值、变量标签及配置见 Excel。",f$options$width,f$options$height,f$options$dpi,f$options$fontSize,f$options$palette))
    for(w in f$warnings)doc<-officer::body_add_par(doc,w)
    add_sheet(paste0("科研图",f$id,"_实际绘图数据"),f$table)
    settings<-q_tables(f[c("module","type","revision","n","options","specification")],paste0("科研图",f$id,"_设置"))
    for(n in names(settings))add_sheet(n,settings[[n]])
  }
  doc<-officer::body_add_par(doc,"筛选及计分审计",style="heading 1")
  for(n in names(audit)){doc<-officer::body_add_par(doc,n,style="heading 2");doc<-q_word_table(doc,audit[[n]])}
  if(length(ruleAttachments)){
    doc<-officer::body_add_par(doc,"使用者添加的筛选规则资料",style="heading 1")
    doc<-officer::body_add_par(doc,"这些附件是使用者提供的参考资料，不代表所有规则已经执行。实际筛选和计分以本报告的数据审计、配置与排除记录为准。")
    for(i in seq_along(ruleAttachments)){
      rule<-ruleAttachments[[i]]
      add_sheet(paste0("规则附件",i),data.frame(file=rule$name,id=rule$id,status="参考资料，执行情况以审计为准",text=rule$text,stringsAsFactors=FALSE))
      doc<-officer::body_add_par(doc,as.character(rule$name),style="heading 2")
      for(line in strsplit(rule$text,"\n",fixed=TRUE)[[1]])doc<-officer::body_add_par(doc,line)
    }
  }
  if(!is.null(research)){for(tab in q_tables(research$works,"文献检索"))add_sheet("文献检索",tab);doc<-officer::body_add_par(doc,"理论与文献检索",style="heading 1");doc<-officer::body_add_par(doc,paste("检索词",research$query));for(w in research$works)doc<-officer::body_add_par(doc,paste(w$title,w$year,w$url));if(nzchar(pv(research,"advice","")))doc<-officer::body_add_par(doc,research$advice)}
  excel<-file.path(directory,"完整分析结果.xlsx");openxlsx::saveWorkbook(wb,excel,overwrite=TRUE);q_validate_xlsx(excel);print(doc,target=file.path(directory,"完整分析报告.docx"))
  jsonlite::write_json(list(name=q$name,revision=q$revision,screening=q$screening,audit=q$audit,results=lapply(records,function(r)r$summary),figures=q$figures,research=research,ruleAttachments=ruleAttachments),file.path(directory,"analysis-audit.json"),pretty=TRUE,auto_unbox=TRUE,na="null",digits=NA)
  utils::write.csv(cbind(source_record=q$source,q$current),file.path(directory,"当前分析数据.csv"),row.names=FALSE,fileEncoding="UTF-8")
  list(directory=directory,files=list.files(directory))
}
q_branch <- function(q,mode="all",class=NULL){
  need(mode%in%c("all","class"),"未知数据分支。")
  if(mode=="all"){need(!is.null(q$screened),"没有筛选后的数据。");q$current<-q$screened;q$source<-q$screened_source;q$branch<-NULL}
  else{
    pool<-q$branch_pool;need(!is.null(pool),"请先完成异质性分析。");need(!is.null(class),"请选择实际分组。")
    keep<-as.character(pool$members$class)==as.character(class);need(any(keep),"该分组不存在。")
    idx<-match(pool$members$source_record[keep],pool$source);need(!anyNA(idx),"分组来源记录不完整。")
    q$current<-pool$data[idx,,drop=FALSE];q$source<-pool$source[idx];q$branch<-list(class=class,n=length(idx),parent_revision=pool$revision,method=pool$summary$method)
  }
  q$audit$branch<-q$branch;q$revision<-q$revision+1;q$results<-q$fits<-q$specs<-list();q$members<-q$predictions<-NULL;q_snapshot(q)
}
q_intervention_status <- function(q){
  f<-q$fits$network;valid<-!is.null(f)&&identical(f$revision,q$revision);metric<-if(valid)f$cs$expectedInfluence else NULL
  cs<-if(is.null(metric))NULL else as.numeric(metric);gate<-!is.null(cs)&&is.finite(cs)&&cs>.25
  list(ida=!is.null(q$fits$dag)&&isTRUE(q$fits$dag$ida_candidate)&&q$fits$dag$revision==q$revision,
    nira=valid&&f$method=="ising"&&gate,ggmnira=valid&&f$method=="ggm"&&gate,network=if(valid)f$method else NULL,cs=cs,metric="expectedInfluence",
    next_step=if(!valid)"先进入网络分析，选择明确节点和数据层次。" else if(is.null(cs))"当前网络未计算 EI 的 CS，请回网络分析开启 case-dropping，或返回同一节点/数据的外部 CS 结果。" else if(!gate)"当前 EI CS 未超过 0.25，不能按该门槛推荐模拟；检查测量和稳定性。" else "满足使用者指定的最低稳定性门槛，还需核对方法假设。",
    warning="GGMNIRA 论文尚未正式发表，已有预印本；使用须披露方法状态。CS 门槛不证明因果或真实干预有效。")
}
