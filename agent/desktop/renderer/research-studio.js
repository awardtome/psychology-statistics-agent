window.researchStudio=(()=>{
 'use strict';let state={},selection='',tab='proposal',renderKey='',error='',loading=false,exporting=false,initialized=false;
 const labels={orchestrator:'调度员',gap_finder:'缺口发现',innovator:'跨领域创新',paper_reader:'论文阅读',reviewer:'方案审阅',report_writer:'报告撰写'};
 const fieldLabels={title:'题目',content:'正文',gaps:'具体缺口',mechanism:'来源机制',mapping:'迁移映射',assumptions:'前提条件',verdict:'审阅判断',objections:'反例与改进',method:'研究方法',evaluation:'评估计划',predictions:'可证伪预测',limitations:'局限与遗漏',writing:'写作建议',references:'证据编号',dataBasis:'数据依据',summary:'阅读摘要',answers:'目标回答',goals:'阅读目标',source:'证据来源',level:'证据等级',truncated:'阅读输入截断',retrievalError:'全文获取情况'};
 const node=(tag,text,cls)=>{const n=document.createElement(tag);if(text!=null)n.textContent=text;if(cls)n.className=cls;return n;};
 const button=(text,fn)=>{const n=node('button',text);n.onclick=()=>Promise.resolve(fn()).catch(e=>{error=e.message;render(state)});return n;};
 const summaryValue=v=>typeof v==='string'?v:JSON.stringify(v,null,2);
 function section(data){const host=node('div');for(const [key,value]of Object.entries(data||{})){if(['role','file','version','created'].includes(key))continue;const detail=node('details');detail.open=['content','method','writing'].includes(key);detail.append(node('summary',fieldLabels[key]||key),node('pre',summaryValue(value),'research-text'));host.append(detail);}return host;}
 function selected(){return (state.researchDatasets||[]).find(d=>d.id===selection);}
 function render(next){
  state=next;const r=state.research,active=r?.status==='running';
  if(r&&!initialized){initialized=true;$('research-query').value=r.keywords||'';$('research-question').value=r.question||'';$('research-budget').value=String(r.callLimit||60);}
  const items=state.researchDatasets||[];if(!items.some(d=>d.id===selection))selection=items.at(-1)?.id||'';
  const key=JSON.stringify(items.map(x=>x.id));if(key!==renderKey){renderKey=key;$('research-dataset').replaceChildren();for(const d of [...items].reverse()){const op=node('option',d.name+' · '+d.rows+' 人 · '+new Date(d.created).toLocaleString('zh-CN'));op.value=d.id;$('research-dataset').append(op);}if(!items.length){const op=node('option','暂无筛选后数据，请先应用问卷筛选规则');op.value='';$('research-dataset').append(op);}}
  $('research-dataset').value=selection;const d=selected();$('research-data-origin').textContent=(d?'筛选输出 · 版本 '+d.revision+' · '+d.rows+' 条保留记录 · '+d.columns.length+' 个变量':'仅可选择“应用当前计分和筛选规则”产生的数据。')+(state.psych?.screenedLibraryError?'\n本次筛选数据未保存成功：'+state.psych.screenedLibraryError:'');
  for(const id of ['research-dataset','research-query','research-question','research-budget','research-start','research-data-preview','vault-import'])$(id).disabled=active||state.vault?.importing===true||loading;
  $('research-start').disabled=active||!d||state.vault?.importing===true||loading;
  $('research-stop').hidden=!active;$('research-export').disabled=active||!r||exporting;$('vault-stop').hidden=!state.vault?.importing;
  $('research-status').textContent=error||(exporting?'正在生成完整 Excel 与 Word 报告…':r?(r.status==='complete'?'已完成':r.status==='running'?'正在协作':r.status==='cancelled'?'已停止':r.status==='failed'?'未完成':'上次任务已中断')+' · API '+r.calls+'/'+r.callLimit+' 次 · '+r.datasetName+' / '+r.rows+' 人'+(r.error?'\n'+r.error:''):'选择筛选后数据并填写关键词，开始科研构思。');
  $('research-binding').textContent=r?'本次任务固定使用：'+r.datasetName+' · '+r.rows+' 人 · '+r.datasetId.slice(0,8)+'。'+(r.datasetId!==selection?'当前下拉框是另一份数据，已有产物仍属于本次任务。':'')+(r.modelSource?'\n'+r.modelSource:''):'';
  $('research-roles').replaceChildren();for(const [role,name]of Object.entries(labels)){const card=node('div',null,'research-role '+(r?.steps?.[role]==='执行中'?'working':''));card.append(node('strong',name),node('small',r?.steps?.[role]||'等待'));$('research-roles').append(card);}
  $('research-events').textContent=(r?.events||[]).slice(-3).map(e=>labels[e.role]+' · '+e.text).join('\n');
  for(const b of document.querySelectorAll('[data-research-tab]')){b.classList.toggle('active',b.dataset.researchTab===tab);b.setAttribute('aria-selected',String(b.dataset.researchTab===tab));}
  const host=$('research-output');host.replaceChildren();
  if(tab==='proposal'){
   const report=[...(r?.artifacts||[])].reverse().find(x=>x.role==='report_writer');if(report){host.append(node('h3',report.title),node('p','这是研究方案与写作建议；其中提议的分析尚未自动执行。','method-callout'),section(report));}else host.append(node('p','完成证据检索、缺口发现、创新与审阅后，完整研究方案会显示在这里。','hint'));
  }else if(tab==='artifacts'){
   for(const a of r?.artifacts||[]){const detail=node('details');detail.append(node('summary',(labels[a.role]||a.role)+' · '+a.file+' · '+(a.version||1)),section(a));host.append(detail);}if(!r?.artifacts?.length)host.append(node('p','各角色会保存自己负责的产物。','hint'));
  }else if(tab==='evidence'){
   if(r?.evidenceAddedAfterReport)host.append(node('p','新增文献尚未纳入已生成方案的审阅；重新运行可结合这些证据。','method-callout'));
   for(const q of r?.queries||[]){const card=node('article',null,'research-card');card.append(node('strong',q.id+' · '+q.query),node('p',q.provider+' · '+q.pages+' 页 · '+q.ids.length+' 条 / '+(q.total??'未知'),'hint'));if(q.nextPage){const b=button('加载下一页',async()=>{loading=true;render(state);try{await invoke('researchMore',{queryId:q.id});}finally{loading=false;render(state)}});b.disabled=active||loading;card.append(b);}host.append(card);}
   for(const w of r?.works||[]){const card=node('article',null,'research-card');card.append(node('small',w.id+' · '+(w.year||'年份未知')+' · '+(w.provider||'')),node('h4',w.title),node('p',w.authors,'hint'),node('p',w.evidence,'hint'),button('打开论文',()=>invoke('openLink',{url:w.url})));const digests=(r.digests||[]).filter(d=>d.sourceId===w.id);for(const d of digests){const detail=node('details');detail.append(node('summary','独立阅读摘要 · '+({fulltext:'开放全文',abstract:'摘要',metadata:'仅书目',masked_annotation:'生成的结果遮蔽注释'}[d.level]||d.level)),section(d));card.append(detail);}host.append(card);}
   if(!r?.works?.length)host.append(node('p','此处只列真实返回的书目。检索结果可分页，读到的证据等级会逐条说明。','hint'));
  }else{
   host.append(node('p','每次数据工具都绑定本次任务的快照编号；不接受模型指定其他文件。','hint'));
   for(const t of r?.toolAudit||[]){const detail=node('details');detail.append(node('summary',(labels[t.role]||t.role)+' · '+t.tool+' · '+(t.ok?'完成':'未完成')),node('pre',JSON.stringify(t,null,2),'research-text'));host.append(detail);}
  }
  $('vault-status').textContent='本地语料 '+(state.vault?.count||0)+' 篇'+(state.vault?.importing?' · 正在导入，可继续本地数据分析':'')+'；可选，未导入时仍可在线检索。';
 }
 $('research-dataset').onchange=()=>{selection=$('research-dataset').value;error='';$('research-preview').replaceChildren();render(state);};
 $('research-data-preview').onclick=async()=>{try{const profile=await invoke('researchData',{datasetId:selection});const detail=node('details');detail.open=true;detail.append(node('summary','所选筛选后数据的描述与适配'),node('pre',JSON.stringify(profile,null,2),'research-text'));$('research-preview').replaceChildren(detail);}catch(e){error=e.message;render(state)}};
 $('research-start').onclick=async()=>{if(!state.settings?.model||!state.settings.shareData){$('settings-button').click();return;}error='';try{await invoke('researchStart',{datasetId:selection,keywords:$('research-query').value,question:$('research-question').value,callLimit:Number($('research-budget').value)});}catch(e){error=e.message;render(state)}};
 $('research-stop').onclick=()=>invoke('researchCancel').catch(()=>{});
 $('research-export').onclick=async()=>{if(exporting)return;exporting=true;render(state);try{const x=await invoke('researchExport');if(x)message('assistant','科研构思 Excel、Word、所选筛选后数据和审计已保存到：\n'+x.directory);}catch(e){error=e.message;}finally{exporting=false;render(state);}};
 $('vault-import').onclick=()=>invoke('vaultImport').catch(()=>{});$('vault-stop').onclick=()=>invoke('vaultCancel').catch(()=>{});
 for(const b of document.querySelectorAll('[data-research-tab]'))b.onclick=()=>{tab=b.dataset.researchTab;render(state);};
 return {render,showLatest(){selection=state.researchDatasets?.at(-1)?.id||'';if($('research-panel').hidden)$('research-toggle').click();render(state);$('research-dataset').focus();}};
})();
