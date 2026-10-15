// Shared desktop shell; dynamic content is rendered as text.
const psyModules={mediation:['中介调节','指定变量与路径，由 AI 核对遗漏。'],heterogeneity:['异质性分析','潜在剖面 · 潜在类别 · 网络树'],network:['网络分析','题项 / 分量表 / 总分 · 稳定性 · 网络比较'],dag:['DAG','默认贝叶斯爬山 · CPDAG · 算法比较'],intervention:['干预模拟','IDA 本地计算 · NIRApost / GGMNIRA 外算'],ml:['机器学习','回归 / 分类 · 保留测试集 · 防止泄漏']};
let psyLast=null,psyResearchBusy=false,psyAutoKey='',psyFocused=false,psyLastModule=null;
let psyDrafts={},psyDataKey='';
window.psychPages.init();
const el=(tag,text,cls)=>{const n=document.createElement(tag);if(text!=null)n.textContent=text;if(cls)n.className=cls;return n};
const psyCall=(action,params={})=>invoke('psych',{action,params});
function psyButton(text,fn,cls=''){const b=el('button',text,cls);b.onclick=()=>Promise.resolve(fn()).catch(()=>{});b.dataset.psychControl='1';return b}
function psyDetails(title,value){const d=el('details');d.append(el('summary',title));const p=el('pre',JSON.stringify(value,null,2));d.append(p);return d}
function psySpec(){try{return JSON.parse($('psy-spec').value||'{}')}catch{throw new Error('配置格式不正确。可以请 AI 整理，也可在高级配置中修正 JSON。')}}
async function psyPrepare(run=false){const module=current.psych.active==='intervention'?'ida':current.psych.active;window.psychPages.validate(current.psych.active);const spec=psySpec();await psyCall('prepare',{module,spec});if(run)await psyCall('run',{module});}
function psyEnter(module,accepted=false){if(current.psych?.stage==='analysis'){const active=current.psych.active;const key=active==='intervention'?'ida':active;try{psyDrafts[active]={spec:psySpec(),base:JSON.stringify(current.psych.specs?.[key]||{})}}catch{}}return psyCall('enter',{module,accepted}).then(()=>{if(accepted)message('assistant','进入'+psyModules[module][0]+'。当前使用 '+current.psych.data?.name+'，数据版本 '+current.psych.data?.revision+'，'+current.psych.data?.rows+' 条记录。请说明变量和研究问题；补充检查可以跳过。')})}
function psyTemplate(module){
 return {mediation:{paths:[],indirectPaths:[],interactions:[],bootstrap:500},heterogeneity:{method:'lpa',variables:[],maxClasses:4,standardize:true},network:{method:'ggm',variables:[],level:'items',bootstrap:0,correlation:'cor'},dag:{variables:[],algorithm:'hc',dataType:'continuous',alpha:.05},intervention:{exposure:'',outcome:''},ml:{target:'',features:[],task:'regression',model:'forest',testFraction:.2}}[module];
}
function renderFigureStudio(state){
 const panel=$('figure-studio');panel.hidden=state.module==='home';
 const q=state.module==='questionnaire';const source=$('figure-module');
 for(const option of source.options)option.disabled=q?option.value==='meta':option.value!=='meta';
 if(!q)source.value='meta';else if(source.value==='meta')source.value='mediation';
 const f=q?state.psych?.figure:state.figure;const image=q?state.psych?.figureImage:state.figureImage;
 $('figure-preview').hidden=!image;if(image)$('figure-preview').src=image;
 if(f)$('figure-status').textContent=f.title+' · 数据版本 '+f.revision+' · '+f.options.dpi+' DPI · '+f.options.width+' × '+f.options.height+' 英寸。已加入报告；可继续调整并生成新图。';
 else $('figure-status').textContent='PNG 高清位图 · SVG/PDF 矢量图 · 中文字体';
}
function psyRender(state){
 renderFigureStudio(state);
 const module=state.module||'home';document.body.classList.toggle('home-mode',module==='home');document.body.classList.toggle('psych-mode',module==='questionnaire');
 if(module!==psyLastModule){psyLastModule=module;$('messages').replaceChildren();if(module!=='home')message('assistant',module==='questionnaire'?'先导入问卷原始表，说明行列、变量、题项与筛选规则。配置 API 后可以直接跟我说；所有补充检查可跳过。':'先导入元分析编码表，确认效应定义与独立样本；本地分析或 AI 对话都可使用。')}
 $('home-panel').hidden=module!=='home';$('messages').hidden=module==='home';document.querySelector('.composer').hidden=module==='home';document.querySelector('.workspace').hidden=module==='home';window.panelLayout.sync(module);
 document.querySelector('.workspace>nav').hidden=module!=='meta';for(const name of ['data','plan','result'])$('tab-'+name).hidden=module!=='meta'||!document.querySelector('[data-tab="'+name+'"]').classList.contains('active');
 $('psych-workspace').hidden=module!=='questionnaire';document.querySelector('.steps').hidden=module!=='meta';$('psych-nav').hidden=module!=='questionnaire';
 $('open-file').textContent=module==='questionnaire'?'＋ 导入问卷原始表':'＋ 选择元分析编码表';$('open-file').hidden=module==='home';$('demo').hidden=module==='home';
 document.querySelector('header h1').textContent=module==='home'?'心理学统计agent':module==='questionnaire'?'问卷研究，从数据说明到完整报告':'元分析';
 document.querySelector('header p').textContent=module==='home'?'选择研究入口，配置模型后可直接对话。':'本地计算 · 可选检查 · 数据版本与结果可核对';
 $('prompt').placeholder=module==='questionnaire'?'例如：Q1–Q6 是某量表，Q2 反向，1–5 分；排除作答时间低于 120 秒。请先检查我漏了什么。':'例如：检查编码表，帮我做元分析并解释结果。';
 $('prompt').setAttribute('aria-label','给心理学统计agent的消息');
 document.querySelectorAll('[data-module]').forEach(n=>n.classList.toggle('active',n.dataset.module===module));
 const sourceData=module==='questionnaire'?state.psych?.data:state.data;
 $('attached-data').hidden=!sourceData?.name;
 $('attached-data').textContent=sourceData?.name?'▦ '+sourceData.name+' · '+(sourceData.rows??'待读取')+' 行':'';
 $('attached-data').title='当前分析数据；使用＋导入另一份数据会替换当前工作区。';
 if(module!=='questionnaire'){psyRenderResearch(state.research);return}
 const p=state.psych||{};const d=p.data;const stage=p.stage||'screen';const active=p.active||'mediation';
 const dataKey=p.file+'|'+d?.revision;if(dataKey!==psyDataKey){psyDataKey=dataKey;psyDrafts={};psyLast=null;}
 document.querySelectorAll('#psych-nav .nav-module').forEach(n=>n.classList.toggle('active',n.dataset.analysisModule===active&&stage==='analysis'));
 $('psy-page-description').textContent=stage==='analysis'?window.psychPages.definitions[active].description:stage==='hub'?'选择一个入口，打开对应的独立分析页面。':'先说明题项、反向计分和筛选规则。所有筛选步骤均可跳过。';
 if(stage==='analysis'){$('prompt').placeholder=window.psychPages.definitions[active].example;document.querySelector('header h1').textContent=psyModules[active][0]+' · AI 助手';}
 $('file-name').textContent=d?.name||'尚未选择问卷数据';
 $('psy-title').textContent=stage==='screen'?'筛选与计分':stage==='hub'?'选择分析入口':psyModules[active]?.[0]||'问卷分析';
 if(stage==='screen')document.querySelector('header h1').textContent='问卷筛选与计分 · AI 助手';
 $('psy-origin').textContent=d?.status==='ready'?d.name+' · 版本 '+d.revision+' · 当前 '+d.rows+' / 原始 '+d.raw_rows+' 条记录':'先导入 CSV 或 Excel；也可以体验合成示例。';
 if(d?.branch)$('psy-origin').textContent+=' · 分组 '+d.branch.class+'（来源版本 '+d.branch.parent_revision+'）';
 for(const s of ['screen','hub','analysis'])$('psy-'+s).hidden=stage!==s;
 $('psy-empty').hidden=d?.status==='ready';$('psy-content').hidden=d?.status!=='ready';
 $('psy-sheet-box').hidden=d?.status!=='choose_sheet';
 if(d?.status==='choose_sheet')options('psy-sheet',list(d.sheets),'','请选择实际工作表');
 if(d?.status==='ready'){
   $('psy-data-preview').replaceChildren(table(list(d.preview)));$('psy-columns').replaceChildren(table(list(d.columns).map(c=>({列:c.column,类型:c.type,缺失:c.missing,不同值:c.unique}))));
   $('psy-audit').textContent=JSON.stringify(d.audit||{},null,2);$('psy-review').replaceChildren();
   for(const r of list(d.review))$('psy-review').append(el('p',r.message,'hint'));
   $('psy-suitability').textContent=JSON.stringify(d.suitability,null,2);
   $('psy-suitability-summary').replaceChildren();
   const su=d.suitability||{};
   for(const [name,text]of [['潜在剖面','适用于连续指标；当前数值列有 '+list(su.numeric).join('、')+'，仍需区分连续总分与低类别题项。'],['潜在类别','适用于分类指标；当前 0/1 候选有 '+(list(su.binary01).join('、')||'尚无明确候选')+'。'],['网络树','需要指定节点与分裂变量；分析相关/协方差异质性。'],['网络节点层次','已声明量表 '+(list(su.known_scales).join('、')||'尚未映射')+'。题项、分量表和总分需按研究问题选择。']])$('psy-suitability-summary').append(el('p',name+'：'+text,'hint'));
   if(!psyFocused)$('psy-screen-config').value=JSON.stringify(d.config||{},null,2);
   $('psy-plan-review').replaceChildren();
   const key=active==='intervention'?'ida':active;const r=p.reviews?.[key];
   if(r){$('psy-plan-review').append(el('p',r.skipped?'已跳过补充问答。':'可补充，也可点击跳过。'));if(list(r.missing).length)$('psy-plan-review').append(el('p','仍需指定：'+list(r.missing).join('、')));for(const t of list(r.optional))$('psy-plan-review').append(el('p','• '+t,'hint'))}
   const stamp=active+'|'+JSON.stringify(p.specs?.[key]||{});
   if(psyLast!==stamp&&!psyFocused){const draft=psyDrafts[active];const saved=p.specs?.[key];const useDraft=draft&&draft.base===JSON.stringify(saved||{});let next=useDraft?draft.spec:saved||psyTemplate(active);if(active==='mediation'&&!useDraft)try{next=window.mediationModel.retainLayout(psySpec(),structuredClone(next),d.revision);}catch{}$('psy-spec').value=JSON.stringify(next,null,2);psyLast=stamp}
   window.psychPages.render(active,psySpec(),d);window.interventionPlan.render(p);
   $('psy-kind-options').replaceChildren();
   const choices={heterogeneity:[['lpa','潜在剖面 LPA'],['lca','潜在类别 LCA'],['networktree','网络树']],network:[['ggm','GGM'],['ising','Ising']],dag:[['hc','爬山 HC'],['tabu','Tabu'],['mmhc','MMHC'],['pc','PC'],['ges','GES'],['fci','FCI / PAG']],ml:[['forest','随机森林'],['linear','线性 / 逻辑基线']]}[active];
   if(choices)for(const [value,label]of choices.filter(c=>active!=='dag'||d.results?.dag||c[0]==='hc'))$('psy-kind-options').append(psyButton(label,async()=>{const s=psySpec();s[active==='dag'?'algorithm':active==='ml'?'model':'method']=value;$('psy-spec').value=JSON.stringify(s,null,2);await psyCall('prepare',{module:active,spec:s})}));
   $('psy-intervention').hidden=active!=='intervention';$('psy-analysis-buttons').hidden=active==='intervention';
   $('psy-ida-confirm').checked=p.idaConfirmed===true;$('psy-ggm-confirm').checked=p.ggmConfirmed===true;
   $('psy-results').replaceChildren();const result=active==='intervention'?(p.externalDisplay?d.results?.intervention:d.results?.ida||d.results?.intervention):d.results?.[key];
   if(result){
     $('psy-results').append(el('p','下方是最近一次实际计算结果。修改上方方案不会自动重算；请核对本次计算配置。','method-callout'),psyDetails('本次结果实际使用的配置',result.specification||{}));
     $('psy-results').append(el('h3',result.method+' 已完成'),el('p','有效样本 '+(result.n??'未核实')+' · 版本 '+(result.revision??'未核实'),'hint'));
     for(const field of ['table','criteria','selected','entropy','sizes','profiles','proportions','centrality','edges','groups','effects','min','max','metrics','comparison','cs','cs_origin','fit','importance','indirect_labels','conditional_labels','interaction_labels']){
       const value=result[field];if(value==null)continue;$('psy-results').append(el('h4',({table:'参数与区间',criteria:'类别选择',centrality:'中心性',edges:'图的边',cs:'CS 稳定性',effects:'可能效应集合',metrics:'测试集表现',comparison:'比较结果'})[field]||field));
       if(Array.isArray(value)&&value.length&&typeof value[0]==='object'){const wrap=el('div',null,'result-table-scroll');wrap.append(table(value));$('psy-results').append(wrap);}else $('psy-results').append(el('pre',JSON.stringify(value,null,2)));
     }
     const image=p.images?.[active==='intervention'&&p.externalDisplay?'intervention':key];if(image){const im=el('img');im.src=image;im.alt=result.method+' 实际结果图';im.className='psych-plot';$('psy-results').append(im)}
     for(const t of [...list(result.limitations),...list(result.warnings)])$('psy-results').append(el('p',t,'hint'));
   }
   $('psy-branches').replaceChildren();const het=d.branch_options?{groups:d.branch_options.groups}:d.results?.heterogeneity;if(het){for(const g of list(het.sizes||het.groups))$('psy-branches').append(psyButton('用分组 '+(g.groups||g.Var1)+'（'+g.Freq+' 人）',()=>psyCall('branch',{mode:'class',class:g.groups||g.Var1})))}
   $('psy-branches').append(psyButton('恢复整份筛选后的数据',()=>psyCall('branch',{mode:'all'})));
 }
 $('psy-transition').replaceChildren();const tr=p.transition;
 $('psy-transition').hidden=!tr;
 if(tr){$('psy-transition').append(el('h3','下一步使用哪份数据'),el('p',tr.message),el('p','来源 '+tr.from+' · 版本 '+tr.revision+' · '+tr.rows+' 个有效案例','hint'));
   $('psy-transition').append(psyButton(tr.to==='hub'?'使用这份数据进入分析入口':'使用这份数据进入'+psyModules[tr.to][0],()=>tr.to==='hub'?psyCall('hub'):psyEnter(tr.to,true),'primary'));
 $('psy-transition').append(psyButton('暂不进入，返回问卷筛选',()=>psyCall('screenPage')));
   if(tr.from==='heterogeneity')$('psy-transition').append(el('p','下面可以选择该次分组；未选择时使用整体筛选后数据。','hint'));
 }
 psyRenderResearch(state.research);
 document.querySelectorAll('[data-psych-control],#psych-workspace button,#psych-workspace input,#psych-workspace select,#psych-workspace textarea').forEach(n=>n.disabled=busy);
 document.querySelectorAll('.mediation-canvas [role="button"]').forEach(n=>n.setAttribute('aria-disabled',String(busy)));
}
function psyRenderResearch(){window.researchStudio.render(current);}
window.renderPsychology=psyRender;
document.querySelectorAll('[data-module]').forEach(n=>n.onclick=()=>invoke('module',{module:n.dataset.module}).catch(()=>{}));
for(const [key,[name,desc]]of Object.entries(psyModules)){
 const card=psyButton('',()=>psyEnter(key),'analysis-card');card.append(el('strong',name),el('span',desc));$('psy-hub-grid').append(card);
 const nav=psyButton(name,()=>psyEnter(key),'nav-module');nav.dataset.analysisModule=key;$('psych-nav').append(nav);
}
const home=$('home-panel');home.append(el('div','PSYCHOLOGY STATISTICS','eyebrow'),el('h2','让每一步分析都有清楚的依据'),el('p','导入数据，告诉 AI 变量与研究问题。选择需要的检查，运行分析，再导出 Excel、Word 与图片。','home-description'));
for(const [module,name,desc]of [['meta','元分析','整合研究效应，三水平模型、CR2 与调节分析。'],['questionnaire','问卷','筛选与计分 → 中介调节、异质性、网络、DAG、干预与机器学习。']]){const card=psyButton('',()=>invoke('module',{module}),'home-card');card.append(el('span',module==='meta'?'m³':'ψ','home-icon'),el('strong',name),el('p',desc),el('span','进入工作区 →'));home.append(card)}
home.append(el('p','Windows 本地桌面版 · 安装后直接打开 · AI 模型服务可配置','hint'));
$('psy-screened-data').onclick=()=>window.researchStudio.showLatest();
$('psy-screen-apply').onclick=()=>{try{psyCall('screen',{config:JSON.parse($('psy-screen-config').value||'{}')}).catch(()=>{})}catch{notice('配置 JSON 无效。请让 AI 整理或修改。')}};
$('psy-screen-skip').onclick=()=>psyCall('screen',{skip:true}).catch(()=>{});
$('psy-score-skip').onclick=()=>{const c=JSON.parse($('psy-screen-config').value||'{}');c.scales=[];psyCall('screen',{config:c}).catch(()=>{})};
$('psy-rules-skip').onclick=()=>{const c=JSON.parse($('psy-screen-config').value||'{}');c.rules=[];psyCall('screen',{config:c}).catch(()=>{})};
$('psy-review-skip').onclick=()=>{notice('可跳过补充检查。当前数据已保留，请点击左侧的中介调节、异质性分析等入口。');$('psych-nav').querySelector('[data-analysis-module]').focus();};
$('psy-hub-back').onclick=()=>psyCall('screenPage').catch(()=>{});
const screenNav=psyButton('筛选与计分',()=>psyCall('screenPage'),'nav-module');$('psych-nav').prepend(screenNav);
$('psy-prepare').onclick=()=>psyPrepare().catch(e=>notice(e.message));$('psy-run').onclick=()=>psyPrepare(true).catch(e=>notice(e.message));
$('psy-skip-analysis-review').onclick=()=>psyCall('skipReview',{module:current.psych.active==='intervention'?'ida':current.psych.active}).catch(()=>{});
$('psy-export').onclick=()=>psyCall('export').then(x=>{if(x)message('assistant','Excel、Word、图片和审计已导出到：\n'+x.directory)}).catch(()=>{});
$('psy-sheet-load').onclick=()=>invoke('sheet',{sheet:$('psy-sheet').value}).catch(()=>{});
$('psy-ida-confirm').onchange=$('psy-ggm-confirm').onchange=()=>psyCall('confirm',{ida:$('psy-ida-confirm').checked,ggm:$('psy-ggm-confirm').checked}).catch(()=>{});
$('psy-status').onclick=()=>psyCall('status').then(x=>{$('psy-intervention-status').textContent=JSON.stringify(x,null,2)}).catch(()=>{});
$('psy-ida-run').onclick=()=>psyPrepare(true).catch(e=>notice(e.message));
for(const method of ['nirapost','ggmnira'])$('psy-'+method).onclick=async()=>{try{const spec=window.interventionPlan.read();if(spec.method!==method)throw new Error('请先在独立模拟方案中选择对应方法。');await psyCall('prepare',{module:'intervention',spec});return psyCall('script',{method}).then(x=>{if(x)message('assistant',x.message+'\n'+x.directory)}).catch(()=>{});}catch(e){notice(e.message)}};
$('psy-cs-return').onclick=()=>psyCall('cs',{value:Number($('psy-cs').value),metric:'expectedInfluence'}).then(x=>{$('psy-intervention-status').textContent=JSON.stringify(x,null,2)}).catch(()=>{});
$('psy-import-result').onclick=()=>psyCall('import').then(()=>message('assistant','已导入实际结果表。可让 AI 解读，并导出完整 Excel 和 Word 报告。')).catch(()=>{});
$('psy-import-image').onclick=()=>psyCall('importImage').catch(()=>{});
$('psy-ask-ai').onclick=()=>{$('prompt').value='请检查当前问卷的数据类型、有效样本和已声明的量表映射，说明适合哪些异质性或网络分析、还漏了哪些信息，让我选择；不要擅自删行或猜构念。';send()};
for(const id of ['psy-spec','psy-screen-config']){$(id).onfocus=()=>psyFocused=true;$(id).onblur=()=>{psyFocused=false;if(id==='psy-spec')try{window.psychPages.render(current.psych.active,psySpec(),current.psych.data)}catch(e){notice(e.message)}}}
api.on('busy',value=>document.querySelectorAll('[data-psych-control],#psych-workspace button,#psych-workspace input,#psych-workspace select,#psych-workspace textarea').forEach(n=>n.disabled=value));
api.invoke('state').then(render).catch(()=>{});

$('figure-render').onclick=async()=>{
 try{
  const labels=JSON.parse($('figure-labels').value||'{}');
  await invoke('figure',{spec:{module:$('figure-module').value,type:$('figure-type').value,title:$('figure-title').value,xLabel:$('figure-x').value,yLabel:$('figure-y').value,labels,palette:$('figure-palette').value,dpi:Number($('figure-dpi').value),width:Number($('figure-width').value),height:Number($('figure-height').value)}});
 }catch(e){notice(e.message)}
};
$('figure-export').onclick=()=> (current.module==='questionnaire'?psyCall('export'):invoke('export')).then(x=>{if(x)message('assistant','科研图片、绘图数据与完整 Excel/Word 报告已保存到：\n'+x.directory)}).catch(()=>{});
api.on('busy',value=>{for(const id of ['figure-render','figure-export'])$(id).disabled=value});
