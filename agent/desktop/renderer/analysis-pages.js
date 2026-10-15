// Each entry has its own persistent form; statistical execution still uses the shared engine.
window.psychPages=(()=>{
 const field=(key,label,type='multi',extra={})=>({key,label,type,...extra});
 const choice=(key,label,values)=>field(key,label,'select',{values});
 const definitions={
  mediation:{title:'中介与调节分析',description:'先确认分析变量，再在交互画布上安排中介和调节路径。AI 对话、画布与实际分析共用同一份方案。',example:'X 影响 M，M 影响 Y，同时保留 X → Y。估计 X → M → Y 的间接效应；W 调节 X → M。',fields:[field('paths','回归路径（每行一条，例如 X → M）','paths'),field('indirectPaths','间接路径（每行一条，例如 X → M → Y）','chains'),field('interactions','调节项（每行 X, W, Y 表示 W 调节 X → Y）','interactions'),field('bootstrap','Bootstrap 次数','number',{min:0,max:5000})]},
  heterogeneity:{title:'异质性分析',description:'选择 LPA、LCA 或网络树。网络树的结果变量指网络节点，需要另外明确指定分裂变量。',example:'用 X、M、Y 作为结果／节点变量，用 Group 和 Age 作为网络树分裂变量。不要猜变量含义。',fields:[choice('method','分析方法',[['lpa','潜在剖面 LPA'],['lca','潜在类别 LCA'],['networktree','网络树']]),field('variables','指标／结果变量（网络树中作为网络节点）'),field('splitVariables','网络树分裂变量（必须指定，与节点不能重叠）','multi',{when:s=>s.method==='networktree'}),field('minSize','网络树最小节点样本量（可留空自动计算）','number',{min:1,when:s=>s.method==='networktree'}),field('maxClasses','比较的最大类别数','number',{min:1,max:8,when:s=>s.method!=='networktree'})]},
  network:{title:'网络分析',description:'选择网络节点及题项、分量表或总分层次。可开启稳定性分析，再考虑网络比较或 DAG。',example:'用 X、M、Y、W 做 GGM 网络，节点是总分；计算 EI 的 CS，再按 Group 比较两组网络。',fields:[choice('method','网络方法',[['ggm','GGM 连续网络'],['ising','Ising 二分类网络']]),choice('level','节点层次',[['items','题项'],['subscales','分量表'],['totals','总分']]),field('variables','网络节点'),choice('correlation','相关估计',[['cor','Pearson 相关'],['cor_auto','自动混合／序数相关']]),field('bootstrap','稳定性重抽样次数（0 跳过，否则至少 50）','number',{min:0,max:2500}),field('group','网络比较的分组变量（可不选）','column'),field('permutations','网络比较置换次数（20–1000）','number',{min:20,max:1000,when:s=>!!s.group})]},
  dag:{title:'DAG 与方向不确定性',description:'默认使用贝叶斯爬山 HC。可以选择其他算法并核对边的差异；方向不能仅凭横断面数据确认为因果。',example:'对 X、M、Y、W 做连续数据 HC，报告 DAG 和 CPDAG；再用 PC 比较边。',fields:[field('variables','图中的变量'),choice('algorithm','算法',[['hc','贝叶斯爬山 HC'],['tabu','Tabu'],['mmhc','MMHC'],['pc','PC'],['ges','GES'],['fci','FCI / PAG']]),choice('dataType','变量类型',[['continuous','连续'],['categorical','分类']]),field('alpha','检验阈值 α','number',{min:.001,max:.5,step:.001})]},
  intervention:{title:'干预与网络模拟',description:'IDA 使用当前连续 CPDAG，暴露和结果变量由你指定。NIRApost / GGMNIRA 在下方导出独立任务并导回结果。',example:'用当前连续 CPDAG，X 是暴露变量，Y 是结果变量。先检查 IDA 的适配与假设。',fields:[field('exposure','IDA 暴露／干预变量','column'),field('outcome','IDA 结果变量','column')]},
  ml:{title:'机器学习',description:'明确预测目标及预测时可获得的特征。测试集保持独立，不能把结局或受访者编号误用为预测特征。',example:'预测 Y，特征为 X、M、W，做随机森林回归，保留 20% 测试集；按受访者／机构分组划分。',fields:[field('target','预测目标','column'),field('features','预测特征'),choice('task','任务',[['regression','回归'],['classification','分类']]),choice('model','模型',[['forest','随机森林'],['linear','线性／逻辑基线']]),field('group','划分分组变量（可不选）','column'),field('testFraction','测试集比例','number',{min:.1,max:.5,step:.05})]}
 };
 const pages={};let stamp='';
 const create=(tag,text)=>{const e=document.createElement(tag);if(text)e.textContent=text;return e};
 function init(){
  for(const [module,d]of Object.entries(definitions)){
   const page=create('section');page.id='page-'+module;page.className='analysis-page';page.hidden=true;page.setAttribute('aria-label',d.title);page.append(create('h3',d.title));
   const intro=create('p',d.description);intro.className='page-description';page.append(intro);
   window.psychMethodStudio.create(module,page);
   const form=create('div');form.className='analysis-fields';page.append(form);pages[module]={page,controls:{}};
   let textConfig;if(module==='mediation'){textConfig=create('details');textConfig.append(create('summary','路径文字配置（与画布同步）'));form.append(textConfig);}
   for(const f of d.fields){
    const wrap=create('div');wrap.className='analysis-field';const label=create('label',f.label);const control=create(['multi','select','column'].includes(f.type)?'select':['paths','chains','interactions'].includes(f.type)?'textarea':'input');control.id='field-'+module+'-'+f.key;label.htmlFor=control.id;
    if(f.type==='multi'){control.multiple=true;control.size=5;}
    if(f.type==='number'){control.type='number';for(const attr of ['min','max','step'])if(f[attr]!=null)control[attr]=f[attr];}
    if(f.type==='select')for(const [value,text]of f.values){const op=create('option',text);op.value=value;control.append(op);}
    if(control.tagName==='TEXTAREA'){control.rows=3;control.placeholder=f.type==='interactions'?'X, W, Y':f.type==='chains'?'X → M → Y':'X → M\nM → Y\nX → Y';}
    control.addEventListener('change',()=>{try{const s=JSON.parse(document.getElementById('psy-spec').value||'{}');const v=read(f,control);if(v==null||(f.key==='group'&&!v))delete s[f.key];else s[f.key]=v;document.getElementById('psy-spec').value=JSON.stringify(s,null,2);window.psychPages.render(module,s,current.psych.data);}catch(e){notice(e.message);}});
    wrap.append(label,control);if(f.type==='multi'){const hint=create('small','按住 Ctrl 多选；变量含义由你确认。');hint.className='hint';wrap.append(hint);}(module==='mediation'&&f.type!=='number'?textConfig:form).append(wrap);pages[module].controls[f.key]={f,control,wrap};
   }
   const ask=create('button','用 AI 完善本页分析方案');ask.className='wide';ask.onclick=()=>{document.getElementById('prompt').value=d.example;document.getElementById('prompt').focus();};page.append(ask);document.getElementById('psy-module-pages').append(page);
  }
 }
 const chains=text=>text.split(/\n/).filter(x=>x.trim()).map(x=>x.split(/\s*(?:→|->)\s*/).map(v=>v.trim()));
 function read(f,c){
  if(f.type==='multi')return [...c.selectedOptions].map(o=>o.value);
  if(f.type==='number'){if(c.value==='')return null;const v=Number(c.value);if(!Number.isFinite(v)||!c.checkValidity())throw new Error('请检查 '+f.label+' 的范围。');return v;}
  if(f.type==='paths'){const lines=chains(c.value);if(lines.some(x=>x.length!==2||x.some(v=>!v)))throw new Error('每条回归路径应为两个实际变量，例如 X → M。');return lines.map(([from,to])=>({from,to}));}
  if(f.type==='chains'){const lines=chains(c.value);if(lines.some(x=>x.length<3||x.some(v=>!v)))throw new Error('间接路径需至少三个变量，例如 X → M → Y。');return lines;}
  if(f.type==='interactions'){return c.value.split(/\n/).filter(x=>x.trim()).map(line=>{const v=line.split(/[,，]/).map(x=>x.trim());if(v.length!==3||v.some(x=>!x))throw new Error('调节项需指定 X, W, Y 三个实际变量。');return {x:v[0],w:v[1],to:v[2]};});}
  return c.value;
 }
 function visibility(module,s){for(const {f,wrap}of Object.values(pages[module].controls))wrap.hidden=!!f.when&&!f.when(s);}
 function render(module,s,data){
  for(const [key,p]of Object.entries(pages))p.page.hidden=key!==module;
  const next=module+'|'+data.revision+'|'+JSON.stringify(s)+'|'+JSON.stringify(data.columns);if(next===stamp)return;stamp=next;
  const names=(data.columns||[]).map(x=>x.column);
  for(const {f,control:c}of Object.values(pages[module].controls)){
   const v=s[f.key];
   if(f.type==='multi'||f.type==='column'){
    c.replaceChildren();if(f.type==='column'){const blank=create('option','请选择／暂不选择');blank.value='';c.append(blank);}
    for(const n of names){const op=create('option',n);op.value=n;op.selected=(Array.isArray(v)?v:[v]).includes(n);c.append(op);}
    if(f.type==='column')c.value=v||'';
   }else if(f.type==='paths')c.value=(v||[]).map(p=>p.from+' → '+p.to).join('\n');
   else if(f.type==='chains')c.value=(v||[]).map(p=>p.join(' → ')).join('\n');
   else if(f.type==='interactions')c.value=(v||[]).map(p=>[p.x,p.w,p.to].join(', ')).join('\n');
   else c.value=v??'';
  }
  visibility(module,s);
  window.psychMethodStudio.render(module,s,data);
 }
 function snapshot(module){
  const editor=document.getElementById('psy-spec'),s=JSON.parse(editor.value||'{}');
  // Read immediately before execution/chat too: a focused field may not have fired change yet.
  for(const {f,control,wrap}of Object.values(pages[module].controls))if(!wrap.hidden){const v=read(f,control);if(v==null||(f.key==='group'&&!v))delete s[f.key];else s[f.key]=v;}
  if(module==='mediation')window.mediationPlanner.snapshot(s);
  editor.value=JSON.stringify(s,null,2);return s;
 }
 function validate(module){const s=snapshot(module);if(module==='mediation')window.mediationPlanner.validate(s,current.psych.data);}
 return {init,render,validate,snapshot,definitions};
})();
