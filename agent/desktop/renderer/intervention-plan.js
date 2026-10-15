window.interventionPlan=(()=>{
 const host=document.getElementById('intervention-plan');let stamp='';const controls={};
 const fields=[
  ['method','独立模拟方法','select',[['nirapost','NIRApost（Ising）'],['ggmnira','GGMNIRA（GGM）']]],
  ['targets','干预变量（必须主动选择；每次分别操作一个）','multi'],
  ['outcomeScope','评估结果范围','select',[['','请选择结果范围'],['whole_network','整个网络'],['selected_nodes','指定结果变量集合']]],
  ['outcomes','结果变量（选择指定集合时必填）','multi'],
  ['direction','NIRApost 阈值方向','select',[['','请选择方向'],['alleviating','降低阈值（alleviating）'],['aggravating','升高阈值（aggravating）']]],
  ['intensity','操作强度（Ising：阈值 SD 倍数；GGM：条件均值操作强度）','number'],
  ['repetitions','稳定性重复次数（正式分析请评估精度；少量仅调试）','number']
 ];
 const title=document.createElement('h3');title.textContent='独立模拟方案';host.append(title);
 const form=document.createElement('div');form.className='intervention-fields';host.append(form);
 for(const [key,label,type,options]of fields){
  const wrap=document.createElement('label');wrap.textContent=label;const c=document.createElement(type==='number'?'input':'select');c.id='simulation-'+key;c.setAttribute('aria-label',label);
  if(type==='multi'){c.multiple=true;c.size=4;}if(type==='number'){c.type='number';c.min=key==='repetitions'?1:.000001;c.step=key==='repetitions'?1:'any';if(key==='repetitions')c.max=5000;}
  for(const [value,text]of options||[]){const op=document.createElement('option');op.value=value;op.textContent=text;c.append(op);}
  c.onchange=visibility;wrap.append(c);form.append(wrap);controls[key]={c,wrap,type};
 }
 const note=document.createElement('p');note.className='hint';note.textContent='不删除完整网络中的其他节点。Ising 结果为所选节点之和；GGM 结果为整体或所选集合的 KL。GGM 指定结果集合必须与干预变量分开。';host.append(note);
 function visibility(){controls.intensity.c.step=controls.method.c.value==='ggmnira'?1:'any';controls.intensity.c.min=controls.method.c.value==='ggmnira'?1:.000001;controls.outcomes.wrap.hidden=controls.outcomeScope.c.value!=='selected_nodes';controls.direction.wrap.hidden=controls.method.c.value!=='nirapost';}
 function read(){const spec={};for(const [key,{c,type}]of Object.entries(controls)){if(type==='multi')spec[key]=[...c.selectedOptions].map(o=>o.value);else if(type==='number'){if(!c.value||!c.checkValidity())throw new Error('请填写有效的操作强度和重复次数。');spec[key]=Number(c.value);}else spec[key]=c.value;}return spec;}
 function render(p){
  const net=p.data?.results?.network;const names=[].concat(net?.nodes||[]);const saved=p.specs?.intervention;
  const next=JSON.stringify([p.file,p.data?.revision,names,saved]);if(next===stamp)return;stamp=next;
  const s=saved||{method:net?.method==='GGM'?'ggmnira':'nirapost',repetitions:1000};
  for(const [key,{c,type}]of Object.entries(controls)){
   if(type==='multi'){c.replaceChildren();for(const name of names){const op=document.createElement('option');op.value=name;op.textContent=name;op.selected=[].concat(s[key]||[]).includes(name);c.append(op);}}
   else c.value=s[key]??'';
  }visibility();
 }
 return {read,render};
})();
