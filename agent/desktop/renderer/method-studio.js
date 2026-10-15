// Method-specific planning diagrams. Plans are distinct from computed statistical results.
window.psychMethodStudio=(()=>{
 const hosts={};let activeData,graphId=0;
 const node=(tag,text,cls)=>{const e=document.createElement(tag);if(text!=null)e.textContent=text;if(cls)e.className=cls;return e};
 const svgNode=(tag,attrs={})=>{const e=document.createElementNS('http://www.w3.org/2000/svg',tag);for(const [k,v]of Object.entries(attrs))e.setAttribute(k,v);return e};
 const rows=x=>Array.isArray(x)?x:[];
 const notes={
  mediation:['可视化路径规划','先确认分析变量，再拖动布局、连接路径，把调节变量指定到具体路径。'],
  heterogeneity:['选择异质性问题','LPA 看连续指标的剖面；LCA 看分类指标的类别；网络树看哪些分裂变量对应关联结构的差异。'],
  network:['设计你的网络','先确定节点代表什么，再选择估计、稳定性与比较。规划图不包含未经估计的边。'],
  dag:['规划方向与算法比较','指定节点、数据类型和理论上禁止的方向，再比较算法；规划不代表已发现因果。'],
  intervention:['选择干预分析路线','IDA 的变量和识别假设需要确认；NIRA 路线基于对应网络的 EI CS，并将长时任务导出。'],
  ml:['设计预测任务','先明确预测什么、用什么信息预测，以及如何保留独立测试集。']
 };
 function create(module,page){const h=node('section',null,'method-studio');h.append(node('h3',notes[module][0]),node('p',notes[module][1],'hint'));const content=node('div');h.append(content);hosts[module]=content;page.append(h);}
 const spec=()=>JSON.parse(document.getElementById('psy-spec').value||'{}');
 function update(module,fn){try{const s=spec();fn(s);document.getElementById('psy-spec').value=JSON.stringify(s,null,2);window.psychPages.render(module,s,activeData);}catch(e){notice(e.message);}}
 function button(text,fn,cls){const b=node('button',text,cls);b.onclick=()=>{try{fn()}catch(e){notice(e.message)}};return b;}
 function select(id,label,values){const wrap=node('div');const l=node('label',label);l.htmlFor=id;const c=node('select');c.id=id;const blank=node('option','请选择实际变量');blank.value='';c.append(blank);for(const v of values){const o=node('option',v);o.value=v;c.append(o);}wrap.append(l,c);return {wrap,c};}
 function flow(h,steps){const f=node('ol',null,'method-flow');for(const [title,desc]of steps){const li=node('li');li.append(node('strong',title),node('span',desc));f.append(li);}h.append(f);}
 function cards(h,items,current,action){const box=node('div',null,'method-cards');for(const [value,title,desc]of items){const b=button('',()=>action(value),'method-card'+(value===current?' selected':''));b.setAttribute('aria-pressed',value===current);b.append(node('strong',title),node('span',desc));box.append(b);}h.append(box);}
 function graph(h,names,edges=[],dashed=[]){
  const sorted=[],pending=[...names];while(pending.length){const i=pending.findIndex(n=>!edges.some(([a,b])=>b===n&&pending.includes(a)));if(i<0)break;sorted.push(...pending.splice(i,1));}if(sorted.length===names.length)names=sorted;
  const arrow='plan-arrow-'+(++graphId);const width=480,height=Math.max(190,Math.ceil(names.length/3)*100+40);const chart=svgNode('svg',{viewBox:`0 0 ${width} ${height}`,role:'img','aria-label':'使用者指定的研究规划图；不是计算结果',class:'planning-graph'});const defs=svgNode('defs');const marker=svgNode('marker',{id:arrow,viewBox:'0 0 10 10',refX:9,refY:5,markerWidth:7,markerHeight:7,orient:'auto-start-reverse'});marker.append(svgNode('path',{d:'M 0 0 L 10 5 L 0 10 z',fill:'#397c65'}));defs.append(marker);chart.append(defs);
  const positions={};for(let i=0;i<names.length;i++)positions[names[i]]=[85+(i%3)*155,70+Math.floor(i/3)*100];
  for(const [from,to]of edges){if(!positions[from]||!positions[to])continue;const [x1,y1]=positions[from],[x2,y2]=positions[to];const dx=x2-x1,dy=y2-y1;const distance=Math.hypot(dx,dy)||1;const ux=dx/distance,uy=dy/distance;const radius=Math.min(ux?54/Math.abs(ux):Infinity,uy?22/Math.abs(uy):Infinity);const attrs={stroke:'#397c65','stroke-width':2,fill:'none','marker-end':`url(#${arrow})`};if(y1===y2&&Math.abs(dx)>160)chart.append(svgNode('path',{...attrs,d:`M ${x1+Math.sign(dx)*58} ${y1-8} Q ${(x1+x2)/2} ${y1-90} ${x2-Math.sign(dx)*64} ${y2-8}`}));else chart.append(svgNode('line',{...attrs,x1:x1+ux*(radius+4),y1:y1+uy*(radius+4),x2:x2-ux*(radius+12),y2:y2-uy*(radius+12)}));}
  for(const [moderator,from,to]of dashed){if(!positions[moderator]||!positions[from]||!positions[to])continue;const [x1,y1]=positions[moderator];const x2=(positions[from][0]+positions[to][0])/2,y2=(positions[from][1]+positions[to][1])/2;chart.append(svgNode('line',{x1,y1,x2,y2,stroke:'#ba7952','stroke-width':2,'stroke-dasharray':'5 5'}),svgNode('circle',{cx:x2,cy:y2,r:4,fill:'#ba7952'}));}
  for(const name of names){const [x,y]=positions[name];chart.append(svgNode('rect',{x:x-54,y:y-22,width:108,height:44,rx:10,fill:'#eef6f1',stroke:'#b4d1c1'}));const text=svgNode('text',{x,y:y+5,'text-anchor':'middle',fill:'#235741','font-size':12});text.textContent=name.length>14?name.slice(0,13)+'…':name;const title=svgNode('title');title.textContent=name;chart.append(title,text);}
  if(!names.length){const text=svgNode('text',{x:240,y:90,'text-anchor':'middle',fill:'#708079','font-size':13});text.textContent='选择变量后，在这里查看研究规划';chart.append(text);}
  h.append(chart,node('p','规划预览 · 不包含未经计算的系数、显著性或因果结论。','hint'));
 }
 function pathBuilder(h,module,s,names,key='paths'){
  const from=select(module+'-path-from',key==='forbidden'?'禁止方向的起点':'路径起点',names);const to=select(module+'-path-to',key==='forbidden'?'禁止方向的终点':'路径终点',names);const grid=node('div',null,'planner-pickers');grid.append(from.wrap,to.wrap);h.append(grid,button(key==='forbidden'?'＋ 禁止这个方向':'＋ 添加路径',()=>update(module,p=>{if(!from.c.value||!to.c.value)throw new Error('请先选择路径两端的变量。');if(from.c.value===to.c.value)throw new Error('路径不能指向自身。');p[key]=rows(p[key]);if(!p[key].some(e=>e.from===from.c.value&&e.to===to.c.value))p[key].push({from:from.c.value,to:to.c.value});}),'wide'));
  const list=node('div',null,'planned-edges');rows(s[key]).forEach((e,i)=>{const r=node('div',null,'planned-edge');r.append(node('span',e.from+(key==='forbidden'?' ↛ ':' → ')+e.to),button('移除',()=>update(module,p=>{p[key].splice(i,1);if(key==='paths'){p.indirectPaths=rows(p.indirectPaths).filter(chain=>chain.slice(1).every((v,j)=>p.paths.some(edge=>edge.from===chain[j]&&edge.to===v)));p.conditionalIndirect=rows(p.conditionalIndirect).filter(item=>rows(item.path).slice(1).every((v,j)=>p.paths.some(edge=>edge.from===item.path[j]&&edge.to===v)));}})));list.append(r);});h.append(list);
 }
 function mediation(h,s,names){
  window.mediationPlanner.render(h,s,activeData,next=>{document.getElementById('psy-spec').value=JSON.stringify(next,null,2);window.psychPages.render('mediation',next,activeData);});
 }
 function heterogeneity(h,s){
  cards(h,[['lpa','潜在剖面','连续指标：比较个体的水平组合与剖面数。'],['lca','潜在类别','分类指标：比较潜在类别及条件概率。'],['networktree','网络树','结果／节点变量 ＋ 分裂变量：研究关联结构异质性。']],s.method,v=>update('heterogeneity',p=>p.method=v));
  if(s.method==='networktree')flow(h,[['结果／网络节点',rows(s.variables).join('、')||'在下方指定至少两个结果变量'],['候选分裂变量',rows(s.splitVariables).join('、')||'在下方指定分裂变量，不能与节点重叠'],['树与分组','实际计算后显示分裂结构和每组人数，可自行选择分组继续网络分析']]);
  else flow(h,[['指标',rows(s.variables).join('、')||'在下方选择实际指标'],['类别数比较','最大 '+(s.maxClasses||4)+' 类；结合拟合、类别大小与理论'],['剖面／概率与成员','结果包含图、拟合表和每条记录的类别；分类不确定性需报告']]);
 }
 function network(h,s){
  cards(h,[['items','题项网络','每个节点对应一道题；按测量映射选择。'],['subscales','分量表网络','节点代表各维度；需要先完成分量表计分。'],['totals','总分网络','节点代表构念总分；避免与组成题项一起放入。']],s.level,v=>update('network',p=>p.level=v));
  graph(h,rows(s.variables));flow(h,[['估计 '+(s.method==='ising'?'Ising':'GGM'),'使用下方选定的 '+rows(s.variables).length+' 个节点'],['稳定性',Number(s.bootstrap)>0?'计划 '+s.bootstrap+' 次重抽样；实际返回 EI CS 后再判断':'当前跳过稳定性；未计算时不会报告 CS'],['网络比较',s.group?'按 '+s.group+' 比较；检查相同测量和各组样本量':'可不比较；需要时在下方选择实际分组变量']]);
 }
 function dag(h,s,names){
  graph(h,rows(s.variables));if(['hc','tabu','mmhc'].includes(s.algorithm||'hc'))pathBuilder(h,'dag',s,rows(s.variables),'forbidden');else h.append(node('p','当前 PC/GES/FCI 实现不支持禁止方向；如已设约束，请切回 HC/Tabu/MMHC，或在高级配置明确移除 forbidden 后再计算。','method-callout'));
  const descriptions={hc:'评分搜索：报告选定 DAG 及 CPDAG；方向可能不唯一。',tabu:'带记忆的评分搜索：可探索不同局部解。',mmhc:'混合约束与评分搜索：依赖独立性检验和评分。',pc:'条件独立检验：报告 CPDAG，受阈值与样本量影响。',ges:'贪婪等价类搜索：报告 CPDAG，并比较算法边差异。',fci:'允许潜在混淆的约束搜索：报告 PAG，不能直接输入标准 IDA。'};
  h.append(node('p',(s.algorithm||'hc').toUpperCase()+'：'+descriptions[s.algorithm||'hc'],'method-callout'));flow(h,[['理论约束',rows(s.forbidden).length+' 个禁止方向；只按已知设计与理论指定'],['实际计算','连续／分类类型与当前变量必须匹配'],['比较与报告','换算法后报告共同边、不同边与方向不确定性，不只挑有利结果']]);
 }
 function intervention(h,s,data){
  const result=data.results||{};const net=result.network;const cs=net?.cs?.expectedInfluence;
  const descriptions=[['IDA','连续 CPDAG → 明确暴露和结果 → 确认识别假设 → 本地计算'],['NIRApost','Ising 网络 → 同版本 EI CS > 0.25 → 导出长时任务 → 导回结果'],['GGMNIRA','GGM 网络 → 同版本 EI CS > 0.25 → 确认论文状态 → 导出长时任务']];
  for(const [title,desc]of descriptions){const card=node('div',null,'route-card');card.append(node('strong',title),node('p',desc));h.append(card);}
  flow(h,[['当前网络',net?net.method+'；EI CS：'+(cs==null?'尚未计算／核实':cs):'尚未完成网络分析'],['IDA 变量',s.exposure&&s.outcome?s.exposure+' → '+s.outcome:'在下方指定暴露和结果变量'],['当前 DAG',result.dag?result.dag.method+'；实际适配见下方检查结果':'尚未完成 DAG；可以先进入 DAG 页面']]);
  h.append(node('p','在下方独立模拟方案中指定干预变量、结果集合与强度；每次操作一个所选变量，保留完整网络。导出任务包含 Excel、Word 与高清图片。','method-callout'));
 }
 function ml(h,s){
  cards(h,[['regression','预测连续结果','例如量表分数；报告测试集误差与 R²。'],['classification','预测类别','例如预先定义的类别；报告测试集分类表现。']],s.task,v=>update('ml',p=>p.task=v));
  flow(h,[['预测目标',s.target||'选择你要预测的实际结果变量'],['可用特征',rows(s.features).join('、')||'选择预测时能够获得的信息'],['独立测试集','计划保留 '+Math.round((s.testFraction??.2)*100)+'%；'+(s.group?'按 '+s.group+' 分组划分':'按记录划分')],['评估','仅用训练数据拟合；实际样本数以缺失处理和分组划分后的输出为准']]);
  h.append(node('p','结果变量不能同时作为预测特征；重复受访者／机构应指定分组，避免训练与测试泄漏。','method-callout'));
 }
 function render(module,s,data){activeData=data;const h=hosts[module];if(!h)return;h.replaceChildren();const names=rows(data.columns).map(c=>c.column);({mediation:()=>mediation(h,s,names),heterogeneity:()=>heterogeneity(h,s),network:()=>network(h,s),dag:()=>dag(h,s,names),intervention:()=>intervention(h,s,data),ml:()=>ml(h,s)})[module]();}
 return {create,render};
})();
