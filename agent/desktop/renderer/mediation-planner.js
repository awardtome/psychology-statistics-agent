// Variable confirmation, interactive path editing and conditional effects, using real column names.
window.mediationPlanner=(()=>{
 'use strict';
 const model=window.mediationModel,list=v=>Array.isArray(v)?v:[],same=(a,b)=>JSON.stringify(a)===JSON.stringify(b);
 const ui={key:'',editing:false,selection:[],mode:'move',from:null,moderator:null,edge:null,expanded:false,error:''};
 const node=(tag,text,cls)=>{const e=document.createElement(tag);if(text!=null)e.textContent=text;if(cls)e.className=cls;return e;};
 const svgNode=(tag,attrs={})=>{const e=document.createElementNS('http://www.w3.org/2000/svg',tag);for(const [k,v]of Object.entries(attrs))e.setAttribute(k,v);return e;};
 const locked=()=>typeof busy!=='undefined'&&busy;
 function render(host,s,data,commit){
  const key=JSON.stringify([data.name,data.revision,data.columns]);
  if(ui.key!==key){Object.assign(ui,{key,editing:false,selection:[],mode:'move',from:null,moderator:null,edge:null,expanded:false,error:''});}
  host.replaceChildren();
  const redraw=()=>render(host,s,data,commit);
  const apply=fn=>{if(locked())return;try{const next=JSON.parse(document.getElementById('psy-spec').value||'{}');fn(next);ui.error='';commit(next);}catch(e){ui.error=e.message;redraw();}};
  const button=(text,fn,cls='')=>{const b=node('button',text,cls);b.type='button';b.disabled=locked();b.onclick=()=>{if(!locked())fn();};return b;};
  const alert=()=>{if(ui.error){const p=node('p',ui.error,'planner-alert');p.setAttribute('role','alert');(valid?host.querySelector('.planner-content'):host).append(p);}};
  const valid=model.confirmed(s,data)&&!ui.editing;
  const steps=node('ol',null,'planner-steps');steps.append(node('li','① 确认分析变量',!valid?'current':''),node('li','② 安排中介与调节路径',valid?'current':''));host.append(steps);
  if(!valid){
   if(!ui.editing)ui.selection=[...new Set([...model.variables(s),...model.references(s)])].filter(n=>list(data.columns).some(c=>c.column===n));
   host.append(node('h4','第一步：这次研究使用哪些变量？'),node('p','先选择已计分或明确编码的变量。AI 整理的路径变量会预选；变量的研究含义由你确定。','hint'));
   const search=node('input');search.type='search';search.placeholder='搜索列名或已声明的变量标签';search.setAttribute('aria-label','搜索分析变量');
   const choices=node('div',null,'planner-variable-list'),count=node('p',null,'hint'),updateCount=()=>count.textContent='已选择 '+ui.selection.length+' 个变量：'+(ui.selection.join('、')||'尚未选择');
   for(const c of list(data.columns)){
    const label=node('label',null,'planner-variable'),box=node('input');box.type='checkbox';box.checked=ui.selection.includes(c.column);box.disabled=locked();
    box.setAttribute('aria-label','分析变量 '+c.column);const text=node('span',c.column),mapped=data.config?.labels?.[c.column];if(mapped)text.append(node('small',mapped));
    label.append(box,text,node('small',c.type));label.dataset.search=(c.column+' '+(mapped||'')).toLowerCase();
    box.onchange=()=>{ui.editing=true;ui.selection=ui.selection.filter(n=>n!==c.column);if(box.checked)ui.selection.push(c.column);updateCount();};choices.append(label);
   }
   search.oninput=()=>{for(const label of choices.children)label.hidden=!label.dataset.search.includes(search.value.trim().toLowerCase());};
   const confirmed=model.variables(s);if(ui.editing&&confirmed.length)host.append(node('p','重新确认后，取消选择的变量及其相关路径、调节和间接效应会移除。保留的路径和布局会保留。','hint'));
   updateCount();host.append(search,choices,count,button('确认变量，生成路径画布',()=>apply(p=>{model.confirm(p,ui.selection,data.revision);ui.editing=false;ui.mode='path';ui.from=ui.moderator=ui.edge=null;}),'primary wide'));alert();return;
  }
  const vars=model.variables(s),head=node('div',null,'planner-confirmed');head.append(node('span','已确认 '+vars.length+' 个变量 · 数据版本 '+data.revision),button('重新选择变量',()=>{ui.editing=true;ui.selection=[...vars];ui.expanded=false;redraw();}));host.append(head);
  const content=node('div',null,'planner-content'),inline=node('div'),dialog=node('dialog',null,'planner-dialog');
  const dialogHead=node('div',null,'dialog-head');dialogHead.append(node('h2','中介与调节路径规划'),button('收起画布',()=>{ui.expanded=false;dialog.close();inline.append(content);}));dialog.append(dialogHead);
  dialog.addEventListener('cancel',()=>{ui.expanded=false;inline.append(content);});host.append(inline,dialog);
  const toolbar=node('div',null,'planner-toolbar');
  for(const [mode,text]of [['move','移动变量'],['path','连接路径'],['moderator','指定调节']]){const b=button(text,()=>{ui.mode=mode;ui.from=ui.moderator=null;ui.error='';redraw();},ui.mode===mode?'selected':'');b.setAttribute('aria-pressed',ui.mode===mode);toolbar.append(b);}
  toolbar.append(button('自动排列',()=>apply(p=>{p.planner.positions=[];})),button('展开路径画布',()=>{ui.expanded=true;dialog.append(content);dialog.showModal();}));content.append(toolbar);
  const hint=node('p',null,'planner-mode-hint');hint.setAttribute('role','status');
  const setHint=()=>hint.textContent=(ui.mode==='move'?'拖动变量调整位置；键盘方向键也可移动。':ui.mode==='path'?(ui.from?'已选起点 '+ui.from+'，请左键点击终点；再点起点可取消。':'左键点击起点，再左键点击终点添加路径。'):(ui.moderator?'已选调节变量 '+ui.moderator+'，请左键点击被调节的实线路径。':'左键点击调节变量，再点击被调节的实线路径。'))+' 右键点击已有路径或调节箭头即可删除。';setHint();content.append(hint);
  const geometry=model.layout(s),positions=geometry.positions;
  const scroll=node('div',null,'planner-canvas-scroll'),svg=svgNode('svg',{viewBox:`0 0 ${geometry.width} ${geometry.height}`,class:'mediation-canvas',role:'group','aria-label':'可交互中介调节路径画布'});
  const defs=svgNode('defs'),marker=svgNode('marker',{id:'mediation-arrow',viewBox:'0 0 10 10',refX:9,refY:5,markerWidth:6,markerHeight:6,orient:'auto'});marker.append(svgNode('path',{d:'M 0 0 L 10 5 L 0 10 z',fill:'#287862'}));defs.append(marker);svg.append(defs);
  const modMarker=svgNode('marker',{id:'moderation-arrow',viewBox:'0 0 10 10',refX:10,refY:5,markerWidth:7,markerHeight:7,orient:'auto'});modMarker.append(svgNode('path',{d:'M 0 0 L 10 5 L 0 10 z',fill:'#b16b32'}));defs.append(modMarker);
  const lines=svgNode('g'),moderators=svgNode('g'),nodes=svgNode('g');svg.append(lines,moderators,nodes);scroll.append(svg);content.append(scroll,node('p','实线箭头：回归路径　·　橙色虚线单箭头：调节变量指向被调节路径。主效应由计算引擎纳入。','planner-legend'));
  const accessible=(g,action)=>{g.setAttribute('tabindex','0');g.setAttribute('role','button');g.setAttribute('aria-disabled',locked());g.onclick=()=>{if(!locked())action();};g.onkeydown=e=>{if(['Enter',' '].includes(e.key)){e.preventDefault();if(!locked())action();}};};
  const bound=(a,b)=>{const dx=b.x-a.x,dy=b.y-a.y,k=1/Math.max(Math.abs(dx)/68,Math.abs(dy)/25,1);return {x:a.x+dx*k,y:a.y+dy*k};};
  function route(from,to){const a=positions.get(from),b=positions.get(to);if(!a||!b)return null;const start=bound(a,b),end=bound(b,a),bend=Math.abs(a.x-b.x)>400?-80:0,control={x:(a.x+b.x)/2,y:(a.y+b.y)/2+bend};return {start,end,control,mid:{x:(start.x+2*control.x+end.x)/4,y:(start.y+2*control.y+end.y)/4},d:`M ${start.x} ${start.y} Q ${control.x} ${control.y} ${end.x} ${end.y}`};}
  const chooseNode=n=>{ui.error='';if(ui.mode==='path'){if(ui.from===n)ui.from=null;else if(!ui.from)ui.from=n;else{const from=ui.from;apply(p=>{model.addPath(p,from,n);ui.from=null;});return;}}else if(ui.mode==='moderator')ui.moderator=ui.moderator===n?null:n;draw();setHint();};
  const chooseEdge=e=>{if(ui.mode==='moderator'){if(!ui.moderator){ui.error='先点击一个调节变量，再选择被调节的路径。';redraw();return;}const w=ui.moderator;apply(p=>{model.addInteraction(p,w,e.from,e.to);ui.moderator=null;ui.edge=e;});return;}ui.edge=e;redraw();};
  let drag=null;
  function draw(){
   lines.replaceChildren();moderators.replaceChildren();nodes.replaceChildren();
   for(const e of list(s.paths)){
    const r=route(e.from,e.to);if(!r)continue;const g=svgNode('g',{'aria-label':'路径 '+e.from+' → '+e.to,class:'canvas-edge'+(same(ui.edge,e)?' selected':'')});
    g.append(svgNode('path',{d:r.d,class:'edge-hit'}),svgNode('path',{d:r.d,class:'edge-line','marker-end':'url(#mediation-arrow)'}));accessible(g,()=>chooseEdge(e));
    const remove=()=>apply(p=>{model.removePath(p,e.from,e.to);ui.edge=ui.from=ui.moderator=null;});
    g.oncontextmenu=event=>{event.preventDefault();event.stopPropagation();if(!locked())remove();};const keys=g.onkeydown;g.onkeydown=event=>{if(['Delete','Backspace'].includes(event.key)){event.preventDefault();if(!locked())remove();}else keys(event);};lines.append(g);
   }
   for(const i of list(s.interactions)){
    const r=route(i.x,i.to),a=positions.get(i.w);if(!r||!a)continue;const start=bound(a,r.mid),g=svgNode('g',{'aria-label':i.w+' 调节 '+i.x+' → '+i.to,class:'canvas-moderator'});
    const d=`M ${start.x} ${start.y} L ${r.mid.x} ${r.mid.y}`;
    // Leave the junction's hit area to the regression edge, so right-click deletes that edge.
    const length=Math.hypot(r.mid.x-start.x,r.mid.y-start.y),ratio=Math.max(0,1-24/Math.max(length,1));
    const hit=`M ${start.x} ${start.y} L ${start.x+(r.mid.x-start.x)*ratio} ${start.y+(r.mid.y-start.y)*ratio}`;
    g.append(svgNode('path',{d:hit,class:'edge-hit'}),svgNode('path',{d,class:'moderator-line','marker-end':'url(#moderation-arrow)','pointer-events':'none'}));const title=svgNode('title');title.textContent=i.w+' 调节 '+i.x+' → '+i.to+'；右键删除';g.append(title);
    const remove=()=>apply(p=>model.removeInteraction(p,i));accessible(g,()=>{ui.error='右键点击这条调节箭头可删除，也可使用下方移除按钮。';redraw();});g.oncontextmenu=event=>{event.preventDefault();event.stopPropagation();if(!locked())remove();};const keys=g.onkeydown;g.onkeydown=event=>{if(['Delete','Backspace'].includes(event.key)){event.preventDefault();if(!locked())remove();}else keys(event);};moderators.append(g);
   }
   for(const n of vars){
    const p=positions.get(n),g=svgNode('g',{transform:`translate(${p.x} ${p.y})`,'aria-label':'画布变量 '+n,class:'canvas-variable'+([ui.from,ui.moderator].includes(n)?' selected':'')});
    const title=svgNode('title');title.textContent=n+(data.config?.labels?.[n]?' · '+data.config.labels[n]:'');g.append(title,svgNode('rect',{x:-68,y:-25,width:136,height:50,rx:10}));const text=svgNode('text',{'text-anchor':'middle',y:6});text.textContent=n.length>12?n.slice(0,11)+'…':n;g.append(text);accessible(g,()=>chooseNode(n));
    if(ui.mode==='move'){
     g.onpointerdown=e=>{if(locked()||e.button!==0)return;e.preventDefault();const c=point(e);drag={variable:n,origin:{...p},start:c,moved:false};svg.setPointerCapture(e.pointerId);};
     const enter=g.onkeydown;g.onkeydown=e=>{if(['ArrowLeft','ArrowRight','ArrowUp','ArrowDown'].includes(e.key)){e.preventDefault();apply(plan=>model.position(plan,n,Math.max(80,Math.min(geometry.width-80,p.x+(e.key==='ArrowRight'?10:e.key==='ArrowLeft'?-10:0))),Math.max(40,Math.min(geometry.height-40,p.y+(e.key==='ArrowDown'?10:e.key==='ArrowUp'?-10:0)))));}else enter(e);};
    }
    nodes.append(g);
   }
  }
  function point(e){const pt=new DOMPoint(e.clientX,e.clientY);return pt.matrixTransform(svg.getScreenCTM().inverse());}
  svg.onpointermove=e=>{if(!drag||locked())return;const c=point(e),dx=c.x-drag.start.x,dy=c.y-drag.start.y;if(Math.hypot(dx,dy)>3)drag.moved=true;if(drag.moved){positions.set(drag.variable,{x:Math.max(80,Math.min(geometry.width-80,Math.round((drag.origin.x+dx)/5)*5)),y:Math.max(40,Math.min(geometry.height-40,Math.round((drag.origin.y+dy)/5)*5))});draw();}};
  svg.onpointerup=e=>{if(!drag)return;const d=drag;drag=null;if(svg.hasPointerCapture(e.pointerId))svg.releasePointerCapture(e.pointerId);if(d.moved){const p=positions.get(d.variable);apply(plan=>model.position(plan,d.variable,p.x,p.y));}};
  svg.onpointercancel=()=>{if(drag){positions.set(drag.variable,drag.origin);drag=null;draw();}};draw();
  if(ui.edge&&!list(s.paths).some(e=>same(e,ui.edge)))ui.edge=null;
  if(ui.edge){const e=ui.edge,panel=node('div',null,'planner-inspector');panel.append(node('strong','已选择路径 '+e.from+' → '+e.to),node('p','移除此边会同时移除绑定的调节关系和依赖它的间接效应。','hint'),button('移除这条路径',()=>apply(p=>{model.removePath(p,e.from,e.to);ui.edge=null;})));content.append(panel);}
  const alternative=node('details',null,'planner-alternative');alternative.append(node('summary','也可用按钮连接路径、指定调节'));
  const select=(label,values)=>{const wrap=node('div'),l=node('label',label),c=node('select');c.setAttribute('aria-label',label);const blank=node('option','请选择');blank.value='';c.append(blank);for(const value of values){const op=node('option',value);op.value=value;c.append(op);}l.append(c);wrap.append(l);return {wrap,c};};
  const from=select('画布路径起点',vars),to=select('画布路径终点',vars),pickers=node('div',null,'planner-pickers');pickers.append(from.wrap,to.wrap);alternative.append(pickers,button('添加这条路径',()=>apply(p=>model.addPath(p,from.c.value,to.c.value))));
  const mod=select('画布调节变量',vars),edge=select('画布被调节路径',list(s.paths).map(e=>e.from+' → '+e.to));
  // Option values use indices: literal variable names may contain arrows or punctuation.
  [...edge.c.options].slice(1).forEach((o,i)=>o.value=String(i));const modPickers=node('div',null,'planner-pickers');modPickers.append(mod.wrap,edge.wrap);alternative.append(modPickers,button('指定这条调节关系',()=>apply(p=>{const e=list(p.paths)[Number(edge.c.value)];if(edge.c.value===''||!e)throw new Error('请选择被调节路径。');model.addInteraction(p,mod.c.value,e.from,e.to);})));content.append(alternative);
  content.append(node('h4','当前模型：'+list(s.paths).length+' 条路径 · '+list(s.interactions).length+' 个调节关系'));
  const edgeList=node('div',null,'planned-edges');for(const e of list(s.paths)){const row=node('div',null,'planned-edge');row.append(button(e.from+' → '+e.to,()=>{ui.edge=e;ui.mode='move';redraw();},'planner-edge-label'),button('移除路径 '+e.from+' → '+e.to,()=>apply(p=>model.removePath(p,e.from,e.to))));edgeList.append(row);}content.append(edgeList);
  for(const i of list(s.interactions)){const row=node('div',null,'planned-edge');row.append(node('span',i.w+' 调节 '+i.x+' → '+i.to),button('移除调节 '+i.w+'：'+i.x+' → '+i.to,()=>apply(p=>model.removeInteraction(p,i))));content.append(row);}
  const used=new Set(model.references(s)),unused=vars.filter(n=>!used.has(n));if(unused.length)content.append(node('p','尚未连入模型：'+unused.join('、')+'。未连入的变量不会进入估计或缺失筛选。','hint'));
  content.append(node('h4','选择要估计的间接效应'));
  const candidates=model.chains(s),chains=[...candidates.paths];for(const c of list(s.indirectPaths))if(!chains.some(v=>same(v,c)))chains.push(c);
  if(!chains.length)content.append(node('p','至少连接两条连续路径（例如 X → M → Y），这里就会出现可勾选的间接效应。','hint'));
  for(const path of chains){
   const wrap=node('div',null,'planner-effect'),label=node('label',null,'check'),box=node('input');box.type='checkbox';box.checked=list(s.indirectPaths).some(c=>same(c,path));box.onchange=()=>apply(p=>{p.indirectPaths=list(p.indirectPaths).filter(c=>!same(c,path));if(box.checked)p.indirectPaths.push(path);else p.conditionalIndirect=list(p.conditionalIndirect).filter(c=>!same(c.path,path));});label.append(box,node('span',path.join(' → ')));wrap.append(label);
   const ws=[...new Set(list(s.interactions).filter(i=>path.slice(1).some((to,j)=>i.x===path[j]&&i.to===to)).map(i=>i.w))];
   if(box.checked)for(const w of ws){
    const conditional=list(s.conditionalIndirect).find(c=>c.moderator===w&&same(c.path,path)),l=node('label',null,'check'),cb=node('input');cb.type='checkbox';cb.checked=!!conditional;cb.onchange=()=>apply(p=>{p.conditionalIndirect=list(p.conditionalIndirect).filter(c=>c.moderator!==w||!same(c.path,path));if(cb.checked)p.conditionalIndirect.push({path,moderator:w});});l.append(cb,node('span','估计 '+w+' 不同取值下的条件间接效应'));wrap.append(l);
    if(conditional){const raw=node('label','调节值（原始量尺，逗号分隔；留空用均值及 ±1 SD）'),input=node('input');input.type='text';input.className='planner-condition-values';input.dataset.path=JSON.stringify(path);input.dataset.moderator=w;input.value=list(conditional.values).join(', ');input.setAttribute('aria-label',w+' 的条件间接效应调节值 '+path.join(' → '));input.placeholder='默认：均值、均值 −1 SD、均值 +1 SD';input.onchange=()=>apply(p=>{const c=p.conditionalIndirect.find(c=>c.moderator===w&&same(c.path,path));const values=model.conditionalValues(input.value);if(values==null)delete c.values;else c.values=values;});raw.append(input);wrap.append(raw);}
   }
   content.append(wrap);
  }
  if(candidates.truncated)content.append(node('p','候选链较多，画布最多列出 60 条。其余指定效应可通过 AI 或下方文字配置补充。','hint'));
  content.append(node('p','调节模型自动纳入预测变量和调节变量的主效应，交互项在本机均值中心化。规划图不显示未经计算的系数；只有点击“开始本地分析”才会估计。','method-callout'));
  for(const issue of model.problems(s,list(data.columns).map(c=>c.column)))content.append(node('p',issue,'planner-alert'));
  inline.append(content);if(ui.expanded){dialog.append(content);dialog.showModal();}alert();
 }
 function validate(s,data){if(ui.editing||!model.confirmed(s,data))throw new Error('请先在中介调节页面确认分析变量，再生成路径画布。');const issues=model.problems(s,list(data.columns).map(c=>c.column));if(issues.length)throw new Error(issues.join(' '));}
 function snapshot(s){
  for(const input of document.querySelectorAll('#page-mediation .planner-condition-values')){
   const path=JSON.parse(input.dataset.path),c=list(s.conditionalIndirect).find(c=>c.moderator===input.dataset.moderator&&same(c.path,path));
   if(c){const values=model.conditionalValues(input.value);if(values==null)delete c.values;else c.values=values;}
  }
  return s;
 }
 return {render,validate,snapshot};
})();
