// Shared, DOM-free planning rules. The canvas edits the same specification the engine fits.
(function(root){
 'use strict';
 const list=v=>Array.isArray(v)?v:[];
 const equal=(a,b)=>JSON.stringify(a)===JSON.stringify(b);
 const edgeKey=e=>JSON.stringify([e.from,e.to]);
 function references(s){return [...new Set(list(s.paths).flatMap(e=>[e.from,e.to]).concat(list(s.interactions).flatMap(e=>[e.x,e.w,e.to]),list(s.indirectPaths).flat(),list(s.conditionalIndirect).flatMap(c=>[...list(c.path),c.moderator])).filter(v=>typeof v==='string'&&v))];}
 function variables(s){return Array.isArray(s.variables)?s.variables:references(s);}
 function expandedEdges(s){return list(s.paths).concat(list(s.interactions).map(i=>({from:i.w,to:i.to})));}
 function cyclic(edges){
  const visiting=new Set(),done=new Set(),names=[...new Set(edges.flatMap(e=>[e.from,e.to]))];
  const visit=n=>{if(visiting.has(n))return true;if(done.has(n))return false;visiting.add(n);if(edges.some(e=>e.from===n&&visit(e.to)))return true;visiting.delete(n);done.add(n);return false;};
  return names.some(visit);
 }
 function hasChain(s,path){return path.length>=3&&new Set(path).size===path.length&&path.slice(1).every((to,j)=>list(s.paths).some(e=>e.from===path[j]&&e.to===to));}
 function moderated(s,path,w){return path.slice(1).some((to,j)=>list(s.interactions).some(i=>i.w===w&&i.x===path[j]&&i.to===to));}
 function problems(s,columns){
  const names=variables(s),issues=[];const actual=new Set(columns);
  if(names.length<2)issues.push('请先选择至少两个分析变量。');
  if(new Set(names).size!==names.length)issues.push('分析变量不能重复。');
  if(names.some(n=>!actual.has(n))||references(s).some(n=>!actual.has(n)))issues.push('方案含当前数据中不存在的变量，请重新确认。');
  if(references(s).some(n=>!names.includes(n)))issues.push('路径用了未确认的变量，请回到第一步补充选择。');
  if(!list(s.paths).length)issues.push('请在画布上添加至少一条回归路径。');
  if(list(s.paths).some(e=>!e.from||!e.to||e.from===e.to))issues.push('回归路径需要两个不同的变量。');
  if(new Set(list(s.paths).map(edgeKey)).size!==list(s.paths).length)issues.push('同一条回归路径不能重复。');
  if(list(s.interactions).some(i=>new Set([i.x,i.w,i.to]).size!==3||!list(s.paths).some(e=>e.from===i.x&&e.to===i.to)))issues.push('调节关系必须绑定一条已存在的路径，并使用三个不同的变量。');
  if(new Set(list(s.interactions).map(i=>JSON.stringify([i.x,i.w,i.to]))).size!==list(s.interactions).length)issues.push('同一条调节关系不能重复。');
  if(cyclic(expandedEdges(s)))issues.push('路径形成了环（包含调节变量的主效应）；当前入口支持无环递归模型。');
  if(list(s.indirectPaths).some(c=>!hasChain(s,c)))issues.push('间接效应必须沿画布上完整的路径估计。');
  if(list(s.conditionalIndirect).some(c=>!hasChain(s,list(c.path))||!moderated(s,list(c.path),c.moderator)))issues.push('条件间接效应需要一条完整路径及沿途已指定的调节关系。');
  for(const c of list(s.conditionalIndirect))if(c.values!=null&&(!Array.isArray(c.values)||!c.values.length||c.values.length>10||c.values.some(v=>typeof v!=='number'||!Number.isFinite(v))))issues.push('条件效应的调节值需为最多十个原量尺数值。');
  return [...new Set(issues)];
 }
 function confirmed(s,data){return s.planner?.confirmed===true&&s.planner.revision===data.revision&&variables(s).length>=2&&variables(s).every(n=>list(data.columns).some(c=>c.column===n))&&references(s).every(n=>variables(s).includes(n));}
 function prune(s){
  const names=variables(s);s.paths=list(s.paths).filter(e=>names.includes(e.from)&&names.includes(e.to));
  s.interactions=list(s.interactions).filter(i=>[i.x,i.w,i.to].every(n=>names.includes(n))&&s.paths.some(e=>e.from===i.x&&e.to===i.to));
  s.indirectPaths=list(s.indirectPaths).filter(c=>hasChain(s,c));
  s.conditionalIndirect=list(s.conditionalIndirect).filter(c=>hasChain(s,list(c.path))&&moderated(s,list(c.path),c.moderator));
 }
 function confirm(s,names,revision){if(names.length<2)throw new Error('请先选择至少两个分析变量。');s.variables=[...new Set(names)];prune(s);s.planner={...s.planner,confirmed:true,revision,positions:list(s.planner?.positions).filter(p=>s.variables.includes(p.variable))};}
 function addPath(s,from,to){
  if(!variables(s).includes(from)||!variables(s).includes(to))throw new Error('请使用已确认的变量。');
  if(from===to)throw new Error('路径不能指向自身。');
  if(list(s.paths).some(e=>e.from===from&&e.to===to))throw new Error('这条路径已经存在。');
  if(cyclic(expandedEdges(s).concat({from,to})))throw new Error('这条路径会形成环，请调整方向。');
  s.paths=list(s.paths).concat({from,to});
 }
 function removePath(s,from,to){s.paths=list(s.paths).filter(e=>e.from!==from||e.to!==to);prune(s);}
 function addInteraction(s,w,from,to){
  if(!variables(s).includes(w)||new Set([w,from,to]).size!==3)throw new Error('请选择与路径两端不同的调节变量。');
  if(!list(s.paths).some(e=>e.from===from&&e.to===to))throw new Error('请先创建被调节的路径。');
  if(list(s.interactions).some(i=>i.w===w&&i.x===from&&i.to===to))throw new Error('这条调节关系已经存在。');
  if(cyclic(expandedEdges(s).concat({from:w,to})))throw new Error('调节变量的主效应会形成环，请调整路径。');
  s.interactions=list(s.interactions).concat({x:from,w,to});
 }
 function removeInteraction(s,i){s.interactions=list(s.interactions).filter(v=>!equal(v,i));prune(s);}
 function chains(s,limit=60){
  const result=[];let truncated=false;
  const walk=path=>{for(const e of list(s.paths).filter(e=>e.from===path.at(-1))){if(path.includes(e.to))continue;const next=[...path,e.to];if(next.length>=3){if(result.length>=limit){truncated=true;return;}result.push(next);}walk(next);if(truncated)return;}};
  for(const n of variables(s)){walk([n]);if(truncated)break;}return {paths:result,truncated};
 }
 function layout(s){
  const names=variables(s),edges=expandedEdges(s),levels=new Map(names.map(n=>[n,0]));
  if(!cyclic(edges))for(let i=0;i<names.length;i++)for(const e of edges)levels.set(e.to,Math.max(levels.get(e.to)||0,(levels.get(e.from)||0)+1));
  const maxLevel=Math.max(0,...levels.values()),width=Math.max(800,180+maxLevel*220),counts=new Map();
  for(const n of names)counts.set(levels.get(n),(counts.get(levels.get(n))||0)+1);
  const height=Math.max(400,100+Math.max(0,...counts.values())*100),next=new Map(),positions=new Map();
  for(const n of names){const l=levels.get(n)||0,index=next.get(l)||0;next.set(l,index+1);positions.set(n,{x:maxLevel?90+l*(width-180)/maxLevel:width/2,y:height/(counts.get(l)+1)*(index+1)});}
  for(const p of list(s.planner?.positions))if(positions.has(p.variable)&&Number.isFinite(p.x)&&Number.isFinite(p.y))positions.set(p.variable,{x:Math.max(80,Math.min(width-80,p.x)),y:Math.max(40,Math.min(height-40,p.y))});
  return {width,height,positions};
 }
 function position(s,variable,x,y){s.planner={...s.planner,positions:list(s.planner?.positions).filter(p=>p.variable!==variable).concat({variable,x,y})};}
 function conditionalValues(text){
  if(!text.trim())return null;const parts=text.split(/[,，]/).map(v=>v.trim()),values=parts.map(Number);
  if(parts.some(v=>!v)||values.length>10||values.some(v=>!Number.isFinite(v)))throw new Error('调节值应为最多十个原量尺数值。');return values;
 }
 function retainLayout(previous,next,revision){
  if(next.planner||!previous?.planner?.confirmed||previous.planner.revision!==revision)return next;
  const pool=variables(previous),explicit=next.variables;
  if(references(next).every(n=>pool.includes(n))&&(!explicit||(explicit.length===pool.length&&explicit.every(n=>pool.includes(n))))){next.variables=[...pool];next.planner=JSON.parse(JSON.stringify(previous.planner));}
  return next;
 }
 const api={references,variables,problems,confirmed,confirm,prune,addPath,removePath,addInteraction,removeInteraction,chains,layout,position,retainLayout,conditionalValues};
 if(typeof module==='object'&&module.exports)module.exports=api;else root.mediationModel=api;
})(typeof window==='object'?window:globalThis);
