const {test}=require('node:test');const assert=require('node:assert/strict');
const {searchResearch,mergeResearch}=require('../research.cjs');
const {PsychologyWorkspace}=require('../psych-controller.cjs');
const items=(start,n)=>Array.from({length:n},(_,i)=>({DOI:'10.1234/'+(start+i),title:['Study '+(start+i)]}));
test('research deep pagination encodes server cursors and keeps prior source IDs',async()=>{
 const urls=[];let call=0;const fetcher=async url=>{urls.push(new URL(url));return {ok:true,json:async()=>({message:{items:call++?items(19,7):items(0,20),'next-cursor':'next+/=','total-results':26}})}};
 const first=await searchResearch('emotion network',fetcher);assert.equal(first.works.length,20);assert.equal(first.nextCursor,'next+/=');
 const page=await searchResearch(first.query,fetcher,{cursor:first.nextCursor});const result=mergeResearch(first,page);
 assert.equal(urls[0].searchParams.get('cursor'),'*');assert.equal(urls[1].searchParams.get('cursor'),'next+/=');
 assert.equal(urls[0].searchParams.get('sort'),'score');assert.equal(urls[0].searchParams.get('order'),'desc');
 assert.equal(result.works.length,26);assert.equal(result.works[19].id,'S20');assert.equal(result.works[20].id,'S21');assert.equal(result.nextCursor,null);assert.equal(result.pages,2);
 assert.throws(()=>mergeResearch(first,{...page,query:'changed query'}),/检索词已变化/);
});
test('empty literature page finishes pagination without inventing records',async()=>{
 const res=await searchResearch('no studies',async()=>({ok:true,json:async()=>({message:{items:[],'total-results':0}})}));
 assert.deepEqual(res.works,[]);assert.equal(res.nextCursor,null);
});
test('all questionnaire entries open their page before importing data',async()=>{
 const desk=new PsychologyWorkspace(null,()=>{});
 for(const module of ['mediation','heterogeneity','network','dag','intervention','ml']){
  await desk.operation('enter',{module});assert.equal(desk.state.active,module);assert.equal(desk.state.stage,'analysis');assert.equal(desk.state.transition,null);
 }
});
test('opening a page with data requires human data confirmation before execution',async()=>{
 let runs=0;const data={status:'ready',revision:4,rows:200,results:{}};
 const worker={call:async name=>{if(name==='q_run'){runs++;return {state:data,summary:{n:200,revision:4}};}return data;}};
 const desk=new PsychologyWorkspace(worker,()=>{});desk.state.data=data;
 await desk.operation('enter',{module:'network'});assert.equal(desk.state.stage,'analysis');assert.equal(desk.state.transition.requiresAcceptance,true);
 await assert.rejects(desk.operation('run',{module:'network'}),/确认使用这份数据/);assert.equal(runs,0);
 await desk.operation('enter',{module:'network',accepted:true});await desk.operation('run',{module:'network'});assert.equal(runs,1);
});
test('adding data from a module page keeps the selected page and discards stale specifications',async()=>{
 const desk=new PsychologyWorkspace({call:async()=>({status:'ready',revision:1,rows:100,results:{}})},()=>{});
 await desk.operation('enter',{module:'heterogeneity'});desk.state.specs={network:{variables:['OLD']}};
 await desk.load('new.xlsx');assert.equal(desk.state.active,'heterogeneity');assert.equal(desk.state.stage,'analysis');assert.deepEqual(desk.state.specs,{});assert.equal(desk.state.transition.requiresAcceptance,true);
});
