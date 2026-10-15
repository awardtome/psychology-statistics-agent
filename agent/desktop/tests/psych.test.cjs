const {test}=require('node:test');const assert=require('node:assert/strict');const {PsychologyWorkspace}=require('../psych-controller.cjs');const {searchResearch}=require('../research.cjs');
test('model context omits image bytes and caps large result tables',()=>{
 const p=new PsychologyWorkspace(null,()=>{});p.state.images={network:'data:image/png;base64,PRIVATE_IMAGE'};p.state.data={results:{network:{image:'C:/private.png',table:Array.from({length:300},(_,i)=>({i}))}}};
 const ctx=p.context();assert.ok(!JSON.stringify(ctx).includes('PRIVATE_IMAGE'));assert.ok(!JSON.stringify(ctx).includes('private.png'));assert.equal(ctx.data.results.network.table.length,150);
 assert.deepEqual(ctx.contextTruncation,[{path:'$.data.results.network.table',total:300,shown:150}]);
});
test('AI specification cannot substitute for human IDA assumption confirmation',async()=>{
 let received;const worker={call:async(name,p)=>{if(name==='q_run'){received=p;return {summary:{},state:{},image:null}}return {}}};const desk=new PsychologyWorkspace(worker,()=>{});desk.state.specs.ida={exposure:'X',outcome:'Y',assumptionsConfirmed:true};await desk.operation('run',{module:'ida'});assert.equal(received.spec.assumptionsConfirmed,false);
});
test('Crossref returns only actual DOI records and labels missing abstracts',async()=>{
 const res=await searchResearch('psychology network',async()=>({ok:true,json:async()=>({message:{items:[{DOI:'10.1/example',title:['Actual title'],author:[]},{title:['Missing DOI']}]}})}));
 assert.equal(res.works.length,1);assert.equal(res.works[0].title,'Actual title');assert.match(res.works[0].evidence,/仅书目/);
});
