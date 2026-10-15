const {test}=require('node:test');const assert=require('node:assert/strict');const fs=require('node:fs/promises');const os=require('node:os');const path=require('node:path');
const {ResearchAgent}=require('../research-agent.cjs');const {LiteratureTools}=require('../literature-tools.cjs');const {ScreenedLibrary}=require('../screened-library.cjs');const {VaultStore}=require('../vault-store.cjs');
const id='11111111-1111-4111-8111-111111111111';const descriptor={id,name:'已筛选问卷',rows:237,revision:2,columns:[{column:'X'},{column:'Y'}]};
const settings={baseUrl:'https://example.org/v1',model:'main',protocol:'chat',shareData:true,checker:{enabled:false,model:'disabled'}};
const library={resolve:async selected=>{assert.equal(selected,id);return {descriptor}},profile:async selected=>{assert.equal(selected,id);return {datasetId:id,rows:237,variables:[{column:'X',mean:1},{column:'Y',mean:2}]}},correlations:async(selected)=>{assert.equal(selected,id);return {rows:237,table:[{from:'X',to:'Y',n:234,correlation:.1}]}}};
const fetcher=async url=>String(url).includes('api.crossref.org')?{ok:true,json:async()=>({message:{items:[{DOI:'10.1234/known',title:['Actual fixture source'],author:[{family:'Author'}]}],'total-results':1}})}:{ok:true,text:async()=>String(url).includes('fullTextXML')?'<article><body><p>FULLTEXT_PRIVATE_SENTINEL. A real method description.</p></body></article>':JSON.stringify({resultList:{result:[{doi:'10.1234/known',pmcid:'PMC123'}]}})};
function scriptedRequest(protocol,seen=[]){let seq=0;return async(s,k,body)=>{
 const system=body.instructions||body.messages[0].content,role=system.match(/当前角色：(\w+)/)[1],conversation=protocol==='responses'?body.input:body.messages.slice(1);seen.push({role,s,body,k});
 const outputs=conversation.filter(x=>x.role==='tool'||x.type==='function_call_output').map(x=>JSON.parse(x.content||x.output));
 const initial=JSON.parse(conversation[0].content),artifact={title:'基于筛选后数据的研究构思',content:'研究方案必须结合明确测量、实际样本与证据，区分描述统计和未经执行的分析计划。参考已核对证据 [S1]。',references:['S1'],dataBasis:{datasetId:id,n:237,columns:['X','Y']}};
 let name,args,text;
 if(role==='paper_reader')text=JSON.stringify({summary:'该材料描述方法，但尚不能证明适合当前研究。',answers:['方法描述已核对。'],limitations:['证据范围有限。']});
 else if(role==='orchestrator'){
  const stage=outputs.length;if(stage===0){name='save_plan';args={content:'先核对筛选后数据和关键词，再逐角色进行缺口、创新、审阅与报告。'};}
  else if(stage<5){name='delegate_task';args={role:['gap_finder','innovator','reviewer','report_writer'][stage-1],task:'核对实际数据和真实证据，写出自己的阶段产物。'};}
  else {name='finish';args={};}
 }else if(role==='report_writer'){name='save_artifact';args={artifact:{...artifact,method:'明确变量再分析',evaluation:'保留验证样本',predictions:['预测关联方向'],limitations:['设计尚待确认'],writing:'先报告测量与样本'}};}
 else if(outputs.length===0){name='keyword_search';args={query:initial.context.keywords,provider:'crossref',scope:role==='innovator'?'cross_domain':'same_domain'};}
 else if(outputs.length===1){name='read_paper';args={sourceId:'S1',goals:['这个方法有哪些限制？']};}
 else {name='save_artifact';args={artifact:{...artifact,...(role==='gap_finder'?{gaps:['测量层次需要核对']}:role==='innovator'?{mechanism:'领域外迁移机制',mapping:'机制与心理测量的结构对应',assumptions:['确认设计']}: {verdict:'survive',objections:['需要独立验证']})}};}
 if(text)return protocol==='responses'?{output:[{type:'message',content:[{type:'output_text',text}]}]}:{choices:[{message:{role:'assistant',content:text}}]};
 const cid='call'+(++seq);return protocol==='responses'?{output:[{type:'function_call',call_id:cid,name,arguments:JSON.stringify(args)}]}:{choices:[{message:{role:'assistant',content:null,tool_calls:[{id:cid,type:'function',function:{name,arguments:JSON.stringify(args)}}]}}]};
};}
for(const protocol of ['chat','responses'])test(protocol+' real role orchestration isolates full text and pins the screened dataset',async()=>{
 const seen=[],a=new ResearchAgent({library,request:scriptedRequest(protocol,seen),fetcher});const result=await a.start({datasetId:id,keywords:'emotion regulation'}, {...settings,protocol},'KEY');
 assert.equal(result.status,'complete',result.error);assert.equal(result.artifacts.length,5);assert.equal(result.rows,237);assert.equal(result.digests[0].level,'fulltext');assert.equal(result.steps.orchestrator,'已完成');
 assert.equal(seen.filter(x=>x.role==='paper_reader').length,1);for(const x of seen){const encoded=JSON.stringify(x.body);assert.equal(encoded.includes('FULLTEXT_PRIVATE_SENTINEL'),x.role==='paper_reader');assert.equal(x.s.model,'main');if(x.role==='orchestrator')assert.deepEqual((x.body.tools||[]).map(t=>t.name||t.function.name),['save_plan','read_artifacts','delegate_task','finish']);}
 assert.ok(result.toolAudit.every(x=>x.datasetId===id));assert.equal(JSON.stringify(result).includes('KEY'),false);
});
test('enabled optional checker is used only by the reviewer role',async()=>{
 const seen=[],a=new ResearchAgent({library,request:scriptedRequest('chat',seen),fetcher});const result=await a.start({datasetId:id,keywords:'emotion regulation'},{...settings,checker:{enabled:true,shared:false,baseUrl:'https://checker.org/v1',protocol:'chat',model:'checker'}},'MAIN_KEY','CHECK_KEY');assert.equal(result.status,'complete',result.error);for(const x of seen)assert.equal(x.s.model,x.role==='reviewer'?'checker':'main');assert.equal(a.key,'');assert.equal(a.checkerKey,'');
});
test('data and citation validation rejects arbitrary files, unselected columns and unread references',async()=>{
 const a=new ResearchAgent({library});a.state={datasetId:id,digests:[],toolAudit:[]};a.dataset=descriptor;a.attempt={gap_finder:1};a.controller=new AbortController();
 await assert.rejects(a.dispatch('gap_finder','inspect_selected_data',{datasetId:'raw',file:'raw.xlsx'}),/未授权字段/);
 await assert.rejects(a.dispatch('orchestrator','keyword_search',{query:'anything'}),/没有此工具权限/);
 const value={title:'t',content:'这是足够长的研究内容，需要引用已经核对的文献，并且仅能使用选定的筛选后数据，不得猜测变量。',references:['S99'],dataBasis:{datasetId:id,n:237,columns:['X']},gaps:['gap']};
 assert.throws(()=>a.validateArtifact('gap_finder',value),/经过独立阅读/);assert.throws(()=>a.validateArtifact('gap_finder',{...value,dataBasis:{datasetId:id,n:240,columns:['RAW']}}),/真实总行数/);
});
test('manual keywords and sharing scope are prerequisites, with no API or data access on failure',async()=>{
 let access=0,calls=0;const a=new ResearchAgent({library:{resolve:async()=>{access++;}},request:async()=>{calls++}});await assert.rejects(a.start({datasetId:id,keywords:'a'},settings,''),/手动填写/);await assert.rejects(a.start({datasetId:id,keywords:'emotion'},{...settings,shareData:false},''),/发送范围/);assert.equal(access,0);assert.equal(calls,0);
});
test('cancellation preserves artifacts and does not report a completed proposal',async()=>{
 const a=new ResearchAgent({library,request:async(s,k,b,signal)=>new Promise((resolve,reject)=>signal.addEventListener('abort',()=>reject(signal.reason),{once:true}))});const pending=a.start({datasetId:id,keywords:'emotion'},settings,'');await new Promise(r=>setImmediate(r));a.cancel();const result=await pending;assert.equal(result.status,'cancelled');assert.equal(result.steps.orchestrator,'已停止');
});
test('screened library persists versions, verifies hashes, never falls back to raw data',async()=>{
 const directory=await fs.mkdtemp(path.join(os.tmpdir(),'screened-library-'));const source=path.join(directory,'worker.rds');await fs.writeFile(source,'ONLY_SELECTED_RECORDS');
 const worker={call:async method=>method==='q_research_profile'?{rows:237,provenance:{origin:'questionnaire_screening'},variables:[{column:'X',mean:1},{column:'Y',mean:2}]}:({file:source,provenance:{origin:'questionnaire_screening',name:'筛选结果',rows:237,revision:2},columns:descriptor.columns})};const l=new ScreenedLibrary(directory,worker);await l.init();const first=await l.capture();const second=await l.capture();assert.notEqual(first.id,second.id);const restored=new ScreenedLibrary(directory,worker);assert.equal((await restored.init()).length,2);
 worker.call=async()=>{throw new Error('A long analysis occupies the compute worker')};const cached=await restored.profile(first.id,['X']);assert.equal(cached.rows,237);assert.equal(cached.variables.length,1);assert.equal(cached.variables[0].mean,1);
 await fs.writeFile(path.join(directory,first.id+'.profile.json'),'tampered');await assert.rejects(restored.profile(first.id),/数据摘要已变化/);
 await assert.rejects(restored.resolve('../raw.xlsx'),/不接受文件路径/);await fs.writeFile(path.join(directory,first.id+'.rds'),'changed');await assert.rejects(restored.resolve(first.id),/快照已变化/);await fs.unlink(path.join(directory,second.id+'.rds'));await assert.rejects(restored.resolve(second.id),/不会改用其他数据/);
});
test('stopping research immediately releases a pending data tool without stopping the statistics worker',async()=>{
 let finish,apiCalls=0;const a=new ResearchAgent({library:{...library,profile:()=>new Promise(r=>{finish=r})},request:async()=>{apiCalls++}});const pending=a.start({datasetId:id,keywords:'emotion'},settings,'');await new Promise(r=>setImmediate(r));a.cancel();const result=await pending;assert.equal(result.status,'cancelled');assert.equal(apiCalls,0);finish({rows:237});await new Promise(r=>setImmediate(r));assert.equal(a.state.status,'cancelled');assert.equal(a.running,false);
});
test('literature paging has stable IDs beyond the first twenty and no fabricated full text',async()=>{
 let count=0;const l=new LiteratureTools({fetcher:async url=>String(url).includes('crossref')?{ok:true,json:async()=>({message:{items:Array.from({length:count++?3:20},(_,i)=>({DOI:'10.1234/'+(count===1?i:i+20),title:['Paper']})),'total-results':23,'next-cursor':'next'}})}:{ok:false,status:404}});
 const first=await l.search({query:'emotion psychology'});const more=await l.search({query:'emotion psychology',more:true});assert.equal(l.works.length,23);assert.equal(more.works[0].id,'S21');assert.equal(more.nextPage,false);const doc=await l.document('S1');assert.equal(doc.level,'metadata');assert.match(doc.note,/仅书目/);await assert.rejects(l.document('S999'),/实际检索/);
});
test('Svalbard JSONL and ZST import provide real BM25 records and mark generated annotations',async()=>{
 const directory=await fs.mkdtemp(path.join(os.tmpdir(),'svalbard-')),file=path.join(directory,'part.jsonl');const line=JSON.stringify({arxiv_id:'2605.00737',title:'Psychological network mechanisms',authors:['Author'],date:'2026-05-01',abstract:'Methods',problem_definition:'emotion regulation',intuition:'graph mechanism',proposal_masked:'Generated masked proposal; not the paper.'})+'\n';await fs.writeFile(file,line);const compressed=path.join(directory,'part.jsonl.zst');await fs.writeFile(compressed,require('node:zlib').zstdCompressSync(line));const v=new VaultStore(directory);await v.init();try{await v.import([file,compressed]);assert.equal(v.status().count,1);assert.equal(v.search('emotion graph').records[0].arxivId,'2605.00737');assert.equal(v.document('2605.00737').level,'masked_annotation');assert.equal(v.search('unknown').total,0);}finally{v.close()}
});
module.exports={scriptedRequest,fetcher};
