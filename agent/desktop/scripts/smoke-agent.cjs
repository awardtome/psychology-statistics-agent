const path=require('node:path');const assert=require('node:assert/strict');const {ComputeWorker,agentTurn}=require('../core.cjs');const {mockServer}=require('./mock-model.cjs');
(async()=>{
 const desktop=path.resolve(__dirname,'..');const worker=new ComputeWorker(path.join(desktop,'runtime','R'),path.join(desktop,'backend'));const server=mockServer();await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
 try{await worker.ready;for(const protocol of ['responses','chat']){
   const data=await worker.call('inspect',{file:path.join(desktop,'backend','example-correlations.csv')});const events=[];let actual;
   const tool=async(name,args)=>{if(name==='inspect_data')return data;if(name==='prepare_analysis')return worker.call('plan',{config:{...args.config,study_is_independent_sample:true,dependence:'nested'}});if(name==='run_meta_analysis'){actual=(await worker.call('run')).summary;return actual}if(name==='get_results')return actual;throw new Error('Unknown tool')};
   const response=await agentTurn({settings:{baseUrl:`http://127.0.0.1:${server.address().port}/v1`,protocol,model:'local-fixture',shareData:true},text:'分析合成示例',context:{sampleConfirmed:true,demo:true},tool,event:e=>{if(e.type==='tool')events.push(e.name)}});
   assert.deepEqual(events,['inspect_data','prepare_analysis','run_meta_analysis','get_results']);assert.equal(actual.studies,18);assert.equal(actual.effects,36);assert.equal(actual.inference,'CR2');assert.ok(response.answer.includes('-0.169'));
   console.log(protocol+': real engine/tool/result/response cycle passed (local simulated model service).');
 }}finally{worker.stop();server.close()}
})().catch(e=>{console.error(e);process.exitCode=1});
