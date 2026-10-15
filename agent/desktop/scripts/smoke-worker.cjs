const path=require('node:path');const fs=require('node:fs/promises');const assert=require('node:assert/strict');
const {ComputeWorker}=require('../core.cjs');
(async()=>{
 const desktop=path.resolve(__dirname,'..');const worker=new ComputeWorker(path.join(desktop,'runtime','R'),path.join(desktop,'backend'));
 try{
   await worker.ready;const file=path.join(desktop,'backend','example-correlations.csv');const inspected=await worker.call('inspect',{file});assert.equal(inspected.rows,36);
   const config={measure:'r',study:'studyID',effect:'effectID',value:'r',n:'n',dependence:'nested',level:'auto',inference:'model',leave:false,bias:false,cont:['age'],groups:[{var:'design',ref:'横断面'}],study_is_independent_sample:false};
   const pending=await worker.call('plan',{config});assert.equal(pending.status,'needs_input');await assert.rejects(worker.call('run'),/确认/);
   config.study_is_independent_sample=true;const plan=await worker.call('plan',{config});assert.equal(plan.studies,18);
   const ordinary=await worker.call('run');assert.equal(ordinary.summary.effects,36);assert.ok(Math.abs(ordinary.summary.overall[0].estimate_reported-(-0.1690849))<1e-6);assert.equal(ordinary.summary.component_audit.length,2);
   config.inference='CR2';await worker.call('plan',{config});const robust=await worker.call('run');assert.equal(robust.summary.inference,'CR2');assert.equal(robust.summary.overall[0].estimate_reported,ordinary.summary.overall[0].estimate_reported);assert.ok(robust.summary.moderators['cont age'][0].df2<18);
   const output=path.join(desktop,'test-artifacts','worker-export');await worker.call('export',{directory:output});assert.ok((await fs.readFile(path.join(output,'meta-analysis-report.txt'),'utf8')).includes('CR2'));assert.ok((await fs.stat(path.join(output,'forest.png'))).size>1000);
   console.log(JSON.stringify({ok:true,rows:inspected.rows,studies:plan.studies,effects:robust.summary.effects,estimate:robust.summary.overall[0].estimate_reported,checks:['unconfirmed sample blocked','real model fit','CR2 estimate preservation','actual moderator F','exported report/code/forest/audit']},null,2));
 }finally{worker.stop()}
})().catch(e=>{console.error(e);process.exitCode=1});
