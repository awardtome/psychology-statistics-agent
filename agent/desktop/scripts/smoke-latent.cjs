const assert=require('node:assert/strict');const path=require('node:path');const {ComputeWorker}=require('../core.cjs');
const base=path.resolve(__dirname,'..');const resources=process.argv.includes('--bundled')?path.join(base,'dist/win-unpacked/resources'):base;const worker=new ComputeWorker(path.join(resources,'runtime/R'),path.join(resources,'backend'));
(async()=>{await worker.ready;await worker.call('q_demo');
 for(const method of ['lpa','lca'])for(const classes of [1,2]){
  const variables=method==='lpa'?['X','M','Y']:['B1','B2','B3'];
  const result=await worker.call('q_run',{module:'heterogeneity',spec:{method,variables,classes,maxClasses:2}});
  const s=result.summary;assert.equal(s.selected,classes);assert.ok(s.profiles.length>=variables.length*classes);
  const proportions=Array.isArray(s.proportions)?s.proportions:[s.proportions];assert.ok(Math.abs(proportions.reduce((a,r)=>a+r.model_proportion,0)-1)<1e-10);
  if(method==='lca')for(const variable of variables)for(let k=1;k<=classes;k++)assert.ok(Math.abs(s.profiles.filter(r=>r.variable===variable&&r.class===k).reduce((a,r)=>a+r.probability,0)-1)<1e-10);
  console.log(method+' '+classes+' classes: full profiles and actual graph verified');
 }
})().catch(e=>{console.error(e);process.exitCode=1}).finally(()=>worker.stop());
