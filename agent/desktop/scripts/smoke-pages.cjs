const assert=require('node:assert/strict');const fs=require('node:fs/promises');const path=require('node:path');
const {ComputeWorker}=require('../core.cjs');const {PsychologyWorkspace}=require('../psych-controller.cjs');
(async()=>{
 const desktop=path.resolve(__dirname,'..');const resources=path.join(desktop,'dist','win-unpacked','resources');
 const worker=new ComputeWorker(path.join(resources,'runtime','R'),path.join(resources,'backend'));
 try{
  await worker.ready;const desk=new PsychologyWorkspace(worker,()=>{});
  await desk.operation('demo');await desk.operation('screen',{config:{id:'ID',rules:[{type:'range',columns:['Duration'],min:120,label:'作答过快'}]}});
  assert.equal(desk.state.stage,'screen');assert.equal(desk.state.transition,null);assert.equal(desk.state.data.rows,237);
  const revision=desk.state.data.revision;
  await desk.operation('enter',{module:'network'});await assert.rejects(desk.operation('run',{module:'network'}),/确认/);
  await desk.operation('enter',{module:'network',accepted:true});await desk.operation('prepare',{module:'network',spec:{variables:['X','M','Y','W'],method:'ggm',level:'totals',bootstrap:0}});
  const net=await desk.operation('run',{module:'network'});assert.equal(net.n,237);assert.equal(net.revision,revision);
  await desk.operation('screenPage');assert.equal(desk.state.stage,'screen');assert.equal(desk.state.data.results.network.n,237);
  await desk.operation('enter',{module:'heterogeneity'});await desk.operation('prepare',{module:'heterogeneity',spec:{method:'networktree',variables:['X','M','Y']}});
  assert.ok(desk.state.reviews.heterogeneity.missing.includes('splitVariables'));
  await desk.operation('enter',{module:'heterogeneity',accepted:true});await desk.operation('prepare',{module:'heterogeneity',spec:{method:'networktree',variables:['X','M','Y'],splitVariables:['Group'],minSize:50}});
  const tree=await desk.operation('run',{module:'heterogeneity'});assert.equal(tree.n,237);assert.deepEqual([].concat(tree.splitVariables),['Group']);
  for(const module of ['mediation','heterogeneity','network','dag','intervention','ml']){await desk.operation('enter',{module});assert.equal(desk.state.stage,'analysis');assert.equal(desk.state.active,module);assert.equal(desk.state.data.revision,revision);assert.equal(desk.state.data.rows,237);}
  const directory=path.join(desktop,'test-artifacts','pages-report');await desk.operation('export',{directory});
  const result={ok:true,raw:240,screened:237,revision,network:net.method,tree:tree.method,pages:6,networkTreeRequiresSplit:true,report:directory};
  await fs.writeFile(path.join(desktop,'test-artifacts','pages-verification.json'),JSON.stringify(result,null,2));console.log(JSON.stringify(result));
 }finally{worker.stop()}
})().catch(e=>{console.error(e);process.exitCode=1});
