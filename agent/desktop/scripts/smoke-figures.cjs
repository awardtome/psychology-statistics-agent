const assert=require('node:assert/strict');
const fs=require('node:fs/promises');
const path=require('node:path');
const {ComputeWorker}=require('../core.cjs');
const base=path.resolve(__dirname,'..');
const resources=process.argv.includes('--bundled')?path.join(base,'dist/win-unpacked/resources'):base;
const worker=new ComputeWorker(path.join(resources,'runtime/R'),path.join(resources,'backend'));
const output=path.join(base,'test-artifacts/figure-report');
(async()=>{
 await worker.ready;await worker.call('q_demo');await worker.call('q_screen',{skip:true});
 await assert.rejects(worker.call('q_plot',{spec:{module:'network'}}),/先完成/);
 const cases=[
  ['mediation',{paths:[{from:'X',to:'M'},{from:'M',to:'Y'},{from:'X',to:'Y'}],indirectPaths:[['X','M','Y']],bootstrap:0},'auto'],
  ['heterogeneity',{method:'lpa',variables:['X','M','Y'],maxClasses:2,classes:2},'auto'],
  ['heterogeneity',{method:'lca',variables:['B1','B2','B3','B4'],maxClasses:2,classes:2},'auto'],
  ['heterogeneity',{method:'networktree',variables:['X','M','Y'],splitVariables:['W'],minSize:40},'auto'],
  ['network',{method:'ggm',variables:['X','M','Y','W'],level:'totals',bootstrap:0},'network'],
  ['network',null,'centrality'],
  ['dag',{variables:['X','M','Y','W'],algorithm:'hc'},'auto'],
  ['ida',{exposure:'X',outcome:'Y',assumptionsConfirmed:true},'auto'],
  ['dag',{variables:['X','M','Y','W'],algorithm:'fci'},'auto'],
  ['ml',{target:'Y',features:['X','M','W'],task:'regression'},'auto'],
  ['network',{method:'ising',variables:['B1','B2','B3','B4','B5'],level:'items',bootstrap:0},'network']
 ];
 const proof=[];
 for(const [module,spec,type]of cases){
  if(spec)await worker.call('q_run',{module,spec});
  const out=await worker.call('q_plot',{spec:{module,type,title:'科研绘图 · '+module,labels:{X:'压力',M:'情绪调节',Y:'焦虑',W:'社会支持'},palette:module==='ida'?'mono':'accessible',width:7,height:5,dpi:300}});
  for(const [format,file]of Object.entries(out.figure.files))assert.ok((await fs.stat(file)).size>1000,format+' empty');
  assert.equal(out.figure.n,240);assert.ok(out.figure.table.length||out.figure.table.from!==undefined);
  proof.push({type:out.figure.type,module,warnings:out.figure.warnings});console.log(module+' '+out.figure.type+' PNG/SVG/PDF OK');
 }
 await assert.rejects(worker.call('q_plot',{spec:{module:'network',dpi:72}}),/dpi/);
 await assert.rejects(worker.call('q_plot',{spec:{module:'network',type:'coefficients'}}),/不匹配/);
 await worker.call('q_export',{directory:output});
 await worker.call('inspect',{file:path.join(resources,'backend/example-correlations.csv')});
 await worker.call('plan',{config:{measure:'r',study:'studyID',effect:'effectID',value:'r',n:'n',dependence:'nested',level:'auto',inference:'model',study_is_independent_sample:true}});
 await worker.call('run');
 const meta=await worker.call('plot',{spec:{module:'meta',title:'元分析森林图',width:8,height:10}});
 assert.equal(meta.figure.type,'forest');
 await worker.call('export',{directory:path.join(output,'meta')});
 await fs.writeFile(path.join(output,'figure-verification.json'),JSON.stringify({ok:true,proof,meta:meta.figure.type},null,2));
 console.log('All templates and report integration passed: '+output);
})().catch(e=>{console.error(e);process.exitCode=1}).finally(()=>worker.stop());
