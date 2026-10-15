// Exercise the canvas specification against the application's embedded engine, then export reports.
const assert=require('node:assert/strict'),fs=require('node:fs/promises'),path=require('node:path');
const {ComputeWorker}=require('../core.cjs'),model=require('../renderer/mediation-model.js');
const base=path.resolve(__dirname,'..'),resources=process.argv.includes('--bundled')?path.join(base,'dist/win-unpacked/resources'):base;
const worker=new ComputeWorker(path.join(resources,'runtime/R'),path.join(resources,'backend'));
(async()=>{
 await worker.ready;await worker.call('q_demo');const data=await worker.call('q_screen',{config:{rules:[{type:'range',columns:['Duration'],min:120}]}});
 const spec={paths:[],bootstrap:20};model.confirm(spec,['X','M','Y','W','Q2'],data.revision);
 assert.equal((await worker.call('q_prepare',{module:'mediation',spec})).status,'needs_input');
 for(const [from,to]of [['X','M'],['M','Y'],['X','Y']])model.addPath(spec,from,to);
 model.addInteraction(spec,'W','X','M');model.position(spec,'W',190,80);spec.indirectPaths=[['X','M','Y']];spec.conditionalIndirect=[{path:['X','M','Y'],moderator:'W',values:[-1,0,1]}];
 assert.equal((await worker.call('q_prepare',{module:'mediation',spec})).status,'ready');
 const result=await worker.call('q_run',{module:'mediation',spec});assert.equal(result.summary.n,237,'unused Q2 missingness must not reduce the analysis sample');assert.deepEqual(result.summary.specification,spec);
 const tab=result.summary.table,j=tab.find(r=>r.label==='j1'),b=tab.find(r=>r.label==='b2');assert.ok(j&&b);assert.match(j.rhs,/X × W/);
 const cond=tab.filter(r=>r.label?.startsWith('conditional'));assert.equal(cond.length,3);assert.ok(Math.abs((cond[2].est-cond[0].est)-2*j.est*b.est)<1e-8,'conditional effects must match the selected moderated edge');
 const cycle={...spec,interactions:[{x:'X',w:'Y',to:'M'}]};await assert.rejects(worker.call('q_prepare',{module:'mediation',spec:cycle}),/有环/);
 const duplicate={...spec,paths:spec.paths.concat(spec.paths[0])};await assert.rejects(worker.call('q_run',{module:'mediation',spec:duplicate}),/不能重复/);
 const unbound={...spec,interactions:[{x:'Y',w:'W',to:'M'}]};await assert.rejects(worker.call('q_prepare',{module:'mediation',spec:unbound}),/绑定/);
 const unmoderated={...spec,conditionalIndirect:[{path:['X','M','Y'],moderator:'Y'}]};await assert.rejects(worker.call('q_prepare',{module:'mediation',spec:unmoderated}),/已有的调节/);
 const figure=await worker.call('q_plot',{spec:{module:'mediation',type:'coefficients',title:'中介调节合成数据验证',dpi:300}});assert.ok(figure);
 const report=await worker.call('q_export',{directory:path.join(base,'test-artifacts','mediation-canvas-report')});for(const f of ['完整分析结果.xlsx','完整分析报告.docx'])assert.ok(report.files.includes(f));
 const proof={ok:true,bundled:process.argv.includes('--bundled'),n:result.summary.n,paths:spec.paths,moderators:spec.interactions,conditionalEffects:cond.length,layout:spec.planner.positions,report:report.directory};
 await fs.writeFile(path.join(base,'test-artifacts','mediation-canvas-verification.json'),JSON.stringify(proof,null,2));console.log(JSON.stringify(proof,null,2));
})().catch(e=>{console.error(e);process.exitCode=1;}).finally(()=>worker.stop());
