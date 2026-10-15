const assert=require('node:assert/strict');const fs=require('node:fs/promises');const path=require('node:path');const {ComputeWorker}=require('../core.cjs');
const base=path.resolve(__dirname,'..');const resources=process.argv.includes('--bundled')?path.join(base,'dist/win-unpacked/resources'):base;const worker=new ComputeWorker(path.join(resources,'runtime/R'),path.join(resources,'backend'));
async function run(){
 await worker.ready;const initial=await worker.call('q_demo');assert.equal(initial.rows,240);
 const config={id:'ID',labels:{X:'Synthetic predictor',Y:'Synthetic outcome'},scales:[{name:'Qscore',items:['Q1','Q2','Q3'],reverse:['Q2'],min:1,max:5,aggregation:'mean',minAnswered:2}],rules:[{type:'range',columns:['Duration'],min:120,label:'Too fast'}]};
 const screened=await worker.call('q_screen',{config});assert.equal(screened.rows,237);assert.equal(screened.audit.excluded,3);
 const again=await worker.call('q_screen',{config});assert.deepEqual(screened.preview,again.preview,'reversals must not accumulate');
 const modules=[
 ['mediation',{paths:[{from:'X',to:'M'},{from:'M',to:'Y'},{from:'X',to:'Y'}],indirectPaths:[['X','M','Y']],interactions:[{x:'X',w:'W',to:'Y'}],bootstrap:50}],
 ['heterogeneity',{method:'lpa',variables:['X','M','Y'],maxClasses:2}],
 ['heterogeneity',{method:'lca',variables:['B1','B2','B3','B4'],maxClasses:2}],
 ['heterogeneity',{method:'networktree',variables:['X','M','Y'],splitVariables:['W'],minSize:40}],
 ['network',{method:'ggm',variables:['X','M','Y','W'],level:'totals',bootstrap:50,group:'Group',permutations:20}],
 ['dag',{variables:['X','M','Y','W'],algorithm:'hc'}],
 ['dag',{variables:['X','M','Y','W'],algorithm:'pc'}],
 ['dag',{variables:['X','M','Y','W'],algorithm:'ges'}],
 ['ida',{exposure:'X',outcome:'Y',assumptionsConfirmed:true}],
 ['dag',{variables:['X','M','Y','W'],algorithm:'fci'}],
 ['ml',{target:'Y',features:['X','M','W'],task:'regression'}],
 ['ml',{target:'Group',features:['X','M','W'],task:'classification'}],
 ['network',{method:'ising',variables:['B1','B2','B3','B4','B5'],level:'items',bootstrap:0}]
 ];
 const proof=[];let ggmCS;
 for(const [module,spec]of modules){const result=await worker.call('q_run',{module,spec});assert.equal(result.summary.n,237);if(result.summary.method==='GGM')ggmCS=result.summary.cs;assert.ok((await fs.stat(result.image)).size>1000);proof.push({module,method:result.summary.method,n:result.summary.n,warnings:result.summary.warnings});process.stdout.write(module+' '+result.summary.method+' OK\n');}
 await assert.rejects(worker.call('q_run',{module:'ida',spec:{exposure:'X',outcome:'Y',assumptionsConfirmed:true}}),/CPDAG/);
 await assert.rejects(worker.call('q_run',{module:'ml',spec:{target:'Qscore',features:['Q1','Q2'],task:'regression'}}),/泄漏/);
 const simulationSpec={method:'nirapost',targets:['B1'],outcomeScope:'whole_network',direction:'alleviating',intensity:2,repetitions:2};
 await assert.rejects(worker.call('q_script',{method:'nirapost',spec:simulationSpec,directory:path.join(base,'test-artifacts','blocked-script')}),/CS/);
 await worker.call('q_cs',{value:.5,metric:'expectedInfluence'});const script=await worker.call('q_script',{method:'nirapost',spec:simulationSpec,directory:path.join(base,'test-artifacts','nirapost-task')});assert.ok(script.files.includes('nirapost.R'));
 const out=await worker.call('q_export',{directory:path.join(base,'test-artifacts','psych-complete-report')});assert.ok(out.files.includes('完整分析结果.xlsx'));assert.ok(out.files.includes('完整分析报告.docx'));
 const audit=JSON.parse(await fs.readFile(path.join(out.directory,'analysis-audit.json'),'utf8'));for(const metric of Object.keys(ggmCS))assert.ok(Math.abs(audit.results.find(r=>r.method==='GGM').cs[metric]-ggmCS[metric])<.000051,'returned CS must not overwrite earlier networks; audit JSON uses four decimals');assert.equal(audit.results.find(r=>r.method==='GGM').cs_origin,undefined);assert.equal(audit.results.find(r=>r.method==='ISING').cs.expectedInfluence,.5);assert.match(audit.results.find(r=>r.method==='ISING').cs_origin,/未核实/);
 const branch=await worker.call('q_branch',{mode:'class',class:1}).catch(e=>({error:e.message}));proof.push({branch});
 await fs.writeFile(path.join(base,'test-artifacts','psych-verification.json'),JSON.stringify({ok:true,proof,report:out},null,2));
}
run().catch(e=>{console.error(e);process.exitCode=1}).finally(()=>worker.stop());
