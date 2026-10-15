// Desktop integration test: launch the same bundled engine for an exported task.
// Dependencies are installed in an isolated test library, never the user's library.
const {spawn}=require('node:child_process');const fs=require('node:fs/promises');const path=require('node:path');
const base=path.resolve(__dirname,'..');
(async()=>{
 const artifacts=path.join(base,'test-artifacts');const library=path.join(artifacts,'external-R-library');await fs.mkdir(library,{recursive:true});
 const normalize=p=>p.replaceAll('\\','/');const r=s=>JSON.stringify(normalize(s));
 const nira=process.argv.includes('--nira');const blocked=process.argv.includes('--blocked');const method=nira?'nirapost':'ggmnira';const task=path.join(artifacts,'audit-'+method+(nira&&!blocked?'-pairwise':'')+'-task');
 if(nira&&!blocked){
  // Prespecified near-exact three-node pairwise Ising distribution; no three-way term.
  const states=Array.from({length:8},(_,i)=>[i&1,(i>>1)&1,(i>>2)&1]);
  const weights=states.map(([a,b,c])=>Math.exp(-.7*a-.2*b+.2*c+.8*a*b+.4*b*c));const total=weights.reduce((a,b)=>a+b,0);
  const csv='B1,B2,B3\n'+states.flatMap((row,i)=>Array(Math.round(2000*weights[i]/total)).fill(row.join(','))).join('\n');
  const file=path.join(artifacts,'pairwise-ising.csv');await fs.writeFile(file,csv);
  const {ComputeWorker}=require('../core.cjs');const worker=new ComputeWorker(path.join(base,'runtime/R'),path.join(base,'backend'));
  try{await worker.ready;await worker.call('q_load',{file});await worker.call('q_run',{module:'network',spec:{method:'ising',variables:['B1','B2','B3'],level:'items',bootstrap:0}});await worker.call('q_cs',{value:.5,metric:'expectedInfluence'});await worker.call('q_script',{method:'nirapost',directory:task,spec:{method:'nirapost',targets:['B1','B3'],outcomeScope:'selected_nodes',outcomes:['B2'],direction:'alleviating',intensity:1.5,repetitions:2}});}finally{worker.stop();}
 }
 await fs.copyFile(path.join(base,'backend/external-intervention.R'),path.join(task,method+'.R'));
 for(const name of ['reports.R','report-template.docx','external-simulation.R'])await fs.copyFile(path.join(base,'backend',name),path.join(task,name));
 const harness=path.join(artifacts,method+'-smoke.R');
 const execution=blocked?`err<-tryCatch({source(${r(path.join(task,method+'.R'))},encoding="UTF-8");NULL},error=function(e)conditionMessage(e));stopifnot(grepl("调节证据",err),file.exists(file.path(result_dir,"precondition-status.csv")))`:`source(${r(path.join(task,method+'.R'))},encoding="UTF-8")`;
 const checks=blocked?'':`tab<-read.csv(file.path(result_dir,"statistics.csv"));${nira?'stopifnot(setequal(tab$target,c("B1","B3")),nrow(tab)==2,all(is.finite(tab$effect)))':'stopifnot(nrow(tab)==1,tab$manipulated_node=="X",is.finite(tab$KL),tab$KL>0)'}`;
 await fs.writeFile(harness,`.libPaths(c(${r(library)},.libPaths()))\nSys.setenv(R_LIBS_USER=${r(library)})\n${execution}\nstopifnot(file.exists(file.path(result_dir,"完整分析结果.xlsx")),file.exists(file.path(result_dir,"完整分析报告.docx")))\n${checks}\ncat("EXTERNAL_TASK_OK\\n")\n`);
 const runtime=path.join(base,'runtime/R');const env={...process.env,R_HOME:runtime,R_LIBS:path.join(runtime,'library'),R_LIBS_USER:library,R_LIBS_SITE:path.join(runtime,'library')};
 for(const name of Object.keys(env))if(name==='LANG'||name==='LANGUAGE'||name.startsWith('LC_'))delete env[name];
 const child=spawn(path.join(runtime,'bin/x64/Rscript.exe'),['--vanilla','--encoding=UTF-8',harness],{windowsHide:true,cwd:base,env});
 let log='';child.stdout.on('data',b=>{log+=b;process.stdout.write(b)});child.stderr.on('data',b=>{log+=b;process.stderr.write(b)});
 const timeout=setTimeout(()=>child.kill(),10*60*1000);const code=await new Promise((resolve,reject)=>{child.on('error',reject);child.on('exit',resolve)});clearTimeout(timeout);
 await fs.writeFile(path.join(artifacts,method+(blocked?'-blocked':'')+'-smoke.log'),log);if(code!==0||!log.includes('EXTERNAL_TASK_OK'))throw new Error('External task failed: '+code);
})().catch(e=>{console.error(e);process.exitCode=1});
