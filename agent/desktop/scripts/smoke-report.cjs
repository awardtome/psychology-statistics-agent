const path=require('node:path');const fs=require('node:fs/promises');const assert=require('node:assert/strict');const {ComputeWorker}=require('../core.cjs');
const base=path.resolve(__dirname,'..');const worker=new ComputeWorker(path.join(base,'runtime/R'),path.join(base,'backend'));
(async()=>{await worker.ready;await worker.call('q_demo');await worker.call('q_screen',{config:{id:'ID',rules:[{type:'range',columns:['Duration'],min:120,label:'作答时间不足'}]}});
await worker.call('q_run',{module:'mediation',spec:{paths:[{from:'X',to:'M'},{from:'M',to:'Y'},{from:'X',to:'Y'}],indirectPaths:[['X','M','Y']],bootstrap:0}});
const out=await worker.call('q_export',{directory:path.join(base,'test-artifacts','report-qa')});for(const name of ['完整分析结果.xlsx','完整分析报告.docx'])assert.ok((await fs.stat(path.join(out.directory,name))).size>5000);console.log(JSON.stringify(out));})().catch(e=>{console.error(e);process.exitCode=1}).finally(()=>worker.stop());
