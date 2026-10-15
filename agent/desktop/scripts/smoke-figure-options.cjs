const assert=require('node:assert/strict');
const fs=require('node:fs/promises');
const path=require('node:path');
const {ComputeWorker}=require('../core.cjs');
const root=path.resolve(__dirname,'../dist/win-unpacked/resources');
const worker=new ComputeWorker(path.join(root,'runtime/R'),path.join(root,'backend'));
(async()=>{
 await worker.ready;await worker.call('q_demo');
 await worker.call('q_run',{module:'network',spec:{method:'ggm',variables:['X','M','Y','W'],level:'totals',bootstrap:0}});
 const result=await worker.call('q_plot',{spec:{module:'network',type:'centrality',title:'600 DPI 黑白科研图',palette:'mono',width:7,height:5,dpi:600}});
 const png=await fs.readFile(result.image);
 assert.equal(png.readUInt32BE(16),4200);assert.equal(png.readUInt32BE(20),3000);
 assert.equal(result.figure.options.palette,'mono');
 await assert.rejects(worker.call('q_plot',{spec:{module:'network',type:'network',labels:{X:'很长的中文标签'.repeat(7)}}}),/标签过长|最多 60 字/);
 console.log('Packaged engine: 600 DPI = 4200 × 3000 pixels; monochrome template and crowded-label guard passed.');
})().catch(e=>{console.error(e);process.exitCode=1}).finally(()=>worker.stop());
