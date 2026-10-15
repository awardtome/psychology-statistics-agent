const fs=require('node:fs/promises'),path=require('node:path'),assert=require('node:assert/strict');
const {ComputeWorker}=require('../core.cjs');
const base=path.resolve(__dirname,'..'),root=path.join(base,'dist/win-unpacked/resources');
const worker=new ComputeWorker(path.join(root,'runtime/R'),path.join(root,'backend'));
(async()=>{
 await worker.ready;const directory=path.join(base,'test-artifacts/branched-tree-report');await fs.mkdir(directory,{recursive:true});
 let seed=917;const uniform=()=>{seed=(Math.imul(seed,1664525)+1013904223)>>>0;return (seed+1)/4294967297;};
 const normal=()=>Math.sqrt(-2*Math.log(uniform()))*Math.cos(2*Math.PI*uniform());
 const rows=['X,Y,Z,Group,ID,LongID,Q'];for(let i=0;i<400;i++){const x=normal(),y=(i<200?.92:-.92)*x+.25*normal();rows.push([x,y,normal(),i<200?'A':'B',String(i+1).padStart(4,'0'),'12345678901234567',i===0?'?':i%5+1].join(','));}
 const file=path.join(directory,'synthetic-tree.csv');await fs.writeFile(file,rows.join('\n'));
 const loaded=await worker.call('q_load',{file});
 assert.equal(loaded.preview[0].ID,'0001');assert.equal(loaded.preview[0].LongID,'12345678901234567');
 assert.equal(loaded.columns.find(x=>x.column==='X').type,'numeric');
 const screened=await worker.call('q_screen',{config:{missingCodes:[{columns:['Q'],values:['?']}],scales:[{name:'Average',items:['X','Y'],aggregation:'mean'}]}});
 assert.equal(screened.preview[0].Q,null);assert.equal(screened.columns.find(x=>x.column==='Q').type,'numeric');
 assert.ok(Math.abs(screened.preview[0].Average-(loaded.preview[0].X+loaded.preview[0].Y)/2)<1e-10);
 const result=await worker.call('q_run',{module:'heterogeneity',spec:{method:'networktree',variables:['X','Y','Z'],splitVariables:['Group'],minSize:80}});
 assert.ok(result.summary.groups.length>=2,'synthetic opposite associations should produce a split');
 const figure=await worker.call('q_plot',{spec:{module:'heterogeneity',type:'tree',width:9,height:6,title:'合成数据 网络树分裂示例'}});
 for(const file of Object.values(figure.figure.files))assert.ok((await fs.stat(file)).size>1000);
 const branch=await worker.call('q_branch',{mode:'class',class:result.summary.groups[0].groups});
 assert.equal(branch.rows,200);
 const next=await worker.call('q_run',{module:'network',spec:{method:'ggm',variables:['X','Y','Z'],level:'totals',bootstrap:0}});
 assert.equal(next.summary.n,200);await worker.call('q_export',{directory});
 console.log('Actual split tree, PNG/SVG/PDF and tree subgroup → network (N=200) passed.');
})().catch(e=>{console.error(e);process.exitCode=1}).finally(()=>worker.stop());
