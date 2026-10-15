// Read rule files with the embedded document reader; assert dataset and report provenance.
const fs=require('node:fs/promises'),path=require('node:path'),assert=require('node:assert/strict');
const {ComputeWorker}=require('../core.cjs'),{readRules}=require('../rule-files.cjs');
const base=path.resolve(__dirname,'..'),resources=process.argv.includes('--bundled')?path.join(base,'dist/win-unpacked/resources'):base;
const worker=new ComputeWorker(path.join(resources,'runtime/R'),path.join(resources,'backend'));
(async()=>{
 await worker.ready;const dir=path.join(base,'test-artifacts','rule-file-smoke');await fs.mkdir(dir,{recursive:true});
 const data=await worker.call('q_demo'),before=await worker.call('q_state');
 const txt=path.join(dir,'问卷筛选规则.txt');await fs.writeFile(txt,'Q2 反向；Duration 小于 120 秒排除。\n这只是参考规则，尚未执行。','utf8');const rule=await readRules(txt,worker);
 assert.deepEqual(await worker.call('q_state'),before);
 const report=await worker.call('q_export',{directory:dir,ruleAttachments:[rule]});
 const document=await readRules(path.join(dir,'完整分析报告.docx'),worker);assert.match(document.text,/Q2 反向/);assert.match(document.text,/不代表所有规则已经执行/);
 const audit=JSON.parse(await fs.readFile(path.join(dir,'analysis-audit.json'),'utf8'));assert.equal(audit.ruleAttachments[0].id,rule.id);assert.equal(audit.revision,data.revision);assert.deepEqual(await worker.call('q_state'),before);
 const proof={ok:true,bundled:process.argv.includes('--bundled'),rows:data.rows,revision:data.revision,unchanged:true,docxRead:true,rule:rule.name,report:report.directory};
 await fs.writeFile(path.join(base,'test-artifacts','rule-file-verification.json'),JSON.stringify(proof,null,2));console.log(JSON.stringify(proof,null,2));
})().catch(e=>{console.error(e);process.exitCode=1;}).finally(()=>worker.stop());
