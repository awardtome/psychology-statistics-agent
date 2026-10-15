const test=require('node:test');const assert=require('node:assert/strict');const fs=require('node:fs/promises');const os=require('node:os');const path=require('node:path');
const {attachment,decodeText,readRules}=require('../rule-files.cjs');
test('rules preserve literal Chinese text, reject binary and never silently truncate',()=>{
 const text='Q2 反向计分；作答时间小于 120 秒排除。';assert.equal(attachment('规则.txt',text).text,text);
 assert.equal(decodeText(Buffer.concat([Buffer.from([255,254]),Buffer.from(text,'utf16le')])),text);
 assert.throws(()=>attachment('规则.txt','a'.repeat(30001)),/未截断/);assert.throws(()=>attachment('x.txt','a\0b'),/二进制/);
});
test('a rule document is read independently without importing or screening a dataset',async t=>{
 const dir=await fs.mkdtemp(path.join(os.tmpdir(),'psych-rules-'));t.after(()=>fs.rm(dir,{recursive:true,force:true}));
 const file=path.join(dir,'规则.docx');await fs.writeFile(file,'fixture');const calls=[];
 const result=await readRules(file,{call:async(method,p)=>{calls.push({method,p});return {text:'Q2 反向；Q1 至 Q3 为某量表。'};}});
 assert.deepEqual(calls,[{method:'q_read_rules',p:{file}}]);assert.equal(result.name,'规则.docx');assert.match(result.text,/反向/);
 const txt=path.join(dir,'规则.txt');await fs.writeFile(txt,'保留缺失值；跳过重复检查。','utf8');assert.match((await readRules(txt)).text,/跳过/);
});
