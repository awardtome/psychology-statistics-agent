const fs=require('node:fs/promises');const path=require('node:path');const {createHash}=require('node:crypto');
const MAX_TEXT=30000;
function decodeText(bytes){
  if(bytes[0]===0xff&&bytes[1]===0xfe)return new TextDecoder('utf-16le',{fatal:true}).decode(bytes.subarray(2));
  if(bytes[0]===0xfe&&bytes[1]===0xff)return new TextDecoder('utf-16be',{fatal:true}).decode(bytes.subarray(2));
  try{return new TextDecoder('utf-8',{fatal:true}).decode(bytes);}catch{return new TextDecoder('gb18030',{fatal:true}).decode(bytes);}
}
function attachment(name,text){
  const content=String(text).replace(/^\uFEFF/,'').trim();
  if(!content)throw new Error('筛选规则文件没有可读取的文字。');
  if(content.includes('\0'))throw new Error('文件含有二进制内容，请使用文字文件或 DOCX。');
  if(content.length>MAX_TEXT)throw new Error('筛选规则超过 30,000 字，请拆分或精简后添加；未截断内容。');
  return {id:createHash('sha256').update(name+'\0'+content).digest('hex').slice(0,24),name:path.basename(name),text:content,characters:content.length};
}
async function readRules(file,worker){
  const ext=path.extname(file).toLowerCase();
  if(!['.txt','.md','.json','.docx'].includes(ext))throw new Error('筛选规则支持 TXT、Markdown、JSON 或 DOCX。');
  if((await fs.stat(file)).size>2*1024*1024)throw new Error('筛选规则文件不能超过 2 MB。');
  const text=ext==='.docx'?(await worker.call('q_read_rules',{file})).text:decodeText(await fs.readFile(file));
  return attachment(file,text);
}
module.exports={readRules,attachment,decodeText,MAX_TEXT};
