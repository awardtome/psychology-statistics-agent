// Optional local Svalbard JSONL corpus. Streaming import and FTS5/BM25; no embedding model required.
const fs=require('node:fs');const fsp=require('node:fs/promises');const path=require('node:path');const readline=require('node:readline');const zlib=require('node:zlib');
const {DatabaseSync}=require('node:sqlite');
class VaultStore {
 constructor(directory){this.directory=directory;this.importing=false;}
 async init(){await fsp.mkdir(this.directory,{recursive:true});this.db=new DatabaseSync(path.join(this.directory,'vault.sqlite'));this.db.exec('PRAGMA journal_mode=WAL; PRAGMA synchronous=NORMAL; CREATE TABLE IF NOT EXISTS papers(id TEXT PRIMARY KEY,record TEXT NOT NULL); CREATE VIRTUAL TABLE IF NOT EXISTS paper_index USING fts5(id UNINDEXED,title,problem,intuition,proposal);');this.count=this.db.prepare('SELECT count(*) AS n FROM papers').get().n;return this.status();}
 status(){return {count:this.count||0,importing:this.importing,source:'Svalbard Idea Vault',license:'CC BY 4.0',url:'https://huggingface.co/datasets/Jerry999/SvalbardIdeaVault',note:'可选本地语料；生成注释不是原文，不用于证明论文效果。'};}
 async import(files,progress=()=>{},signal){
  if(this.importing)throw new Error('已有语料正在导入。');this.importing=true;let imported=0,skipped=0,seen=0;const insert=this.db.prepare('INSERT OR IGNORE INTO papers(id,record) VALUES(?,?)'),index=this.db.prepare('INSERT INTO paper_index(id,title,problem,intuition,proposal) VALUES(?,?,?,?,?)');
  try{for(const file of files){if(!/\.jsonl(?:\.zst)?$/i.test(file))throw new Error('请选择 Svalbard 的 .jsonl 或 .jsonl.zst 分片。');let stream=fs.createReadStream(file);if(/\.zst$/i.test(file)){if(!zlib.createZstdDecompress)throw new Error('此运行环境不支持 zstd，请先解压为 JSONL。');const compressed=stream;stream=compressed.pipe(zlib.createZstdDecompress());compressed.on('error',e=>stream.destroy(e));}
   const lines=readline.createInterface({input:stream,crlfDelay:Infinity});try{for await(const line of lines){signal?.throwIfAborted();if(!line.trim())continue;if(line.length>2_000_000)throw new Error('语料中有过长记录，已保留先前导入数据。');let r;try{r=JSON.parse(line);}catch{throw new Error('语料 JSONL 格式不正确。');}
    if(!/^(?:\d{4}\.\d{4,5}|[a-z.-]+\/\d{7})(?:v\d+)?$/i.test(String(r.arxiv_id))||typeof r.title!=='string'||typeof r.proposal_masked!=='string'){skipped++;continue;}
    const record={arxiv_id:r.arxiv_id,title:r.title,authors:Array.isArray(r.authors)?r.authors:[],abstract:String(r.abstract||''),date:String(r.date||''),problem_definition:String(r.problem_definition||''),intuition:String(r.intuition||''),proposal_masked:r.proposal_masked,generated_at:r.generated_at,split:r.split};
    this.db.exec('BEGIN');try{const added=insert.run(r.arxiv_id,JSON.stringify(record)).changes;if(added)index.run(r.arxiv_id,record.title,record.problem_definition,record.intuition,record.proposal_masked);this.db.exec('COMMIT');if(added){imported++;this.count++;}}catch(e){this.db.exec('ROLLBACK');throw e;}
    if(++seen%250===0){progress({...this.status(),imported,skipped});await new Promise(resolve=>setImmediate(resolve));}
   }}finally{lines.close();stream.destroy();}}
   return {...this.status(),importing:false,imported,skipped};
  }finally{this.importing=false;progress({...this.status(),imported,skipped});}
 }
 search(query,offset=0){const terms=String(query).match(/[\p{L}\p{N}_-]+/gu)?.slice(0,48)||[];if(!terms.length)throw new Error('语料关键词不能为空。');const match=terms.map(x=>'"'+x.replace(/"/g,'""')+'"').join(' OR ');const total=this.db.prepare('SELECT count(*) AS n FROM paper_index WHERE paper_index MATCH ?').get(match).n;
  const hits=this.db.prepare('SELECT id FROM paper_index WHERE paper_index MATCH ? ORDER BY bm25(paper_index),id LIMIT 20 OFFSET ?').all(match,offset);
  return {total,records:hits.map(({id})=>{const r=JSON.parse(this.db.prepare('SELECT record FROM papers WHERE id=?').get(id).record);return {title:r.title,arxivId:r.arxiv_id,authors:r.authors.join(', '),year:+r.date.slice(0,4)||null,abstract:r.abstract,provider:'Svalbard 本地语料',url:'https://arxiv.org/abs/'+r.arxiv_id,vaultId:r.arxiv_id};}),next:offset+20<total?String(offset+20):null};
 }
 document(id){const row=this.db.prepare('SELECT record FROM papers WHERE id=?').get(id);if(!row)throw new Error('本地语料记录不存在。');const r=JSON.parse(row.record);return {level:'masked_annotation',text:'以下是数据集模型生成的结果遮蔽注释，不是论文全文。\n研究问题：'+r.problem_definition+'\n迁移直觉：'+r.intuition+'\n方法构想：'+r.proposal_masked,source:'https://huggingface.co/datasets/Jerry999/SvalbardIdeaVault',note:'仅结果遮蔽生成注释；需核对原论文，不能据此引用已测得结果。'};}
 close(){this.db?.close();}
}
module.exports={VaultStore};
