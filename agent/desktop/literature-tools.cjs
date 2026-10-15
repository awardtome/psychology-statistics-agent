// Real bibliographic records and open full text; no arbitrary URL/file tool is exposed to models.
const {searchResearch}=require('./research.cjs');
const decode=s=>String(s||'').replace(/&#x([0-9a-f]+);/gi,(_,n)=>String.fromCodePoint(Math.min(parseInt(n,16),0x10ffff))).replace(/&#(\d+);/g,(_,n)=>String.fromCodePoint(Math.min(+n,0x10ffff))).replace(/&(?:amp|lt|gt|quot|apos|nbsp);/g,x=>({'&amp;':'&','&lt;':'<','&gt;':'>','&quot;':'"','&apos;':"'",'&nbsp;':' '})[x]);
const plain=s=>decode(String(s||'').replace(/<(script|style)\b[^>]*>[\s\S]*?<\/\1>/gi,'').replace(/<!--[^]*?-->/g,'').replace(/<\/(?:p|div|section|title|sec|abstract|tr|h[1-6])>/gi,'\n').replace(/<[^>]+>/g,' ')).replace(/[ \t]+/g,' ').replace(/\n\s*\n/g,'\n').trim();
const tag=(s,name)=>decode((String(s).match(new RegExp('<'+name+'(?:\\s[^>]*)?>([\\s\\S]*?)</'+name+'>','i'))||[])[1]||'').trim();
const arxivId=s=>/^(?:\d{4}\.\d{4,5}|[a-z.-]+\/\d{7})(?:v\d+)?$/i.test(String(s));
const providers=['crossref','europepmc','arxiv','vault'];
class LiteratureTools {
 constructor({fetcher=fetch,signal,vault}={}){this.fetcher=fetcher;this.signal=signal;this.vault=vault;this.works=[];this.queries=[];this.documents=new Map();this.arxivLast=0;}
 async fetchText(url){
  const response=await this.fetcher(url,{signal:this.signal?AbortSignal.any([this.signal,AbortSignal.timeout(30000)]):AbortSignal.timeout(30000),headers:{'User-Agent':'PsychologyStatisticsAgent/0.8.0 (research evidence)'}});
  if(!response.ok)throw new Error('文献服务返回 HTTP '+response.status+'，没有生成替代文献。');
  const size=+response.headers?.get?.('content-length');if(size>8_000_000)throw new Error('文献正文过大，请选择其他开放版本。');
  const text=await response.text();if(text.length>8_000_000)throw new Error('文献正文过大。');return text;
 }
 register(records){const added=[];
  for(const w of records){if(!w.title||!w.url)continue;const key=w.doi?'doi:'+w.doi.toLowerCase():w.pmcid?'pmc:'+w.pmcid:w.arxivId?'arxiv:'+w.arxivId:w.url;
   let existing=this.works.find(x=>x.key===key);if(existing){for(const k of ['abstract','pmcid','arxivId'])if(!existing[k]&&w[k])existing[k]=w[k];added.push(existing);continue;}
   const item={...w,key,id:'S'+(this.works.length+1),evidence:w.abstract?'书目与摘要（全文未读）':'仅书目（全文未读）'};this.works.push(item);added.push(item);
  }return added;
 }
 async search({query,provider='crossref',more=false,scope='same_domain'}){
  query=String(query||'').trim().slice(0,400);if(query.length<3)throw new Error('检索关键词至少需要三个字符。');if(!providers.includes(provider))throw new Error('请选择支持的文献来源。');
  let entry=this.queries.find(x=>x.query===query&&x.provider===provider);
  if(entry&&!more)return {queryId:entry.id,works:this.works.filter(w=>entry.ids.includes(w.id)),nextPage:!!entry.cursor,cached:true};
  if(more&&(!entry||!entry.cursor))throw new Error('这条检索没有可继续加载的下一页。');
  const cursor=more?entry.cursor:null;let records,total,next;
  if(provider==='crossref'){
   const page=await searchResearch(query,this.fetcher,{cursor:cursor||undefined,signal:this.signal});records=page.works.map(w=>({...w,provider:'Crossref'}));total=page.total;next=page.nextCursor;
  }else if(provider==='europepmc'){
   const url='https://www.ebi.ac.uk/europepmc/webservices/rest/search?format=json&resultType=core&pageSize=20&cursorMark='+encodeURIComponent(cursor||'*')+'&query='+encodeURIComponent(query);
   const data=JSON.parse(await this.fetchText(url));const rows=data.resultList?.result||[];total=data.hitCount;
   records=rows.map(w=>({title:w.title,doi:w.doi||null,pmcid:w.pmcid||null,authors:w.authorString||'',year:+w.pubYear||null,venue:w.journalInfo?.journal?.title||'',abstract:plain(w.abstractText),provider:'Europe PMC',url:w.doi?'https://doi.org/'+encodeURIComponent(w.doi):w.pmcid?'https://europepmc.org/articles/'+encodeURIComponent(w.pmcid):'https://europepmc.org/article/'+encodeURIComponent(w.source)+'/'+encodeURIComponent(w.id)}));
   next=rows.length===20&&data.nextCursorMark&&data.nextCursorMark!==cursor?data.nextCursorMark:null;
  }else if(provider==='vault'){
   if(!this.vault?.status().count)throw new Error('还没有导入 Svalbard 本地语料；可以使用 Crossref、Europe PMC 或 arXiv。');
   const page=this.vault.search(query,+(cursor||0));records=page.records;total=page.total;next=page.next;
  }else{
   const delay=Math.max(0,3000-(Date.now()-this.arxivLast));if(delay){await require('node:timers/promises').setTimeout(delay,undefined,{signal:this.signal});}this.arxivLast=Date.now();
   const start=+(cursor||0);const xml=await this.fetchText('https://export.arxiv.org/api/query?start='+start+'&max_results=20&sortBy=relevance&search_query='+encodeURIComponent(query));
   const rows=[...xml.matchAll(/<entry\b[^>]*>([\s\S]*?)<\/entry>/g)].map(x=>x[1]);total=+(tag(xml,'opensearch:totalResults')||0);
   records=rows.map(w=>{const raw=tag(w,'id'),id=raw.split('/abs/')[1];return {title:plain(tag(w,'title')),arxivId:arxivId(id)?id:null,doi:tag(w,'arxiv:doi')||null,authors:[...w.matchAll(/<author\b[^>]*>([\s\S]*?)<\/author>/g)].map(x=>tag(x[1],'name')).join(', '),year:+tag(w,'published').slice(0,4)||null,venue:'arXiv 预印本',abstract:plain(tag(w,'summary')),provider:'arXiv',url:arxivId(id)?'https://arxiv.org/abs/'+id:''}});
   next=rows.length===20&&start+20<total?String(start+20):null;
  }
  const works=this.register(records);if(!entry){entry={id:'Q'+(this.queries.length+1),query,provider,scope,pages:0,ids:[]};this.queries.push(entry);}
  entry.pages++;entry.cursor=next;entry.total=total;entry.ids=[...new Set([...entry.ids,...works.map(w=>w.id)])];
  return {queryId:entry.id,query,provider,scope,works,nextPage:!!next,total,pages:entry.pages,note:'检索结果是候选证据；摘要/书目与真正读到的全文分别记录。可继续翻页，不限于前几篇。'};
 }
 async document(id){
  const w=this.works.find(x=>x.id===id);if(!w)throw new Error('只能阅读本次实际检索到的文献编号。');if(this.documents.has(id))return this.documents.get(id);
  let pmcid=w.pmcid,arxiv=w.arxivId,full='',source='',error='';
  try{
   if(!pmcid&&w.doi){const d=JSON.parse(await this.fetchText('https://www.ebi.ac.uk/europepmc/webservices/rest/search?format=json&resultType=core&pageSize=1&query='+encodeURIComponent('DOI:"'+w.doi.replace(/"/g,'')+'"')));const record=d.resultList?.result?.find(x=>x.doi?.toLowerCase()===w.doi.toLowerCase());pmcid=record?.pmcid;if(!w.abstract&&record?.abstractText)w.abstract=plain(record.abstractText);}
   if(pmcid&&/^PMC\d+$/.test(pmcid)){
    const xml=await this.fetchText('https://www.ebi.ac.uk/europepmc/webservices/rest/'+pmcid+'/fullTextXML');if(!/<article\b/i.test(xml)||!/<body\b/i.test(xml))throw new Error('未获取到开放论文正文。');full=plain(xml);source='https://europepmc.org/articles/'+pmcid;
   }else{
    if(!arxiv&&/^10\.48550\/arxiv\./i.test(w.doi||''))arxiv=w.doi.split(/arxiv\./i)[1];
    if(arxiv&&arxivId(arxiv)){const html=await this.fetchText('https://arxiv.org/html/'+arxiv);if(!/ltx_document|ltx_title_document/.test(html))throw new Error('此论文没有可用的 HTML 全文。');full=plain(html.replace(/^[\s\S]*?(?=<article\b)/i,''));source='https://arxiv.org/html/'+arxiv;}
   }
  }catch(e){if(this.signal?.aborted)throw e;error=e.message;}
  const annotation=!full&&w.vaultId?this.vault?.document(w.vaultId):null;
  const text=full||annotation?.text||w.abstract||('标题：'+w.title+'；没有可读摘要或全文。');
  const doc={id,level:full?'fulltext':annotation?'masked_annotation':w.abstract?'abstract':'metadata',text:text.slice(0,100000),characters:text.length,truncated:text.length>100000,source:source||annotation?.source||w.url,error,note:full?'实际获取的开放全文；有截断时不能声称读完全部正文。':annotation?annotation.note:w.abstract?'未获得全文，仅摘要可供核对。':'仅书目，不得推断论文方法与发现。'};
  w.evidence=full?(doc.truncated?'已获取开放全文（阅读输入有长度上限）':'已获取开放全文'):annotation?'仅结果遮蔽生成注释；原文未获得':w.abstract?'仅摘要；全文未获得':'仅书目；全文未获得';this.documents.set(id,doc);return doc;
 }
 public(){return {works:this.works.map(({key,...w})=>w),queries:this.queries.map(x=>({...x,nextPage:!!x.cursor,cursor:undefined}))};}
}
module.exports={LiteratureTools,plain,providers};
