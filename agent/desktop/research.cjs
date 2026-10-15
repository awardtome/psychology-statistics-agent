// Bibliographic retrieval is independent of the computation worker.
const strip=x=>String(x||'').replace(/<[^>]*>/g,' ').replace(/\s+/g,' ').trim();
async function searchResearch(query,fetcher=fetch,options={}){
  query=String(query||'').trim().slice(0,400);if(query.length<3)throw new Error('请先说明变量构念和研究方法，或输入检索词。');
  const rows=20;
  const url='https://api.crossref.org/works?rows='+rows+'&sort=score&order=desc&cursor='+encodeURIComponent(options.cursor||'*')+'&query.bibliographic='+encodeURIComponent(query);
  const response=await fetcher(url,{headers:{'User-Agent':'PsychologyStatisticsAgent/0.8.0 (bibliographic research)'},signal:options.signal?AbortSignal.any([options.signal,AbortSignal.timeout(30000)]):AbortSignal.timeout(30000)});
  if(!response.ok)throw new Error('文献服务返回 HTTP '+response.status+'。可稍后重试；不会生成替代文献。');
  const data=await response.json();
  const works=(data.message?.items||[]).filter(w=>w.DOI&&w.title?.[0]).map((w,i)=>({id:'S'+(i+1),title:strip(w.title[0]),authors:(w.author||[]).slice(0,8).map(a=>[a.given,a.family].filter(Boolean).join(' ')).join(', '),year:w['published']?.['date-parts']?.[0]?.[0]||w['published-print']?.['date-parts']?.[0]?.[0]||null,venue:strip(w['container-title']?.[0]),doi:w.DOI,url:'https://doi.org/'+encodeURIComponent(w.DOI),abstract:strip(w.abstract).slice(0,4500),evidence:w.abstract?'注册摘要与书目元数据':'仅书目元数据，需要阅读全文核实理论和发现'}));
  const items=data.message?.items||[];
  return {query,provider:'Crossref',retrieved:new Date().toISOString(),works,total:data.message?.['total-results']??null,received:items.length,pageSize:rows,pages:1,nextCursor:items.length===rows?(data.message?.['next-cursor']||null):null,note:'每次加载 20 条，可继续检索后续结果；注册记录总数不等于研究相关文献数。候选按相关性返回，无摘要时不能据标题推断结论。',advice:''};
}
function mergeResearch(previous,page){
  if(!previous)return page;
  if(previous.query!==page.query)throw new Error('检索词已变化，请重新检索后再加载更多。');
  const seen=new Set(previous.works.map(w=>w.doi.toLowerCase()));const works=[...previous.works];
  for(const w of page.works)if(!seen.has(w.doi.toLowerCase())){seen.add(w.doi.toLowerCase());works.push({...w,id:'S'+(works.length+1)})}
  return {...page,works,received:(previous.received||0)+page.received,pages:(previous.pages||1)+1,advice:''};
}
async function researchAdvice(settings,key,research,context,modelRequest){
  if(!settings.shareData)throw new Error('请先在模型设置中允许发送对话和分析摘要。');
  if(!research?.works?.length)throw new Error('请先检索真实文献。');
  const prompt='用中文给出写作、变量与理论建议。仅引用下面已检索的 S 编号和 DOI，不得编造文献、理论内容或研究发现。没有摘要的文献只能作为待阅读全文的候选。分别说明来源证据、当前实际数据/分析结果、推测与待验证内容；不能按显著性挑变量，不将横断面结果写成因果。提供引言逻辑、方法记录、结果报告和讨论限制。当前上下文：'+JSON.stringify(context)+'；文献：'+JSON.stringify(research);
  const result=await modelRequest(settings,key,settings.protocol==='responses'?{model:settings.model,input:prompt,store:false}:{model:settings.model,messages:[{role:'user',content:prompt}]});
  const answer=settings.protocol==='responses'?(result.output||[]).filter(x=>x.type==='message').flatMap(x=>(x.content||[]).filter(c=>c.type==='output_text').map(c=>c.text)).join('\n'):result.choices?.[0]?.message?.content;
  if(!answer)throw new Error('模型没有返回写作建议。');return String(answer);
}
module.exports={searchResearch,mergeResearch,researchAdvice};
