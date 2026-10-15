// A transparent local protocol fixture for integration tests; never shipped.
const http=require('node:http');
function mockServer(){return http.createServer(async(req,res)=>{try{let raw='';for await(const part of req)raw+=part;const body=JSON.parse(raw);const responses=req.url.endsWith('/responses');const items=responses?body.input:body.messages;
 if(typeof items==='string'||items?.at(-1)?.content==='Reply with OK.'){res.setHeader('Content-Type','application/json');res.end(JSON.stringify(responses?{output:[{type:'message',role:'assistant',content:[{type:'output_text',text:'OK'}]}]}:{choices:[{message:{role:'assistant',content:'OK'}}]}));return}
 const lastUser=items.findLastIndex(x=>x.role==='user');const recent=items.slice(lastUser+1);const toolResults=recent.filter(x=>x.type==='function_call_output'||x.role==='tool');const round=toolResults.length;
 const calls=[['inspect_data',{}],['prepare_analysis',{config:{measure:'r',study:'studyID',effect:'effectID',value:'r',n:'n',level:'auto',inference:'CR2',cont:['age'],groups:[{var:'design',ref:'横断面'}],leave:false,bias:false}}],['run_meta_analysis',{}],['get_results',{}]];
 let result;if(round<calls.length){const [name,args]=calls[round];result=responses?{output:[{id:'fc'+round,type:'function_call',call_id:'call'+round,name,arguments:JSON.stringify(args)}]}:{choices:[{message:{role:'assistant',content:null,tool_calls:[{id:'call'+round,type:'function',function:{name,arguments:JSON.stringify(args)}}]}}]}}
 else{const actual=JSON.parse(toolResults.at(-1).output||toolResults.at(-1).content);const row=actual.overall?.[0];const text=actual.error?'[本地模拟服务联调] 工具返回：'+actual.error:`[本地模拟服务联调]\n实际工具已计算：${actual.studies} 个独立样本、${actual.effects} 个效应。总体 r = ${Number(row.estimate_reported).toFixed(3)}，95% 区间 [${Number(row.ci_lb_reported).toFixed(3)}, ${Number(row.ci_ub_reported).toFixed(3)}]。\n这是合成示例；本条回复由测试服务返回，未连接真实语言模型。`;
 result=responses?{output:[{type:'message',role:'assistant',content:[{type:'output_text',text}]}]}:{choices:[{message:{role:'assistant',content:text}}]}}
 res.setHeader('Content-Type','application/json');res.end(JSON.stringify(result));
 }catch(e){res.writeHead(500,{'Content-Type':'application/json'});res.end(JSON.stringify({error:e.message}))}})}
if(require.main===module)mockServer().listen(9147,'127.0.0.1',()=>console.log('Local simulated provider at http://127.0.0.1:9147/v1'));
module.exports={mockServer};
