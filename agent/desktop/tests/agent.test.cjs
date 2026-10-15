const test=require('node:test');const assert=require('node:assert/strict');const http=require('node:http');
const {validateSettings,agentTurn}=require('../core.cjs');
test('remote endpoints require HTTPS and cannot embed secrets',()=>{
 assert.throws(()=>validateSettings({baseUrl:'http://example.com/v1',model:'x',protocol:'chat'}),/HTTPS/);
 assert.throws(()=>validateSettings({baseUrl:'https://secret@example.com/v1',model:'x',protocol:'chat'}),/账号/);
 assert.equal(validateSettings({baseUrl:'http://127.0.0.1:11434/v1',model:'local',protocol:'chat'}).protocol,'chat');
});
test('AI cannot send data until the user chooses the sending scope',async()=>{
 await assert.rejects(agentTurn({settings:{shareData:false},text:'hello'}),/确认/);
});
for(const protocol of ['responses','chat'])test(protocol+' executes tool calls and feeds real results back to the model',async(t)=>{
 let round=0;let seenResult=false;const requests=[];
 const server=http.createServer(async(req,res)=>{let body='';for await(const chunk of req)body+=chunk;const input=JSON.parse(body);requests.push(input);res.setHeader('Content-Type','application/json');
   if(round++===0)res.end(JSON.stringify(protocol==='chat'?{choices:[{message:{role:'assistant',content:null,tool_calls:[{id:'call1',type:'function',function:{name:'get_results',arguments:'{}'}}]}}]}:{output:[{id:'fn1',type:'function_call',call_id:'call1',name:'get_results',arguments:'{}'}]}));
   else{seenResult=JSON.stringify(input).includes('0.314159');res.end(JSON.stringify(protocol==='chat'?{choices:[{message:{role:'assistant',content:'结果已读取。'}}]}:{output:[{type:'message',role:'assistant',content:[{type:'output_text',text:'结果已读取。'}]}]}))}
 });await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));t.after(()=>server.close());
 const result=await agentTurn({settings:{baseUrl:`http://127.0.0.1:${server.address().port}/v1`,model:'mock',protocol,shareData:true},key:'synthetic-test-key',text:'读取结果',context:{sampleConfirmed:true},tool:async(name)=>{assert.equal(name,'get_results');return {estimate:0.314159}}});
 assert.equal(result.answer,'结果已读取。');assert.equal(seenResult,true);assert.equal(requests.length,2);
 assert.equal(protocol==='responses'?requests[0].store:false,false);
});
test('unsupported tool names cannot run arbitrary commands',async(t)=>{
 const server=http.createServer((_q,r)=>{r.setHeader('Content-Type','application/json');r.end(JSON.stringify({choices:[{message:{role:'assistant',tool_calls:[{id:'x',function:{name:'run_shell',arguments:'{}'}}]}}]}))});await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));t.after(()=>server.close());let called=false;
 await assert.rejects(agentTurn({settings:{baseUrl:`http://127.0.0.1:${server.address().port}`,model:'mock',protocol:'chat',shareData:true},text:'go',context:{},tool:async()=>{called=true}}),/未授权/);assert.equal(called,false);
});
for(const protocol of ['responses','chat'])test(protocol+' exposes the embedded plotting tool and returns actual figure metadata',async(t)=>{
 let round=0;let sawFigure=false;let offered=false;
 const args={spec:{module:'network',type:'network',palette:'mono',dpi:600}};
 const server=http.createServer(async(req,res)=>{let bytes='';for await(const part of req)bytes+=part;const body=JSON.parse(bytes);
  offered=offered||body.tools.some(x=>(x.name||x.function?.name)==='render_research_figure');res.setHeader('Content-Type','application/json');
  if(round++===0){const call={name:'render_research_figure',arguments:JSON.stringify(args)};res.end(JSON.stringify(protocol==='responses'?{output:[{type:'function_call',call_id:'figure1',...call}]}:{choices:[{message:{role:'assistant',tool_calls:[{id:'figure1',type:'function',function:call}]}}]}));}
  else{sawFigure=JSON.stringify(body).includes('actual-figure-17');res.end(JSON.stringify(protocol==='responses'?{output:[{type:'message',role:'assistant',content:[{type:'output_text',text:'科研图已生成。'}]}]}:{choices:[{message:{role:'assistant',content:'科研图已生成。'}}]}));}
 });await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));t.after(()=>server.close());
 const answer=await agentTurn({settings:{baseUrl:`http://127.0.0.1:${server.address().port}`,model:'fixture',protocol,shareData:true},text:'用内嵌工具画网络',context:{module:'questionnaire'},tool:async(name,input)=>{assert.equal(name,'render_research_figure');assert.deepEqual(input,args);return {id:'actual-figure-17',dpi:600};}});
 assert.equal(answer.answer,'科研图已生成。');assert.equal(offered,true);assert.equal(sawFigure,true);
});
