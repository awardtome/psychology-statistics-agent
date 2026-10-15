const test=require('node:test');const assert=require('node:assert/strict');const http=require('node:http');
const {validateSettings,agentTurn,reviewAnswer,testModelConnections}=require('../core.cjs');
test('checker inherits service but keeps its model; independent service is validated',()=>{
 const main={baseUrl:'https://example.com/v1',protocol:'chat',model:'worker',shareData:true};
 const s=validateSettings({...main,checker:{enabled:true,model:'reviewer',shared:true,apiKey:'never-return'}});
 assert.equal(s.checker.model,'reviewer');assert.equal(s.checker.baseUrl,main.baseUrl);assert.equal(s.checker.protocol,'chat');assert.equal(s.checker.apiKey,undefined);
 assert.throws(()=>validateSettings({...main,checker:{enabled:true,model:'reviewer',shared:false,baseUrl:'http://remote.test/v1',protocol:'chat'}}),/HTTPS/);
});
for(const protocol of ['chat','responses'])test(protocol+' checker receives actual tool outcomes and refreshed context without tool permissions',async t=>{
 const requests=[];let executed=0;const server=http.createServer(async(req,res)=>{
  let raw='';for await(const x of req)raw+=x;const body=JSON.parse(raw);requests.push({body,key:req.headers.authorization});res.setHeader('Content-Type','application/json');
  if(body.model==='reviewer'){
   res.end(JSON.stringify(protocol==='chat'?{choices:[{message:{role:'assistant',content:'需确认反向题。'}}]}:{output:[{type:'message',content:[{type:'output_text',text:'需确认反向题。'}]}]}));
  }else if(executed===0){res.end(JSON.stringify(protocol==='chat'?{choices:[{message:{role:'assistant',tool_calls:[{id:'a',function:{name:'get_results',arguments:'{}'}}]}}]}:{output:[{type:'function_call',call_id:'a',name:'get_results',arguments:'{}'}]}));}
  else res.end(JSON.stringify(protocol==='chat'?{choices:[{message:{role:'assistant',content:'实际样本 237。'}}]}:{output:[{type:'message',content:[{type:'output_text',text:'实际样本 237。'}]}]}));
 });await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));t.after(()=>server.close());
 const url=`http://127.0.0.1:${server.address().port}/v1`;
 const settings=validateSettings({baseUrl:url,model:'worker',protocol,shareData:true,checker:{enabled:true,model:'reviewer',shared:false,baseUrl:url,protocol}});
 const result=await agentTurn({settings,key:'worker-fixture',checkerKey:'checker-fixture',text:'分析',context:{revision:1},getContext:()=>({revision:2}),tool:async()=>{executed++;return {n:237};}});
 assert.equal(result.answer,'实际样本 237。');assert.equal(result.review,'需确认反向题。');assert.equal(executed,1);assert.equal(requests.at(-1).key,'Bearer checker-fixture');assert.equal(requests[0].key,'Bearer worker-fixture');
 const reviewRequest=requests.at(-1).body,reviewInput=JSON.parse(protocol==='responses'?reviewRequest.input:reviewRequest.messages[1].content);
 assert.equal(reviewRequest.tools,undefined);assert.equal(reviewInput.currentState.revision,2);assert.equal(reviewInput.toolRecords[0].result.n,237);
});
test('review failure is explicit and does not discard worker output',async t=>{
 const server=http.createServer(async(req,res)=>{let raw='';for await(const c of req)raw+=c;const b=JSON.parse(raw);res.setHeader('Content-Type','application/json');if(b.model==='reviewer'){res.writeHead(503);res.end('{}');}else res.end(JSON.stringify({choices:[{message:{role:'assistant',content:'已完成主力回复。'}}]}));});
 await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));t.after(()=>server.close());
 const settings=validateSettings({baseUrl:`http://127.0.0.1:${server.address().port}`,model:'worker',protocol:'chat',shareData:true,checker:{enabled:true,model:'reviewer'}});
 const result=await agentTurn({settings,key:'fixture',text:'检查',context:{},tool:async()=>assert.fail('checker cannot execute tools')});
 assert.equal(result.answer,'已完成主力回复。');assert.equal(result.review,null);assert.match(result.reviewError,/503/);assert.equal(result.history.length,2);
});

test('checker is off by default and disabled configuration never blocks the main model',()=>{
 const main={baseUrl:'https://example.com/v1',protocol:'chat',model:'worker',shareData:true};
 assert.equal(validateSettings(main).checker,undefined);
 const legacy=validateSettings({...main,checker:{model:'saved-reviewer',shared:false,baseUrl:'not an endpoint',protocol:'unknown',apiKey:'secret'}});
 assert.equal(legacy.checker.enabled,false);assert.equal(legacy.checker.model,'saved-reviewer');assert.equal(legacy.checker.apiKey,undefined);
 const disabled=validateSettings({...main,checker:{enabled:false,model:'',shared:false,baseUrl:'',protocol:'chat'}});
 assert.equal(disabled.checker.enabled,false);
 assert.throws(()=>validateSettings({...main,checker:{enabled:true,model:''}}),/关闭检测/);
});
for(const protocol of ['chat','responses'])test(protocol+' disabled checker sends only main requests for connection tests and chat',async t=>{
 const requests=[];
 const server=http.createServer(async(req,res)=>{let raw='';for await(const c of req)raw+=c;const b=JSON.parse(raw);requests.push(b.model);res.setHeader('Content-Type','application/json');res.end(JSON.stringify(protocol==='chat'?{choices:[{message:{role:'assistant',content:'主力回复。'}}]}:{output:[{type:'message',content:[{type:'output_text',text:'主力回复。'}]}]}));});
 await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));t.after(()=>server.close());
 const input={baseUrl:`http://127.0.0.1:${server.address().port}`,model:'worker',protocol,shareData:true,checker:{enabled:false,model:'saved-reviewer',shared:false,baseUrl:'invalid-unused-service',protocol:'chat'}};
 const disabled=validateSettings(input);
 assert.equal((await testModelConnections({settings:disabled})).message,'主力模型连接成功。');
 const events=[];
 const result=await agentTurn({settings:disabled,text:'检查',context:{},event:e=>events.push(e),tool:async()=>assert.fail('unexpected tool')});
 assert.equal(result.answer,'主力回复。');assert.equal(result.review,null);assert.equal(result.reviewError,null);
 assert.deepEqual(requests,['worker','worker']);assert.equal(events.some(e=>e.text?.includes('检测')),false);
 assert.equal(await reviewAnswer({settings:{shareData:false,checker:{model:'saved-reviewer',enabled:false}},event:()=>assert.fail('checker status while off')}),null);
 const enabled=validateSettings({...input,checker:{...input.checker,enabled:true,shared:true}});
 assert.equal((await testModelConnections({settings:enabled})).message,'主力与检测模型连接成功。');
 const reviewed=await agentTurn({settings:enabled,text:'检查',context:{},tool:async()=>assert.fail('unexpected tool')});
 assert.equal(reviewed.review,'主力回复。');
 assert.deepEqual(requests,['worker','worker','worker','saved-reviewer','worker','saved-reviewer']);
 // Keeping a saved model ID after switching off must not silently re-enable checking.
 const offAgain=validateSettings({...enabled,checker:{...enabled.checker,enabled:false}});
 assert.equal(offAgain.checker.model,'saved-reviewer');
 await testModelConnections({settings:offAgain});
 await agentTurn({settings:offAgain,text:'再检查',context:{},tool:async()=>assert.fail('unexpected tool')});
 assert.deepEqual(requests.slice(-2),['worker','worker']);
});
