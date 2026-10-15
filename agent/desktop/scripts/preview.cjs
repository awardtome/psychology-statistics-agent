// Development-only preview using the real bundled engine. Not included in the installer.
const http=require('node:http');const fs=require('node:fs/promises');const path=require('node:path');
const {ComputeWorker,validateSettings,agentTurn,modelRequest,testModelConnections}=require('../core.cjs');
const {modelContext}=require('../model-context.cjs');const {PsychologyWorkspace}=require('../psych-controller.cjs');const {searchResearch,mergeResearch,researchAdvice}=require('../research.cjs');
const {readRules}=require('../rule-files.cjs');
const {ScreenedLibrary}=require('../screened-library.cjs');const {ResearchAgent}=require('../research-agent.cjs');const {VaultStore}=require('../vault-store.cjs');
const desktop=path.resolve(__dirname,'..');const worker=new ComputeWorker(path.join(desktop,'runtime','R'),path.join(desktop,'backend'));
let state={module:'home',psych:null,research:null,researchDatasets:[],vault:null,rules:[],engine:'正在启动计算环境…',data:null,config:null,plan:null,result:null,image:null,sampleConfirmed:false,demo:false};
let settings={baseUrl:'https://api.openai.com/v1',protocol:'responses',model:'',shareData:false};let key='',checkerKey='',history=[];const clients=new Set();
const event=(channel,value)=>{for(const client of clients)client.write('data: '+JSON.stringify({channel,value})+'\n\n')};const update=()=>event('state',state);
const library=new ScreenedLibrary(path.join(desktop,'test-artifacts','preview-screened'),worker),vault=new VaultStore(path.join(desktop,'test-artifacts','preview-vault'));
const researchAgent=new ResearchAgent({library,vault,...(process.env.PSYCH_RESEARCH_FIXTURE==='1'?{fetcher:require('./research-fixture.cjs').fetcher}:{}),directory:path.join(desktop,'test-artifacts','preview-research'),onUpdate:r=>{state.research=r;update()}});
const psych=new PsychologyWorkspace(worker,value=>{state.psych=value;update()},async()=>{await library.capture();state.researchDatasets=library.list();update()});state.psych=psych.state;
async function prepare(config){state.config={...state.config,...config,study_is_independent_sample:state.sampleConfirmed};state.result=null;state.image=null;state.plan=await worker.call('plan',{config:state.config});update();return state.plan}
async function run(){const x=await worker.call('run');state.result=x.summary;state.image='data:image/png;base64,'+(await fs.readFile(x.image)).toString('base64');update();return x.summary}
async function tool(name,args){if(name==='search_research')return searchResearch(args.query);if(state.module==='questionnaire')return psych.tool(name,args);if(name==='inspect_data')return state.data;if(name==='prepare_analysis')return prepare(args.config);if(name==='run_meta_analysis')return run();if(name==='get_results')return state.result;throw new Error('Unknown tool')}
async function action(method,params={}){
 if(method==='state')return {...state,settings:{...settings,keyConfigured:!!key}};
 if(method==='module'){state.module=params.module;if(params.module==='questionnaire')await psych.operation('screenPage');history=[];update();return state}
 if(method==='researchStart')return researchAgent.start(params,settings,key,checkerKey);
 if(method==='researchCancel')return researchAgent.cancel();
 if(method==='researchData')return library.profile(params.datasetId);
 if(method==='researchMore')return researchAgent.more(params.queryId);
 if(method==='researchExport')return library.export(state.research.datasetId,state.research,path.join(desktop,'test-artifacts','research-export-'+Date.now()));
 if(method==='vaultImport')throw new Error('安装版支持选择本地 JSONL/ZST 文件导入。');
 if(method==='openLink')return true;
 if(method==='settings'){if(researchAgent.running)throw new Error('请先停止科研构思。');settings=validateSettings(params);key=params.apiKey||'';checkerKey=params.checker?.apiKey||'';history=[];return {...settings,keyConfigured:!!key}}
 if(method==='addRules'){if(!process.env.PSYCH_PREVIEW_RULE_FILE)throw new Error('开发预览未配置规则测试文件；安装版使用系统文件选择器。');const rule=await readRules(process.env.PSYCH_PREVIEW_RULE_FILE,worker);if(!state.rules.some(r=>r.id===rule.id))state.rules.push(rule);update();return rule;}
 if(method==='removeRules'){state.rules=state.rules.filter(r=>r.id!==params.id);update();return true;}
 if(method==='openFile')throw new Error('这是开发预览，请点击“体验合成示例”。桌面版使用系统文件选择器。');
 if(method==='testConnection'){const s=validateSettings(params);return testModelConnections({settings:s,key:params.apiKey||key,checkerKey:params.checker?.apiKey||checkerKey});}
 event('busy',true);try{
 if(state.module==='questionnaire'&&method==='demo')return await psych.operation('demo');
 if(method==='figure')return await psych.operation('plot',params);
 if(method==='psych'){const p=params.params||{};if(['export','script'].includes(params.action)){p.directory=path.join(desktop,'test-artifacts','psych-export-'+Date.now());p.ruleAttachments=state.rules}return await psych.operation(params.action,p)}
 if(method==='demo'){const d=await worker.call('inspect',{file:path.join(desktop,'backend','example-correlations.csv')});state={...state,data:d,config:{measure:'r',study:'studyID',effect:'effectID',value:'r',n:'n',dependence:'nested',direction:'',level:'auto',inference:'model',rho:.6,cont:[],groups:[],leave:false,bias:false},sampleConfirmed:false,demo:true,plan:null,result:null,image:null};history=[];update();return d}
 if(method==='config'){state.config=params.config;state.sampleConfirmed=params.sampleConfirmed===true;state.plan=null;state.result=null;state.image=null;await worker.call('invalidate');history=[];update();return true}
 if(method==='plan')return prepare(params.config||state.config);
 if(method==='run'){await prepare(state.config);return run()}
 if(method==='chat'){const result=await agentTurn({settings,key,checkerKey,text:params.text,history,context:state.module==='questionnaire'?{module:state.module,screeningRuleAttachments:state.rules,questionnaire:{...psych.context(),canvasDraft:params.planningDraft?.revision===psych.state.data?.revision?modelContext(params.planningDraft):undefined}}:{data:state.data,config:state.config,sampleConfirmed:state.sampleConfirmed,result:state.result,demo:state.demo},tool,event:e=>event('agentEvent',e)});history=result.history;return {answer:result.answer,review:result.review,reviewError:result.reviewError}}
 if(method==='export'){const directory=path.join(desktop,'test-artifacts','preview-export-'+Date.now());return worker.call('export',{directory})}
 throw new Error('Unknown preview action')
 }finally{event('busy',false)}
}
const bridge=`window.meta={invoke:async(method,params)=>{const response=await fetch('/rpc',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({method,params})});const value=await response.json();if(!response.ok)throw new Error(value.error);return value;},on:(channel,callback)=>window.addEventListener('meta:'+channel,e=>callback(e.detail))};const events=new EventSource('/events');events.onmessage=e=>{const x=JSON.parse(e.data);window.dispatchEvent(new CustomEvent('meta:'+x.channel,{detail:x.value}))};`;
const server=http.createServer(async(req,res)=>{
 try{
 if(req.url==='/events'){res.writeHead(200,{'Content-Type':'text/event-stream','Cache-Control':'no-cache','Connection':'keep-alive'});clients.add(res);req.on('close',()=>clients.delete(res));return}
 if(req.url==='/rpc'&&req.method==='POST'){let body='';for await(const part of req){body+=part;if(body.length>1000000)throw new Error('Request too large')};const x=JSON.parse(body);const result=await action(x.method,x.params);res.setHeader('Content-Type','application/json');res.end(JSON.stringify(result));return}
 if(req.url==='/bridge.js'){res.setHeader('Content-Type','text/javascript');res.end(bridge);return}
 const files={'/':'index.html','/style.css':'style.css','/psych.css':'psych.css','/app.js':'app.js','/panel-layout.js':'panel-layout.js','/psych-ui.js':'psych-ui.js','/research-studio.js':'research-studio.js','/analysis-pages.js':'analysis-pages.js','/method-studio.js':'method-studio.js','/mediation-model.js':'mediation-model.js','/mediation-planner.js':'mediation-planner.js','/intervention-plan.js':'intervention-plan.js'};const file=files[req.url];if(!file){res.writeHead(404);res.end();return}
 let bytes=await fs.readFile(path.join(desktop,'renderer',file));if(file==='index.html')bytes=bytes.toString('utf8').replace('<script src="app.js">','<script src="bridge.js"></script><script src="app.js">');res.setHeader('Content-Type',file.endsWith('.css')?'text/css':file.endsWith('.js')?'text/javascript':'text/html;charset=utf-8');res.end(bytes);
 }catch(e){res.writeHead(400,{'Content-Type':'application/json'});res.end(JSON.stringify({error:e.message}))}
});
Promise.all([worker.ready,library.init(),vault.init()]).then(()=>{state.researchDatasets=library.list();state.vault=vault.status();state.engine='本地计算已就绪';update()},e=>{state.engine=e.message;update()});server.listen(8146,'127.0.0.1',()=>console.log('Desktop UI preview at http://127.0.0.1:8146'));
process.on('SIGTERM',()=>{worker.stop();server.close();process.exit()});process.on('SIGINT',()=>{worker.stop();server.close();process.exit()});
