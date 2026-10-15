const {app,BrowserWindow,ipcMain,dialog,safeStorage,session,shell}=require('electron');
const fs=require('node:fs/promises');const path=require('node:path');
const {ComputeWorker,validateSettings,agentTurn,modelRequest,testModelConnections,reviewAnswer}=require('./core.cjs');
const {readRules}=require('./rule-files.cjs');
const {PsychologyWorkspace}=require('./psych-controller.cjs');
const {searchResearch}=require('./research.cjs');
const {ScreenedLibrary}=require('./screened-library.cjs');const {ResearchAgent}=require('./research-agent.cjs');const {VaultStore}=require('./vault-store.cjs');
const {modelContext}=require('./model-context.cjs');
// Installer tests must never add synthetic snapshots to the user's persistent data library.
if(process.argv.includes('--smoke-test')){
 const index=process.argv.indexOf('--smoke-output');const output=index>=0?process.argv[index+1]:path.join(app.getPath('temp'),'meta-agent-smoke.json');
 const profile=path.join(path.dirname(path.resolve(output)),'smoke-profile-'+require('node:crypto').randomUUID());
 require('node:fs').mkdirSync(profile,{recursive:true});app.setPath('userData',profile);
}
let window,worker,psych,settings,key='',checkerKey='',history=[],busy=false,controller;
let library,researchAgent,vault,vaultController;
let state={module:'home',psych:null,research:null,researchDatasets:[],vault:null,rules:[],data:null,config:null,plan:null,result:null,image:null,sampleConfirmed:false,demo:false,engine:'正在启动计算环境…'};
const defaults={baseUrl:'https://api.openai.com/v1',protocol:'responses',model:'',shareData:false};
const publicSettings=()=>({...settings,keyConfigured:!!key,checker:settings.checker?{...settings.checker,keyConfigured:settings.checker.shared?!!key:!!checkerKey}:null});
const send=(channel,value)=>{if(window&&!window.isDestroyed())window.webContents.send(channel,value)};
const update=()=>send('state',state);
function defaultConfig(data){
 const names=data.columns.map(c=>c.column);const choose=(candidates)=>candidates.find(x=>names.includes(x))||names[0];
 return {measure:'r',study:choose(['studyID','样本编号']),effect:names.includes('effectID')?'effectID':null,value:choose(['r','相关系数']),n:choose(['n','N','样本量']),dependence:'nested',direction:'',level:'auto',inference:'model',rho:.6,cont:[],groups:[],leave:false,bias:false};
}
async function loadFile(file,sheet,demo=false){
 const inspection=await worker.call('inspect',{file,sheet});history=[];
 state={...state,file,data:inspection,config:inspection.status==='ready'?defaultConfig(inspection):null,plan:null,result:null,image:null,figure:null,figureImage:null,sampleConfirmed:false,demo};update();return inspection;
}
async function prepare(config){
 const allowed=['measure','study','effect','value','n','n1','n2','vi','se','direction','dependence','level','inference','rho','cont','groups','leave','bias','design'];
 const sanitized=Object.fromEntries(Object.entries(config||{}).filter(([k])=>allowed.includes(k)));
 state.config={...state.config,...sanitized,study_is_independent_sample:state.sampleConfirmed};
 state.result=null;state.image=null;state.figure=null;state.figureImage=null;state.plan=await worker.call('plan',{config:state.config});update();return state.plan;
}
async function run(){
 const out=await worker.call('run'); state.result=out.summary;
 state.image='data:image/png;base64,'+(await fs.readFile(out.image)).toString('base64');update();return out.summary;
}
async function useTool(name,args){
 if(name==='render_research_figure')return plotFigure(args.spec);
 if(name==='search_research')return modelContext(await searchResearch(args.query));
 if(state.module==='questionnaire')return psych.tool(name,args);
 if(name==='inspect_data')return state.data||{status:'no_data',message:'请通过界面选择编码表或加载示例。'};
 if(name==='prepare_analysis'){
   if(args.config?.study&&args.config.study!==state.config?.study)state.sampleConfirmed=false;
   return prepare({...args.config,dependence:state.config?.dependence||'nested'});
 }
 if(name==='run_meta_analysis')return run();
 if(name==='get_results')return state.result||{status:'not_run'};
 throw new Error('未知工具。');
}
async function plotFigure(spec){
 if(state.module==='questionnaire')return psych.operation('plot',{spec});
 if(state.module!=='meta')throw new Error('请先进入分析工作区。');
 const out=await worker.call('plot',{spec:{...spec,module:'meta'}});
 state.figure=modelContext(out.figure);state.figureImage='data:image/png;base64,'+(await fs.readFile(out.image)).toString('base64');update();return state.figure;
}
async function persistSettings(input){
 if(researchAgent?.running)throw new Error('请先停止科研构思，再修改模型服务。');
 const checked=validateSettings(input);const nextKey=input.clearKey?'':(input.apiKey||(checked.baseUrl===settings.baseUrl?key:''));
 const nextCheckerKey=checked.checker&&!checked.checker.shared&&!input.checker?.clearKey?(input.checker?.apiKey||(checked.checker.baseUrl===settings.checker?.baseUrl&&!settings.checker?.shared?checkerKey:'')):'';
 if((nextKey||nextCheckerKey)&&!safeStorage.isEncryptionAvailable())throw new Error('当前系统无法加密保存密钥。请检查 Windows 用户环境。');
 const stored={...checked,encryptedKey:nextKey?safeStorage.encryptString(nextKey).toString('base64'):null,encryptedCheckerKey:nextCheckerKey?safeStorage.encryptString(nextCheckerKey).toString('base64'):null};
 await fs.mkdir(app.getPath('userData'),{recursive:true});await fs.writeFile(path.join(app.getPath('userData'),'settings.json'),JSON.stringify(stored,null,2),'utf8');
 settings=checked;key=nextKey;checkerKey=nextCheckerKey;history=[];return publicSettings();
}
function chatContext(planningDraft){
 const context=state.module==='questionnaire'?{module:state.module,questionnaire:psych.context(),screeningRuleAttachments:state.rules}:{module:state.module,data:state.data,config:state.config,sampleConfirmed:state.sampleConfirmed,demo:state.demo,result:state.result};
 if(context.questionnaire&&psych.state.active==='mediation'&&planningDraft?.module==='mediation'&&planningDraft.revision===psych.state.data?.revision)context.questionnaire.canvasDraft=modelContext(planningDraft);
 return context;
}
async function operation(method,params={}){
 if(method==='state')return {...state,settings:publicSettings()};
 if(method==='cancel'){controller?.abort();return true}
 if(method==='openLink'){const url=String(params.url||'');if(!state.research?.works?.some(w=>w.url===url))throw new Error('只能打开本次检索返回的文献链接。');await shell.openExternal(url);return true}
 if(method==='researchStart')return researchAgent.start(params,settings,key,checkerKey);
 if(method==='researchCancel')return researchAgent.cancel();
 if(method==='researchData')return library.profile(params.datasetId);
 if(method==='researchMore')return researchAgent.more(params.queryId);
 if(method==='researchExport'){
   if(researchAgent.running||!state.research)throw new Error('请先完成或停止科研构思。');
   const report=structuredClone(state.research);
   const selected=await dialog.showOpenDialog(window,{title:'保存所选筛选后数据的科研构思完整报告',properties:['openDirectory','createDirectory']});if(selected.canceled)return null;
   return library.export(report.datasetId,report,path.join(selected.filePaths[0],'科研构思报告-'+new Date().toISOString().replace(/[:.]/g,'-')));
 }
 if(method==='vaultCancel'){vaultController?.abort();return true;}
 if(method==='vaultImport'){
   if(researchAgent.running||vault.importing)throw new Error('请先停止科研构思或当前导入。');
   const picked=await dialog.showOpenDialog(window,{title:'选择 Svalbard Idea Vault JSONL 分片（可多选）',properties:['openFile','multiSelections'],filters:[{name:'Svalbard 语料',extensions:['jsonl','zst']}]});if(picked.canceled)return null;
   vaultController=new AbortController();return vault.import(picked.filePaths,v=>{state.vault=v;update()},vaultController.signal);
 }
 if(busy)throw new Error('当前任务正在执行，请等待完成或停止对话。');
 if(method==='settings')return persistSettings(params);
 if(method==='testConnection'){
   const check=validateSettings({...settings,...params});const testKey=params.clearKey?'':(params.apiKey||(check.baseUrl===settings.baseUrl?key:''));
   const c=check.checker;const testCheckerKey=c&&!c.shared?(params.checker?.clearKey?'':params.checker?.apiKey||(c.baseUrl===settings.checker?.baseUrl&&!settings.checker?.shared?checkerKey:'')):'';
   return testModelConnections({settings:check,key:testKey,checkerKey:testCheckerKey});
 }
 busy=true;send('busy',true);
 try{
   if(['addRules','removeRules'].includes(method)){
     if(state.module!=='questionnaire')throw new Error('请进入问卷工作区后添加筛选规则。');
     if(method==='removeRules'){state.rules=state.rules.filter(r=>r.id!==params.id);history=[];update();return true;}
     if(state.rules.length>=5)throw new Error('最多添加五份筛选规则，请先移除不需要的附件。');
     const selected=await dialog.showOpenDialog(window,{title:'添加问卷筛选规则（不替换数据）',properties:['openFile'],filters:[{name:'筛选规则文字',extensions:['txt','md','json','docx']}]});if(selected.canceled)return null;
     const rule=await readRules(selected.filePaths[0],worker);if(!state.rules.some(r=>r.id===rule.id))state.rules=[...state.rules,rule];history=[];update();return rule;
   }
   if(method==='figure')return await plotFigure(params.spec);
   if(method==='module'){if(!['home','meta','questionnaire'].includes(params.module))throw new Error('未知入口。');state.module=params.module;if(params.module==='questionnaire')await psych.operation('screenPage');history=[];update();return state}
   if(state.module==='questionnaire'&&['demo','openFile','sheet'].includes(method)){
     history=[];
     if(method==='demo')return await psych.operation('demo');
     if(method==='sheet'){if(!state.psych?.file||!state.psych.data?.sheets?.includes(params.sheet))throw new Error('请选择实际工作表。');return await psych.load(state.psych.file,params.sheet)}
     const selection=await dialog.showOpenDialog(window,{title:'选择问卷原始数据',properties:['openFile'],filters:[{name:'数据表',extensions:['csv','tsv','xlsx','xls']}]});
     if(selection.canceled)return null;const file=selection.filePaths[0];if((await fs.stat(file)).size>30*1024*1024)throw new Error('请选择不超过 30 MB 的数据表。');return await psych.load(file);
   }
   if(method==='psych'){
     if(state.module!=='questionnaire')throw new Error('请先进入问卷工作区。');
     const action=params.action;const p=params.params||{};
     if(['export','script'].includes(action)){
       const selected=await dialog.showOpenDialog(window,{title:'选择完整报告保存位置',properties:['openDirectory','createDirectory']});if(selected.canceled)return null;
       p.directory=path.join(selected.filePaths[0],(action==='script'?'独立干预任务-':'心理学统计报告-')+new Date().toISOString().replace(/[:.]/g,'-'));if(action==='export')p.ruleAttachments=state.rules;
     }
     if(['import','importImage'].includes(action)){
       const selected=await dialog.showOpenDialog(window,{title:'导入外部干预结果',properties:['openFile'],filters:action==='import'?[{name:'实际结果表',extensions:['csv','xlsx','xls']}]:[{name:'实际结果图',extensions:['png','jpg','jpeg']}]});if(selected.canceled)return null;p.file=selected.filePaths[0];if((await fs.stat(p.file)).size>30*1024*1024)throw new Error('请选择不超过 30 MB 的结果。');
     }
     const result=await psych.operation(action,p);
     if(['screen','branch'].includes(action)){history=[];update()}
     return result;
   }
   if(method==='demo')return await loadFile(path.join(app.isPackaged?process.resourcesPath:__dirname,'backend','example-correlations.csv'),undefined,true);
   if(method==='openFile'){
     const selection=await dialog.showOpenDialog(window,{title:'选择元分析编码表',properties:['openFile'],filters:[{name:'编码表',extensions:['csv','tsv','xlsx','xls']}]});
     if(selection.canceled)return null;const stat=await fs.stat(selection.filePaths[0]);if(stat.size>30*1024*1024)throw new Error('请选择不超过 30 MB 的编码表。');return await loadFile(selection.filePaths[0]);
   }
   if(method==='sheet'){if(!state.file||state.data?.status!=='choose_sheet')throw new Error('当前没有等待选择工作表的文件。');if(![].concat(state.data.sheets).includes(params.sheet))throw new Error('工作表不存在。');return await loadFile(state.file,params.sheet)}
   if(method==='config'){
     state.sampleConfirmed=params.sampleConfirmed===true;state.config={...params.config,study_is_independent_sample:state.sampleConfirmed};state.plan=null;state.result=null;state.image=null;await worker.call('invalidate');history=[];update();return true;
   }
   if(method==='plan')return await prepare(params.config||state.config);
   if(method==='run'){await prepare(state.config);return await run()}
   if(method==='export'){
     if(!state.result)throw new Error('请先完成分析。');
     const selected=await dialog.showOpenDialog(window,{title:'选择结果保存位置',properties:['openDirectory','createDirectory']});if(selected.canceled)return null;
     const directory=path.join(selected.filePaths[0],'元分析结果-'+new Date().toISOString().replace(/[:.]/g,'-'));
     return await worker.call('export',{directory});
   }
   if(method==='chat'){
     if(!String(params.text||'').trim())throw new Error('请输入问题。');
     validateSettings(settings);controller=new AbortController();
     const context=chatContext(params.planningDraft);
     const answer=await agentTurn({settings,key,checkerKey,text:String(params.text).slice(0,20000),history,context,getContext:()=>{const fresh=chatContext();if(context.questionnaire?.canvasDraft)fresh.questionnaire.requestCanvasDraft=context.questionnaire.canvasDraft;return fresh;},tool:useTool,event:e=>send('agentEvent',e),signal:controller.signal});
     history=answer.history;return {answer:answer.answer,review:answer.review,reviewError:answer.reviewError};
   }
   throw new Error('未知界面操作。');
 }finally{busy=false;controller=null;send('busy',false)}
}
app.whenReady().then(async()=>{
 settings={...defaults};try{const saved=JSON.parse(await fs.readFile(path.join(app.getPath('userData'),'settings.json'),'utf8'));settings=saved.model?validateSettings({...defaults,...saved,shareData:saved.shareScopeVersion===2&&saved.shareData===true}):{...defaults};if(safeStorage.isEncryptionAvailable()){if(saved.encryptedKey)key=safeStorage.decryptString(Buffer.from(saved.encryptedKey,'base64'));if(saved.encryptedCheckerKey)checkerKey=safeStorage.decryptString(Buffer.from(saved.encryptedCheckerKey,'base64'))}}catch{}
 session.defaultSession.setPermissionRequestHandler((_w,_p,callback)=>callback(false));
 window=new BrowserWindow({width:1480,height:960,minWidth:1150,minHeight:720,title:'心理学统计agent',show:!process.argv.includes('--smoke-test'),autoHideMenuBar:true,backgroundColor:'#f5f6f7',webPreferences:{preload:path.join(__dirname,'preload.cjs'),contextIsolation:true,nodeIntegration:false,sandbox:true}});
 window.webContents.setWindowOpenHandler(()=>({action:'deny'}));window.webContents.on('will-navigate',e=>e.preventDefault());
 ipcMain.handle('meta:invoke',async(event,method,params)=>{if(event.sender!==window.webContents||event.senderFrame!==window.webContents.mainFrame)throw new Error('无效界面来源。');return operation(method,params)});
 await window.loadFile(path.join(__dirname,'renderer','index.html'));
 const resources=app.isPackaged?process.resourcesPath:__dirname;worker=new ComputeWorker(path.join(resources,'runtime','R'),path.join(resources,'backend'));
 library=new ScreenedLibrary(path.join(app.getPath('userData'),'screened-research'),worker);state.researchDatasets=await library.init();
 vault=new VaultStore(path.join(app.getPath('userData'),'idea-vault'));state.vault=await vault.init();
 researchAgent=new ResearchAgent({library,vault,directory:path.join(app.getPath('userData'),'research-runs'),onUpdate:value=>{state.research=value;update()}});await researchAgent.restore();
 psych=new PsychologyWorkspace(worker,value=>{state.psych=value;update()},async()=>{await library.capture();state.researchDatasets=library.list();update()});state.psych=psych.state;
 worker.on('failure',error=>{state.engine='计算环境异常：'+error.message;update()});
 try{await worker.ready;state.engine='本地计算已就绪';update();
   if(process.argv.includes('--smoke-test')){
     state.module='meta';
     await operation('demo');state.sampleConfirmed=true;
     const changed=await useTool('prepare_analysis',{config:{study:'effectID'}});if(changed.status!=='needs_input')throw new Error('AI sample-column confirmation guard failed');
     state.config=defaultConfig(state.data);state.sampleConfirmed=true;state.config.dependence='complex';
     const complex=await useTool('prepare_analysis',{config:{dependence:'nested'}});if(complex.status!=='needs_native_model')throw new Error('AI dependence guard failed');
     state.config.dependence='nested';await prepare(state.config);await run();
     await psych.operation('demo');await psych.operation('screen',{config:{id:'ID',scales:[{name:'ScaleQ',items:['Q1','Q2','Q3'],reverse:['Q2'],min:1,max:5,aggregation:'mean',minAnswered:2}],rules:[{type:'range',columns:['Duration'],min:120,label:'作答过快'}]}});await psych.operation('prepare',{module:'dag',spec:{variables:['X','M','Y','W']}});await psych.operation('run',{module:'dag'});
     const outIndex=process.argv.indexOf('--smoke-output');const target=outIndex>=0?process.argv[outIndex+1]:path.join(app.getPath('temp'),'meta-agent-smoke.json');
     await psych.operation('plot',{spec:{module:'dag',title:'安装验证 科研图',dpi:300}});
     const rulesDirectory=path.join(path.dirname(target),'smoke-rules-documents');await fs.mkdir(rulesDirectory,{recursive:true});const rulesFile=path.join(rulesDirectory,'筛选规则.txt');await fs.writeFile(rulesFile,'Q2 反向；Duration 小于 120 秒排除。只作为资料核对，执行情况以审计为准。','utf8');
     const rule=await readRules(rulesFile,worker),before=await worker.call('q_state');
     await psych.operation('export',{directory:rulesDirectory,ruleAttachments:[rule]});
     const doc=await readRules(path.join(rulesDirectory,'完整分析报告.docx'),worker);
     if(!doc.text.includes('Q2 反向')||JSON.stringify(await worker.call('q_state'))!==JSON.stringify(before))throw new Error('Rule attachment/document read changed the analysis workspace');
     const snapshot=library.list().at(-1);if(!snapshot||snapshot.rows!==237)throw new Error('Packaged screened snapshot capture failed');const pinned=await library.profile(snapshot.id);if(pinned.rows!==237||JSON.stringify(pinned).includes('excluded_records')||JSON.stringify(await worker.call('q_state'))!==JSON.stringify(before))throw new Error('Packaged research read-only snapshot failed');
     const researchDirectory=path.join(path.dirname(target),'smoke-research-report');const exported=await library.export(snapshot.id,{datasetId:snapshot.id,status:'complete',query:'合成安装验证',artifacts:[],works:[],advice:'安装验证科研方案，只使用所选筛选后数据。'},researchDirectory);if(!exported.files.includes('完整分析报告.docx'))throw new Error('Packaged research report export failed');
     const ui=await window.webContents.executeJavaScript("({resizeHandles:document.querySelectorAll('.panel-splitter').length,ruleMenu:!!document.getElementById('add-rules-file'),checkerInput:!!document.getElementById('checker-model'),checkerOptional:!!document.getElementById('checker-enabled'),checkerInitiallyOff:!document.getElementById('checker-enabled').checked,literatureHidden:document.getElementById('research-panel').hidden,researchDataset:!!document.getElementById('research-dataset'),manualKeywords:!!document.getElementById('research-query'),researchRoles:!!document.getElementById('research-roles'),screenedButton:!!document.getElementById('psy-screened-data')})");
     if(ui.resizeHandles!==2||!ui.ruleMenu||!ui.checkerInput||!ui.checkerOptional||!ui.checkerInitiallyOff||!ui.literatureHidden||!ui.researchDataset||!ui.manualKeywords||!ui.researchRoles||!ui.screenedButton)throw new Error('Packaged renderer feature check failed');
     await fs.writeFile(target,JSON.stringify({ok:true,version:app.getVersion(),packaged:app.isPackaged,isolatedProfile:app.getPath('userData'),ui,rules:{docxRead:true,unchanged:true},result:state.result,psych:psych.state.data,figure:psych.state.figure},null,2),'utf8');app.quit();
   }
 }catch(error){state.engine='启动失败：'+error.message;update();if(process.argv.includes('--smoke-test')){console.error(error);app.exit(1)}}
});
app.on('window-all-closed',()=>app.quit());app.on('before-quit',()=>{controller?.abort();researchAgent?.cancel();vaultController?.abort();worker?.stop();vault?.close()});
