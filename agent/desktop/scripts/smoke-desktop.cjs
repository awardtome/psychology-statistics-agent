const {spawn}=require('node:child_process');const fs=require('node:fs/promises');const path=require('node:path');
(async()=>{
 const desktop=path.resolve(__dirname,'..');await fs.mkdir(path.join(desktop,'test-artifacts'),{recursive:true});
 const output=path.join(desktop,'test-artifacts','desktop-smoke.json');
 await fs.rm(output,{force:true});
 const executable=path.join(desktop,'dist','win-unpacked','Psychology Statistics Agent.exe');
 const env={...process.env};delete env.ELECTRON_RUN_AS_NODE;
 const child=spawn(executable,['--smoke-test','--smoke-output',output],{windowsHide:true,env});let log='';
 child.stdout.on('data',b=>log+=b);child.stderr.on('data',b=>log+=b);
 const timer=setTimeout(()=>child.kill(),180000);
 const code=await new Promise((resolve,reject)=>{child.on('error',reject);child.on('exit',resolve)});clearTimeout(timer);
 await fs.writeFile(path.join(desktop,'test-artifacts','desktop-smoke.log'),log,'utf8');
 if(code!==0)throw new Error('Desktop exited with '+code+'\n'+log);
 const result=JSON.parse(await fs.readFile(output,'utf8'));if(!result.ok||!result.packaged||!result.isolatedProfile||result.version!==require('../package.json').version||result.result.effects!==36||result.psych?.rows!==237||result.psych?.results?.dag?.method!=='HC'||result.figure?.type!=='dag')throw new Error('Packaged desktop check failed');
 console.log(JSON.stringify({ok:true,packaged:result.packaged,studies:result.result.studies,effects:result.result.effects,estimate:result.result.overall[0].estimate_reported,questionnaireRows:result.psych.rows,dag:result.psych.results.dag.method,figure:result.figure.type},null,2));
})().catch(e=>{console.error(e);process.exitCode=1});
