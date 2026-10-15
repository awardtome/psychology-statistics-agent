const {spawn,execFileSync}=require('node:child_process');const path=require('node:path');const fs=require('node:fs/promises');
async function execute(file,args){const env={...process.env};delete env.ELECTRON_RUN_AS_NODE;const child=spawn(file,args,{windowsHide:true,env});let log='';child.stdout.on('data',b=>log+=b);child.stderr.on('data',b=>log+=b);const timer=setTimeout(()=>child.kill(),600000);const code=await new Promise((resolve,reject)=>{child.on('error',reject);child.on('exit',resolve)});clearTimeout(timer);if(code!==0)throw new Error(file+' exited with '+code+'\n'+log);return log}
(async()=>{const desktop=path.resolve(__dirname,'..');const version=require('../package.json').version;const delivery=path.resolve(desktop,'../../../PsychologyStatisticsAgent-'+version+'-standalone');const directory=path.join(delivery,'安装验证_中文_最终');
 const environment=JSON.parse(execFileSync('powershell.exe',['-NoProfile','-Command',`[Console]::OutputEncoding=[System.Text.UTF8Encoding]::new(); $installedApps=Get-ItemProperty -Path 'HKCU:\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\*','HKLM:\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\*','HKLM:\\Software\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\*' -ErrorAction SilentlyContinue | Where-Object { $_.DisplayName -eq 'Psychology Statistics Agent' -or $_.DisplayName -eq '心理学统计agent' } | Select-Object InstallLocation; [pscustomobject]@{desktop=[Environment]::GetFolderPath('Desktop');menu=[Environment]::GetFolderPath('StartMenu');installed=@($installedApps)} | ConvertTo-Json -Compress`],{encoding:'utf8',windowsHide:true}));
 if(environment.installed.length&&!process.argv.includes('--resume-owned-test'))throw new Error('A user installation already exists. Refusing to replace it during a smoke test.');
 if(environment.installed.some(i=>!i.InstallLocation||path.resolve(i.InstallLocation)!==directory))throw new Error('Existing installation does not belong to this test.');
 try{await fs.access(directory);
  if(!process.argv.includes('--resume-owned-test'))throw new Error('Test installation directory already exists. Refusing to overwrite.');
  const owned=JSON.parse(await fs.readFile(path.join(desktop,'test-artifacts','owned-installation.json'),'utf8'));if(path.resolve(owned.directory)!==directory)throw new Error('Owned interrupted test path mismatch');
  const entries=await fs.readdir(directory,{recursive:true,withFileTypes:true});for(const entry of entries){if(entry.isSymbolicLink())throw new Error('Unexpected link in interrupted test installation');if(entry.isFile()){const relative=path.relative(directory,path.join(entry.parentPath,entry.name)).split(path.sep).join('/');if(!owned.files.includes(relative))throw new Error('Unexpected file in interrupted test installation: '+relative)}}
 }catch(e){if(e.code!=='ENOENT')throw e}
 const installer=path.join(delivery,'Psychology-Statistics-Agent-'+version+'-Setup.exe');const output=path.join(delivery,'installer-verification.json');
 const shortcuts=[path.join(environment.desktop,'心理学统计agent.lnk'),path.join(environment.menu,'Programs','心理学统计agent.lnk')];
 const backups=await Promise.all(shortcuts.map(file=>fs.readFile(file).catch(e=>{if(e.code==='ENOENT')return null;throw e})));
 const backupDirectory=path.join(desktop,'test-artifacts','installer-shortcuts-'+Date.now());await fs.mkdir(backupDirectory,{recursive:true});
 for(let i=0;i<backups.length;i++)if(backups[i])await fs.writeFile(path.join(backupDirectory,i+'.lnk'),backups[i]);
 await fs.writeFile(path.join(backupDirectory,'manifest.json'),JSON.stringify(shortcuts));
 try{
 console.log('Installing into the new Unicode test directory...');await execute(installer,['/S','/D='+directory]);
 console.log('Installation completed; running the installed application...');
 await fs.rm(output,{force:true});
 await execute(path.join(directory,'Psychology Statistics Agent.exe'),['--smoke-test','--smoke-output',output]);
 const result=JSON.parse(await fs.readFile(output,'utf8'));if(!result.ok||!result.packaged||!result.isolatedProfile||result.version!==version||result.result.studies!==18||result.psych?.rows!==237||result.psych?.results?.dag?.method!=='HC'||result.figure?.type!=='dag')throw new Error('Installed application verification failed');
 console.log(JSON.stringify({ok:true,installed:true,unicodePath:true,effects:result.result.effects,studies:result.result.studies,estimate:result.result.overall[0].estimate_reported,questionnaireRows:result.psych.rows,dag:result.psych.results.dag.method,figure:result.figure.type},null,2));
 // Uninstall only this freshly created test installation; its resolved path is fixed within delivery.
 const resolved=path.resolve(directory);if(!resolved.startsWith(path.resolve(delivery)+path.sep))throw new Error('Unsafe uninstall target');
 // Run in place, so waiting on the process also waits on the actual uninstaller.
 const uninstaller=path.join(directory,'Uninstall Psychology Statistics Agent.exe');
 await execute(uninstaller,['/S','_?='+directory]);
 // Windows cannot unlink a running executable. Remove only the known test uninstaller after exit.
 const remaining=await fs.readdir(directory).catch(e=>{if(e.code==='ENOENT')return [];throw e});
 if(remaining.length){if(remaining.length!==1||remaining[0]!=='Uninstall Psychology Statistics Agent.exe')throw new Error('Unexpected files remain in the owned test installation.');await fs.unlink(uninstaller);await fs.rmdir(directory);}
 console.log('Temporary installation removed after verification.');
 }finally{
  for(let i=0;i<backups.length;i++)if(backups[i]){await fs.mkdir(path.dirname(shortcuts[i]),{recursive:true});await fs.writeFile(shortcuts[i],backups[i]);}
 }
})().catch(e=>{console.error(e);process.exitCode=1});
