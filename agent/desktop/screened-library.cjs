const fs=require('node:fs/promises');const path=require('node:path');const {randomUUID,createHash}=require('node:crypto');
const {modelContext}=require('./model-context.cjs');
const digest=buffer=>createHash('sha256').update(buffer).digest('hex');
const validId=id=>typeof id==='string'&&/^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$/.test(id);
class ScreenedLibrary {
 constructor(directory,worker){this.directory=path.resolve(directory);this.worker=worker;this.items=[];this.warning='';}
 async init(){
  await fs.mkdir(this.directory,{recursive:true});
  try{
   const items=JSON.parse(await fs.readFile(path.join(this.directory,'index.json'),'utf8'));
   if(!Array.isArray(items))throw new Error('数据索引格式不正确');
   this.items=items.filter(x=>validId(x.id)&&x.origin==='questionnaire_screening'&&/^[0-9a-f]{64}$/.test(x.sha256));
  }catch(e){if(e.code!=='ENOENT')this.warning='筛选后数据索引无法读取；未把任何原始文件当成备选数据。';}
  return this.list();
 }
 list(){return this.items.map(({id,name,created,rows,revision,excluded,columns,origin})=>({id,name,created,rows,revision,excluded,columns,origin}));}
 async capture(){
  const frozen=await this.worker.call('q_research_freeze');
  if(frozen.provenance?.origin!=='questionnaire_screening')throw new Error('只能保存筛选后按钮产生的数据。');
  const bytes=await fs.readFile(frozen.file),id=randomUUID(),p=frozen.provenance;
  const profile=await this.worker.call('q_research_profile',{file:frozen.file});
  if(profile.rows!==p.rows||profile.provenance?.origin!=='questionnaire_screening')throw new Error('筛选后快照与数据摘要不一致。');
  const profileBytes=Buffer.from(JSON.stringify(profile),'utf8');
  const item={id,origin:p.origin,name:p.name,created:new Date().toISOString(),rows:p.rows,revision:p.revision,excluded:p.excluded,
   columns:frozen.columns,sha256:digest(bytes),profileSha256:digest(profileBytes)};
  await fs.writeFile(path.join(this.directory,id+'.rds'),bytes,{flag:'wx'});
  await fs.writeFile(path.join(this.directory,id+'.profile.json'),profileBytes,{flag:'wx'});
  const next=[...this.items,item];
  const temp=path.join(this.directory,'index-'+randomUUID()+'.tmp');await fs.writeFile(temp,JSON.stringify(next,null,2),'utf8');await fs.rename(temp,path.join(this.directory,'index.json'));this.items=next;
  return this.list().find(x=>x.id===id);
 }
 async resolve(id){
  if(!validId(id))throw new Error('请选择已保存的筛选后数据，不接受文件路径或原始数据。');
  const item=this.items.find(x=>x.id===id);if(!item)throw new Error('所选筛选后数据不存在。');
  const file=path.join(this.directory,id+'.rds');let bytes;
  try{bytes=await fs.readFile(file)}catch{throw new Error('所选筛选后快照已丢失；不会改用其他数据。')}
  if(digest(bytes)!==item.sha256)throw new Error('所选筛选后快照已变化，请重新从筛选按钮保存；不会改用原表。');
  return {file,descriptor:this.list().find(x=>x.id===id)};
 }
 async profile(id,columns){const {file,descriptor}=await this.resolve(id);const item=this.items.find(x=>x.id===id);let profile;
  if(item.profileSha256){
   let bytes;try{bytes=await fs.readFile(path.join(this.directory,id+'.profile.json'))}catch{throw new Error('所选数据摘要已丢失，请重新应用筛选规则保存；不会改用其他数据。')}
   if(digest(bytes)!==item.profileSha256)throw new Error('所选数据摘要已变化，请重新保存筛选后快照。');profile=JSON.parse(bytes);
   if(columns!=null){if(!Array.isArray(columns)||columns.length>40||columns.some(c=>!profile.variables.some(v=>v.column===c)))throw new Error('请选择这份快照中至多40个变量。');if(columns.length)profile.variables=profile.variables.filter(v=>columns.includes(v.column));}
  }else profile=await this.worker.call('q_research_profile',{file,columns});
  if(profile.rows!==descriptor.rows||profile.provenance?.origin!=='questionnaire_screening')throw new Error('数据索引与所选快照不一致，请重新保存筛选后数据。');
  return modelContext({...profile,datasetId:id});
 }
 async correlations(id,columns,method){const {file}=await this.resolve(id);return {...await this.worker.call('q_research_correlations',{file,columns,method}),datasetId:id};}
 async export(id,research,directory){const {file}=await this.resolve(id);if(research.datasetId!==id)throw new Error('报告与所选筛选后数据不一致。');return this.worker.call('q_research_export',{file,research,directory});}
}
module.exports={ScreenedLibrary};
