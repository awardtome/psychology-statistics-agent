const {test}=require('node:test');const assert=require('node:assert/strict');
const model=require('../renderer/mediation-model.js');
const data={revision:7,columns:['X','M','Y','W','Spare'].map(column=>({column}))};
function plan(){const p={paths:[],bootstrap:0};model.confirm(p,['X','M','Y','W','Spare'],7);return p;}
test('variables must be confirmed for the current data version, without guessed paths',()=>{
 const p=plan();assert.equal(model.confirmed(p,data),true);assert.deepEqual(p.paths,[]);assert.equal(model.confirmed(p,{...data,revision:8}),false);
 assert.throws(()=>model.confirm({},['X'],7),/至少两个/);assert.match(model.problems(p,data.columns.map(c=>c.column)).join(),/添加至少/);
});
test('canvas edits specify serial and parallel mediation, anchored moderation and conditions',()=>{
 const p=plan();for(const [a,b]of [['X','M'],['M','Y'],['X','Y']])model.addPath(p,a,b);
 model.addInteraction(p,'W','X','M');p.indirectPaths=[['X','M','Y']];p.conditionalIndirect=[{path:['X','M','Y'],moderator:'W',values:[-1,0,1]}];
 assert.deepEqual(model.problems(p,data.columns.map(c=>c.column)),[]);assert.deepEqual(model.chains(p).paths,[['X','M','Y']]);
 assert.throws(()=>model.addPath(p,'Y','X'),/形成环/);assert.throws(()=>model.addPath(p,'X','M'),/已经存在/);
 assert.throws(()=>model.addInteraction(p,'W','M','X'),/先创建/);assert.throws(()=>model.addInteraction(p,'X','X','M'),/不同/);
 assert.throws(()=>model.addPath(p,'M','W'),/形成环/,'implicit W→M main effect also participates in cycle checks');
});
test('deleting a path or a moderator removes dependent effects and retains unrelated edges',()=>{
 const p=plan();model.addPath(p,'X','M');model.addPath(p,'M','Y');model.addPath(p,'X','Y');model.addInteraction(p,'W','X','M');
 p.indirectPaths=[['X','M','Y']];p.conditionalIndirect=[{path:['X','M','Y'],moderator:'W'}];
 model.removeInteraction(p,{x:'X',w:'W',to:'M'});assert.deepEqual(p.conditionalIndirect,[]);assert.equal(p.indirectPaths.length,1);
 model.addInteraction(p,'W','X','M');model.removePath(p,'X','M');assert.deepEqual(p.interactions,[]);assert.deepEqual(p.indirectPaths,[]);assert.equal(p.paths.length,2);
});
test('reselecting variables prunes dependencies; layout survives compatible AI updates',()=>{
 const p=plan();model.addPath(p,'X','M');model.addPath(p,'M','Y');model.addInteraction(p,'W','X','M');model.position(p,'M',260,130);
 const next=model.retainLayout(p,{paths:p.paths,interactions:p.interactions},7);assert.equal(model.confirmed(next,data),true);assert.deepEqual(next.planner.positions,p.planner.positions);
 assert.equal(model.retainLayout(p,{variables:['X','Y'],paths:[{from:'X',to:'Y'}]},7).planner,undefined);
 assert.equal(model.retainLayout(p,{paths:p.paths},8).planner,undefined);
 model.confirm(p,['X','Y'],7);assert.deepEqual(p.paths,[]);assert.deepEqual(p.interactions,[]);assert.deepEqual(p.planner.positions,[]);
});
test('special column names are exact identifiers, and arbitrary conditional effects are rejected',()=>{
 const p={paths:[]};const names=['a → b','__proto__','c|d','W,中文'];model.confirm(p,names,1);model.addPath(p,names[0],names[1]);model.addPath(p,names[1],names[2]);model.addInteraction(p,names[3],names[0],names[1]);
 assert.deepEqual(model.problems(p,names),[]);assert.equal(model.layout(p).positions.size,4);
 p.conditionalIndirect=[{path:names.slice(0,3),moderator:names[2]}];assert.match(model.problems(p,names).join(),/已指定的调节/);
 p.conditionalIndirect=[{path:names.slice(0,3),moderator:names[3],values:['1']}];assert.match(model.problems(p,names).join(),/原量尺/);
 assert.deepEqual(model.conditionalValues('-1, 0，1'),[-1,0,1]);assert.equal(model.conditionalValues(' '),null);
 assert.throws(()=>model.conditionalValues('1,,2'),/数值/);assert.throws(()=>model.conditionalValues('1,NaN'),/数值/);
});

