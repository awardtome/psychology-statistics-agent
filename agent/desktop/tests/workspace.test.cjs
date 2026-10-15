const {test} = require('node:test');
const assert = require('node:assert/strict');
const {PsychologyWorkspace} = require('../psych-controller.cjs');

function workspace() {
  const calls = [];
  const data = {status: 'ready', revision: 2, rows: 40, results: {}};
  const worker = {call: async (name, params) => {
    calls.push({name, params});
    if (name === 'q_run') return {state: data, summary: {n: 40, revision: 2}};
    if (name === 'q_prepare') return {status: 'ready'};
    if (name === 'q_cs') {data.results.network = {cs: {expectedInfluence: .4}}; return {cs: .4};}
    return data;
  }};
  return {desk: new PsychologyWorkspace(worker, () => {}), data, calls};
}

for (const action of ['demo', 'screen', 'branch']) test(action + ' resets stale analysis and consent', async () => {
  const {desk} = workspace();
  Object.assign(desk.state, {specs: {dag: {variables: ['OLD']}}, images: {ida: 'old'},
    reviews: {network: {skipped: true}}, idaConfirmed: true, ggmConfirmed: true, externalDisplay: true});
  await desk.operation(action);
  for (const key of ['specs','images','reviews']) assert.deepEqual(desk.state[key], {});
  for (const key of ['idaConfirmed','ggmConfirmed','externalDisplay']) assert.equal(desk.state[key], false);
});

test('loading a new file replaces the full workspace', async () => {
  const {desk} = workspace(); desk.state.externalDisplay = true;
  await desk.load('new.csv'); assert.equal(desk.state.file, 'new.csv'); assert.equal(desk.state.externalDisplay, false);
});

test('new DAG and network fits invalidate old IDA confirmation', async () => {
  const {desk} = workspace();
  for (const module of ['dag', 'network']) {
    desk.state.idaConfirmed = true; desk.state.specs.dag = {variables: ['OLD']};
    await desk.operation('run', {module}); assert.equal(desk.state.idaConfirmed, false);
    if (module === 'network') assert.equal(desk.state.specs.dag, undefined);
  }
});

test('same specification preserves skipped review; changed IDA variables require reconfirmation', async () => {
  const {desk} = workspace(); const spec = {exposure: 'X', outcome: 'Y'};
  await desk.operation('prepare', {module: 'ida', spec});
  await desk.operation('skipReview', {module: 'ida'});
  await desk.operation('confirm', {ida: true});
  await desk.operation('prepare', {module: 'ida', spec});
  assert.equal(desk.state.reviews.ida.skipped, true); assert.equal(desk.state.idaConfirmed, true);
  await desk.operation('prepare', {module: 'ida', spec: {exposure: 'Z', outcome: 'Y'}});
  assert.equal(desk.state.reviews.ida.skipped, false); assert.equal(desk.state.idaConfirmed, false);
});

test('returned CS synchronizes UI and model context', async () => {
  const {desk} = workspace(); await desk.operation('cs', {value: .4, metric: 'expectedInfluence'});
  assert.equal(desk.context().data.results.network.cs.expectedInfluence, .4);
});

test('model tool outputs never leak image paths or bytes', async () => {
  const {desk, data} = workspace(); data.results.network = {image: 'SECRET', table: Array(160).fill(1)};
  const result = await desk.tool('configure_questionnaire', {config: {}});
  assert.equal(JSON.stringify(result).includes('SECRET'), false);
  assert.equal(result.data.results.network.table.length, 150);
  assert.equal(result.contextTruncation[0].total, 160);
});

test('failed screening preserves the previous workspace', async () => {
  const {desk} = workspace(); const before = JSON.stringify(desk.state);
  desk.worker.call = async () => {throw new Error('invalid rule');};
  await assert.rejects(desk.operation('screen'), /invalid rule/);
  assert.equal(JSON.stringify(desk.state), before);
});

test('AI can prepare explicit intervention targets but cannot supply publication consent',async()=>{
  const {desk,calls}=workspace();const spec={method:'ggmnira',targets:['X'],outcomeScope:'selected_nodes',outcomes:['Y'],intensity:2,repetitions:1000};
  await desk.tool('prepare_questionnaire_analysis',{module:'intervention',spec});
  await desk.operation('script',{method:'ggmnira',directory:'job',confirmed:true,spec:{targets:['WRONG']}});
  const call=calls.find(c=>c.name==='q_script');assert.deepEqual(call.params.spec,spec);assert.equal(call.params.confirmed,false);
});

test('network replacement invalidates intervention targets and report image',async()=>{
  const {desk}=workspace();desk.state.specs.intervention={targets:['OLD']};desk.state.images.intervention='old';
  await desk.operation('run',{module:'network'});
  assert.equal(desk.state.specs.intervention,undefined);assert.equal(desk.state.images.intervention,undefined);
});

test('external task export respects pending data acceptance',async()=>{
  const {desk,data,calls}=workspace();desk.state.data=data;
  await desk.operation('enter',{module:'intervention'});
  await assert.rejects(desk.operation('script',{method:'ggmnira'}),/确认使用/);
  assert.equal(calls.some(c=>c.name==='q_script'),false);
});
