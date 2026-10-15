const fs = require('node:fs/promises');
const {modelContext} = require('./model-context.cjs');
const MODULES = ['mediation', 'heterogeneity', 'network', 'dag', 'intervention', 'ml'];
const ANALYSES = ['mediation', 'heterogeneity', 'network', 'dag', 'ida', 'ml'];
const PLANS = [...ANALYSES, 'intervention'];

function initialState(data = null, file) {
  return {data, file, stage: 'screen', active: 'mediation', specs: {}, reviews: {}, images: {},
    idaConfirmed: false, ggmConfirmed: false, externalDisplay: false, transition: null, figureImage: null, figure: null};
}

class PsychologyWorkspace {
  constructor(worker, update, onScreened) {
    this.worker = worker;
    this.update = update;
    this.onScreened = onScreened;
    this.state = initialState();
  }
  context() { return modelContext(this.state); }
  async sync() {
    this.state.data = await this.worker.call('q_state');
    this.update(this.state);
    return this.state.data;
  }
  async load(file, sheet) {
    const page=this.state.stage==='analysis'?this.state.active:null;
    const data = await this.worker.call('q_load', {file, sheet});
    this.state = initialState(data, file);
    if(page)await this.operation('enter',{module:page});
    this.update(this.state);
    return data;
  }
  resetAnalysis(data) {
    this.state = {...initialState(data, this.state.file), stage: this.state.stage};
  }
  async run(module) {
    if (!ANALYSES.includes(module)) throw new Error('未知分析入口。');
    if(this.state.transition?.requiresAcceptance)throw new Error('请先在当前页面确认使用这份数据，再开始分析。');
    const spec = {...this.state.specs[module]};
    if (module === 'ida') spec.assumptionsConfirmed = this.state.idaConfirmed;
    const out = await this.worker.call('q_run', {module, spec});
    this.state.data = out.state;
    this.state.active = module === 'ida' ? 'intervention' : module;
    this.state.stage = 'analysis';
    if (module === 'ida') this.state.externalDisplay = false;
    if (module === 'dag' || module === 'network') {
      this.state.idaConfirmed = false;
      this.state.externalDisplay = false;
      delete this.state.images.ida;
      if (module === 'network') {
        this.state.ggmConfirmed = false;
        for (const key of ['dag', 'ida', 'intervention']) {
          delete this.state.specs[key];
          delete this.state.reviews[key];
          delete this.state.images[key];
        }
      }
    }
    if (out.image) this.state.images[module] = 'data:image/png;base64,' + (await fs.readFile(out.image)).toString('base64');
    const next = {heterogeneity: 'network', network: 'dag', dag: 'intervention'}[module];
    this.state.transition = next ? {
      from: module, to: next, revision: out.summary.revision, rows: out.summary.n,
      message: module === 'heterogeneity' ? '可用当前整体数据或本次分组继续网络；请选择数据和是否进入。'
        : module === 'network' ? '网络节点对应的原始记录可用于后续 DAG；网络边不会直接当成因果边。是否进入 DAG？'
        : out.summary.ida_candidate ? '当前连续 CPDAG 可作为 IDA 候选输入；需再确认识别假设。是否进入干预入口？'
        : '当前图不能直接用于标准 IDA；可改算连续 CPDAG，或检查网络模拟入口。'
    } : null;
    this.update(this.state);
    return out.summary;
  }
  async operation(method, p = {}) {
    switch (method) {
      case 'demo':
        this.state = initialState(await this.worker.call('q_demo'));
        break;
      case 'screen':
        this.resetAnalysis(await this.worker.call('q_screen', {config: p.config || {}, skip: p.skip === true}));
        this.state.stage = 'screen';
        this.state.transition = null;
        if(p.skip!==true&&this.onScreened){try{await this.onScreened(this.state.data)}catch(e){this.state.screenedLibraryError=e.message;}}
        break;
      case 'hub':
        this.state.stage = 'hub'; this.state.transition = null;
        break;
      case 'screenPage':
        this.state.stage='screen';this.state.transition=null;
        break;
      case 'enter':
        if (!MODULES.includes(p.module)) throw new Error('未知分析入口。');
        this.state.active = p.module; this.state.stage = 'analysis';
        if (this.state.data?.status !== 'ready') {this.state.transition=null;break;}
        if (!p.accepted) {
          this.state.transition = {from: this.state.data.screening === 'screened' ? '筛选后的数据' : '当前数据（筛选未完成或已跳过）',
            to: p.module, revision: this.state.data.revision, rows: this.state.data.rows,requiresAcceptance:true,
            message: '是否使用这份数据进入所选分析？未执行的计分或筛选不会自动补做。计算前还会检查变量、缺失和有效样本。'};
        } else {
          this.state.active = p.module; this.state.stage = 'analysis'; this.state.transition = null;
          if (p.module === 'dag' && !this.state.specs.dag && this.state.data.results?.network) {
            const network = this.state.data.results.network;
            this.state.specs.dag = {variables: network.nodes, algorithm: 'hc', dataType: network.method === 'ISING' ? 'categorical' : 'continuous'};
          }
          await this.sync();
          if (p.module === 'dag' && !this.state.data.results?.dag && this.state.specs.dag?.variables?.length) return this.run('dag');
        }
        break;
      case 'prepare': {
        if (!PLANS.includes(p.module)) throw new Error('未知分析入口。');
        const review = await this.worker.call('q_prepare', {module: p.module, spec: p.spec});
        const changed = JSON.stringify(this.state.specs[p.module]) !== JSON.stringify(p.spec);
        if (p.module === 'ida' && changed && this.state.specs.ida) this.state.idaConfirmed = false;
        this.state.specs[p.module] = p.spec;
        this.state.reviews[p.module] = {...review, skipped: !changed && this.state.reviews[p.module]?.skipped === true};
        break;
      }
      case 'skipReview':
        this.state.reviews[p.module] = {...this.state.reviews[p.module], skipped: true};
        break;
      case 'confirm':
        this.state.idaConfirmed = p.ida === true; this.state.ggmConfirmed = p.ggm === true;
        break;
      case 'branch':
        this.resetAnalysis(await this.worker.call('q_branch', p));
        this.state.transition = {from: '异质性分组', to: 'network', rows: this.state.data.rows, revision: this.state.data.revision,
          message: '已选择分析数据。可使用当前数据继续网络分析；计算时会检查节点、缺失与样本量。'};
        break;
      case 'run': return this.run(p.module);
      case 'plot': {
        const out = await this.worker.call('q_plot', {spec: p.spec});
        this.state.data = out.state;
        this.state.figure = modelContext(out.figure);
        this.state.figureImage = 'data:image/png;base64,' + (await fs.readFile(out.image)).toString('base64');
        this.update(this.state);
        return this.state.figure;
      }
      case 'status': return this.worker.call('q_status');
      case 'cs': {
        const status = await this.worker.call('q_cs', p);
        await this.sync();
        return status;
      }
      case 'export': return this.worker.call('q_export', p);
      case 'script':
        if(this.state.transition?.requiresAcceptance)throw new Error('请先确认使用当前数据。');
        return this.worker.call('q_script', {...p, spec: this.state.specs.intervention, confirmed: this.state.ggmConfirmed});
      case 'import':
        this.state.data = await this.worker.call('q_import_result', p); this.state.externalDisplay = true;
        delete this.state.images.intervention;
        break;
      case 'importImage':
        this.state.data = await this.worker.call('q_import_image', p);
        this.state.images.intervention = 'data:image/' + (p.file.toLowerCase().endsWith('.png') ? 'png' : 'jpeg') + ';base64,' + (await fs.readFile(p.file)).toString('base64');
        this.state.externalDisplay = true;
        break;
      default: throw new Error('未知问卷操作。');
    }
    this.update(this.state);
    return this.state;
  }
  async tool(name, args) {
    if (name === 'render_research_figure') return this.operation('plot', args);
    if (name === 'inspect_questionnaire') return this.context();
    if (name === 'configure_questionnaire') { await this.operation('screen', {config: args.config}); return this.context(); }
    if (name === 'check_questionnaire_omissions') return this.worker.call('q_review');
    if (name === 'prepare_questionnaire_analysis') { await this.operation('prepare', args); return this.state.reviews[args.module]; }
    if (name === 'run_questionnaire_analysis') return modelContext(await this.run(args.module));
    if (name === 'get_questionnaire_results') return {...this.context(), intervention: await this.worker.call('q_status')};
    throw new Error('未知问卷工具。');
  }
}
module.exports = {PsychologyWorkspace};

