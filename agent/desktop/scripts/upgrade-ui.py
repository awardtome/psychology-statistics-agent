from pathlib import Path
desktop=Path(__file__).resolve().parents[1]
p=desktop/'renderer/index.html'
s=p.read_text(encoding='utf-8')
s=s.replace('<title>元分析助手</title>','<title>心理学统计agent</title>')
s=s.replace('<span class="mark">m³</span><div>元分析助手<small>Meta Analysis Agent</small></div>','<span class="mark">ψ</span><div>心理学统计agent<small>Psychology Statistics Agent</small></div>')
s=s.replace('</div><button id="open-file"','</div><div class="module-nav"><button data-module="home">首页</button><button data-module="meta">元分析</button><button data-module="questionnaire">问卷</button></div><div id="psych-nav" hidden></div><button id="open-file"',1)
s=s.replace('Windows 本地版 · 0.1.0','Windows 本地版 · 0.2.0')
s=s.replace('<section id="messages"','<section id="home-panel" hidden></section><section id="messages"',1)
psych='''<section id="psych-workspace" class="workspace-content" hidden>
<h2 id="psy-title"></h2><p id="psy-origin" class="hint"></p><button id="psy-hub-back">返回分析入口</button>
<div id="psy-empty" class="empty">请先导入问卷原始数据。题项所属变量、反向题和筛选条件均由你说明。</div>
<div id="psy-sheet-box" hidden><select id="psy-sheet"></select><button id="psy-sheet-load">读取工作表</button></div>
<div id="psy-content" hidden>
<details><summary>当前列与前六行</summary><div id="psy-columns" class="table-wrap"></div><div id="psy-data-preview" class="table-wrap"></div></details>
<div id="psy-screen"><h3>告诉 AI 怎么处理这份问卷</h3><p>一行一个人或一列一个人？哪些题项属于哪些变量？量表范围、反向题、最少答题数，以及要采用什么筛选规则？都可以直接在对话里说。</p>
<details><summary>高级配置 可直接编辑</summary><textarea id="psy-screen-config" class="json-editor" rows="12" aria-label="问卷筛选计分配置"></textarea></details>
<button id="psy-screen-apply" class="primary wide">应用当前计分和筛选规则</button><div class="button-row"><button id="psy-score-skip">跳过计分</button><button id="psy-rules-skip">跳过筛选</button></div><button id="psy-screen-skip" class="wide">跳过整个筛选步骤</button>
<h3>筛选后再检查遗漏</h3><div id="psy-review"></div><button id="psy-review-skip" class="wide">跳过补充检查，选择后续分析</button>
<details><summary>全部筛选与计分审计</summary><pre id="psy-audit"></pre></details></div>
<div id="psy-hub" hidden><h3>接下来想回答什么研究问题</h3><div id="psy-hub-grid"></div><details><summary>当前数据的方法适配候选</summary><pre id="psy-suitability"></pre></details></div>
<div id="psy-analysis" hidden><p class="hint">在对话里描述变量、路径和研究问题。AI 会整理配置、检查遗漏并调用实际计算；你可以跳过补充问答。</p><div id="psy-kind-options" class="button-row"></div>
<details open><summary>当前分析配置</summary><textarea id="psy-spec" class="json-editor" rows="10" aria-label="当前分析配置"></textarea></details>
<div id="psy-plan-review"></div><button id="psy-skip-analysis-review" class="wide">跳过补充问答</button>
<div id="psy-analysis-buttons" class="button-row"><button id="psy-prepare">检查当前方案</button><button id="psy-run" class="primary">开始本地分析</button></div>
<div id="psy-intervention" hidden><button id="psy-status" class="wide">检查 IDA 与模拟算法的适配</button><pre id="psy-intervention-status"></pre>
<label class="check"><input id="psy-ida-confirm" type="checkbox">我理解并愿意在当前分析中采用线性高斯、无未测混淆、无环与忠实性假设。</label><button id="psy-ida-run" class="primary wide">运行 IDA</button>
<p>Ising 的 EI CS &gt; 0.25 可进入 NIRApost；GGM 的 EI CS &gt; 0.25 可考虑 GGMNIRA。这只是最低稳定性门槛，不能证明真实干预有效。</p>
<label>返回同一节点和数据版本的外部 EI CS</label><input id="psy-cs" type="number" min="0" max="1" step="0.01"><button id="psy-cs-return">记录使用者报告的 CS</button>
<label class="check"><input id="psy-ggm-confirm" type="checkbox">我已了解 GGMNIRA 尚未正式发表，已有预印本，报告时需披露方法状态。</label>
<button id="psy-nirapost" class="wide">导出 NIRApost 独立计算代码</button><button id="psy-ggmnira" class="wide">导出 GGMNIRA 独立计算代码</button>
<p class="hint">模拟及稳定性计算较久。独立运行可避免逐轮请求 AI 浪费 token。输出在脚本旁 results/，完成后导入 statistics.csv。</p><button id="psy-import-result" class="wide">导入外部实际结果</button></div>
<div id="psy-results"></div><div id="psy-branches" class="button-row"></div></div>
<div id="psy-transition" class="transition"></div><button id="psy-export" class="primary wide">导出完整 Excel Word 与图片报告</button></div></section>'''
s=s.replace('</section></aside>','</section>'+psych+'</aside>',1)
research='''<aside id="research-panel" class="research-panel"><h2>理论与前人研究</h2><p class="hint">与本地分析同时使用。检索词会发送给 Crossref；候选文献须核实与研究问题的关系。</p><label for="research-query">变量构念和方法检索词</label><textarea id="research-query" rows="3" placeholder="例如 emotion regulation mediation theory"></textarea><label class="check"><input id="research-auto" type="checkbox">使用我已说明的构念和当前方法自动检索</label><button id="research-search" class="wide">搜索真实文献</button><div id="research-status" role="status"></div><div id="research-results"></div><button id="research-write" class="primary wide">结合数据生成写作与理论建议</button><div id="research-advice" class="research-advice"></div></aside>'''
s=s.replace('<dialog id="settings-dialog">',research+'<dialog id="settings-dialog">')
s=s.replace('<script src="app.js"></script>','<script src="app.js"></script><script src="psych-ui.js"></script>')
p.write_text(s,encoding='utf-8')
p=desktop/'renderer/app.js';s=p.read_text(encoding='utf-8')
s=s.replace("const before=current;current={...current,...state};","const before=current;current={...current,...state};if(window.renderPsychology)window.renderPsychology(current);")
s=s.replace("if(current.config)await syncConfig();","if(current.module==='meta'&&current.config)await syncConfig();")
s=s.replace("'工具 · '+names[e.name]","'工具 · '+(names[e.name]||e.name)")
s=s.replace("已加载 18 个独立样本、36 个效应的合成示例。请在右侧确认列含义和独立样本，再生成方案。","已加载合成示例，仅用于演示。请查看实际列，明确变量含义和方案后运行。")
p.write_text(s,encoding='utf-8')
