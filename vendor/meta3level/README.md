# meta3level

`meta3level` 是一套自动化三水平与普通随机效应元分析 R 包。本包是对众多元分析所需用到的r包的汇总，您可以只安装本包即可便捷展开自动化的元分析，本包可运行单水平或三水平元分析。此外，本包还会输出原始代码供您审查。如果您还是觉得麻烦，可以使用本项目下包含的skill，安装到您的ai即可实现与本包一致的流程。
This is an R package for automated three‑level and conventional random‑effects meta‑analysis. It integrates numerous existing R packages required for meta‑analysis. You only need to install this package to conveniently conduct automated meta‑analyses, supporting both single‑level and three‑level meta‑analysis. In addition, the package outputs raw code for your inspection. If you still find the workflow cumbersome, you may use the skill included in this project. Once installed to your AI, it can reproduce the identical analytical pipeline of this package.

[English README](README.en.md) | [快速开始](docs/GETTING_STARTED.md) |
[数据要求](docs/DATA_REQUIREMENTS.md) | [统计规则](docs/STATISTICAL_WORKFLOW.md) |
[结果与报告](docs/OUTPUT_AND_REPORTING.md) | [AI skill](docs/AI_SKILL.md) |
[完整中文说明书](USER_MANUAL.zh-CN.md) | [英文说明书](USER_MANUAL.en.md) |
[函数速查](FUNCTION_REFERENCE.zh-CN.md) |
[整套文件说明](FILES.zh-CN.md)

## 函数

| 函数 | 用途 |
|---|---|
| `m3read()` | 读取 CSV 或 Excel |
| `m3prep()` | 准备 `r`、`d`、`g`、`OR` 或自定义效应量 |
| `m3fit()` | 拟合三水平或普通随机效应主模型 |
| `m3study()` | 将同一研究的多个效应量合并为一个 |
| `m3cont()` | 连续调节变量 |
| `m3group()` | 分类调节变量 |
| `m3spline()` | 非线性 spline 比较 |
| `m3bias()` | egger发表偏误与 PET-PEESE |
| `m3leave()` | leave-one-out |
| `m3run()` | 一键完成全部分析 |
| `m3plot()` | 根据结果类型自动作图 |
| `m3report()` | 在控制台重新输出完整结果 |
| `m3code()` | 输出不依赖本包封装的底层复现代码 |
| `m3source()` | 查看安装包实际执行的函数源码 |

它将以下规则固定为可重复运行的函数：

- `r` 先转换为 Fisher's z，报告时自动转回 `r`。
- `d` 先进行小样本校正，转换为 Hedges' `g`。
- `OR` 先转换为 `log(OR)`，需要报告效应量时自动转回 OR。
- Hedges校正使用Gamma函数的精确 `J`；独立组抽样方差与 `metafor` 的大样本SMD公式保持一致。
- 主模型采用 `rma.mv(..., random = ~ 1 | studyID/effectID, method = "REML", tdist = TRUE)`。
- 普通元分析使用 `level = "single"`；程序先按 `rho` 用 GLS 合并同一研究的效应量，再运行 `rma(..., method = "REML", test = "knha")`。
- 普通元分析报告合并前效应量数、逐研究合并数量、总体合并效应、Q、tau-squared、I-squared、H-squared和预测区间。
- 调节效应统一采用 `metafor` 在 `tdist = TRUE` 下提供的 F 检验。
- 连续调节变量自动中心化，只运行有截距模型。
- 分类调节变量同时运行有截距和无截距模型。
- spline 候选模型使用同一批完整数据和 ML 比较 AIC、AICc、BIC。
- Level 2、Level 3 方差使用单侧似然比检验，并报告多层 I²。
- 多层 Egger、PET-PEESE 保留依赖效应量；选择模型先聚合至研究层面。
- 补充提供研究级单层 Egger、trim-and-fill、Begg、3PSM、Vevea-Woods、Copas、p-uniform、p-curve、z-curve 与失效安全数；不满足使用条件时会明确跳过。
- 支持 leave-one-effect-out 和 leave-one-study-out。

## 安装

从 GitHub 安装：

```r
install.packages("remotes")
remotes::install_github(
  "awardtome/meta3level",
  dependencies = NA,
  upgrade = "never"
)
library(meta3level)
```

也可以从 GitHub Releases 下载源码包后安装：

```r
install.packages("metafor")
install.packages("meta3level_0.6.2.tar.gz",
                 repos = NULL, type = "source")
library(meta3level)
```

如需 Copas、p-uniform、Excel 读取等补充功能：

```r
install.packages(c("meta", "metasens", "puniform", "readxl", "openxlsx"))
```

不想安装本包时，可以安装仓库中的 `run-meta-analysis-r` AI skill，让支持
Agent Skills 的 AI 根据编码表生成直接调用 `metafor` 的原生 R 代码。具体见
[AI skill 安装与使用](docs/AI_SKILL.md)。

## 示例：相关系数

```r
library(meta3level)

raw <- m3read(
  file = "编码文件.xlsx",
  sheet = 1
)

dat <- m3prep(
  raw,
  measure = "r",
  study = "studyID",
  effect = "effectID",
  value = "r",
  n = "样本量"
)

result <- m3run(
  dat,
  cont = c("平均年龄", "出版年份"),
  groups = list(var = "研究设计", ref = "横向", name = "横向/纵向"),
  spline = list(var = "平均年龄", df = 1:3),
  bias = TRUE,
  leave = TRUE,
  show = TRUE,
  keep = TRUE,
  code = TRUE
)
```

`show = TRUE` 会把完整统计结果打印在 R 控制台；`code = TRUE` 会在结果
后面继续打印本次分析对应的底层 R 代码。

## 结果与代码审计

```r
# 重新在控制台显示所有结果
m3report(result)

# 同时显示结果和底层复现代码
m3report(result, code = TRUE)

# 只在控制台显示底层代码
m3code(result)

# 同时生成可交给审稿人的 R 脚本和精确数据快照
m3code(result, file = "three-level-meta-analysis-audit.R")
# 同目录还会生成 three-level-meta-analysis-audit_data.rds

# 查看某个封装函数实际执行的源码
m3source("m3bias")

# 查看包内全部公开、内部、清洗、转换和绘图函数源码
m3source("all")

# 将安装版本的全部真实函数体写入审查文件
m3source("all", print = FALSE, file = "meta3level-source.R")
```

`m3code()` 与 `m3source()` 用途不同：

- `m3code()` 根据本次数据列名和分析参数生成可直接复现的代码，直接调用
  `metafor::rma.mv()`、`splines::ns()`、
  `metafor::trimfill()` 等底层函数，不使用 `m3fit()` 等封装。
- 使用 `file` 保存时，`m3code()` 会把实际进入模型的筛选后数据另存为同名
  `_data.rds`；脚本与数据快照必须一起保留。脚本中仍保留原始导入和效应量
  转换代码供审查，但执行时读取快照，以精确复现派生变量和筛选后的样本。
  数据快照只包含研究/效应ID、`yi`、`vi` 与本次实际使用的调节变量，不会
  自动打包编码表中的其他未使用列。
- `m3source()` 显示安装版本真正执行的函数体，包括效应量转换、数据清洗、
  错误拦截、模型拟合和结果转换逻辑。
- 生成脚本末尾包含 `sessionInfo()`，用于记录 R 和依赖包版本。
- 只在控制台生成代码且数据没有被二次筛选时，脚本可从原始 `raw` 或
  `m3read()` 记录的文件路径重建；若数据经过筛选或新增派生列，应使用
  `m3code(..., file = "audit.R")` 生成可精确复现的脚本与快照。

## 独立干预组与对照组的 d

```r
dat <- m3prep(
  raw,
  measure = "d",
  design = "independent",
  study = "studyID",
  effect = "effectID",
  value = "干预组与对照组效应量",
  n1 = "干预组人数",
  n2 = "对照组人数"
)
```

## OR（优势比）

OR 在模型内部自动转换为 `log(OR)`，总体效应、分类组效应、预测图、
发表偏误校正效应和 leave-one-out 结果自动转回 OR。连续调节变量的斜率
仍保留在 `log(OR)` 尺度。

如果编码表有 OR 和 95% CI：

```r
dat <- m3prep(
  raw, measure = "or",
  study = "studyID", effect = "effectID", value = "OR",
  lower = "OR下限", upper = "OR上限"
)
```

如果编码表有 OR 和 `log(OR)` 的标准误：

```r
dat <- m3prep(
  raw, measure = "or",
  study = "studyID", effect = "effectID", value = "OR",
  se = "logOR标准误"
)
```

如果编码表有 OR 和 `log(OR)` 的抽样方差：

```r
dat <- m3prep(
  raw, measure = "or",
  study = "studyID", effect = "effectID", value = "OR",
  vi = "logOR方差"
)
```

如果编码表是 2×2 四格表：

```r
dat <- m3prep(
  raw, measure = "or",
  study = "studyID", effect = "effectID",
  cellA = "干预组事件", cellB = "干预组非事件",
  cellC = "对照组事件", cellD = "对照组非事件"
)
```

四格表存在零格时，默认仅对该四格表的四个格子各加 `0.5`。事件方向和
组别顺序必须在所有研究中保持一致，否则 OR 的方向会被反转。

## 单独运行某一部分

```r
main <- m3fit(dat)
summary(main)                 # 控制台显示完整 metafor 输出
m3effect(main)                # z 自动转回 r；g 保持为 g
m3i2(main)
m3lrt(main)

age <- m3cont(dat, "平均年龄")
print(age)
m3plot(age)

design <- m3group(dat, "研究设计", ref = "横向", name = "横向/纵向")
print(design)

age_spline <- m3spline(dat, "平均年龄", df = 1:3)
print(age_spline)
m3plot(age_spline)

bias <- m3bias(dat, rho = 0.60)
print(bias)
m3plot(bias)

loo_effect <- m3leave(dat, "effect")
loo_study <- m3leave(dat, "study")
m3plot(loo_effect)
m3plot(loo_study)
```

## 普通随机效应元分析

同一套 `dat` 不需要重新计算效应量，只需增加 `level = "single"`：

```r
single <- m3run(
  dat,
  cont = c("平均年龄", "出版年份"),
  groups = list(var = "研究设计", ref = "横向"),
  spline = list(var = "平均年龄", df = 1:3),
  level = "single",
  rho = 0.60,
  bias = TRUE,
  leave = TRUE,
  show = TRUE,
  code = TRUE
)

single$main$aggregation  # 每篇研究合并了几个效应量
single$main$overall      # 总体合并效应量
single$main$i2           # Q、tau-squared、I-squared、H-squared
single$main$prediction   # 预测区间

m3plot(single, "forest")
m3plot(single, "cont", 1)
m3plot(single, "spline", 1)
m3plot(single, "bias")
m3plot(single, "effect")
m3plot(single, "study")
```

普通元分析的调节变量必须是研究层面变量。同一研究内若出现不同的连续值
或分类编码，程序会停止并指出冲突，不会擅自平均或取第一行。

`studyID` 必须表示统计上独立的样本，而不只是论文编号；一篇论文包含两个
独立样本时应编码为两个不同的 `studyID`。只有针对同一目标构念和同一比较
的多个效应量才适合研究内合并。`rho` 是研究内抽样相关的假设值，不能由
当前效应量表自动估计，建议至少用几个合理值重复分析并比较结论。
通常直接把未合并的 `dat` 交给 `m3run(..., level = "single")`，这样
leave-one-effect-out 还能逐个删除原始效应量。若先运行 `m3study(dat)` 再分析，
包会保留合并记录，但后续只能对已合并的研究行做删除分析。

## 限制

1. 单组前后测效应量的抽样方差通常需要前后测相关。没有该信息时，包不会静默假定；通过公开 API 必须显式使用 `variance = "approximate"`，并会产生警告。优先提供设计特定的 `vi` 并使用 `variance = "known"`。
2. Egger 显著表示小样本效应，不等同于证明发表偏误。
   对Hedges' g而言，效应量与其标准误/方差还存在一定机械相关，因此Egger与PET-PEESE必须结合选择模型和研究设计解释。
3. PET 截距不显著时参考 PET；PET 截距显著时参考 PEESE。
4. p-uniform、Begg、Copas、3PSM、Vevea-Woods 和失效安全数属于研究级补充证据。
5. ROB-ME 需要检索记录、注册方案和未报告结果信息，不能由效应量表自动评分。

## 自动检查与常见错误

- CSV 默认 `encoding = "auto"`，会尝试 UTF-8、GB18030 和 GBK。若仍读取为一列，通常需要检查CSV分隔符。
- CSV/TSV 默认先按字符读取，以保留 `001` 这类带前导零的研究 ID；`m3prep()` 只会把明确指定的效应量、方差和样本量列转成数值。
- 从 CSV/TSV/Excel 导入时若有重复表头，程序会警告并按原始列号修复全部同名列，例如两个 `热执行功能` 会变成 `热执行功能...12` 和 `热执行功能...14`；原名与修复名映射保存在 `attr(raw, "meta3_source")$name_map`。直接传入未修复的重复列表格时仍会停止，以避免选错变量。
- CSV 会自动尝试逗号、分号和制表符分隔；使用小数逗号的文件必须设置 `decimal = ","`，例如 `m3read("data.csv", decimal = ",")`。
- 直接把字符型小数逗号数据框传给 `m3prep()` 时，也应设置 `decimal = ","`；通过 `m3read(..., decimal = ",")` 读取时会自动继承。
- 空列名、重复列名、空研究ID、重复的 `studyID + effectID` 会在建模前被拦截。没有效应量ID列时，可在 `m3prep()` 中省略 `effect`，程序会按原始行号生成唯一 ID。
- 非法相关系数（`|r| >= 1`）、`n <= 3`、非正抽样方差会被删除，并报告删除数量。
- 若独立组 d 提供的部分抽样方差缺失或非法，程序会明确报告回退行数，并仅对这些行使用样本量 SMD 方差公式；不会静默混用。
- 若存在 `|g| > 3` 的标准化均差，程序会保留该效应但提示复核效应定义、组别方向，以及是否误把标准误当作标准差。
- 如果每篇研究都只有一个效应量，三水平模式会停止并提示使用 `level = "single"`。
- 连续变量中混入文字会明确报错，不会静默转换为缺失值。
- 分类变量某个类别只有一篇研究时会警告，该类别系数和F检验不能稳定解释。
- 研究数少于10篇时，F 检验、发表偏误和方差成分检验会提示统计功效不足。
- 百分比连续变量若混合写成比例与百分号，例如 `0.52` 与 `52%`，会自动统一为0-1比例；若混合写成 `52` 与 `52%`，按0-100百分点解释。只有尺度无法明确判断时才停止分析。
- 若无百分号的值只有 `0/1`，同时又混有 `50%` 一类值，程序不会猜测 `1` 代表1%还是100%，而会要求研究者先统一尺度。
- 三水平模型目前使用对角抽样方差 `V = vi`。若多个效应量因共享被试或共享对照组而具有已知抽样协方差，应进行设计特定的协方差建模；三水平随机效应本身不能保证完全替代该抽样协方差。
- 若只有一个独立研究提供多个效应量，程序会警告 Level 2 方差识别很弱；此时即使模型能收敛，也不应把 Level 2 I² 当作稳定结论。
- spline 使用ML和同一批数据比较；在 `Delta AICc <= 2` 的模型中优先选择更简单模型。
- `m3run(..., keep = TRUE)` 时，某个可选分析失败不会中断后续分析；错误原因保存在相应结果位置的 `$message` 中。设置 `keep = FALSE` 可在第一个错误处停止。
- 同一类分析中的调节结果显示名必须唯一；重复 `name` 会停止运行，避免列表提取和审计代码覆盖同名模型。
- 相关系数模型的截距和无截距组效应可以转回 `r`；连续调节斜率仍在Fisher z尺度，不能直接用 `tanh(斜率)`解释为相关系数变化量。
- OR 模型内部使用 `log(OR)`；`se` 必须是 `log(OR)` 的标准误，`vi` 必须是 `log(OR)` 的方差，不能填原始 OR 的标准误或方差。
- OR 的连续调节斜率是 `log(OR)` 的变化量；只有总体效应、分类组效应、预测值和敏感性结果自动转回 OR。
- RStudio 绘图窗通常可直接显示中文。若基础 `pdf()` 设备提示中文字符转换失败，建议改用 `grDevices::cairo_pdf()`；保存位图时可使用 `png(..., type = "cairo", res = 300)`，并确认系统已安装所选中文字体。该警告来自图形设备字体编码，不改变模型结果。

## 更新介绍（2026-10-02）

本次更新为开发版 **0.6.2.9000**，保留原有公开函数及默认模型推断。

- 新增可选 `inference = "CR2"`：按研究聚类，使用 Satterthwaite 系数检验与置信区间、HTZ 整体 F 检验。需要安装 `clubSandwich`；不改变点估计与方差成分。
- 修复缺失调节值造成的数据错位、复现脚本遗漏高级拟合参数、方差检验放开已固定分量，以及特殊列名、spline 自由度和 F 统计量校验问题。
- 统一模型拟合与推断逻辑。79 项测试、391 个断言和四个公开示例通过；本地 R CMD check 为 0 ERROR、0 WARNING、0 NOTE（未编译手册）。

用法示例：`m3run(dat, inference = "CR2")`。原有 0.6.2 静态说明书对应旧版；新增参数以当前 R 帮助和[重构审查记录](docs/REFACTOR_REVIEW.zh-CN.md)为准。
