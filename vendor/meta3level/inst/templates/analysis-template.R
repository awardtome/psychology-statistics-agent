# ============================================================
# meta3level 可替换分析模板
# 所有【替换】位置都需要按自己的编码表修改
# ============================================================

library(meta3level)

# 1. 读取文件 ---------------------------------------------------------------
raw <- m3read(
  file = "【替换：编码文件完整路径.xlsx】",
  sheet = 1
)

# 2A. 如果效应量是相关系数 r，使用这一段 ----------------------------------
# 如果编码表没有效应量ID列，可以删除 effect = 这一行；程序将按原始行号生成。
dat <- m3prep(
  raw,
  measure = "r",
  study = "【替换：研究ID列名】",
  effect = "【替换：效应量ID列名】",
  value = "【替换：r列名】",
  n = "【替换：样本量列名】"
)

# 2B. 如果效应量是独立干预组与对照组 Cohen's d，改用这一段 ----------------
# dat <- m3prep(
#   raw,
#   measure = "d",
#   design = "independent",
#   study = "【替换：研究ID列名】",
#   effect = "【替换：效应量ID列名】",
#   value = "【替换：d列名】",
#   n1 = "【替换：干预组人数列名】",
#   n2 = "【替换：对照组人数列名】"
# )

# 2C. 如果已计算好 Hedges' g 与抽样方差 vi，改用这一段 --------------------
# dat <- m3prep(
#   raw,
#   measure = "g",
#   study = "【替换：研究ID列名】",
#   effect = "【替换：效应量ID列名】",
#   value = "【替换：g列名】",
#   vi = "【替换：g的抽样方差列名】"
# )

# 2D. 如果是单组/前后测 d，优先提供研究者计算好的抽样方差 ----------------
# dat <- m3prep(
#   raw,
#   measure = "d",
#   design = "onegroup",
#   variance = "known",
#   study = "【替换：研究ID列名】",
#   effect = "【替换：效应量ID列名】",
#   value = "【替换：d列名】",
#   n = "【替换：样本量列名】",
#   vi = "【替换：d的抽样方差列名】"
# )
# 若无法获得设计特定方差，才把 variance 改为 "approximate" 并删除 vi 行；
# 该近似不使用前后测相关，只能作为明确披露的备选分析。

# 2E. 如果已计算好其它效应量 yi 与抽样方差 vi，改用这一段 ------------------
# dat <- m3prep(
#   raw,
#   measure = "custom",
#   study = "【替换：研究ID列名】",
#   effect = "【替换：效应量ID列名】",
#   value = "【替换：yi列名】",
#   vi = "【替换：vi列名】"
# )

# 2F. 如果效应量是 OR 且报告了 95% CI，改用这一段 --------------------------
# dat <- m3prep(
#   raw,
#   measure = "or",
#   study = "【替换：研究ID列名】",
#   effect = "【替换：效应量ID列名】",
#   value = "【替换：OR列名】",
#   lower = "【替换：OR的95%CI下限列名】",
#   upper = "【替换：OR的95%CI上限列名】"
# )

# 2G. 如果效应量来自2×2四格表，改用这一段 -------------------------------
# dat <- m3prep(
#   raw,
#   measure = "or",
#   study = "【替换：研究ID列名】",
#   effect = "【替换：效应量ID列名】",
#   cellA = "【替换：干预组事件数】",
#   cellB = "【替换：干预组非事件数】",
#   cellC = "【替换：对照组事件数】",
#   cellD = "【替换：对照组非事件数】"
# )

# 3. 指定连续调节变量：自动中心化，不运行无截距模型 ------------------------
continuous_specs <- c(
  "【替换：连续变量1列名】",
  "【替换：连续变量2列名】"
)

# 4. 指定分类调节变量及参照组：自动运行有/无截距模型 -----------------------
categorical_specs <- list(
  list(var = "【替换：分类变量列名】",
       ref = "【替换：参照组在数据中的原始取值】",
       name = "【替换：分类变量名称】")
)

# 5. 指定需要比较的非线性模型：全部用同一批数据和 ML -----------------------
spline_specs <- list(
  list(var = "【替换：需要非线性检验的连续变量列名】",
       df = 1:3,
       linear = TRUE,
       name = "【替换：变量名称】")
)

# 6. 一键运行：结果全部打印在R控制台 ---------------------------------------
result <- m3run(
  dat,
  cont = continuous_specs,
  groups = categorical_specs,
  spline = spline_specs,
  bias = TRUE,
  leave = TRUE,
  rho = 0.60,
  level = "three", # 【替换】普通随机效应元分析改为 "single"
  show = TRUE,
  keep = TRUE,
  code = TRUE
)

# level="single" 时 rho 是研究内效应量抽样相关的假设值；建议至少重复
# rho=0、0.30、0.60、0.90，比较合并效应和结论是否稳定。

# show=TRUE：完整结果显示在R控制台
# code=TRUE：结果后继续显示底层包的完整复现代码

# 7. 作图：直接显示，不自动保存 ---------------------------------------------
m3plot(result, "forest")
m3plot(result, "cont", 1)
m3plot(result, "spline", 1)
m3plot(result, "bias")
m3plot(result, "effect")
m3plot(result, "study")

# 8. 如需表格，可直接提取 ----------------------------------------------------
result$main$overall
result$main$i2
result$main$lrt
result$cont[[1]]$coefficients
result$groups[[1]]$group_effects
result$spline[[1]]$comparison
result$effectleave$results
result$studyleave$results

# 9. 审计与复现 -------------------------------------------------------------
# 重新在控制台显示全部结果和底层代码
m3report(result, code = TRUE)

# 生成可独立运行、可提交给审稿人的底层R脚本
m3code(result, file = "three-level-meta-analysis-audit.R")

# 查看本包实际执行的全部函数源码
m3source("all")

# 或输出为可提交审查的源码文件
m3source("all", print = FALSE, file = "meta3level-source.R")
