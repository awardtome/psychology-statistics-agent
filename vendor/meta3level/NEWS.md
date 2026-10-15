# meta3level 0.6.2.9000 (development)

* Preserve other fixed variance components when constraining one component
  for an LRT; do not test components already fixed by the researcher.
* Reject no-intercept models in the overall-effect extractor.

* Centralize model-row preparation, model dispatch and inference.
* Add optional `inference = "CR2"` to fitting, moderator, spline, PET-PEESE,
  leave-one-out and complete-workflow APIs. Preserve the ordinary-inference
  default. CR2 uses study clusters, Satterthwaite t tests/CIs and HTZ F tests.
* Retain advanced model arguments such as `dfs` and optimizer controls in
  generated audit code and reduced variance-component fits.
* Align model metadata with rows retained after missing/transformed moderator
  values are excluded, before creating CR2 clusters.
* Treat character moderator specifications as literal column names.
* Reject chi-square statistics in `m3ftest()` and fractional/non-finite spline
  degrees of freedom. Protect conflicting model/subset overrides.
* Escape control characters in audit string literals and use row indices for
  leave-one-effect-out scripts so distinct identifier pairs cannot collide.
* Reports print the selected inference explicitly and show moderator F tests
  without duplicated ordinary model summaries.


# meta3level 0.6.2

- Added three-level and conventional random-effects workflows for correlations,
  Cohen's d, Hedges' g, odds ratios, and custom effect sizes.
- Added small-sample t/F inference through `metafor`, centered continuous
  moderators, categorical intercept/no-intercept models, and ML spline model
  comparison.
- Added multilevel I-squared decomposition and one-sided likelihood-ratio tests
  for variance components.
- Added multilevel and study-level small-study-effect diagnostics, PET-PEESE,
  optional selection-model diagnostics, and funnel plots.
- Added leave-one-effect-out and leave-one-study-out analyses with connected
  plots and complete result tables.
- Added conventional meta-analysis with generalized least-squares pooling of
  dependent effects within studies.
- Added auditable native R code generation through `m3code()` and installed
  source inspection through `m3source()`.
- Added strict checks for duplicate identifiers, malformed numeric columns,
  ambiguous percentages, unsupported single-group SMD variances, and extreme g.
- Added GitHub release documentation, cross-platform CI, a repository checker,
  executable examples, and a standalone Agent Skill with native `metafor` code.
- Allowed `m3plot()` to select continuous and spline workflow results by either
  their display name or their original moderator column name.
