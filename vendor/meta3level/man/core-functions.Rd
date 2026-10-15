\name{m3prep}
\alias{m3read}
\alias{m3prep}
\alias{m3fit}
\alias{m3effect}
\alias{m3ftest}
\alias{m3i2}
\alias{m3lrt}
\title{Core functions with short, package-specific names}
\description{Reads data, prepares effect sizes, fits either a three-level or
conventional random-effects model, and extracts the overall effect, moderator
F test, heterogeneity, and one-sided likelihood-ratio tests.}
\usage{
m3read(file, sheet = 1, encoding = "auto", decimal = ".", ...)

m3prep(data, measure = c("r", "d", "g", "or", "custom"), study,
  effect = NULL, value = NULL, n = NULL, n1 = NULL, n2 = NULL, vi = NULL,
  se = NULL, lower = NULL, upper = NULL, cellA = NULL, cellB = NULL,
  cellC = NULL, cellD = NULL, conf = 0.95, correction = 0.5,
  design = c("independent", "onegroup"),
  variance = c("known", "approximate"), decimal = NULL)

m3fit(data, mods = NULL, method = "REML", f = TRUE,
  sigma2 = c(NA, NA), warn = TRUE,
  level = c("three", "single"), rho = 0.60, ...,
  inference = c("model", "CR2"))

m3effect(model, back = TRUE)

m3ftest(model)

m3i2(model)

m3lrt(model, data = model$meta3_data)
}
\arguments{
\item{file}{Path to CSV, TSV, XLSX, or XLS data.}
\item{sheet}{Excel sheet name or index.}
\item{encoding}{Delimited-file encoding.}
\item{decimal}{Decimal mark for delimited files or text-form numeric cells. In m3prep it is inherited from m3read when omitted.}
\item{data}{Input or prepared effect-size data.}
\item{measure}{Effect-size measure.}
\item{study}{Study or independent-sample identifier column.}
\item{effect}{Optional effect identifier column. If omitted, IDs are generated from original row numbers.}
\item{value}{Effect-size column; omit for OR calculated from table cells.}
\item{n, n1, n2}{Sample-size columns.}
\item{vi}{Known sampling-variance column; for OR this is log(OR) variance.}
\item{se}{Standard-error column for log(OR).}
\item{lower, upper}{OR confidence-limit columns.}
\item{cellA, cellB, cellC, cellD}{Two-by-two table columns: event/non-event in group 1 and event/non-event in group 2.}
\item{conf}{Confidence level for OR limits.}
\item{correction}{Continuity correction for a table containing a zero cell.}
\item{design}{Independent-groups or one-group design.}
\item{variance}{Known variance or explicit one-group approximation.}
\item{mods}{Moderator formula or character vector.}
\item{method}{Estimation method.}
\item{f}{Use metafor t/F inference.}
\item{inference}{Use ordinary model-based inference (the default), or study-clustered CR2 inference via clubSandwich. CR2 uses Satterthwaite coefficient tests and confidence intervals and HTZ omnibus F tests.}
\item{sigma2}{Fixed/free variance-component vector.}
\item{warn}{Warn when fewer than ten independent studies are available.}
\item{level}{Use three-level or conventional single-level random-effects analysis.}
\item{rho}{Assumed within-study sampling correlation used to pool effects when level is single.}
\item{model}{A fitted metafor model.}
\item{back}{Transform Fisher's z back to r.}
\item{...}{Additional reader or model arguments.}
}
\value{A data frame, fitted model, or result table.}
\details{Missing or non-finite moderator values are removed before fitting, and
the stored model data match the fitted rows. Character moderator specifications
are literal column names; use a formula for interactions or transformations.
The optional CR2 mode changes inference, not point estimates or variance
components. Ordinary and CR2 inference can be compared using the same data.
With CR2, ordinary coefficient results are retained in meta3_model_coefficients.
The F extractor refuses to label a chi-square statistic as F.
Overall-effect extraction requires an intercept. Variance-component tests
preserve other fixed components and are undefined for a component already fixed
in the fitted model.
Filter the input data directly instead of supplying subset through dots.}
