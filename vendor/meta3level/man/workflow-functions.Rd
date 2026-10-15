\name{m3run}
\alias{m3run}
\alias{m3report}
\title{Run and print the complete three-level meta-analysis workflow}
\description{Runs the main model, moderators, splines, publication-bias
analyses, and leave-one-out analyses using compact specifications.}
\usage{
m3run(data, cont = NULL, groups = NULL, spline = NULL,
  bias = TRUE, leave = TRUE, rho = 0.60,
  level = c("three", "single"), show = TRUE, keep = TRUE, code = FALSE,
  inference = c("model", "CR2"))

m3report(x, code = FALSE, ...)
}
\arguments{
\item{data}{Prepared effect-size data.}
\item{cont}{Continuous moderator names or specifications.}
\item{groups}{Categorical specifications using var and ref.}
\item{spline}{Spline specifications using var and df.}
\item{bias}{Run publication-bias analyses.}
\item{leave}{Run both leave-one-out analyses.}
\item{rho}{Assumed within-study sampling correlation.}
\item{level}{Use three-level analysis, or pool effects by study and use a conventional random-effects analysis.}
\item{show}{Print the complete report.}
\item{keep}{Continue optional analyses and store a readable error object when one component fails.}
\item{code}{Print underlying-package audit code after the results.}
\item{inference}{Ordinary model-based inference, or study-clustered CR2. The chosen mode is applied to main, continuous, categorical, spline, PET-PEESE and leave-one-out models. Supplementary study-level selection models retain their own inference.}
\item{x}{A package result object.}
\item{...}{Additional print arguments.}
}
\value{A structured workflow result.}
