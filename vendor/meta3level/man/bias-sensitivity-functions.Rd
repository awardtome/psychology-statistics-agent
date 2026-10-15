\name{m3bias}
\alias{m3study}
\alias{m3bias}
\alias{m3leave}
\title{Publication-bias and leave-one-out analyses}
\description{Aggregates dependent effects by study, runs multilevel
PET-PEESE and optional study-level methods, and performs leave-one-out
sensitivity analyses.}
\usage{
m3study(data, rho = 0.60, keep = character())

m3bias(data, rho = 0.60, extra = TRUE,
  direction = c("auto", "greater", "less", "two.sided"),
  level = c("three", "single"), inference = c("model", "CR2"))

m3leave(data, by = c("effect", "study"), method = "REML",
  level = c("three", "single"), rho = 0.60,
  inference = c("model", "CR2"))
}
\arguments{
\item{data}{Prepared effect-size data.}
\item{rho}{Assumed within-study sampling correlation.}
\item{keep}{Study-level moderator columns to preserve during pooling.}
\item{extra}{Run supplementary study-level methods.}
\item{direction}{Selection direction; auto follows the pooled-effect sign.}
\item{by}{Effect or study to omit.}
\item{method}{Estimation method.}
\item{inference}{Ordinary model-based inference, or study-clustered CR2 for PET-PEESE and leave-one-out models. Supplementary selection models are unaffected.}
\item{level}{Use three-level or conventional single-level random-effects analysis.}
}
\value{A study-level data frame or structured analysis result.}
