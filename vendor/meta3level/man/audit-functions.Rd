\name{m3code}
\alias{m3code}
\alias{m3source}
\title{Show reproducible dependency code and installed package source}
\description{Generates directly executable audit code that calls dependency
packages without meta3level analysis wrappers, or prints the actual installed
function definitions used by meta3level.}
\usage{
m3code(x, print = TRUE, file = NULL)

m3source(fun = "all", print = TRUE, file = NULL)
}
\arguments{
\item{x}{A workflow, prepared data, fitted model, moderator, publication-bias,
or leave-one-out result. Prepared data generate preparation plus main-model code.}
\item{print}{Print code or source in the R console.}
\item{file}{Optional path for writing generated reproducible code or function
definitions. For \code{m3code()}, an exact prepared-data \code{_data.rds}
sidecar is also written beside the script; keep both files together.}
\item{fun}{Function name, vector of names, or all.}
}
\value{A character vector of reproducible code or a named list of functions.}
