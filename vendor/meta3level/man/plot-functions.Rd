\name{m3plot}
\alias{m3plot}
\title{Automatically plot a meta3level result}
\description{Selects the correct forest, moderator, spline, funnel, or
leave-one-out plot.}
\usage{
m3plot(x, what = NULL, index = 1, name = NULL, ...)
}
\arguments{
\item{x}{A result object produced by this package.}
\item{what}{For a complete workflow: forest, cont, spline, bias, effect, or study.}
\item{index}{Result index when several moderators were analysed.}
\item{name}{Display name or original variable name for continuous or spline
workflow results; an alternative to \code{index}.}
\item{...}{Options passed to the corresponding plotting function.}
}
\value{Plot data or model, invisibly.}
