% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Copula.R
\name{setMarginal<-}
\alias{setMarginal<-}
\alias{setMarginal<-,CopulaObj,list-method}
\alias{setMarginal,ANY-method}
\title{Set the marginal distributions of the copula.}
\usage{
setMarginal(this, ...) <- value

\S4method{setMarginal}{CopulaObj,list}(this) <- value
}
\arguments{
\item{this}{Copula Object}

\item{...}{Additional function arguments}

\item{value}{The list of marginal distributions.}
}
\description{
Set the marginal distributions of the copula.
}
\examples{
library(cascsim)
dist1<-new("Pareto",p1=20,p2=3)
dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
cop <- new("CopulaObj", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
dist3<-new("Pareto",p1=10,p2=3)
dist4<-new("Normal",p1=2,p2=3,min=0,max=20,truncated=TRUE)
setMarginal(cop) <- list(dist1=dist3,dist2=dist4)
}
