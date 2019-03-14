#' setID
#' Set the ID for an object
#' @name setID<-
#' @param this Self
#' @param ... Additional function arguments
#' @param value ID
#' @rdname setID-methods
#' @exportMethod setID<-
setGeneric("setID<-", function(this, ..., value) standardGeneric("setID<-"))

#' Set the start date for the claim simulation exercise
#' @name setStartDate<-
#' @param this Self
#' @param ... Additional function arguments
#' @param value Start Date
#' @rdname setStartDate-methods
#' @exportMethod setStartDate<-
setGeneric("setStartDate<-", function(this, ..., value) standardGeneric("setStartDate<-"))

#' Input the raw data.
#' @name setObservation<-
#' @param this FitDist Object or Copula Object
#' @param value A data frame or a matrix. For FitDist object, it could be a two-column data frame with the occurrence date and loss size/number of occurrence. Or a one-column data frame with loss size (ifreq == FALSE) or number of occurrence (ifreq == TRUE && idate == FALSE) or occurrence dates (ifreq == TRUE && idate == TRUE). For Copula object, it could be a matrix with each column contains the experience data of a variable.
#' @rdname setObservation-methods
#' @export setObservation<-
setGeneric("setObservation<-", function(this,value) standardGeneric("setObservation<-"))

setGeneric("toString", function(object, ...) standardGeneric("toString"))

#' Density function.
#' @name Density
#' @param object Distribution Object
#' @param x Variable value
#' @param ... Additional function arguments
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' Density(xPareto,50)
#' @rdname Density-methods
#' @exportMethod Density
setGeneric("Density", function(object, x, ...) standardGeneric("Density"))

#' Probability function.
#' @name Probability
#' @param object Distribution Object
#' @param q Variable value
#' @param ... Additional function arguments
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' Probability(xPareto,50)
#' @rdname Probability-methods
#' @exportMethod Probability
setGeneric("Probability", function(object, q, ...) standardGeneric("Probability"))

#' Quantile function.
#' @name Quantile
#' @param object Distribution Object
#' @param p Probability
#' @param ... Additional function arguments
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' Quantile(xPareto,0.6)
#' @rdname Quantile-methods
#' @export Quantile
setGeneric("Quantile", function(object, p, ...) standardGeneric("Quantile"))

#' Plot function.
#' @name doPlot
#' @param object Object
#' @param ... Additional function arguments
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' doPlot(xPareto)
#' @rdname doPlot-methods
#' @exportMethod doPlot
setGeneric("doPlot", function(object, ...) standardGeneric("doPlot"))

#' Get input data from an object.
#' @name getObservation
#' @param object Object
#' @param ... Additional function arguments
#' @rdname getObservation-methods
#' @exportMethod getObservation
setGeneric("getObservation", function(object,...) standardGeneric("getObservation"))

#' Get the trend index.
#' @name getTrend
#' @param object Object
#' @param ... Additional function arguments
#' @rdname getTrend-methods
#' @exportMethod getTrend
setGeneric("getTrend", function(object,...) standardGeneric("getTrend"))
