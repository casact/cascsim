#' Set the ID for an object
#' @param this Self
#' @param value ID
#' @rdname setID
#' @export
setGeneric("setID<-", function(this,value, ...) standardGeneric("setID<-"))

#' Set the start date for the claim simulation exercise
#' @param this Self
#' @param value Start Date
#' @rdname setStartDate
#' @export
setGeneric("setStartDate<-", function(this,value, ...) standardGeneric("setStartDate<-"))

#' Input the raw data.
#' @param this FitDist Object or Copula Object
#' @param value A data frame or a matrix. For FitDist object, it could be a two-column data frame with the occurrence date and loss size/number of occurrence. Or a one-column data frame with loss size (ifreq == FALSE) or number of occurrence (ifreq == TRUE && idate == FALSE) or occurrence dates (ifreq == TRUE && idate == TRUE). For Copula object, it could be a matrix with each column contains the experience data of a variable.
#' @rdname setObservation
#' @export
setGeneric("setObservation<-", function(this,value) standardGeneric("setObservation<-"))

#' Object explanation.
#' @param object Object
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' toString(xPareto)
#' @rdname toString
#' @export
setGeneric("toString", function(object, ...) standardGeneric("toString"))

#' Density function.
#' @param object Distribution Object
#' @param x Variable value
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' Density(xPareto,50)
#' @rdname Density
#' @export
setGeneric("Density", function(object, x, ...) standardGeneric("Density"))

#' Probability function.
#' @param object Distribution Object
#' @param q Variable value
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' Probability(xPareto,50)
#' @rdname Probability
#' @export
setGeneric("Probability", function(object, q, ...) standardGeneric("Probability"))

#' Quantile function.
#' @param object Distribution Object
#' @param p Probability
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' Quantile(xPareto,0.6)
#' @rdname Quantile
#' @export
setGeneric("Quantile", function(object, p, ...) standardGeneric("Quantile"))

#' Plot function.
#' @param object Object
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' doPlot(xPareto)
#' @rdname doPlot
#' @export
setGeneric("doPlot", function(object, ...) standardGeneric("doPlot"))

#' Get input data from an object.
#' @param object Object
#' @rdname getObservation
#' @export
setGeneric("getObservation", function(object,...) standardGeneric("getObservation"))

#' Get the trend index.
#' @param object Object
#' @rdname getTrend
#' @export
setGeneric("getTrend", function(object,...) standardGeneric("getTrend"))
