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

#' Sample Claim Data
#'
#' A dataset containing about 10,000 simulated claim records from 2012 to 2016 for illustration.
#' The variables are as follows:
#'
#' \itemize{
#'   \item ClaimID. Claim ID
#'   \item LoB. Line of Business (Auto, Liab, Property)
#'   \item Type. Claim Type (N: Normal, H: High)
#'   \item status. Current Claim Status (Closed, Open)
#'   \item occurrenceDate. Claim Occurrence Date
#'   \item reportDate. Claim Report Date
#'   \item incurredLoss. Incurred Loss. For closed claim, it is the ultimate loss. For open claim, it is the estimated or booked loss.
#'   \item osRatio. Outstanding Ratio
#'   \item settlementDate. Claim Settlement Date.
#'   \item Paid. Paid Loss by the valuation date. It equals incurredLoss * (1-osRatio)
#'   \item totalLoss. Total loss before deductible and limit. If not available, it will be set as incurredLoss and not used for fitting.
#'   \item Deductible. Deductible applied to the claim.
#'   \item Limit. Limit applied to the claim.
#'   \item LAE. Loss adjustment expense at the claim level. It can be omitted if idemnity and LAE are modeled together as incurred loss.
#'   \item claimLiability. Indicating whether the claim is invalid and leads to zero payment. It excludes valid claims that are smaller than deductibles.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name claimdata
#' @usage data(claimdata)
#' @format A data frame with 10030 rows and 15 variables
NULL