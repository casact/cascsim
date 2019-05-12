#' An S4 class to represent a time index for frequency or severity distribution.
#'
#' @slot indexID A string to identify the index.
#' @slot startDate The date the index starts. It is expected to be consistent with the start date of the claim analysis.
#' @slot tabulate A boolean to indicate whether the index is determined by a constant rate (FALSE) or a series of index values (TRUE).
#' @slot annualizedRate A yearly index growth rate. It is only used when tabulate == FALSE.
#' @slot yearlyIndex A vector that contains index value on a yearly basis.
#' @slot monthlyIndex A vector that contains index value on a monthly basis.
#' @slot seasonality A vector that contains seasonal adjustment factor on a monthly basis.

setClass("Index",
	slots=c(
			indexID="character",
			startDate="Date",
			tabulate="logical",
			annualizedRate="numeric",
			yearlyIndex="vector",
			monthlyIndex="vector",
			seasonality="vector"
	),
	prototype=list(
			indexID="XXXXXX",
			startDate=as.Date("2012-01-01"),
			tabulate=FALSE,
			annualizedRate=0.02,
			yearlyIndex=vector(),
			monthlyIndex=vector(),
			seasonality=rep(1,12)
	)
)

#' @rdname setID-methods
#' @aliases setID,ANY-method
setReplaceMethod("setID",signature("Index", "character"), function(this, value) {
	this@indexID<- as.character(value)
	this
})

#' @rdname setStartDate-methods
#' @aliases setStartDate,ANY-method
setReplaceMethod("setStartDate",signature("Index", "Date"), function(this, value) {
	this@startDate<- as.Date(value)
	this
})

#' Determine whether the index values are constructed from a constant rate or provided directly
#' @name setTabulate<-
#' @param this Index Object
#' @param ... Additional function arguments
#' @param value Logical Value (default:FALSE)
#' @examples
#' xindex <- new("Index")
#' setID(xindex)<-"IDX1"
#' setTabulate(xindex)<-FALSE
#' setAnnualizedRate(xindex)<-0.03
#' xindex<-setIndex(xindex)
#' xindex@monthlyIndex
#' @rdname setTabulate-methods
#' @exportMethod setTabulate<-
setGeneric("setTabulate<-", function(this, ..., value) standardGeneric("setTabulate<-"))
#' @rdname setTabulate-methods
#' @aliases setTabulate,ANY-method
setReplaceMethod("setTabulate",signature("Index", "logical"), function(this, value) {
	this@tabulate<- value
	this
})

#' Set the annualized level rate to construct the index.
#' Only used when tabulate == FALSE.
#' @name setAnnualizedRate<-
#' @param this Index Object
#' @param ... Additional function arguments
#' @param value Numeric Value (default:0.02)
#' @examples
#' xindex <- new("Index")
#' setID(xindex)<-"IDX1"
#' setTabulate(xindex)<-FALSE
#' setAnnualizedRate(xindex)<-0.03
#' xindex<-setIndex(xindex)
#' xindex@monthlyIndex
#' @rdname setAnnualizedRate-methods
#' @exportMethod setAnnualizedRate<-
setGeneric("setAnnualizedRate<-", function(this, ..., value) standardGeneric("setAnnualizedRate<-"))
#' @rdname setAnnualizedRate-methods
#' @aliases setAnnualizedRate,ANY-method
setReplaceMethod("setAnnualizedRate",signature("Index", "numeric"), function(this, value) {
	this@annualizedRate<- value
	this
})

#' @title Set yearly index values.
#' @description
#' \code{setYearlyIndex<-} sets yearly index values. Monthly index will be constructed assuming constant growth rate during a year.
#' @name setYearlyIndex<-
#' @param this Index Object
#' @param ... Additional function arguments
#' @param value Numeric Vector
#' @examples
#' xindex <- new("Index")
#' setID(xindex)<-"IDX1"
#' setTabulate(xindex)<-TRUE
#' setYearlyIndex(xindex)<- c(1,1.05,1.2,0.95,1.3)
#' xindex@yearlyIndex
#' @rdname setYearlyIndex-methods
#' @exportMethod setYearlyIndex<-
setGeneric("setYearlyIndex<-", function(this, ..., value) standardGeneric("setYearlyIndex<-"))
#' @rdname setYearlyIndex-methods
#' @aliases setYearlyIndex,ANY-method
setReplaceMethod("setYearlyIndex",signature("Index", "vector"), function(this, value) {
	this@yearlyIndex<- value
	this@tabulate<- TRUE
	this
})

#' @title Set monthly index values.
#' @description
#' \code{setMonthlyIndex<-} sets monthly index values.
#' @name setMonthlyIndex<-
#' @param this Index Object
#' @param ... Additional function arguments
#' @param value Numeric Vector
#' @examples
#' xindex <- new("Index")
#' setID(xindex)<-"IDX1"
#' setTabulate(xindex)<-TRUE
#' setMonthlyIndex(xindex)<- rep(1,360)
#' xindex<-setIndex(xindex)
#' xindex@monthlyIndex
#' @rdname setMonthlyIndex-methods
#' @exportMethod setMonthlyIndex<-
setGeneric("setMonthlyIndex<-", function(this, ..., value) standardGeneric("setMonthlyIndex<-"))
#' @rdname setMonthlyIndex-methods
#' @aliases setMonthlyIndex,ANY-method
setReplaceMethod("setMonthlyIndex",signature("Index", "vector"), function(this, value) {
	this@monthlyIndex<- value
	this@tabulate<- TRUE
	this
})

#' @title Set seasonality on a monthly basis.
#' @description
#' \code{setSeasonality<-} sets monthly multiplier to reflect seasonal impact.
#' @name setSeasonality<-
#' @param this Index Object
#' @param ... Additional function arguments
#' @param value Numeric Vector (default:rep(1,12))
#' @examples
#' xindex <- new("Index")
#' setID(xindex)<-"IDX1"
#' setTabulate(xindex)<-TRUE
#' setAnnualizedRate(xindex)<-0.03
#' setYearlyIndex(xindex)<- c(1,1.05,1.2,0.95,1.3)
#' set.seed(123)
#' setSeasonality(xindex)<-rnorm(12,mean=1,sd=0.03)
#' xindex<-setIndex(xindex)
#' xindex@monthlyIndex
#' @rdname setSeasonality-methods
#' @exportMethod setSeasonality<-
setGeneric("setSeasonality<-", function(this, ..., value) standardGeneric("setSeasonality<-"))
#' @rdname setSeasonality-methods
#' @aliases setSeasonality,ANY-method
setReplaceMethod("setSeasonality",signature("Index", "vector"), function(this, value) {
	this@seasonality<- value
	this
})

#' @title Set up a time index for frequency or severity.
#' @description
#' \code{setIndex} sets a time index to reflect inflation, underwriting cycle or seasonality.
#' @name setIndex
#' @param object Index Object
#' @param ... Additional function arguments
#' @examples
#' xindex <- new("Index", indexID = "IDX1", tabulate = FALSE, annualizedRate = 0.03)
#' xindex<-setIndex(xindex)
#' xindex@monthlyIndex
#'
#' xindex <- new("Index")
#' setID(xindex)<-"IDX1"
#' setTabulate(xindex)<-TRUE
#' setAnnualizedRate(xindex)<-0.03
#' setYearlyIndex(xindex)<- c(1,1.05,1.2,0.95,1.3)
#' set.seed(123)
#' setSeasonality(xindex)<-rnorm(12,mean=1,sd=0.03)
#' xindex<-setIndex(xindex)
#' xindex@monthlyIndex
#' @rdname setIndex-methods
#' @exportMethod setIndex
setGeneric("setIndex", function(object,...) standardGeneric("setIndex"))
#' @rdname setIndex-methods
#' @aliases setIndex,ANY-method
setMethod("setIndex",signature("Index"), function(object) {
	tryCatch({
		yearlylen <- length(object@yearlyIndex)
		if (object@tabulate == FALSE) {
			object@monthlyIndex<- cumprod(c(1, rep((1+object@annualizedRate)^(1/12),359)))
			for (i in c(1:length(object@monthlyIndex))) {
				mth <- i %% 12
				if (mth == 0) {mth<-12}
				object@monthlyIndex[i] <- object@monthlyIndex[i]*object@seasonality[mth]
			}
		} else if (length(object@monthlyIndex)==0 && yearlylen>0) {
			if (yearlylen<30) {
				warning(paste0("Index ",object@indexID,": yearly index input is less than 30 years and is extrapolated using annualized rate."))
				object@yearlyIndex <- c(object@yearlyIndex, cumprod(c(object@yearlyIndex[yearlylen]*(1+object@annualizedRate),rep(1+object@annualizedRate,30-yearlylen-1))))
			}

			if (sum(object@yearlyIndex<0)>0){
				stop("yearlyIndex cannot be negative.")
			}

			object@monthlyIndex <- rep(1,360)
			object@monthlyIndex[1] <- object@yearlyIndex[1]
			for (i in c(2:length(object@monthlyIndex))) {
				yr <- ceiling(i/12)
				rte <- (object@yearlyIndex[yr]/object@yearlyIndex[max(1,yr-1)])^(1/12)
				#print(rte)
				object@monthlyIndex[i] <- object@monthlyIndex[i-1]*rte
			}
			for (i in c(1:length(object@monthlyIndex))) {
				mth <- i %% 12
				if (mth == 0) {mth<-12}
				object@monthlyIndex[i] <- object@monthlyIndex[i]*object@seasonality[mth]
			}
		} else if (length(object@monthlyIndex)==0) {
			warning(paste0("Index ",object@indexID,": No index value provided. Index value will be set to 1 with seasonal adjustment."))
			object@monthlyIndex<- rep(1,360)
			for (i in c(1:length(object@monthlyIndex))) {
				mth <- i %% 12
				if (mth == 0) {mth<-12}
				object@monthlyIndex[i] <- object@monthlyIndex[i]*object@seasonality[mth]
			}
		}

		if (sum(object@monthlyIndex<0)>0){
			stop("monthlyIndex cannot be negative.")
		}


		gc()
		object
	}, error = function(err){
		message(paste0(">>>Critical Error for ", "Index ", object@indexID, ": ", err))
		gc()
		return(-1)
	})

})

#' @title Retrieve index value based on dates.
#' @description
#' \code{getIndex} get a time index to reflect inflation, underwriting cycle or seasonality.
#' @name getIndex
#' @param object Index Object
#' @param ... Additional function arguments
#' @examples
#' xindex <- new("Index", indexID = "IDX1", tabulate = FALSE, annualizedRate = 0.03)
#' xindex<-setIndex(xindex)
#' xindex@monthlyIndex
#' dates<-as.Date("2015-12-31")
#' getIndex(xindex,dates)
#' @rdname getIndex-methods
#' @exportMethod getIndex
setGeneric("getIndex", function(object,...) standardGeneric("getIndex"))
#' @param dates dates to get index information
#' @rdname getIndex-methods
#' @aliases getIndex,ANY-method
setMethod("getIndex",signature("Index"), function(object,dates) {
	tryCatch({
		years <- as.numeric(substr(as.character(dates),1,4))
		months <- as.numeric(substr(as.character(dates),6,7))
		startyear <- as.numeric(substr(as.character(object@startDate),1,4))
		startmonth <- as.numeric(substr(as.character(object@startDate),6,7))
		indices <- pmax(1,pmin(360,(years-startyear)*12+(months-startmonth)+1))
		gc()
		return(object@monthlyIndex[indices])
	}, error = function(err){
		message(paste0(">>>Critical Error for getting index values", "Index ", object@indexID, ": ", err))
		gc()
		return(-1)
	})
})

#' @title Shift monthly index with a new start date and replace the unknown index value with zero.
#' @name shiftIndex
#' @param object Index Object
#' @param ... Additional function arguments
#' @examples
#' xindex <- new("Index", indexID = "IDX1", tabulate = FALSE, annualizedRate = 0.03)
#' xindex<-setIndex(xindex)
#' xindex@monthlyIndex
#' shiftIndex(xindex,as.Date("2016-10-15"),as.Date("2018-10-15"))
#' shiftIndex(xindex,as.Date("2010-10-15"),as.Date("2013-10-15"))
#' @rdname shiftIndex-methods
#' @exportMethod shiftIndex
setGeneric("shiftIndex", function(object,...) standardGeneric("shiftIndex"))
#' @param newStartDate new start date
#' @param endDate end date
#' @rdname shiftIndex-methods
#' @aliases shiftIndex,ANY-method
setMethod("shiftIndex",signature("Index"), function(object,newStartDate,endDate) {
	tryCatch({
		newyear <- as.numeric(substr(as.character(newStartDate),1,4))
		newmonth <- as.numeric(substr(as.character(newStartDate),6,7))
		endyear <- as.numeric(substr(as.character(endDate),1,4))
		endmonth <- as.numeric(substr(as.character(endDate),6,7))
		startyear <- as.numeric(substr(as.character(object@startDate),1,4))
		startmonth <- as.numeric(substr(as.character(object@startDate),6,7))
		sindex <- (newyear-startyear)*12+(newmonth-startmonth)+1
		nlen <- (endyear-newyear)*12+(endmonth-newmonth)+1
		newMI <- c(rep(0,max(0,1-sindex)),object@monthlyIndex[max(1,sindex):min(length(object@monthlyIndex),length(object@monthlyIndex)+sindex-1)])
		if(nlen>length(newMI)){
			newMI <- c(newMI,rep(0,nlen-length(newMI)))
		} else {
			newMI <- newMI[1:nlen]
		}
		gc()
		return(newMI)
	}, error = function(err){
		message(paste0(">>>Critical Error for shifting index values", "Index ", object@indexID, ": ", err))
		gc()
		return(-1)
	})
})

setMethod("toString",signature("Index"), function(object) { return(paste("Index ", object@indexID, " start date=", object@startDate, sep=""))})
