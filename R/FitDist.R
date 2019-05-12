#'
#' An S4 class to represent distribution fitting.
#'
#' @slot observation Raw data input containing loss sizes for severity analysis and number of losses for frequency analysis.
#' @slot fitdata Processed data for distribution fitting. Frequency data may be provided as occurrence dates. The class will transform them into frequency data before distribution fitting.
#' @slot trend Index object for detrending the data.
#' @slot startDate Start date of claim data used for distribution fitting. The trend Index should also start from the same date (year-month).
#' @slot endDate End date of claim data used for distribution fitting.
#' @slot trail Trial Distribution object to start fitting.
#' @slot fitted Fitted Distribution object.
#' @slot reportLag Report lag distribution to adjust frequency data.
#' @slot iLag Whether to adjust the frequency data with report lag distribution.
#' @slot method Distribution fitting method. Maximum likelihood estimation (mle), moment matching estimation(mme) and quantile matching estimation(qme) are available.
#' @slot probs A vector containing the percentiles to be matched if qme is used for fitting.
#' @slot ifreq A boolean indicating whether it is frequency data or severity data.
#' @slot idate A boolean indicating whether frequency data is provided as occurrence dates (TRUE) or number of occurrences (FALSE).
#' @slot datelist A vector containing occurrence dates. It could be a data field in a claim file.
#' @slot freq A character string indicating the frequency: "Annual" or "Monthly".
#' @slot iDL A boolean indicating whether deductible and limit is considered in distribution fitting.
#' @slot limit A vector containing the limit for each claim.
#' @slot deductible A vector containing the deductible for each claim.
#' @slot p0 A number that is the probability of having a zero-amount claim after deductible.
#' @slot dof Degree of freedom.
#' @slot psd A vector containing the standard deviation of parameter estimation. It is only available for mle.
#' @slot aic Akaike information criterion.
#' @slot bic Bayesian information criterion.
#' @slot chisq Chi-Squared Test Statistic.
#' @slot pchisq p-value of Chi-Squared Test.
#' @slot kstest K-S Test Statistic. Only used for continuous distribution.
#' @slot pkstest p-value of K-S Test. Only used for continuous distribution.
#' @slot soutput Distribution fitting summary.

setClass("FitDist",
         slots=c(
           observation="data.frame",
           fitdata="vector",
           trend="Index",
           startDate="Date",
           endDate="Date",
           trial="Distribution",
           fitted="Distribution",
           reportLag="Distribution",
           iLag="logical",
           method="character",
           probs="vector",
           ifreq="logical",
           idate="logical",
           freq="character",
           iDL="logical",
           limit="vector",
           deductible="vector",
           p0="numeric",
           dof="numeric",
           psd="vector",
           loglik="numeric",
           aic="numeric",
           bic="numeric",
           chisq="numeric",
           pchisq="numeric",
           kstest="numeric",
           pkstest="numeric",
           soutput="data.frame"
         ),
         prototype=list(
           fitdata=vector(),
           startDate=as.Date("2012-01-01"),
           endDate=as.Date("2016-12-31"),
           trend=new("Index",monthlyIndex=rep(1,360)),
           trial=new("Normal"),
           fitted=new("Normal"),
           reportLag=new("Exponential"),
           iLag=FALSE,
           method="mle",
           ifreq=TRUE,
           idate=FALSE,
           freq="Monthly",
           iDL=FALSE,
           limit=vector(),
           deductible=vector(),
           p0=NaN,
           dof=0,
           psd=vector(),
           aic=0,
           bic=0,
           chisq=0,
           pchisq=0,
           kstest=0,
           pkstest=0,
           soutput=data.frame(matrix(NA, 1, 12))
         )
)

#' @rdname getObservation-methods
#' @aliases getObservation,ANY-method
setMethod("getObservation",signature("FitDist"), function(object) { return(object@observation)})

setGeneric("getFitdata", function(object,...) standardGeneric("getFitdata"))
setMethod("getFitdata",signature("FitDist"), function(object) { return(object@fitdata)})

#' @rdname getTrend-methods
#' @aliases getTrend,ANY-method
setMethod("getTrend",signature("FitDist"), function(object) { return(object@trend)})

setGeneric("getFittedDist", function(object,...) standardGeneric("getFittedDist"))
setMethod("getFittedDist",signature("FitDist"), function(object) { return(object@fitted)})

setGeneric("getMean", function(object,...) standardGeneric("getMean"))
setMethod("getMean",signature("FitDist"), function(object) { return(mean(object@fitdata))})

setGeneric("getSd", function(object,...) standardGeneric("getSd"))
setMethod("getSd",signature("FitDist"), function(object) { return(sd(object@fitdata))})

setGeneric("getDoF", function(object, ...) standardGeneric("getDoF"))
setMethod("getDoF",signature("FitDist"), function(object) {object@dof})

setGeneric("getPSD", function(object, ...) standardGeneric("getPSD"))
setMethod("getPSD",signature("FitDist"), function(object) {object@psd})

setGeneric("getAIC", function(object, ...) standardGeneric("getAIC"))
setMethod("getAIC",signature("FitDist"), function(object) {object@aic})

setGeneric("getBIC", function(object, ...) standardGeneric("getBIC"))
setMethod("getBIC",signature("FitDist"), function(object) {object@bic})

setGeneric("getChiSq", function(object, ...) standardGeneric("getChiSq"))
setMethod("getChiSq",signature("FitDist"), function(object) {object@chisq})

setGeneric("getpChiSq", function(object, ...) standardGeneric("getpChiSq"))
setMethod("getpChiSq",signature("FitDist"), function(object) {object@pchisq})

setGeneric("getiDate", function(object, ...) standardGeneric("getiDate"))
setMethod("getiDate",signature("FitDist"), function(object) {object@idate})

setGeneric("getFreq", function(object, ...) standardGeneric("getFreq"))
setMethod("getFreq",signature("FitDist"), function(object) {object@freq})

setGeneric("getfitmethod", function(object, ...) standardGeneric("getfitmethod"))
setMethod("getfitmethod",signature("FitDist"), function(object) {object@method})

setGeneric("getfoutput", function(object, ...) standardGeneric("getfoutput"))
setMethod("getfoutput",signature("FitDist"), function(object) {object@foutput})

setGeneric("getsoutput", function(object, ...) standardGeneric("getsoutput"))
setMethod("getsoutput",signature("FitDist"), function(object) {object@soutput})

setReplaceMethod("setStartDate",signature("Index", "Date"), function(this, value) {
  this@startDate<- as.Date(value)
  this
})

#' Set the trend with an Index Object.
#' @name setTrend<-
#' @param this FitDist Object
#' @param ... Additional function arguments
#' @param value An Index Object
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' setTrend(xFit) <- findex
#' xFit@trend
#' @rdname setTrend-methods
#' @exportMethod setTrend<-
setGeneric("setTrend<-", function(this, ..., value) standardGeneric("setTrend<-"))
#' @rdname setTrend-methods
#' @aliases setTrend,ANY-method
setReplaceMethod("setTrend",signature("FitDist", "Index"), function(this, value) {
  this@trend<- value
  this
})

#' Set distribution fitting method.
#' @name setfitmethod<-
#' @param this FitDist Object
#' @param ... Additional function arguments
#' @param value A character string: "mle", "mme", or "qme"
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' setfitmethod(xFit) <- "mme"
#' xFit@method
#' @rdname setfitmethod-methods
#' @exportMethod setfitmethod<-
setGeneric("setfitmethod<-", function(this, ..., value) standardGeneric("setfitmethod<-"))
#' @rdname setfitmethod-methods
#' @aliases setfitmethod,ANY-method
setReplaceMethod("setfitmethod",signature("FitDist", "character"), function(this, value) {
  this@method<- value
  this
})

#' Set whether occurrence dates will be used for frequency data.
#' @name setidate<-
#' @param this FitDist Object
#' @param ... Additional function arguments
#' @param value A boolean
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=FALSE, freq="Monthly")
#' setidate(xFit) <- TRUE
#' xFit@idate
#' @rdname setidate-methods
#' @exportMethod setidate<-
setGeneric("setidate<-", function(this, ..., value) standardGeneric("setidate<-"))
#' @rdname setidate-methods
#' @aliases setidate,ANY-method
setReplaceMethod("setidate",signature("FitDist", "logical"), function(this, value) {
  this@idate<- value
  this
})

#' Set the data frequency.
#' @name setfreq<-
#' @param this FitDist Object
#' @param ... Additional function arguments
#' @param value A character string: "Annual" or "Monthly"
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Annual")
#' setfreq(xFit) <- "Monthly"
#' xFit@freq
#' @rdname setfreq-methods
#' @exportMethod setfreq<-
setGeneric("setfreq<-", function(this, ..., value) standardGeneric("setfreq<-"))
#' @rdname setfreq-methods
#' @aliases setfreq,ANY-method
setReplaceMethod("setfreq",signature("FitDist", "character"), function(this, value) {
  this@freq<- value
  this
})

#' Set the data type: frequency or severity/time lag.
#' @name setifreq<-
#' @param this FitDist Object
#' @param ... Additional function arguments
#' @param value A boolean
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' setifreq(xFit) <- FALSE
#' xFit@ifreq
#' @rdname setifreq-methods
#' @exportMethod setifreq<-
setGeneric("setifreq<-", function(this, ..., value) standardGeneric("setifreq<-"))
#' @rdname setifreq-methods
#' @aliases setifreq,ANY-method
setReplaceMethod("setifreq",signature("FitDist", "logical"), function(this, value) {
  this@ifreq<- value
  this
})

#' Set the percentiles to be matched. Only used when qme is chosen for fitting method.
#' @name setprobs<-
#' @param this FitDist Object
#' @param ... Additional function arguments
#' @param value A numeric vector with values between 0 and 1.
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' setprobs(xFit) <- c(0.1,0.5,0.9)
#' xFit@probs
#' @rdname setprobs-methods
#' @exportMethod setprobs<-
setGeneric("setprobs<-", function(this, ..., value) standardGeneric("setprobs<-"))
#' @rdname setprobs-methods
#' @aliases setprobs,ANY-method
setReplaceMethod("setprobs",signature("FitDist", "vector"), function(this, value) {
  this@probs<- value
  this
})

#' @rdname setObservation-methods
#' @aliases setObservation,ANY-method
setReplaceMethod("setObservation",signature("FitDist", "matrix"), function(this, value) {
  if (!is.null(value)){
    if(ncol(value)==1){
      value<-value[value[,1] >= 0]
    } else {
      value<-value[value[,2] >= 0] # assuming we use the first columns even we have more than two columns' data
    }
  }
  value<-value[!is.na(value)]

  this@observation<- value
  this
})

#' Preparing the input data (observation) for distribution fitting, including detrending, translating occurrence dates to frequency data, etc.
#' @name setFitdata
#' @param object FitDist Object
#' @param ... Additional function arguments
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' xFit@fitdata
#' @rdname setFitdata-methods
#' @exportMethod setFitdata
setGeneric("setFitdata", function(object, ...) standardGeneric("setFitdata"))
#' @rdname setFitdata-methods
#' @aliases setFitdata,ANY-method
setMethod("setFitdata",signature("FitDist"), function(object) {

  tryCatch({
    if(ncol(object@observation)==1 && object@idate == FALSE) {
      object@fitdata<-as.numeric(object@observation[,1])
      object@p0 <- sum(object@fitdata <=0)/length(object@fitdata) #p0 will be replaced by the fitted distribution function
      #object@fitdata <- object@fitdata[object@fitdata>0]
    } else if(ncol(object@observation)==1 && object@idate == TRUE && object@ifreq == TRUE) {
      oldcol <- colnames(object@observation)
      object@observation <- as.data.frame(object@observation[object@observation[,1] >= object@startDate,]) #may cause a bug, need test more
      colnames(object@observation) <- oldcol
      if (object@freq == "Annual") {
        tmp <- as.matrix(table(format(as.Date(object@observation[,1]), format = "%Y")))
        tmp <- cbind(tmp,rownames(tmp))
        class(tmp) <- "numeric"
        startYear <- as.numeric(substr(as.character(object@trend@startDate),1,4))
        endYear <- as.numeric(substr(as.character(object@endDate),1,4))
        if(object@iLag==TRUE) {tmp[,1]<-tmp[,1]/Probability(object@reportLag,(endYear-tmp[,2])*365+182.5)}
        di <- (tmp[,2] - startYear)*12+12
        di <- ifelse(di>360,360,ifelse(di<1,1,di))
        tmp[,1] <- tmp[,1]/object@trend@monthlyIndex[di]*object@trend@seasonality[12]
        object@fitdata <- round(as.vector(tmp[,1]))
      } else { #Monthly frequency
        tmp <- as.matrix(table(format(as.Date(object@observation[,1]), format = "%Y-%m")))
        tmp <- cbind(tmp,rownames(tmp))
        tmp <- cbind(tmp,as.numeric(substr(tmp[,2],1,4)),as.numeric(substr(tmp[,2],6,7)))
        tmp <- tmp[,-2]
        class(tmp) <- "numeric"
        startYear <- as.numeric(substr(as.character(object@trend@startDate),1,4))
        startMonth <- as.numeric(substr(as.character(object@trend@startDate),6,7))
        endYear <- as.numeric(substr(as.character(object@endDate),1,4))
        endMonth <- as.numeric(substr(as.character(object@endDate),6,7))
        if(object@iLag==TRUE) {tmp[,1]<-tmp[,1]/Probability(object@reportLag,(endYear-tmp[,2])*365+(endMonth-tmp[,3])*30+15)}
        di <- (tmp[,2] - startYear)*12+(tmp[,3] - startMonth)+1
        di <- ifelse(di>360,360,ifelse(di<1,1,di))
        tmp[,1] <- tmp[,1]/object@trend@monthlyIndex[di]
        object@fitdata <- round(as.vector(tmp[,1]))
      }
    } else if(object@ifreq == FALSE) {
      #object@observation <- object@observation[object@observation[,1] >= object@startDate,]
      startYear <- as.numeric(substr(as.character(object@trend@startDate),1,4))
      startMonth <- as.numeric(substr(as.character(object@trend@startDate),6,7))
      tmp <- cbind(object@observation, as.numeric(substr(as.character(object@observation[,1]),1,4)),as.numeric(substr(as.character(object@observation[,1]),6,7)))
      tmp <- tmp[,-1]
      di <- (tmp[,2] - startYear)*12+(tmp[,3] - startMonth)+1
      di <- ifelse(di>360,360,ifelse(di<1,1,di))
      tmp[,1] <- tmp[,1]/object@trend@monthlyIndex[di]
      object@fitdata <- as.vector(tmp[,1])
      object@p0 <- sum(object@fitdata <=0)/length(object@fitdata) #p0 will be replaced by the fitted distribution function
      #object@fitdata <- object@fitdata[object@fitdata>0]
      if(object@iDL == TRUE) {
        object@deductible <- object@observation[,3]
        object@limit <- object@observation[,4]
        if(sum(ifelse((!is.na(object@limit) & object@limit < object@fitdata), TRUE, FALSE))>0){
          stop("loss is greater than limit for some records. Please check the data.")
        }
      }
    } else {
      object@observation <- object@observation[object@observation[,1] >= object@startDate,]
      if (object@freq == "Annual") {
        startYear <- as.numeric(substr(as.character(object@trend@startDate),1,4))
        tmp <- cbind(object@observation,as.numeric(substr(as.character(object@observation[,1]),1,4)))
        tmp <- tmp[,-1]
        colnames(tmp) <- c("data","year")
        tmp <- aggregate(data ~ year, data=tmp, FUN="sum")
        endYear <- as.numeric(substr(as.character(object@endDate),1,4))
        if(object@iLag==TRUE) {tmp$data<-tmp$data/Probability(object@reportLag,(endYear-tmp$year)*365+182.5)}
        di <- (tmp$year - startYear)*12+12
        di <- ifelse(di>360,360,ifelse(di<1,1,di))
        tmp$data <- tmp$data/object@trend@monthlyIndex[di]*object@trend@seasonality[12]
        object@fitdata <- round(as.vector(tmp$data))
      } else { #Monthly frequency
        startYear <- as.numeric(substr(as.character(object@trend@startDate),1,4))
        startMonth <- as.numeric(substr(as.character(object@trend@startDate),6,7))
        tmp <- cbind(object@observation,substr(as.character(object@observation[,1]),1,7))
        tmp <- tmp[,-1]
        colnames(tmp) <- c("data","ym")
        tmp <- aggregate(data ~ ym, data=tmp, FUN="sum")
        tmp <- cbind(tmp,as.numeric(substr(tmp$ym,1,4)),as.numeric(substr(tmp$ym,6,7)))
        tmp <- tmp[,!colnames(tmp) %in% c("ym")]
        endYear <- as.numeric(substr(as.character(object@endDate),1,4))
        endMonth <- as.numeric(substr(as.character(object@endDate),6,7))
        if(object@iLag==TRUE) {tmp$data<-tmp$data/Probability(object@reportLag,(endYear-tmp[,2])*365+(endMonth-tmp[,3])*30+15)}
        di <- (tmp[,2] - startYear)*12+(tmp[,3] - startMonth)+1
        di <- ifelse(di>360,360,ifelse(di<1,1,di))
        tmp$data <- tmp$data/object@trend@monthlyIndex[di]
        object@fitdata <- round(as.vector(tmp$data))
      }
    }
    gc()
    return(object)
  }, error = function(err){
    message(paste0(">>>Critical Error for distribution fitting: ", err))
    gc()
    return(-1)
  })
})

#' Distribution fitting and testing.
#' @name setTrialDist<-
#' @param this FitDist Object
#' @param value Distribution to fit to
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Poisson")
#' xFit@soutput
#' observationPlot(xFit)
#' fitPlot(xFit)
#' @rdname setTrialDist-methods
#'
#' @importFrom methods new
#' @importFrom fitdistrplus fitdist
#'
#' @exportMethod setTrialDist<-
setGeneric("setTrialDist<-", function(this, value) standardGeneric("setTrialDist<-"))
#' @rdname setTrialDist-methods
#' @aliases setTrialDist,ANY-method
setReplaceMethod("setTrialDist",signature("FitDist", "Distribution"), function(this, value) {
  tryCatch({
    #require(fitdistrplus)
    this@trial <- value

    if (!is.null(this@fitdata)){

      if(this@iDL==TRUE){
        nl <- length(unique(this@limit))
        nl <- max(nl, length(unique(this@deductible)))
      } else {
        nl <- 0
      }

      if(this@iDL==TRUE & nl == 1 & unique(this@deductible)[1] == 0 & sum(this@fitdata>=this@limit)==0) {
        uni <- 1
      } else {
        uni <- 0
      }

      if(fitClassName(value) == "empirical"){
        perc <- seq(0,1,0.001)
        quantiles <- as.vector(quantile(this@fitdata,perc))
        distr <- as.matrix(cbind(perc,quantiles))
        this@fitted<-new(objName(this@trial),
                         min=value@min,
                         max=value@max,
                         empirical=distr,
                         truncated=this@trial@truncated
        )
        this@soutput = data.frame(Distribution=character(),
                                  Method=character(),
                                  Parameter=character(),
                                  SD=character(),
                                  p0=numeric(),
                                  DoF=integer(),
                                  ChiSq=double(),
                                  p=double(),
                                  KS=double(),
                                  pks=double(),
                                  loglik=double(),
                                  AIC=double(),
                                  BIC=double(),
                                  stringsAsFactors=FALSE)

        this@soutput[1,] = c(objName(this@fitted), NA, NA, NA, round(this@p0,4), NA, NA, NA, NA, NA, NA, NA, NA)
      } else if(this@iDL==FALSE | nl==0 | uni==1){
        if (this@trial@truncated==TRUE){
          struncate <- this@trial@truncated
          this@trial@truncated <- FALSE
          memp  <-  function(x, order) mean(x^order)
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), fitStartValue(this@trial), method="mme", discrete=this@ifreq, memp=memp, order=c(1:nParameter(value)))
          startvalue <- list()
          for(par in c(1:length(obj$estimate))){
            startvalue[[names(obj$estimate)[par]]] <- as.numeric(obj$estimate[par])
          }
          this@trial@truncated <- struncate
        } else {
          startvalue <- fitStartValue(this@trial)
        }
        if(this@method == "qme" & this@trial@truncated == FALSE) {
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, method=this@method, probs=this@probs, discrete=this@ifreq)
        } else if (this@method == "qme" & this@trial@truncated == TRUE) {
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, fix.arg=list(min=this@trial@min,max=this@trial@max), method=this@method, probs=this@probs, discrete=this@ifreq)
        } else if (this@method == "mme" & this@trial@truncated == FALSE){
          memp  <-  function(x, order) mean(x^order)
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, method=this@method, discrete=this@ifreq, memp=memp, order=c(1:nParameter(value)))
        } else if (this@method == "mle" & this@trial@truncated == FALSE){
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, method=this@method, discrete=this@ifreq)
        } else {
          this@method <- "mle"
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, fix.arg=list(min=this@trial@min,max=this@trial@max), method=this@method, discrete=this@ifreq)
        }
        this@dof = length(this@fitdata) - nParameter(this@trial)

        if (!is.null(obj$loglik)) {
          this@loglik = obj$loglik
        } else {
          this@loglik = NaN
        }

        if (!is.null(obj$aic)) {
          this@aic = obj$aic
        } else {
          this@aic = NaN
        }

        if (!is.null(obj$bic)) {
          this@bic = obj$bic
        } else {
          this@bic = NaN
        }

        if (this@method=="mle") {
          this@psd = obj$sd
        } else {
          this@psd = vector()
        }

        if (nParameter(value) == 1) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           min=value@min,
                           max=value@max,
                           truncated=this@trial@truncated
          )
        } else if (nParameter(value) == 2) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           p2=as.numeric(obj$estimate[2]),
                           min=value@min,
                           max=value@max,
                           truncated=this@trial@truncated
          )
        } else {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           p2=as.numeric(obj$estimate[2]),
                           p3=as.numeric(obj$estimate[3]),
                           min=value@min,
                           max=value@max,
                           truncated=this@trial@truncated
          )
        }

        if (this@fitted@truncated == TRUE) {
          this@p0 <- Probability(this@fitted,0)
        } else {
          this@p0 <- Probability(this@fitted,this@fitted@min)
        }

        x<-this@fitdata + max(abs(this@fitdata))*0.0001*runif(length(this@fitdata),0,1)
        if(!this@ifreq) {
          par<-unlist(params(this@fitted))
          pname<-paste("p", fitClassName(this@fitted), sep="")
          if (length(par)==1){
            z<-ks.test(x,pname, par)
          }else{
            z<-ks.test(x,pname, par[1], par[2])
          }
          this@kstest <- round(as.numeric(z$statistic),3)
          this@pkstest <- round(z$p.value, 5)
        } else {
          this@kstest <- NaN
          this@pkstest <- NaN
        }

        m = mean(x)
        s=sqrt(var(x))

        mybreak<-c(m-4*s, m-3*s,m-2*s,m-s, m-s/2, m-s/4,m,m+s/4,m+s/2,m+s,m+2*s,m+3*s,m+4*s)
        if(this@ifreq) {
          mybreak <-mybreak[mybreak>=0]
          mybreak<-unique(round(mybreak))
        } else {
          mybreak<-unique(mybreak)
        }
        mycut<-cut(this@fitdata,breaks = mybreak)
        empirical<-as.vector(table(mycut))
        mybreak2<-mybreak[seq(2, length(mybreak), by=1)]
        mybreak1<-mybreak[seq(1, length(mybreak)-1, by=1)]

        prob<- Probability(this@fitted, mybreak2)-Probability(this@fitted, mybreak1)
        z<-chisq.test(empirical, p=prob,  rescale.p=TRUE)
        this@chisq <- round(as.numeric(z$statistic),3)
        this@pchisq <- round(z$p.value,5)

        sdx <- ""
        if(length(this@psd)>0) {
          for (i in c(1:length(this@psd))) {
            sdx <- paste0(sdx,round(this@psd[i],4),"; ")
          }
        } else {
          sdx <- "NA"
        }

        params <- ""

        if(length(params(this@fitted))>0){
          for (i in c(1:length(params(this@fitted)))) {
            params <- paste0(params,names(params(this@fitted))[i],":",round(as.numeric(params(this@fitted)[i]),3),"; ")
          }
        } else {
          params <- "NA"
        }

        this@soutput = data.frame(Distribution=character(),
                                  Method=character(),
                                  Parameter=character(),
                                  SD=character(),
                                  p0=numeric(),
                                  DoF=integer(),
                                  ChiSq=double(),
                                  p=double(),
                                  KS=double(),
                                  pks=double(),
                                  loglik=double(),
                                  AIC=double(),
                                  BIC=double(),
                                  stringsAsFactors=FALSE)

        this@soutput[1,] = c(objName(this@fitted), this@method, params, sdx, round(this@p0,4), round(this@dof,0), round(this@chisq,2), round(this@pchisq,2), round(this@kstest,2), round(this@pkstest,2), round(this@loglik,2), round(this@aic,2), round(this@bic,2))
      } else if(this@iDL==TRUE & nl==1){
        struncate <- this@trial@truncated
        this@trial@truncated <- FALSE
        memp  <-  function(x, order) mean(x^order)
        obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), fitStartValue(this@trial), method="mme", discrete=this@ifreq, memp=memp, order=c(1:nParameter(value)))
        startvalue <- list()
        for(par in c(1:length(obj$estimate))){
          startvalue[[names(obj$estimate)[par]]] <- as.numeric(obj$estimate[par])
        }
        this@trial@truncated <- struncate

        this@trial@truncated <- TRUE
        this@trial@min <- unique(this@deductible)[1]
        this@trial@max <- unique(this@limit)[1]
        this@method <- "mle"
        obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, fix.arg=list(min=this@trial@min,max=this@trial@max), method=this@method, discrete=this@ifreq)

        this@dof = length(this@fitdata) - nParameter(this@trial)

        if (!is.null(obj$loglik)) {
          this@loglik = obj$loglik
        } else {
          this@loglik = NaN
        }

        if (!is.null(obj$aic)) {
          this@aic = obj$aic
        } else {
          this@aic = NaN
        }

        if (!is.null(obj$bic)) {
          this@bic = obj$bic
        } else {
          this@bic = NaN
        }

        if (this@method=="mle") {
          this@psd = obj$sd
        } else {
          this@psd = vector()
        }

        if (nParameter(value) == 1) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        } else if (nParameter(value) == 2) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           p2=as.numeric(obj$estimate[2]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        } else {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           p2=as.numeric(obj$estimate[2]),
                           p3=as.numeric(obj$estimate[3]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        }

        #if (this@fitted@truncated == TRUE) {
        #	this@p0 <- max(0,this@p0 - Probability(this@fitted,0))
        #} else {
        #	this@p0 <- max(0,this@p0 - Probability(this@fitted,this@fitted@min))
        #}

        if (this@fitted@truncated == TRUE) {
          this@p0 <- Probability(this@fitted,0)
        } else {
          this@p0 <- Probability(this@fitted,this@fitted@min)
        }

        x<-this@fitdata + max(abs(this@fitdata))*0.0001*runif(1,0,1)
        if(!this@ifreq) {
          par<-unlist(params(this@fitted))
          pname<-paste("p", fitClassName(this@fitted), sep="")
          if (length(par)==1){
            z<-ks.test(x,pname, par)
          }else{
            z<-ks.test(x,pname, par[1], par[2])
          }
          this@kstest <- round(as.numeric(z$statistic),3)
          this@pkstest <- round(z$p.value, 5)
        } else {
          this@kstest <- NaN
          this@pkstest <- NaN
        }

        m = mean(x)
        s=sqrt(var(x))

        mybreak<-c(m-4*s, m-3*s,m-2*s,m-s, m-s/2, m-s/4,m,m+s/4,m+s/2,m+s,m+2*s,m+3*s,m+4*s)
        if(this@ifreq) {
          mybreak <-mybreak[mybreak>=0]
          mybreak<-unique(round(mybreak))
        } else {
          mybreak<-unique(mybreak)
        }
        mycut<-cut(this@fitdata,breaks = mybreak)
        empirical<-as.vector(table(mycut))
        mybreak2<-mybreak[seq(2, length(mybreak), by=1)]
        mybreak1<-mybreak[seq(1, length(mybreak)-1, by=1)]

        prob<- Probability(this@fitted, mybreak2)-Probability(this@fitted, mybreak1)
        z<-chisq.test(empirical, p=prob,  rescale.p=TRUE)
        this@chisq <- round(as.numeric(z$statistic),3)
        this@pchisq <- round(z$p.value,5)

        sdx <- ""
        if(length(this@psd)>0) {
          for (i in c(1:length(this@psd))) {
            sdx <- paste0(sdx,round(this@psd[i],4),"; ")
          }
        } else {
          sdx <- "NA"
        }

        params <- ""

        if(length(params(this@fitted))>0){
          for (i in c(1:length(params(this@fitted)))) {
            params <- paste0(params,names(params(this@fitted))[i],":",round(as.numeric(params(this@fitted)[i]),3),"; ")
          }
        } else {
          params <- "NA"
        }

        this@soutput = data.frame(Distribution=character(),
                                  Method=character(),
                                  Parameter=character(),
                                  SD=character(),
                                  p0=numeric(),
                                  DoF=integer(),
                                  ChiSq=double(),
                                  p=double(),
                                  KS=double(),
                                  pks=double(),
                                  loglik=double(),
                                  AIC=double(),
                                  BIC=double(),
                                  stringsAsFactors=FALSE)

        this@soutput[1,] = c(objName(this@fitted), this@method, params, sdx, round(this@p0,4), round(this@dof,0), round(this@chisq,2), round(this@pchisq,2), round(this@kstest,2), round(this@pkstest,2), round(this@loglik,2), round(this@aic,2), round(this@bic,2))

        this@fitted@truncated=FALSE
      } else {

        struncate <- this@trial@truncated
        this@trial@truncated <- FALSE
        memp  <-  function(x, order) mean(x^order)
        obj<-fitdist(pmax(0.01,this@fitdata), distr=fitClassName(this@trial), fitStartValue(this@trial), method="mme", discrete=this@ifreq, memp=memp, order=c(1:nParameter(value)))
        startvalue <- vector()
        for(par in c(1:length(obj$estimate))){
          startvalue<-c(startvalue,as.numeric(obj$estimate[par]))
        }

        obj<-optim(par=startvalue, fn=nloglik, dist=this@trial, fitdata=this@fitdata, deductible=this@deductible, limit=this@limit,control=list(trace=1))#,  maxit=300, fnscale=c(-10, -10), parscale=c(10,10), factr=0.001, pgtol=0.01, abstol=0.01, reltol=0.01))

        this@dof = length(this@fitdata) - nParameter(this@trial)

        if (!is.null(obj$value)) {
          this@loglik = -obj$value
        } else {
          this@loglik = NaN
        }

        if (!is.null(obj$value)) {
          this@aic = 2*nParameter(this@trial)+2*obj$value
        } else {
          this@aic = NaN
        }

        if (!is.null(obj$value)) {
          this@bic = length(this@fitdata)*nParameter(this@trial)+2*obj$value
        } else {
          this@bic = NaN
        }

        this@psd = vector()

        if (nParameter(this@trial) == 1) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$par[1]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        } else if (nParameter(value) == 2) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$par[1]),
                           p2=as.numeric(obj$par[2]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        } else {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$par[1]),
                           p2=as.numeric(obj$par[2]),
                           p3=as.numeric(obj$par[3]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        }

        this@p0 <- mean(Probability(this@fitted, this@deductible))

        #this@p0 <- max(0, this@p0 - mean(Probability(this@fitted, this@deductible)))

        this@kstest <- NaN
        this@pkstest <- NaN
        this@chisq <- NaN
        this@pchisq <- NaN

        sdx <- ""
        if(obj$convergence==0) {
          sdx <- "successful convergence"
        } else if (obj$convergence==1){
          sdx <- "max iteration"
        } else {
          sdx <- "failed convergence"
        }

        params <- ""

        if(length(params(this@fitted))>0){
          for (i in c(1:length(params(this@fitted)))) {
            params <- paste0(params,names(params(this@fitted))[i],":",round(as.numeric(params(this@fitted)[i]),5),"; ")
          }
        } else {
          params <- "NA"
        }

        this@soutput = data.frame(Distribution=character(),
                                  Method=character(),
                                  Parameter=character(),
                                  SD=character(),
                                  p0=numeric(),
                                  DoF=integer(),
                                  ChiSq=double(),
                                  p=double(),
                                  KS=double(),
                                  pks=double(),
                                  loglik=double(),
                                  AIC=double(),
                                  BIC=double(),
                                  stringsAsFactors=FALSE)

        this@soutput[1,] = c(objName(this@fitted), this@method, params, sdx, round(this@p0,4), round(this@dof,0), round(this@chisq,2), round(this@pchisq,2), round(this@kstest,2), round(this@pkstest,2), round(this@loglik,2), round(this@aic,2), round(this@bic,2))

        this@fitted@truncated=FALSE

      }
    }
    this@fitted@fitsucc <- TRUE
    this
  }, error = function(err){
    message(paste0(">>>Critical Error for distribution fitting: ", err))
    gc()
    this@fitted@fitsucc <- FALSE
    this@soutput[1,] <- c(value, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    this
  })
})

#' Distribution fitting and testing. Same as setTrialDist except for error tolerance.
#' @name setTrialDistErr<-
#' @param this FitDist Object
#' @param value Distribution to fit to
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDistErr(xFit) <- new("Poisson")
#' xFit@soutput
#' observationPlot(xFit)
#' fitPlot(xFit)
#' @rdname setTrialDistErr-methods
#'
#' @importFrom methods new
#' @importFrom fitdistrplus fitdist
#'
#' @exportMethod setTrialDistErr<-
setGeneric("setTrialDistErr<-", function(this, value) standardGeneric("setTrialDistErr<-"))
#' @rdname setTrialDistErr-methods
#' @aliases setTrialDistErr,ANY-method
setReplaceMethod("setTrialDistErr",signature("FitDist", "Distribution"), function(this, value) {
  tryCatch({
    #require(fitdistrplus)
    this@trial <- value

    if (!is.null(this@fitdata)){
      if(this@iDL==TRUE){
        nl <- length(unique(this@limit))
        nl <- max(nl, length(unique(this@deductible)))
      } else {
        nl <- 0
      }

      if(this@iDL==TRUE & nl == 1 & unique(this@deductible)[1] == 0 & sum(this@fitdata>=this@limit)==0) {
        uni <- 1
      } else {
        uni <- 0
      }

      if(fitClassName(value) == "empirical"){
        perc <- seq(0,1,0.001)
        quantiles <- as.vector(quantile(this@fitdata,perc))
        distr <- as.matrix(cbind(perc,quantiles))
        this@fitted<-new(objName(this@trial),
                         min=value@min,
                         max=value@max,
                         empirical=distr,
                         truncated=this@trial@truncated
        )
        this@soutput = data.frame(Distribution=character(),
                                  Method=character(),
                                  Parameter=character(),
                                  SD=character(),
                                  p0=numeric(),
                                  DoF=integer(),
                                  ChiSq=double(),
                                  p=double(),
                                  KS=double(),
                                  pks=double(),
                                  loglik=double(),
                                  AIC=double(),
                                  BIC=double(),
                                  stringsAsFactors=FALSE)

        this@soutput[1,] = c(objName(this@fitted), NA, NA, NA, round(this@p0,4), NA, NA, NA, NA, NA, NA, NA, NA)
      } else if(this@iDL==FALSE | nl==0 | uni==1){
        if (this@trial@truncated==TRUE){
          struncate <- this@trial@truncated
          this@trial@truncated <- FALSE
          memp  <-  function(x, order) mean(x^order)
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), fitStartValue(this@trial), method="mme", discrete=this@ifreq, memp=memp, order=c(1:nParameter(value)))
          startvalue <- list()
          for(par in c(1:length(obj$estimate))){
            startvalue[[names(obj$estimate)[par]]] <- as.numeric(obj$estimate[par])
          }
          this@trial@truncated <- struncate
        } else {
          startvalue <- fitStartValue(this@trial)
        }
        if(this@method == "qme" & this@trial@truncated == FALSE) {
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, method=this@method, probs=this@probs, discrete=this@ifreq)
        } else if (this@method == "qme" & this@trial@truncated == TRUE) {
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, fix.arg=list(min=this@trial@min,max=this@trial@max), method=this@method, probs=this@probs, discrete=this@ifreq)
        } else if (this@method == "mme" & this@trial@truncated == FALSE){
          memp  <-  function(x, order) mean(x^order)
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, method=this@method, discrete=this@ifreq, memp=memp, order=c(1:nParameter(value)))
        } else if (this@method == "mle" & this@trial@truncated == FALSE){
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, method=this@method, discrete=this@ifreq)
        } else {
          this@method <- "mle"
          obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, fix.arg=list(min=this@trial@min,max=this@trial@max), method=this@method, discrete=this@ifreq)
        }
        this@dof = length(this@fitdata) - nParameter(this@trial)


        if (!is.null(obj$loglik)) {
          this@loglik = obj$loglik
        } else {
          this@loglik = NaN
        }

        if (!is.null(obj$aic)) {
          this@aic = obj$aic
        } else {
          this@aic = NaN
        }

        if (!is.null(obj$bic)) {
          this@bic = obj$bic
        } else {
          this@bic = NaN
        }

        if (this@method=="mle") {
          this@psd = obj$sd
        } else {
          this@psd = vector()
        }

        if (nParameter(value) == 1) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           min=value@min,
                           max=value@max,
                           truncated=this@trial@truncated
          )
        } else if (nParameter(value) == 2) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           p2=as.numeric(obj$estimate[2]),
                           min=value@min,
                           max=value@max,
                           truncated=this@trial@truncated
          )
        } else {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           p2=as.numeric(obj$estimate[2]),
                           p3=as.numeric(obj$estimate[3]),
                           min=value@min,
                           max=value@max,
                           truncated=this@trial@truncated
          )
        }

        if (this@fitted@truncated == TRUE) {
          this@p0 <- Probability(this@fitted,0)
        } else {
          this@p0 <- Probability(this@fitted,this@fitted@min)
        }

        x<-this@fitdata + max(abs(this@fitdata))*0.0001*runif(length(this@fitdata),0,1)
        if(!this@ifreq) {
          par<-unlist(params(this@fitted))
          pname<-paste("p", fitClassName(this@fitted), sep="")
          if (length(par)==1){
            z<-ks.test(x,pname, par)
          }else{
            z<-ks.test(x,pname, par[1], par[2])
          }
          this@kstest <- round(as.numeric(z$statistic),3)
          this@pkstest <- round(z$p.value, 5)
        } else {
          this@kstest <- NaN
          this@pkstest <- NaN
        }

        m = mean(x)
        s=sqrt(var(x))

        mybreak<-c(m-4*s, m-3*s,m-2*s,m-s, m-s/2, m-s/4,m,m+s/4,m+s/2,m+s,m+2*s,m+3*s,m+4*s)
        if(this@ifreq) {
          mybreak <-mybreak[mybreak>=0]
          mybreak<-unique(round(mybreak))
        } else {
          mybreak<-unique(mybreak)
        }
        mycut<-cut(this@fitdata,breaks = mybreak)
        empirical<-as.vector(table(mycut))
        mybreak2<-mybreak[seq(2, length(mybreak), by=1)]
        mybreak1<-mybreak[seq(1, length(mybreak)-1, by=1)]

        prob<- Probability(this@fitted, mybreak2)-Probability(this@fitted, mybreak1)
        z<-chisq.test(empirical, p=prob,  rescale.p=TRUE)
        this@chisq <- round(as.numeric(z$statistic),3)
        this@pchisq <- round(z$p.value,5)

        sdx <- ""
        if(length(this@psd)>0) {
          for (i in c(1:length(this@psd))) {
            sdx <- paste0(sdx,round(this@psd[i],4),"; ")
          }
        } else {
          sdx <- "NA"
        }

        params <- ""

        if(length(params(this@fitted))>0){
          for (i in c(1:length(params(this@fitted)))) {
            params <- paste0(params,names(params(this@fitted))[i],":",round(as.numeric(params(this@fitted)[i]),3),"; ")
          }
        } else {
          params <- "NA"
        }

        this@soutput = data.frame(Distribution=character(),
                                  Method=character(),
                                  Parameter=character(),
                                  SD=character(),
                                  p0=numeric(),
                                  DoF=integer(),
                                  ChiSq=double(),
                                  p=double(),
                                  KS=double(),
                                  pks=double(),
                                  loglik=double(),
                                  AIC=double(),
                                  BIC=double(),
                                  stringsAsFactors=FALSE)

        this@soutput[1,] = c(objName(this@fitted), this@method, params, sdx, round(this@p0,4), round(this@dof,0), round(this@chisq,2), round(this@pchisq,2), round(this@kstest,2), round(this@pkstest,2), round(this@loglik,2), round(this@aic,2), round(this@bic,2))
      } else if(this@iDL==TRUE & nl==1){

        struncate <- this@trial@truncated
        this@trial@truncated <- FALSE
        memp  <-  function(x, order) mean(x^order)
        obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), fitStartValue(this@trial), method="mme", discrete=this@ifreq, memp=memp, order=c(1:nParameter(value)))
        startvalue <- list()
        for(par in c(1:length(obj$estimate))){
          startvalue[[names(obj$estimate)[par]]] <- as.numeric(obj$estimate[par])
        }
        this@trial@truncated <- struncate

        this@trial@truncated <- TRUE
        this@trial@min <- unique(this@deductible)[1]
        this@trial@max <- unique(this@limit)[1]
        this@method <- "mle"
        obj<-fitdist(this@fitdata, distr=fitClassName(this@trial), startvalue, fix.arg=list(min=this@trial@min,max=this@trial@max), method=this@method, discrete=this@ifreq)

        this@dof = length(this@fitdata) - nParameter(this@trial)

        if (!is.null(obj$loglik)) {
          this@loglik = obj$loglik
        } else {
          this@loglik = NaN
        }

        if (!is.null(obj$aic)) {
          this@aic = obj$aic
        } else {
          this@aic = NaN
        }

        if (!is.null(obj$bic)) {
          this@bic = obj$bic
        } else {
          this@bic = NaN
        }

        if (this@method=="mle") {
          this@psd = obj$sd
        } else {
          this@psd = vector()
        }

        if (nParameter(value) == 1) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        } else if (nParameter(value) == 2) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           p2=as.numeric(obj$estimate[2]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        } else {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$estimate[1]),
                           p2=as.numeric(obj$estimate[2]),
                           p3=as.numeric(obj$estimate[3]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        }

        #if (this@fitted@truncated == TRUE) {
        #	this@p0 <- max(0,this@p0 - Probability(this@fitted,0))
        #} else {
        #	this@p0 <- max(0,this@p0 - Probability(this@fitted,this@fitted@min))
        #}

        if (this@fitted@truncated == TRUE) {
          this@p0 <- Probability(this@fitted,0)
        } else {
          this@p0 <- Probability(this@fitted,this@fitted@min)
        }

        x<-this@fitdata + max(abs(this@fitdata))*0.0001*runif(length(this@fitdata),0,1)
        if(!this@ifreq) {
          par<-unlist(params(this@fitted))
          pname<-paste("p", fitClassName(this@fitted), sep="")
          if (length(par)==1){
            z<-ks.test(x,pname, par)
          }else{
            z<-ks.test(x,pname, par[1], par[2])
          }
          this@kstest <- round(as.numeric(z$statistic),3)
          this@pkstest <- round(z$p.value, 5)
        } else {
          this@kstest <- NaN
          this@pkstest <- NaN
        }

        m = mean(x)
        s=sqrt(var(x))

        mybreak<-c(m-4*s, m-3*s,m-2*s,m-s, m-s/2, m-s/4,m,m+s/4,m+s/2,m+s,m+2*s,m+3*s,m+4*s)
        if(this@ifreq) {
          mybreak <-mybreak[mybreak>=0]
          mybreak<-unique(round(mybreak))
        } else {
          mybreak<-unique(mybreak)
        }
        mycut<-cut(this@fitdata,breaks = mybreak)
        empirical<-as.vector(table(mycut))
        mybreak2<-mybreak[seq(2, length(mybreak), by=1)]
        mybreak1<-mybreak[seq(1, length(mybreak)-1, by=1)]

        prob<- Probability(this@fitted, mybreak2)-Probability(this@fitted, mybreak1)
        z<-chisq.test(empirical, p=prob,  rescale.p=TRUE)
        this@chisq <- round(as.numeric(z$statistic),3)
        this@pchisq <- round(z$p.value,5)

        sdx <- ""
        if(length(this@psd)>0) {
          for (i in c(1:length(this@psd))) {
            sdx <- paste0(sdx,round(this@psd[i],4),"; ")
          }
        } else {
          sdx <- "NA"
        }

        params <- ""

        if(length(params(this@fitted))>0){
          for (i in c(1:length(params(this@fitted)))) {
            params <- paste0(params,names(params(this@fitted))[i],":",round(as.numeric(params(this@fitted)[i]),3),"; ")
          }
        } else {
          params <- "NA"
        }

        this@soutput = data.frame(Distribution=character(),
                                  Method=character(),
                                  Parameter=character(),
                                  SD=character(),
                                  p0=numeric(),
                                  DoF=integer(),
                                  ChiSq=double(),
                                  p=double(),
                                  KS=double(),
                                  pks=double(),
                                  loglik=double(),
                                  AIC=double(),
                                  BIC=double(),
                                  stringsAsFactors=FALSE)

        this@soutput[1,] = c(objName(this@fitted), this@method, params, sdx, round(this@p0,4), round(this@dof,0), round(this@chisq,2), round(this@pchisq,2), round(this@kstest,2), round(this@pkstest,2), round(this@loglik,2), round(this@aic,2), round(this@bic,2))
        this@fitted@truncated=FALSE
      } else {

        struncate <- this@trial@truncated
        this@trial@truncated <- FALSE
        memp  <-  function(x, order) mean(x^order)
        obj<-fitdist(pmax(0.01,this@fitdata), distr=fitClassName(this@trial), fitStartValue(this@trial), method="mme", discrete=this@ifreq, memp=memp, order=c(1:nParameter(value)))
        startvalue <- vector()
        for(par in c(1:length(obj$estimate))){
          startvalue<-c(startvalue,as.numeric(obj$estimate[par]))
        }

        obj<-optim(par=startvalue, fn=nloglik, dist=this@trial, fitdata=this@fitdata, deductible=this@deductible, limit=this@limit)#,control=list(trace=1))#,  maxit=300, fnscale=c(10, 10), parscale=c(10,10), factr=0.001, pgtol=0.01, abstol=0.01, reltol=0.01))

        this@dof = length(this@fitdata) - nParameter(this@trial)

        if (!is.null(obj$value)) {
          this@loglik = -obj$value
        } else {
          this@loglik = NaN
        }

        if (!is.null(obj$value)) {
          this@aic = 2*nParameter(this@trial)+2*obj$value
        } else {
          this@aic = NaN
        }

        if (!is.null(obj$value)) {
          this@bic = length(this@fitdata)*nParameter(this@trial)+2*obj$value
        } else {
          this@bic = NaN
        }

        this@psd <- vector()

        if (nParameter(this@trial) == 1) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$par[1]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        } else if (nParameter(value) == 2) {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$par[1]),
                           p2=as.numeric(obj$par[2]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        } else {
          this@fitted<-new(objName(this@trial),
                           p1=as.numeric(obj$par[1]),
                           p2=as.numeric(obj$par[2]),
                           p3=as.numeric(obj$par[3]),
                           min=this@trial@min,
                           max=this@trial@max,
                           truncated=this@trial@truncated
          )
        }

        this@p0 <- mean(Probability(this@fitted, this@deductible))

        #this@p0 <- max(0, this@p0 - mean(Probability(this@fitted, this@deductible)))

        this@kstest <- NaN
        this@pkstest <- NaN
        this@chisq <- NaN
        this@pchisq <- NaN

        sdx <- ""
        if(obj$convergence==0) {
          sdx <- "successful convergence"
        } else if (obj$convergence==1){
          sdx <- "max iteration"
        } else {
          sdx <- "failed convergence"
        }

        params <- ""

        if(length(params(this@fitted))>0){
          for (i in c(1:length(params(this@fitted)))) {
            params <- paste0(params,names(params(this@fitted))[i],":",round(as.numeric(params(this@fitted)[i]),5),"; ")
          }
        } else {
          params <- "NA"
        }

        this@soutput = data.frame(Distribution=character(),
                                  Method=character(),
                                  Parameter=character(),
                                  SD=character(),
                                  p0=numeric(),
                                  DoF=integer(),
                                  ChiSq=double(),
                                  p=double(),
                                  KS=double(),
                                  pks=double(),
                                  loglik=double(),
                                  AIC=double(),
                                  BIC=double(),
                                  stringsAsFactors=FALSE)

        this@soutput[1,] = c(objName(this@fitted), this@method, params, sdx, round(this@p0,4), round(this@dof,0), round(this@chisq,2), round(this@pchisq,2), round(this@kstest,2), round(this@pkstest,2), round(this@loglik,2), round(this@aic,2), round(this@bic,2))

        this@fitted@truncated=FALSE

      }
    }
    this@fitted@fitsucc <- TRUE
    this
  }, error = function(err){
    #message(paste0(">>>Critical Error for distribution fitting: ", err))
    gc()
    this@soutput[1,] <- c(value, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    this@fitted@fitsucc <- FALSE
    this
  })
})

#' Directly set the fitted distribution without fitting it to the data.
#' @name setFittedDist<-
#' @param this FitDist Object
#' @param value Fitted distribution
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Poisson")
#' xFit@fitted
#' @rdname setFittedDist-methods
#' @exportMethod setFittedDist<-
setGeneric("setFittedDist<-", function(this, value) standardGeneric("setFittedDist<-"))
#' @rdname setFittedDist-methods
#' @aliases setFittedDist,ANY-method
setReplaceMethod("setFittedDist",signature("FitDist", "Distribution"), function(this, value) {
  this@fitted<- value
  this
})

#' Compare the raw data and fitted distribution on density, CDF, Q-Q plot and P-P plot
#' @name fitPlot
#' @param object FitDist Object
#' @param ... Additional function arguments
#' @param n Number of samples, should not be used in current setting
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Poisson")
#' xFit@soutput
#' fitPlot(xFit)
#' @rdname fitPlot-methods
#' @exportMethod fitPlot
setGeneric("fitPlot", function(object, ...) standardGeneric("fitPlot"))
#' @rdname fitPlot-methods
#' @aliases fitPlot,ANY-method
setMethod("fitPlot",signature("FitDist"), function(object, n=missing) {
  tryCatch({
    par(mfrow = c(2, 2))
    x<-getFitdata(object)

    #draw some basic features with fitted fitted, for a good visual compare
    if(missing(n) || n==1){
      y<-object@fitted
      if(object@iDL==TRUE){
        avgDeductible <- mean(object@deductible)
        avgLimit <- mean(object@limit)
        y@min <- avgDeductible
        y@max <- avgLimit
        y@truncated <- TRUE
      }
      d0<-density(x)#,bw=1e-5
      d1<-density(doSample(y, length(object@fitdata)),na.rm=TRUE)#,bw=1e-5
      plot(range(d0$x, d1$x), range(d0$y, d1$y), type = "n", xlab = "x", ylab = "Density", main = "Probability Density Function")
      lines(d0, col = "blue")
      lines(d1, col = "red")
      #		legend("topright",c("observed",objName(y)),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"))

      ysample<-doSample(y, 1000)
      plot(ecdf(x), main = "Cumulative Distribution Function", col="blue")
      d1<-ecdf(ysample)
      plot(d1, col="red", add=TRUE) #type="l",
      legend("bottomright",c("observed",objName(y)),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"))

      #		hist(ysample,breaks=15, main="Histogram of the Fitted Result", xlab="Fitted", ylab="Frequency", col="red")

      qqplot(object@fitdata, doSample(y, length(object@fitdata)), ylim=(c(min(object@fitdata), max(object@fitdata))), main="Q-Q Plot", xlab = "Observed Quantile", ylab = "Fitted Quantile")
      abline(0, 1)

      probDist <- Probability(y, object@fitdata)
      plot(ppoints(length(object@fitdata)), sort(probDist), main = "P-P Plot", xlab = "Observed Probability", ylab = "Expected Probability")
      abline(0, 1)

    }
  }, error = function(err){
    message(paste0(">>>Critical Error for plotting fitted distribution: ", err))
    gc()
  })


})

#' Plotting the data for distribution fitting
#' @name observationPlot
#' @param object FitDist Object
#' @param ... Additional function arguments
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Poisson")
#' xFit@soutput
#' observationPlot(xFit)
#' @rdname observationPlot-methods
#' @exportMethod observationPlot
setGeneric("observationPlot", function(object, ...) standardGeneric("observationPlot"))
#' @rdname observationPlot-methods
#' @aliases observationPlot,ANY-method
setMethod("observationPlot",signature("FitDist"), function(object) {
  tryCatch({
    par(mfrow = c(2, 2))
    x<-getFitdata(object)

    #draw some basic features
    plot(density(x), main = "Observation Empirical Density", col = "blue")
    plot(ecdf(x), main = "Observation Empirical CDF", col="blue")
    hist(x,breaks=15, main="Histogram of the Observation", xlab="observations", ylab="Frequency", col="blue")
    plot(object@trend@monthlyIndex, xlab="Time", ylab="Index")
  }, error = function(err){
    message(paste0(">>>Critical Error for plotting observation data: ", err))
    gc()
  })

})


#' Plotting the PDF of data and fitted distribution
#' @name PDFPlot
#' @param object FitDist Object
#' @param ... Additional function arguments
#' @param n Number of samples, should not be used in current setting
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Poisson")
#' xFit@soutput
#' PDFPlot(xFit)
#' @rdname PDFPlot-methods
#' @export PDFPlot
setGeneric("PDFPlot", function(object, ...) standardGeneric("PDFPlot"))
#' @rdname PDFPlot-methods
#' @aliases PDFPlot,ANY-method
setMethod("PDFPlot",signature("FitDist"), function(object, n=missing) {
  tryCatch({
    par(mfrow = c(1, 1))
    x<-getFitdata(object)

    if(missing(n) || n==1){
      y<-object@fitted

      d0<-density(x)
      d1<-density(doSample(y, length(object@fitdata)))
      plot(range(d0$x, d1$x), range(d0$y, d1$y), type = "n", xlab = "x", ylab = "Density", main = "Probability Density Function")
      lines(d0, col = "blue")
      lines(d1, col = "red")
      legend("topright",c("observed","fitted"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"))
    }
  }, error = function(err){
    message(paste0(">>>Critical Error for PDF plot in distribution fitting: ", err))
    gc()
  })
})

#' Plotting the CDF of data and fitted distribution
#' @name CDFPlot
#' @param object FitDist Object
#' @param ... Additional function arguments
#' @param n Number of samples, should not be used in current setting
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Poisson")
#' xFit@soutput
#' CDFPlot(xFit)
#' @rdname CDFPlot-methods
#' @exportMethod CDFPlot
setGeneric("CDFPlot", function(object, ...) standardGeneric("CDFPlot"))
#' @rdname CDFPlot-methods
#' @aliases CDFPlot,ANY-method
setMethod("CDFPlot",signature("FitDist"), function(object, n=missing) {
  tryCatch({
    par(mfrow = c(1, 1))
    x<-getFitdata(object)

    if(missing(n) || n==1){
      y<-object@fitted

      ysample<-doSample(y, 1000)
      plot(ecdf(x), main = "Cumulative Distribution Fun.", col="blue")
      d1<-ecdf(ysample)
      plot(d1, col="red", add=TRUE) #type="l",
      legend("bottomright",c("observed",objName(y)),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"))

    }
  }, error = function(err){
    message(paste0(">>>Critical Error for CDF plot in distribution fitting: ", err))
    gc()
  })
})

#' Q-Q Plot of data and fitted distribution
#' @name QQPlot
#' @param object FitDist Object
#' @param ... Additional function arguments
#' @param n Number of samples, should not be used in current setting
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Poisson")
#' xFit@soutput
#' QQPlot(xFit)
#' @rdname QQPlot-methods
#' @exportMethod QQPlot
setGeneric("QQPlot", function(object, ...) standardGeneric("QQPlot"))
#' @rdname QQPlot-methods
#' @aliases QQPlot,ANY-method
setMethod("QQPlot",signature("FitDist"), function(object, n=missing) {
  tryCatch({
    par(mfrow = c(1, 1))
    if(missing(n) || n==1){
      qqplot(object@fitdata, doSample(object@fitted, length(object@fitdata)), ylim=(c(min(object@fitdata), max(object@fitdata))), main="Q-Q Plot", xlab = "Observed Quantile", ylab = "Fitted Quantile")
    }

    abline(0, 1)
  }, error = function(err){
    message(paste0(">>>Critical Error for QQ plot in distribution fitting: ", err))
    gc()
  })
})

#' P-P Plot of data and fitted distribution
#' @name PPPlot
#' @param object FitDist Object
#' @param ... Additional function arguments
#' @param n Number of samples, should not be used in current setting
#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, monthlyIndex = c(rep(1,11),
#' cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Poisson")
#' xFit@soutput
#' observationPlot(xFit)
#' PPPlot(xFit)
#' @rdname PPPlot-methods
#' @exportMethod PPPlot
setGeneric("PPPlot", function(object, ...) standardGeneric("PPPlot"))
#' @rdname PPPlot-methods
#' @aliases PPPlot,ANY-method
setMethod("PPPlot",signature("FitDist"), function(object, n=missing) {
  tryCatch({
    par(mfrow = c(1, 1))
    if(missing(n) || n==1){
      probDist <- Probability(object@fitted, object@fitdata)
      plot(ppoints(length(object@fitdata)), sort(probDist), main = "P-P Plot", xlab = "Observed Probability", ylab = "Expected Probability")
      #qqplot(object@observation, doSample(object@fitted, length(object@observation)), ylim=(c(min(object@observation), max(object@observation))), main="QQ-Plot observation vs. fitted distr")
    }

    abline(0, 1)
  }, error = function(err){
    message(paste0(">>>Critical Error for PP plot in distribution fitting: ", err))
    gc()
  })
})

#' K-S Test
#' @name KSTest
#' @param object FitDist Object
#' @param ... Additional function arguments
#' @param n Number of samples, should not be used in current setting
#' @rdname KSTest-methods
setGeneric("KSTest", function(object, ...) standardGeneric("KSTest"))
#' @rdname KSTest-methods
#' @aliases KSTest,ANY-method
setMethod("KSTest",signature("FitDist"), function(object, n=missing) {
  tryCatch({
    if(missing(n) || n==1){
      #let us do some prepare for ks.test
      x<-object@fitdata + max(abs(object@fitdata))*0.0001*runif(length(object@fitdata),0,1)

      par<-unlist(params(object@fitted))
      pname<-paste("p", fitClassName(object@fitted), sep="")

      if (length(par)==1){
        z<-ks.test(x,pname, par)
      }else{
        z<-ks.test(x,pname, par[1], par[2])
      }

      return (paste("Statistic=", round(as.numeric(z$statistic),3), ", P-Value=", round(z$p.value, 5), ", Alternative Hypothesis: ", z$alternative, sep=""))
    }
    gc()
  }, error = function(err){
    message(paste0(">>>Critical Error for K-S Test: ", err))
    gc()
  })
})

#' Chi-Squared Test
#' @name ChiSqrTest
#' @param object FitDist Object
#' @param ... Additional function arguments
#' @rdname ChiSqrTest-methods
setGeneric("ChiSqrTest", function(object, ...) standardGeneric("ChiSqrTest"))
#' @rdname ChiSqrTest-methods
#' @aliases ChiSqrTest,ANY-method
setMethod("ChiSqrTest",signature("FitDist"), function(object) {

  tryCatch({
    m = mean(object@fitdata);
    s=sqrt(var(object@fitdata))

    mybreak<-c(m-4*s, m-3*s,m-2*s,m-s, m-s/2, m-s/4,m,m+s/4,m+s/2,m+s,m+2*s,m+3*s,m+4*s)

    if(object@ifreq) {
      mybreak <-mybreak[mybreak>=0]
      mybreak<-unique(round(mybreak))
    } else {
      mybreak<-unique(mybreak)
    }

    mycut<-cut(object@fitdata,breaks = mybreak)
    empirical<-as.vector(table(mycut))
    mybreak2<-mybreak[seq(2, length(mybreak), by=1)]
    mybreak1<-mybreak[seq(1, length(mybreak)-1, by=1)]

    prob<- Probability(object@fitted, mybreak2)-Probability(object@fitted, mybreak1)

    z<-chisq.test(empirical, p=prob,  rescale.p=TRUE)
    return (paste("X-Squared=", round(as.numeric(z$statistic), 3), ", df=", round(as.numeric(z$parameter), 5), ", P-Value=", round(z$p.value,5),sep=""))
    gc()
  }, error = function(err){
    message(paste0(">>>Critical Error for Chi-Squared Test: ", err))
    gc()
  })
})
