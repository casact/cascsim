#' An S4 class to represent a loss development schedule.
#'
#' @slot FacID A character string to identify the loss development schedule.
#' @slot FacModel A boolean to indicate whether the loss development schedule is described as a model (TRUE) or a list of value (FALSE).
#' @slot fun A character string that indicates the model format in link function. Currently identity(linear), inverse(reciprocal linear), log(exponential), and exponential(loglinear) link functions(models) are supported. It is only used when model == TRUE.
#' @slot distType A character string that indicates the distribution of development factors. Currently normal, lognormal, and gamma distributions are supported. It is only used when model == FALSE.
#' @slot xname A vector that includes the names of explanatory variables. They will have to be matched exactly to the claim data file. It is only used when model == TRUE.
#' @slot paras A vector that contains the parameters of the model. It is only used when model == TRUE.
#' @slot meanList A vector that contains the mean yearly development factor if distribution type is Normal. It is mu for Lognormal distribution and shape for Gamma distribution. It is only used when model == FALSE.
#' @slot volList A vector that contains the volatility of yearly development factor if distribution type is Normal. It is sigma for Lognormal distribution and scale for Gamma distribution. It is used for simulating IBNER factors. It is only used when model == FALSE. 

setClass("DevFac", 
	slots=c(
			FacID="character",
			FacModel="logical",
			fun="character",
			distType="character",
			xname="vector",
			paras="vector",
			meanList="vector",
			volList="vector"
	),
	prototype=list(	
			FacID="XXXXXX",
			FacModel=FALSE,
			fun="identity",
			distType="normal",
			xname=vector(),
			paras=vector(),
			meanList=vector(),
			volList=vector()
	)
)

setReplaceMethod("setID",signature("DevFac", "character"), function(this, value) { 
	this@FacID<- as.character(value)
	this
})

#' Determine whether the development factor is determined by a predictive model or a fixed schedule by development year
#' @param this DevFac Object
#' @param value Logical Value (default:FALSE)
#' @rdname setFacModel
#' @export
setGeneric("setFacModel<-", function(this,value, ...) standardGeneric("setFacModel<-"))
setReplaceMethod("setFacModel",signature("DevFac", "logical"), function(this, value) { 
	this@FacModel<- value
	this
})

#' Set the model format/link function (identity/inverse/log/exponential).
#' Only used when FacModel == TRUE.
#' @param this DevFac Object
#' @param value String Value (default:"identity")
#' @rdname setFun
#' @export
setGeneric("setFun<-", function(this,value, ...) standardGeneric("setFun<-"))
setReplaceMethod("setFun",signature("DevFac", "character"), function(this, value) { 
	this@fun<- value
	this
})

#' @title Set additional explanatory variable names.
#' @description
#' \code{setXname<-} sets explanatory variable names in addition to "Intercept","DevelopmentYear","IncurredLoss", and "OSRatio". Additional variable names must match exactly with claim data. The xname vector is only used when ibnerfModel == TRUE.
#' @param this DevFac Object
#' @param value Character Vector
#' @rdname setXname
#' @export
setGeneric("setXname<-", function(this,value, ...) standardGeneric("setXname<-"))
setReplaceMethod("setXname",signature("DevFac", "vector"), function(this, value) { 
	this@xname<- value
	this@FacModel<- TRUE
	this
})

#' @title Set the values of model parameters.
#' @description
#' \code{setParas<-} sets model parameters. Their order must match the order of c("Intercept","DevelopmentYear","IncurredLoss","OSRatio",xname,"Volatility"). "Volatility" stands for the volatility of the error term in the model and used to simulate IBNER development factors. The parameter vector is only used when ibnerfModel == TRUE.
#' @param this DevFac Object
#' @param value Numeric Vector
#' @rdname setParas
#' @export
setGeneric("setParas<-", function(this,value, ...) standardGeneric("setParas<-"))
setReplaceMethod("setParas",signature("DevFac", "vector"), function(this, value) { 
	this@paras<- value
	this@FacModel<- TRUE
	this
})

#' @title Set the year-to-year loss development factor.
#' @description
#' \code{setMeanList<-} sets expected year-to-year loss development factor. Years after It is only used when ibnerfModel == FALSE.
#' @param this DevFac Object
#' @param value Numeric Vector
#' @rdname setMeanList
#' @export
setGeneric("setMeanList<-", function(this,value, ...) standardGeneric("setMeanList<-"))
setReplaceMethod("setMeanList",signature("DevFac", "vector"), function(this, value) { 
	this@meanList<- value
	this@FacModel<- FALSE
	this
})

#' @title Set the year-to-year loss development factor volatility.
#' @description
#' \code{setMeanList<-} sets year-to-year loss development factor volatility. It is used to simulate loss development factor assuming a normal distribution. It can be set to zero for deterministic estimation. It is only used when ibnerfModel == FALSE.
#' @param this DevFac Object
#' @param value Numeric Vector
#' @rdname setVolList
#' @export
setGeneric("setVolList<-", function(this,value, ...) standardGeneric("setVolList<-"))
setReplaceMethod("setVolList",signature("DevFac", "vector"), function(this, value) { 
	this@volList<- value
	this@FacModel<- FALSE
	this
})

#' @title Set up an IBNER loss development schedule.
#' @description
#' \code{setDevFac} sets a loss development schedule, from either a predictive model or a year-to-year factor vector.
#' @param object DevFac Object
#' @examples
#' xIBNERFactor <- new("DevFac", FacID = "IF1", FacModel = FALSE, meanList = c(1.26,1.1,1.05,1.02,1), 
#' volList = rep(0.02,5))
#' xIBNERFactor<-setDevFac(xIBNERFactor)
#' xIBNERFactor
#' 
#' xIBNERFactor <- new("DevFac")
#' setID(xIBNERFactor)<-"IF1"
#' setFacModel(xIBNERFactor)<-TRUE
#' setFun(xIBNERFactor)<-"identity"
#' setXname(xIBNERFactor)<- c("x1","x2","x3")
#' setParas(xIBNERFactor)<-c(0.6,-0.2,0.01,-0.3,0.02,0.03,0.01,0.02)
#' xIBNERFactor<-setDevFac(xIBNERFactor)
#' xIBNERFactor
#' @rdname setDevFac
#' @export
setGeneric("setDevFac", function(object,...) standardGeneric("setDevFac"))
setMethod("setDevFac",signature("DevFac"), function(object) {
	tryCatch({
		xnamelen <- length(object@xname)
		paraslen <- length(object@paras)
		if (object@FacModel == TRUE && (paraslen-xnamelen)!=5) {
			stop(paste0("DevFac ",object@ibnerfID,": paras and xname does not match. paras contains parameters for variables in the order of Intercept, DevelopmentYear, IncurredLoss, OSRatio ,variables in xname, and Volatility."))
		}

		if (sum(object@meanList<0)>0){
			stop(paste0("DevFac ",object@ibnerfID,": year-to-year development factor in meanList cannot be negative."))		
		}

		if (sum(object@volList<0)>0){
			stop(paste0("DevFac ",object@ibnerfID,": volatility of year-to-year development factors in volList cannot be negative."))		
		}

		gc()
		object
	}, error = function(err){
		print(paste0(">>>Critical Error: ", err))
		gc()
		return(-1)
	})

})

setMethod("toString",signature("DevFac"), function(object) {
	if(object@FacModel==TRUE){
		ns<-c("DevelopmentYear","IncurredLoss","OSRatio",object@xname,"e")
		result<-as.character(round(object@paras[1],4))
		for(i in c(2:length(object@paras))){
			result<-paste0(result,"+",round(object@paras[i],4),"*",ns[i-1])
		}
		if(object@fun=="inverse"){
			result<-paste0("1/(",result,")")
		}
		if(object@fun=="exponential"){
			result<-paste0("log(",result,")")
		}
		if(object@fun=="log"){
			result<-paste0("exp(",result,")")
		}		
	}else{
		result<-rbind(object@meanList,object@volList)
		if (object@distType == "normal"){
			rownames(result)<-c("mean","vol")
		} else if (object@distType == "lognormal") {
			rownames(result)<-c("meanlog","sdlog")		
		} else {
			rownames(result)<-c("shape","scale")				
		}
		colnames(result)<-c(1:length(object@meanList))
	}
	return(result)
})
