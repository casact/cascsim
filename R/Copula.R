#' An S4 class to represent a copula object to model the correlation.
#'
#' @slot type The type of the copula object.
#' @slot para A numeric vector that contains copula parameter(s).
#' @slot marginal A list of Distribution objects.
#' @slot dispstr The format of symmetric positive definite matrix used by elliptical copula (Normal Copula, t Copula). The default is "un" for unstructured. Other choices include "ex" for exchangeable, "ar1" for AR(1), and "toep" for Toeplitz (toeplitz).
#' @slot df The number of degrees of freedom used in t Copula.
#' @slot observation A matrix that contains the experience data for copula fitting.
#' @slot fitmethod The method of copula fitting. Default is "mpl":maximum pseudo-likelihood estimator. Others include "ml": maximum likelihood assuming it is the true distribution; "itau": inversion of Kendall’s tau estimator; "irho": inversion of Spearman’s rho estimator.
#' @slot fittest Whether to run goodness of fit test for copula fitting. Goodness of fit test could take a long time to finish.
#' @slot fitsucc Whether a copula fitting is successful.
#' @slot coutput Goodness of fit results.
#' @slot info A character string that contains additional information of the copula to identify line/type/frequency/time lag/severity.

setClass("CopulaObj",
	slots=c(
		type="character",
		param="numeric",
		marginal="list",
		dimension="numeric",
		dispstr="character",
		df="numeric",
		observation="matrix",
		fitmethod="character",
		fittest="logical",
		coutput="data.frame",
		fitsucc="logical",
		info="character"
	),
	prototype=list(
		type="normal",
		param=c(0),
		marginal=list(),
		dimension=2,
		dispstr="un",
		df=3,
		fitmethod="mpl",
		fittest=FALSE,
		fitsucc=FALSE,
		info=""
	)
)

#' Set copula type.
#' @name setCopulaType<-
#' @param this Copula Object
#' @param ... Additional function arguments
#' @param value The copula type
#' @examples
#' library(cascsim)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' cop <- new("CopulaObj", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
#' setCopulaType(cop) <- "joe"
#' @rdname setCopulaType-methods
#' @exportMethod setCopulaType<-
setGeneric("setCopulaType<-", function(this, ..., value) standardGeneric("setCopulaType<-"))
#' @rdname setCopulaType-methods
#' @aliases setCopulaType,ANY-method
setReplaceMethod("setCopulaType",signature("CopulaObj", "character"), function(this, value) {
	this@type <- value
	this
})

#' Set copula parameters.
#' @name setCopulaParam<-
#' @param this Copula Object
#' @param ... Additional function arguments
#' @param value The copula parameters
#' @examples
#' library(cascsim)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' cop <- new("CopulaObj", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
#' setCopulaParam(cop) <- 0.6
#' @rdname setCopulaParam-methods
#' @exportMethod setCopulaParam<-
setGeneric("setCopulaParam<-", function(this, ..., value) standardGeneric("setCopulaParam<-"))
#' @rdname setCopulaParam-methods
#' @aliases setCopulaParam,ANY-method
setReplaceMethod("setCopulaParam",signature("CopulaObj", "numeric"), function(this, value) {
	this@param<- value
	this
})

#' Set the marginal distributions of the copula.
#' @name setMarginal<-
#' @param this Copula Object
#' @param ... Additional function arguments
#' @param value The list of marginal distributions.
#' @examples
#' library(cascsim)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' cop <- new("CopulaObj", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
#' dist3<-new("Pareto",p1=10,p2=3)
#' dist4<-new("Normal",p1=2,p2=3,min=0,max=20,truncated=TRUE)
#' setMarginal(cop) <- list(dist1=dist3,dist2=dist4)
#' @rdname setMarginal-methods
#' @exportMethod setMarginal<-
setGeneric("setMarginal<-", function(this, ..., value) standardGeneric("setMarginal<-"))
#' @rdname setMarginal-methods
#' @aliases setMarginal,ANY-method
setReplaceMethod("setMarginal",signature("CopulaObj", "list"), function(this, value) {
	this@marginal<- value
	this@dimension<-length(value)
	this
	})

#' Set the dimension of the copula.
#' @name setDimension<-
#' @param this Copula Object
#' @param ... Additional function arguments
#' @param value The dimension of the copula. It can also be set by providing marginal distributions
#' @examples
#' library(cascsim)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' cop <- new("CopulaObj", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
#' dist3<-new("Pareto",p1=10,p2=3)
#' setDimension(cop) <- 3
#' setMarginal(cop) <- list(dist1=dist1,dist2=dist2,dist3=dist3)
#' @rdname setDimension-methods
#' @exportMethod setDimension<-
setGeneric("setDimension<-", function(this, ..., value) standardGeneric("setDimension<-"))
#' @rdname setDimension-methods
#' @aliases setDimension,ANY-method
setReplaceMethod("setDimension",signature("CopulaObj", "numeric"), function(this, value) {
	this@dimension<- max(2,value)
	this
})

#' Set parameter matrix format of Elliptical copula.
#' @name setDispstr<-
#' @param this Copula Object
#' @param ... Additional function arguments
#' @param value The matrix format. The default is "un" for unstructured. Other choices include "ex" for exchangeable, "ar1" for AR(1), and "toep" for Toeplitz (toeplitz).
#' @examples
#' library(cascsim)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' cop <- new("CopulaObj", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
#' setDispstr(cop) <- "ex"
#' @rdname setDispstr-methods
#' @exportMethod setDispstr<-
setGeneric("setDispstr<-", function(this, ..., value) standardGeneric("setDispstr<-"))
#' @rdname setDispstr-methods
#' @aliases setDispstr,ANY-method
setReplaceMethod("setDispstr",signature("CopulaObj", "character"), function(this, value) {
	this@dispstr<- value
	this
})

#' Set the degree of freedom for t Copula.
#' @name setDf<-
#' @param this Copula Object
#' @param ... Additional function arguments
#' @param value The degree of freedom. The default value is 3.
#' @examples
#' library(cascsim)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' cop <- new("CopulaObj", type="t", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
#' setDf(cop) <- 5
#' @rdname setDf-methods
#' @exportMethod setDf<-
setGeneric("setDf<-", function(this, ..., value) standardGeneric("setDf<-"))
#' @rdname setDf-methods
#' @aliases setDf,ANY-method
setReplaceMethod("setDf",signature("CopulaObj", "numeric"), function(this, value) {
	this@df<- value
	this
})

#' @rdname setObservation-methods
#' @aliases setObservation,ANY-method
setReplaceMethod("setObservation",signature("CopulaObj", "matrix"), function(this, value) {
	this@observation<- value
	this
})

#' Get the R copula object.
#' @name getCopula
#' @param object R copula object
#' @param ... Additional parameters that may or may not be used
#' @examples
#' library(cascsim)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' nom.cop <- new("CopulaObj", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
#' getCopula(nom.cop)
#' @rdname getCopula-methods
#'
#' @importFrom copula archmCopula ellipCopula
#'
#' @exportMethod getCopula
setGeneric("getCopula", function(object, ...) standardGeneric("getCopula"))
#' @rdname getCopula-methods
#' @aliases getCopula,ANY-method
setMethod("getCopula", signature("CopulaObj"), function(object)
{
	tryCatch({
		#require(copula)
		if (object@type=="normal" || object@type=="t"){
			obj<-ellipCopula(object@type, param=object@param, dim = object@dimension, dispstr = object@dispstr, df = object@df)
		}
		else{
			obj<-archmCopula(object@type, param=object@param, dim = object@dimension)
		}
		gc()
		return(obj)
	}, error = function(err){
		message(paste0(">>>Critical Error for copula construction: ", object@info,", ",err))
		gc()
		return(-1)
	})
})

#' Copula sampling. It will generate correlated variables or percentiles when marginal distributions are not specified.
#' @name copulaSample
#' @param object Copula Object
#' @param n Number of samples
#' @param ... Additional parameters that may or may not be used
#' @examples
#' library(cascsim)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' nom.cop <- new("CopulaObj", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
#' copulaSample(nom.cop,100)
#' @rdname copulaSample-methods
#' @exportMethod copulaSample
setGeneric("copulaSample", function(object, n, ...) standardGeneric("copulaSample"))
#' @rdname copulaSample-methods
#' @aliases copulaSample,ANY-method
setMethod("copulaSample", signature("CopulaObj","numeric"), function(object, n)
{
	tryCatch({
		#require(copula)
		obj<-getCopula(object)
		cp <- rCopula(n, obj)
		if(length(object@marginal) < 2) {
			#warning(paste0("Copula ",object@info,", marginal distributions have not been set up. Percentiles will be returned."))
			colnames(cp) <- paste("Marginal",c(1:ncol(cp)))
			return (cp)
		} else {
			cp <- rCopula(n, obj)
			if(ncol(cp)==length(object@marginal)){
				coln <- vector()
				for (i in c(1:ncol(cp))){
					cp[,i] <- Quantile(object@marginal[[i]],cp[,i])
					coln <- c(coln,objName(object@marginal[[i]]))
				}
				colnames(cp) <- coln
				return(cp)
			} else {
				#warning(paste0("Copula ",object@info,", marginal distributions have not been set up. Percentiles will be returned."))
				colnames(cp) <- paste("Marginal",c(1:ncol(cp)))
				return(cp)
			}
		}
		gc()
	}, error = function(err){
		message(paste0(">>>Critical Error for copula sampling: ", object@info,", ",err))
		gc()
		return(-1)
	})
})

#' Copula plotting. Only for 2 or 3 variables
#' @name copulaPlot
#' @param object Copula Object
#' @param ... Additional parameters that may or may not be used
#' @examples
#' library(cascsim)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' nom.cop <- new("CopulaObj", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
#' copulaPlot(nom.cop)
#' @rdname copulaPlot-methods
#' @import scatterplot3d
#' @exportMethod copulaPlot
setGeneric("copulaPlot", function(object, ...) standardGeneric("copulaPlot"))
#' @rdname copulaPlot-methods
#' @aliases copulaPlot,ANY-method
setMethod("copulaPlot", signature("CopulaObj"), function(object)
{
	samples<-copulaSample(object, 1000)
	par(mfrow = c(1, 1))
	if (object@dimension==2)
	{
		plot(samples, col="blue")
	}
	else if (object@dimension>=2)
	{
		#require(scatterplot3d)
		scatterplot3d(samples[,1:3], color="blue")
	}
})


setMethod("toString",signature("CopulaObj"), function(object)
{
	return(paste("Copula type=", object@type, " info=", object@info, " dim=", length(object@marginal), " param=c(", paste(object@param, collapse=";"), ") marginal=list(", paste(unlist(lapply(object@marginal, toString)), collapse=";") , ") df=", object@df, " dispstr=", object@dispstr, sep=""))
})

#' Experience data plotting.
#' @name copulaDataPlot
#' @param object Copula Object
#' @param ... Additional parameters that may or may not be used
#' @examples
#' library(cascsim)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' nom.cop <- new("CopulaObj", param=c(0.5),marginal=list(dist1=dist1,dist2=dist2),dimension=2)
#' setObservation(nom.cop)<-copulaSample(nom.cop,100)
#' copulaDataPlot(nom.cop)
#' @rdname copulaDataPlot-methods
#' @import scatterplot3d
#' @exportMethod copulaDataPlot
setGeneric("copulaDataPlot", function(object, ...) standardGeneric("copulaDataPlot"))
#' @rdname copulaDataPlot-methods
#' @aliases copulaDataPlot,ANY-method
setMethod("copulaDataPlot",signature("CopulaObj"), function(object) {
	par(mfrow = c(1,1))
	if (nrow(object@observation) >0 && ncol(object@observation) < 3){
		plot(object@observation, main = "Correlated Observation Plot", col = "blue")
	}else if (nrow(object@observation) >0 && ncol(object@observation) >= 3){
		#require(scatterplot3d)
		scatterplot3d(object@observation[,1:3], main = "Correlated Observation Plot", color = "blue")
	} else {
		plotText("The observation data is not available")
	}
})

#' Copula fitting
#' @name copulaFit
#' @param object Copula Object
#' @param ... Additional parameters that may or may not be used
#' @examples
#' library(cascsim)
#' #Prepare pseudo observation data
#' library(copula)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' dist3<-new("Lognormal",p1=2,p2=1,min=0,max=100,truncated=TRUE)
#' normal.cop <- normalCopula(c(0.6, 0.36, 0.6), dim=3, dispstr="un")
#' x <- rCopula(1000, normal.cop)
#' x[,1]<-Quantile(dist1,x[,1])
#' x[,2]<-Quantile(dist2,x[,2])
#' x[,3]<-Quantile(dist3,x[,3])
#' #Create Copula Object and Fit it to observation data without goodness of fit test
#' nom.cop <- new("CopulaObj", param=c(0.5,0.5,0.5),marginal=list(dist1=dist1,dist2=dist2,dist3=dist3),
#' dimension=3,observation=x,fittest=FALSE)
#' nom.cop <- copulaFit(nom.cop)
#' nom.cop@coutput
#' #Create Copula Object and Fit it to observation data with goodness of fit test
#' clayton.cop <- claytonCopula(c(3), dim=2)
#' x <- rCopula(1000, clayton.cop)
#' x[,1]<-Quantile(dist1,x[,1])
#' x[,2]<-Quantile(dist2,x[,2])
#' cla.cop <- new("CopulaObj", type="clayton",param=c(3),
#' marginal=list(dist1=dist1,dist2=dist2),dimension=2,observation=x,fittest=TRUE)
#' cla.cop <- copulaFit(cla.cop)
#' cla.cop@coutput
#' @rdname copulaFit-methods
#'
#' @importFrom copula fitCopula gofCopula pobs rCopula
#'
#' @exportMethod copulaFit
setGeneric("copulaFit", function(object, ...) standardGeneric("copulaFit"))
#' @rdname copulaFit-methods
#' @aliases copulaFit,ANY-method
setMethod("copulaFit",signature("CopulaObj"), function(object)
{
	tryCatch({
		#require(copula)
		if(nrow(object@observation)>0){
			u <- pobs(object@observation)
			fitcop <- fitCopula(getCopula(object), u, method=object@fitmethod, hideWarnings = TRUE)
			para <- coef(fitcop)
			if (object@type == "t") {
				object@param <- para[1:length(para)-1]
				object@df <- para[length(para)]
			} else {
				object@param <- para
			}

			sdx <- coef(fitcop,SE=TRUE)[,2]

			if (object@fittest==TRUE){
				gof <- gofCopula(getCopula(object), u, N=200, estim.method=object@fitmethod, simulation="mult", method="Sn", ties=FALSE, hideWarnings = TRUE)

				object@coutput = data.frame(Copula=character(),
									Method=character(),
									Parameter=character(),
									SD=character(),
									DoF=integer(),
									Sn=double(),
									p=double(),
									stringsAsFactors=FALSE)

				object@coutput[1,] = c(object@type, object@fitmethod, paste(round(object@param,4),collapse=';'), paste(round(sdx,4),collapse=';'), object@df, round(gof$statistic,4), round(gof$p.value,4))
			} else {
				object@coutput = data.frame(Copula=character(),
									Method=character(),
									Parameter=character(),
									SD=character(),
									DoF=integer(),
									Sn=double(),
									p=double(),
									stringsAsFactors=FALSE)

				object@coutput[1,] = c(object@type, object@fitmethod, paste(round(object@param,4),collapse=';'), paste(round(sdx,4),collapse=';'), object@df, NA, NA)
			}
			object@fitsucc <- TRUE
			return(object)
		} else {
			warning(paste0("Copula object is not fed with experience data. Copula is not fitted. ", object@info))
		}
		gc()
	}, warning = function(war){
		message(paste0(">>>Warning for copula fitting: ", object@info,", ",war))
		gc()
		object@fitsucc <- FALSE
		return(object)
	}, error = function(err){
		message(paste0(">>>Critical Error for copula fitting: ", object@info,", ",err))
		gc()
		object@coutput = data.frame(Copula=character(),
							Method=character(),
							Parameter=character(),
							SD=character(),
							DoF=integer(),
							Sn=double(),
							p=double(),
							stringsAsFactors=FALSE)

		object@coutput[1,] <- c(object@type, object@fitmethod, paste(round(object@param,4),collapse=';'), paste(round(sdx,4),collapse=';'), object@df, NA, NA)
		object@fitsucc <- FALSE
		return(object)
	})
})

setGeneric("copulaFitErr", function(object, ...) standardGeneric("copulaFitErr"))
setMethod("copulaFitErr",signature("CopulaObj"), function(object)
{
	tryCatch({
		#require(copula)
		if(nrow(object@observation)>0){
			u <- pobs(object@observation)
			fitcop <- fitCopula(getCopula(object), u, method=object@fitmethod, hideWarnings = TRUE)
			para <- coef(fitcop)
			if (object@type == "t") {
				object@param <- para[1:length(para)-1]
				object@df <- para[length(para)]
			} else {
				object@param <- para
			}

			sdx <- coef(fitcop,SE=TRUE)[,2]

			if (object@fittest==TRUE){
				gof <- gofCopula(getCopula(object), u, N=200, estim.method=object@fitmethod, simulation="mult", method="Sn", ties=FALSE, hideWarnings = TRUE)

				object@coutput = data.frame(Copula=character(),
									Method=character(),
									Parameter=character(),
									SD=character(),
									DoF=integer(),
									Sn=double(),
									p=double(),
									stringsAsFactors=FALSE)

				object@coutput[1,] = c(object@type, object@fitmethod, paste(round(object@param,4),collapse=';'), paste(round(sdx,4),collapse=';'), object@df, round(gof$statistic,4), round(gof$p.value,4))
			} else {
				object@coutput = data.frame(Copula=character(),
									Method=character(),
									Parameter=character(),
									SD=character(),
									DoF=integer(),
									Sn=double(),
									p=double(),
									stringsAsFactors=FALSE)

				object@coutput[1,] = c(object@type, object@fitmethod, paste(round(object@param,4),collapse=';'), paste(round(sdx,4),collapse=';'), object@df, NA, NA)
			}
			object@fitsucc <- TRUE
			return(object)
		} else {
			warning(paste0("Copula object is not fed with experience data. Copula is not fitted. ", object@info))
		}
		gc()
	}, warning = function(war){
		#message(paste0(">>>Warning for copula fitting: ", object@info,", ",war))
		gc()
		object@fitsucc <- FALSE
		return(object)
	}, error = function(err){
		#message(paste0(">>>Critical Error for copula fitting: ", object@info,", ",err))
		gc()
		object@coutput = data.frame(Copula=character(),
							Method=character(),
							Parameter=character(),
							SD=character(),
							DoF=integer(),
							Sn=double(),
							p=double(),
							stringsAsFactors=FALSE)

		object@coutput[1,] <- c(object@type, object@fitmethod, paste(round(object@param,4),collapse=';'), paste(round(sdx,4),collapse=';'), object@df, NA, NA)
		object@fitsucc <- FALSE
		return(object)
	})
})

#' Visualization Copula fitting
#' @name copulaFitPlot
#' @param object Copula Object
#' @param ... Additional parameters that may or may not be used
#' @examples
#' library(cascsim)
#' #Prepare pseudo observation data
#' library(copula)
#' dist1<-new("Pareto",p1=20,p2=3)
#' dist2<-new("Normal",p1=5,p2=3,min=0,max=20,truncated=TRUE)
#' dist3<-new("Lognormal",p1=2,p2=1,min=0,max=100,truncated=TRUE)
#' normal.cop <- normalCopula(c(0.6, 0.36, 0.6), dim=3, dispstr="un")
#' x <- rCopula(1000, normal.cop)
#' x[,1]<-Quantile(dist1,x[,1])
#' x[,2]<-Quantile(dist2,x[,2])
#' x[,3]<-Quantile(dist3,x[,3])
#' #Create Copula Object and Fit it to observation data without goodness of fit test
#' nom.cop <- new("CopulaObj", param=c(0.5,0.5,0.5),marginal=list(dist1=dist1,dist2=dist2,dist3=dist3),
#' dimension=3,observation=x,fittest=FALSE)
#' nom.cop <- copulaFit(nom.cop)
#' copulaFitPlot(nom.cop)
#' #Create Copula Object and Fit it to observation data with goodness of fit test
#' clayton.cop <- claytonCopula(c(3), dim=2)
#' x <- rCopula(1000, clayton.cop)
#' x[,1]<-Quantile(dist1,x[,1])
#' x[,2]<-Quantile(dist2,x[,2])
#' cla.cop <- new("CopulaObj", type="clayton",param=c(3),marginal=list(dist1=dist1,dist2=dist2),
#' dimension=2,observation=x,fittest=TRUE)
#' cla.cop <- copulaFit(cla.cop)
#' copulaFitPlot(cla.cop)
#' @rdname copulaFitPlot-methods
#' @import scatterplot3d
#' @exportMethod copulaFitPlot
setGeneric("copulaFitPlot", function(object, ...) standardGeneric("copulaFitPlot"))
#' @rdname copulaFitPlot-methods
#' @aliases copulaFitPlot,ANY-method
setMethod("copulaFitPlot",signature("CopulaObj"), function(object) {
	tryCatch({

		par(mfrow = c(1,2))
		xlabs="Margin 1"; ylabs="Margin 2"; zlabs="Margin 3"
		marginals<-copulaSample(object, 1000)

		y<-object@observation
			if (nrow(y) >0 && ncol(y)==2){
				xmin <- round(min(y[,1],marginals[,1]))
				xmax <- round(max(y[,1],marginals[,1]))
				ymin <- round(min(y[,2],marginals[,2]))
				ymax <- round(max(y[,2],marginals[,2]))
				plot(y, main = "Observation Plot", col = "blue", cex=0.3, xlab = xlabs, ylab =ylabs, xlim=c(xmin,xmax),ylim=c(ymin,ymax))
			}
			else if (nrow(y) >0 && ncol(y)>=3){
				#require(scatterplot3d)
				xmin <- round(min(y[,1],marginals[,1]))
				xmax <- round(max(y[,1],marginals[,1]))
				ymin <- round(min(y[,2],marginals[,2]))
				ymax <- round(max(y[,2],marginals[,2]))
				zmin <- round(min(y[,3],marginals[,3]))
				zmax <- round(max(y[,3],marginals[,3]))
				scatterplot3d(y[,1:3], color="blue",  main = "Observation Plot", cex.symbols=0.3, xlab = xlabs, ylab =ylabs, zlab=zlabs, , xlim=c(xmin,xmax),ylim=c(ymin,ymax),zlim=c(zmin,zmax))
			}
			else{
				plotText("The observation data is not available.")
			}

		if (is.null(colnames(marginals)) && !is.null(colnames(y))){
			colnames(marginals)<-colnames(y)
		}

		#mtx <- ifelse(length(object@marginal)==0,"Fitted Marginal Percentile Plot","Fitted Marginal Plot")
		mtx <- paste0("Fitted ",object@type, " Copula Plot")
		if (ncol(marginals)==2){
			plot(marginals, main = mtx, cex=0.3, col="red")
		}
		else if (ncol(marginals)>=3){
			require(scatterplot3d)
			scatterplot3d(marginals[,1:3], color="red",  cex.symbols=0.3, main = mtx )
		}
		else{
			plotText("The marginal data cannot be drawn")
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for copula fit plotting: ", object@info,", ",err))
		gc()
		plotText("The copula fitting cannot be drawn")
	})
})
