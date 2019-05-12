#' An S4 class to represent a distribution, either parametric or non-parametric.
#'
#' @slot p1 A number for the value of the first parameter (default: 0.8).
#' @slot p2 A number for the value of the second parameter (default: 1).
#' @slot p3 A number for the value of the third parameter (default: 0).
#' @slot empirical A matrix that defines an empirical distribution with values and probabilities.
#' @slot min A number that defines the minimum value of the variable (default: 1e-8 considering it is used for frequency and severity modeling).
#' @slot max A number that defines the maximum value of the variable (default: 1e8).
#' @slot fitsucc Whether a distribution fitting is successful.
#' @slot info A character string that contains additional information of the distribution to identify line/type/frequency or severity.

setClass("Distribution",
	slots=c(
		p1="numeric",
		p2="numeric",
		p3="numeric",
		empirical="matrix",
		min="numeric",
		max="numeric",
		truncated="logical",
		fitsucc="logical",
		info="character"
	),
	prototype=list(
		p1=0.8,
		p2=1,
		p3=0.0,
		min=1e-8,
		max=1e8,
		truncated=FALSE,
		fitsucc=FALSE,
		info=""
	)
)

setClass("Normal", contains="Distribution")
setClass("Beta", contains="Distribution")
setClass("Exponential", contains="Distribution")
setClass("Gamma", contains="Distribution")
setClass("Geometric", contains="Distribution")
setClass("Lognormal", contains="Distribution")
setClass("NegativeBinomial", contains="Distribution")
setClass("Pareto", contains="Distribution")
setClass("Poisson", contains="Distribution")
setClass("Uniform", contains="Distribution")
setClass("Weibull", contains="Distribution")
setClass("Empirical", contains="Distribution")

#' Set distribution parameters.
#' @name setParams<-
#' @param this Distribution Object
#' @param ... Additional function arguments.
#' @param value Numeric vector containing parameters
#' examples
#' dist <- new("Normal")
#' setParams(dist) <- c(2,3)
#' dist
#' @rdname setParams-methods
#' @exportMethod setParams<-
setGeneric("setParams<-", function(this, ..., value) standardGeneric("setParams<-"))
#' @rdname setParams-methods
#' @aliases setParams,ANY-method
setReplaceMethod("setParams",signature("Distribution","numeric"), function(this, value) {
	this@p1<-value[1]
	this@p2<-value[2]
	this@p3<-value[3]
	this
})

#' Set the list of values and corresponding probabilities (Pr(X<value) for continuous variable and Pr(X==value) for discrete variable).
#' It is only used for empirical distribution.
#' @name setEmpirical<-
#' @param this Distribution Object
#' @param ... Additional function arguments.
#' @param value Two-column matrix with values and probabilities
#' dist <- new("Normal")
#' setEmpirical(dist) <- matrix(c(0.01,0.25,0.5,0.75,0.99, 11,12,13,14,15), nrow = 5, ncol = 2)
#' dist
#' @rdname setEmpirical-methods
#' @exportMethod setEmpirical<-
setGeneric("setEmpirical<-", function(this, ..., value) standardGeneric("setEmpirical<-"))
#' @rdname setEmpirical-methods
#' @aliases setEmpirical,ANY-method
setReplaceMethod("setEmpirical",signature("Distribution", "matrix"), function(this, value) {
	this@empirical<- value
	this
	})

#' Set the min and max of the variable.
#' @name setRange<-
#' @param this Distribution Object
#' @param ... Additional function arguments.
#' @param value a two-element vector contains min and max.
#' @rdname setRange-methods
#' @exportMethod setRange<-
setGeneric("setRange<-", function(this, ..., value) standardGeneric("setRange<-"))
#' @rdname setRange-methods
#' @aliases setRange,ANY-method
setReplaceMethod("setRange",signature("Distribution","numeric"), function(this, value) {
	this@min <-value[1]
	this@max <-value[2]
	this
})

#' Set the indicator of truncated distribution.
#' @name setTruncated<-
#' @param this Distribution Object
#' @param ... Additional function arguments.
#' @param value Boolean to indicate whether the distribution is truncated by min and max or not.
#' @rdname setTruncated-methods
#' @exportMethod setTruncated<-
setGeneric("setTruncated<-", function(this, ..., value) standardGeneric("setTruncated<-"))
#' @rdname setTruncated-methods
#' @aliases setTruncated,ANY-method
setReplaceMethod("setTruncated",signature("Distribution","logical"), function(this, value) {
	this@truncated<-value
	this
})

#' Set the minimum of the distribution. For example, the distribution of settlement lag for open claims
#' @name setMin
#' @param object A Distribution Object
#' @param ... Additional function arguments.
#' @examples
#' xLognormal <- new("Lognormal",p1=2,p2=3)
#' xLognormal <- setMin(xLognormal,50)
#' @rdname setMin-methods
#' @exportMethod setMin
setGeneric("setMin", function(object,...) standardGeneric("setMin"))
#' @param minval The minimum value.
#' @rdname setMin-methods
#' @aliases setMin,ANY-method
setMethod("setMin",signature("Distribution"), function(object,minval) {
	results <- list()
	for(x in minval){
		object@min<-x
		object@truncated<-TRUE
		results<-c(results,object)
	}
	results
})


#' Calculate the mean of 100000 sampled values from the distribution.
#' @name sampleMean
#' @param object A Distribution Object
#' @param ... Additional function arguments
#' @examples
#' xLognormal <- new("Lognormal",p1=2,p2=3)
#' sampleMean(xLognormal)
#' @rdname sampleMean-methods
#' @exportMethod sampleMean
setGeneric("sampleMean", function(object,...) standardGeneric("sampleMean"))
#' @rdname sampleMean-methods
#' @aliases sampleMean,ANY-method
setMethod("sampleMean",signature("Distribution"), function(object) { return (mean(doSample(object, 100000)))})

#' Calculate the standard deviation of 10000 sampled values from the distribution.
#' @name sampleSd
#' @param object A Distribution Object
#' @param ... Additional function arguments
#' @examples
#' xLognormal <- new("Lognormal",p1=2,p2=3)
#' sampleSd(xLognormal)
#' @rdname sampleSd-methods
#' @exportMethod sampleSd
setGeneric("sampleSd", function(object,...) standardGeneric("sampleSd"))
#' @rdname sampleSd-methods
#' @aliases sampleSd,ANY-method
setMethod("sampleSd",signature("Distribution"), function(object) { return (sd(doSample(object, 10000)))})

#' Calculate the skewness of 10000 sampled values from the distribution.
#' @name sampleSkew
#' @param object A Distribution Object
#' @param ... Additional function arguments
#' @examples
#' xLognormal <- new("Lognormal",p1=2,p2=3)
#' sampleSkew(xLognormal)
#' @rdname sampleSkew-methods
#' @importFrom moments skewness
#' @exportMethod sampleSkew
setGeneric("sampleSkew", function(object,...) standardGeneric("sampleSkew"))
#' @rdname sampleSkew-methods
#' @aliases sampleSkew,ANY-method
setMethod("sampleSkew",signature("Distribution"), function(object) {
	#require(moments)
	return (skewness(doSample(object, 10000)))
})

#' Calculate the excess kurtosis of 10000 sampled values from the distribution.
#' @name sampleKurtosis
#' @param object A Distribution Object
#' @param ... Additional function arguments
#' @examples
#' xLognormal <- new("Lognormal",p1=2,p2=3)
#' sampleKurtosis(xLognormal)
#' @rdname sampleKurtosis-methods
#' @importFrom moments kurtosis
#' @exportMethod sampleKurtosis
setGeneric("sampleKurtosis", function(object,...) standardGeneric("sampleKurtosis"))
#' @rdname sampleKurtosis-methods
#' @aliases sampleKurtosis,ANY-method
setMethod("sampleKurtosis",signature("Distribution"), function(object) {
	#require(moments)
	return (kurtosis(doSample(object, 10000))-3)
})

#' Sampling from the distribution.
#' @name doSample
#' @param object A Distribution Object
#' @param n Number of samples
#' @param ... Additional function arguments
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' doSample(xPareto,10000)
#' @rdname doSample-methods
#' @exportMethod doSample
setGeneric("doSample", function(object, n, ...) standardGeneric("doSample"))

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Normal", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rnorm(n,object@p1, object@p2)
		} else {
			rtnorm(n,object@p1, object@p2, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Normal distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Beta", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rbeta(n,object@p1, object@p2, object@p3)
		} else {
			rtbeta(n,object@p1, object@p2, object@p3, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Beta distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Exponential", "numeric"),  function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rexp(n,object@p1)
		} else {
			rtexp(n,object@p1, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Exponential distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Gamma", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rgamma(n,object@p1, object@p2)
		} else {
			rtgamma(n,object@p1, object@p2, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Gamma distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Lognormal", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rlnorm(n,object@p1, object@p2)
		} else {
			rtlnorm(n,object@p1, object@p2, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Lognormal distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Pareto", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rpareto(n,object@p1, object@p2)
		} else {
			rtpareto(n,object@p1, object@p2, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Pareto distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Poisson", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rpois(n,object@p1)
		} else {
			rtpois(n,object@p1, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Poisson distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("NegativeBinomial", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rnbinom(n,object@p1, object@p2)
		} else {
			rtnbinom(n,object@p1, object@p2, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Negative Binomial distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Geometric", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rgeom(n,object@p1)
		} else {
			rtgeom(n,object@p1, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Geometric distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Uniform", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			runif(n,object@p1, max(object@p1,object@p2))
		} else {
			runif(n,max(object@min, object@p1), min(max(object@min,object@max), object@p2))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Uniform distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Weibull", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rweibull(n,object@p1, object@p2)
		} else {
			rtweibull(n,object@p1, object@p2, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for Weibull distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#' @rdname doSample-methods
#' @aliases doSample,ANY-method
setMethod("doSample",signature("Empirical", "numeric"), function(object, n) {
	tryCatch({
		if(object@truncated==FALSE){
			rempirical(n,object@empirical)
		} else {
			rtempirical(n,object@empirical, object@min, max(object@min, object@max))
		}
	}, error = function(err){
		message(paste0(">>>Critical Error for empirical distribution ", object@info, ": ", err))
		gc()
		return(-1)
	})
	}
)

#Density function
#' @param log Boolean variable to indicate whether to return log of probability
#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Normal"), function(object, x, log = FALSE) {
	if (object@truncated == FALSE) {
		dnorm(x, mean=object@p1, sd=object@p2, log=log)
	}else{
		dtnorm(x, mean=object@p1, sd=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Beta"), function(object, x, log = FALSE) {
 	if (object@truncated == FALSE) {
		dbeta(x, object@p1, object@p2, object@p3, log=log)
	}else{
		dtbeta(x, object@p1, object@p2, object@p3, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Exponential"),  function(object, x, log = FALSE) {
 	if (object@truncated == FALSE) {
		dexp(x, rate=object@p1, log=log)
	}else{
		dtexp(x, rate=object@p1, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Gamma"), function(object, x, log = FALSE) {
 	if (object@truncated == FALSE) {
		dgamma(x, shape =object@p1, scale=object@p2, log=log)
	}else{
		dtgamma(x, shape =object@p1, scale=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Geometric"), function(object, x, log = FALSE) {
 	if (object@truncated == FALSE) {
		dgeom(x, prob =object@p1, log=log)
	}else{
		dtgeom(x, prob =object@p1, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Lognormal"), function(object, x, log = FALSE) {
	if (object@truncated == FALSE) {
		dlnorm(x, meanlog=object@p1, sdlog=object@p2, log=log)
	}else{
		dtlnorm(x, meanlog=object@p1, sdlog=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("NegativeBinomial"), function(object, x, log = FALSE) {
	if (object@truncated == FALSE) {
		dnbinom(x, size =object@p1, prob=object@p2, log=log)
	}else{
		dtnbinom(x, size =object@p1, prob=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Pareto"), function(object, x, log = FALSE) {
	if (object@truncated == FALSE) {
		dpareto(x, xm =object@p1, alpha=object@p2)
	}else{
		dtpareto(x, xm =object@p1, alpha=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Poisson"), function(object, x, log = FALSE) {
 	if (object@truncated == FALSE) {
		dpois(x, lambda =object@p1, log=log)
	}else{
		dtpois(x, lambda =object@p1, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Uniform"), function(object, x, log = FALSE) {
	if (object@truncated == FALSE) {
		dunif(x, min =object@p1, max=max(object@p1,object@p2), log=log)
	}else{
		dunif(x, max(object@min, object@p1), min(max(object@min,object@max), object@p2))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Weibull"), function(object, x, log = FALSE) {
 	if (object@truncated == FALSE) {
		dweibull(x, shape =object@p1, scale=object@p2, log=log)
	}else{
		dtweibull(x, shape =object@p1, scale=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Density-methods
#' @aliases Density,ANY-method
setMethod("Density",signature("Empirical"), function(object, x, log = FALSE) {
 	if (object@truncated == FALSE) {
		dempirical(x, cdf =object@empirical)
	}else{
		dtempirical(x, cdf =object@empirical, min=object@min, max=max(object@min, object@max))
	}
})

#Probability function
#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Normal"), function(object, q) {
	if (object@truncated == FALSE) {
		pnorm(q, mean=object@p1, sd=object@p2)
	}else{
		ptnorm(q, mean=object@p1, sd=object@p2, min=object@min, max=max(object@min, object@max))
	}
 })

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Beta"), function(object, q) {
	if (object@truncated == FALSE) {
		pbeta(q, object@p1, object@p2)
	}else{
		ptbeta(q, object@p1, object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Exponential"),  function(object, q) {
	if (object@truncated == FALSE) {
		pexp(q, rate =object@p1)
	}else{
		ptexp(q, rate =object@p1, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Gamma"), function(object, q) {
	if (object@truncated == FALSE) {
		pgamma(q, shape =object@p1, scale=object@p2)
	}else{
		ptgamma(q, shape =object@p1, scale=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Geometric"), function(object, q) {
	if (object@truncated == FALSE) {
		pgeom(q, prob =object@p1)
	}else{
		ptgeom(q, prob =object@p1, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Lognormal"), function(object, q) {
	if (object@truncated == FALSE) {
		plnorm(q, meanlog =object@p1, sdlog=object@p2)
	}else{
		ptlnorm(q, meanlog =object@p1, sdlog=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("NegativeBinomial"), function(object, q) {
	if (object@truncated == FALSE) {
		pnbinom(q, size =object@p1, prob=object@p2)
	}else{
		ptnbinom(q, size =object@p1, prob=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Pareto"), function(object, q) {
	if (object@truncated == FALSE) {
		ppareto(q, xm =object@p1, alpha=object@p2)
	}else{
		ptpareto(q, xm =object@p1, alpha=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Poisson"), function(object, q) {
	if (object@truncated == FALSE) {
		ppois(q, lambda =object@p1)
	}else{
		ptpois(q, lambda =object@p1, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Uniform"), function(object, q) {
	if (object@truncated == FALSE) {
		punif(q, min=object@p1, max=max(object@p1,object@p2))
	}else{
		punif(q, max(object@min, object@p1), min(max(object@min,object@max), object@p2))
	}
})

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Weibull"), function(object, q) {
	if (object@truncated == FALSE) {
		pweibull(q, shape =object@p1, scale=object@p2)
	}else{
		ptweibull(q, shape =object@p1, scale=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Probability-methods
#' @aliases Probability,ANY-method
setMethod("Probability",signature("Empirical"), function(object, q) {
	if (object@truncated == FALSE) {
		pempirical(q, cdf =object@empirical)
	}else{
		ptempirical(q, cdf =object@empirical, min=object@min, max=max(object@min, object@max))
	}
})

#Quantile function
#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Normal"), function(object, p) {
	if (object@truncated == FALSE) {
		qnorm(p, mean=object@p1, sd=object@p2)
	}else{
		qtnorm(p, mean=object@p1, sd=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Beta"), function(object, p) {
	if (object@truncated == FALSE) {
		qbeta(p, object@p1, object@p2)
	}else{
		qtbeta(p, object@p1, object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Exponential"),  function(object, p) {
	if (object@truncated == FALSE) {
		qexp(p, rate =object@p1)
	}else{
		qtexp(p, rate =object@p1, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Gamma"), function(object, p) {
	if (object@truncated == FALSE) {
		qgamma(p, shape =object@p1, scale=object@p2)
	}else{
		qtgamma(p, shape =object@p1, scale=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Geometric"), function(object, p) {
	if (object@truncated == FALSE) {
		qgeom(p, prob =object@p1)
	}else{
		qtgeom(p, prob =object@p1, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Lognormal"), function(object, p) {
	if (object@truncated == FALSE) {
		qlnorm(p, meanlog =object@p1, sdlog=object@p2)
	}else{
		qtlnorm(p, meanlog =object@p1, sdlog=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("NegativeBinomial"), function(object, p) {
	if (object@truncated == FALSE) {
		qnbinom(p, size =object@p1, prob=object@p2)
	}else{
		qtnbinom(p, size =object@p1, prob=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Pareto"), function(object, p) {
	if (object@truncated == FALSE) {
		qpareto(p, xm =object@p1, alpha=object@p2)
	}else{
		qtpareto(p, xm =object@p1, alpha=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Poisson"), function(object, p) {
	if (object@truncated == FALSE) {
		qpois(p, lambda =object@p1)
	}else{
		qtpois(p, lambda =object@p1, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Uniform"), function(object, p) {
	if (object@truncated == FALSE) {
		qunif(p, min=object@p1, max=max(object@p1,object@p2))
	}else{
		qunif(p, max(object@min, object@p1), min(max(object@min,object@max), object@p2))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Weibull"), function(object, p) {
	if (object@truncated == FALSE) {
		qweibull(p, shape =object@p1, scale=object@p2)
	}else{
		qtweibull(p, shape =object@p1, scale=object@p2, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname Quantile-methods
#' @aliases Quantile,ANY-method
setMethod("Quantile",signature("Empirical"), function(object, p) {
	if (object@truncated == FALSE) {
		qempirical(p, cdf =object@empirical)
	}else{
		qtempirical(p, cdf =object@empirical, min=object@min, max=max(object@min, object@max))
	}
})

#' @rdname doPlot-methods
#' @aliases doPlot,ANY-method
setMethod("doPlot",signature("Distribution"), function(object) {
	par(mfrow = c(2, 2))
	#draw some basic features
	x<-doSample(object, 1000)
	p<-seq(0,1, by=0.005)
	q<-Quantile(object, p)

	plot(q, Density(object, q),type="l",  main = "PDF Plot", xlab="q", ylab="density", col = "red")
	plot(q, p, type="l", main = "CDF Plot", col="red")
	hist(x,breaks=100, main="Histogram", xlab="observations", ylab="Frequency", col="red")
	plot(p, q, type="l", main = "Quantile Plot", col="red")
})

#ToString function
setMethod("toString",signature("Normal"), function(object) { return(paste("Gaussian p1=", round(object@p1,4), " p2=", round(object@p2,4), " info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("Beta"), function(object) { return(paste("Beta p1=", round(object@p1,4), " p2=", round(object@p2,4), " info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("Exponential"),  function(object) { return(paste("Exponential p1=", round(object@p1,4), " info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("Gamma"), function(object) { return(paste("Gamma p1=", round(object@p1,4), " p2=", round(object@p2,4), " info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("Geometric"), function(object) { return(paste("Geometric p1=", round(object@p1,4), " info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("Lognormal"), function(object) { return(paste("Lognormal p1=", round(object@p1,4), " p2=", round(object@p2,4), " info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("NegativeBinomial"), function(object) { return(paste("Negative Binomial p1=", round(object@p1,4), " p2=", round(object@p2,4), " info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("Pareto"), function(object) { return(paste("Pareto p1=", round(object@p1,4), " p2=", round(object@p2,4), " info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("Poisson"), function(object) { return(paste("Poisson p1=", round(object@p1,4)," info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("Uniform"), function(object) { return(paste("Uniform p1=", round(object@p1,4), " p2=", round(object@p2,4), " info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("Weibull"), function(object) { return(paste("Weibull p1=", round(object@p1,4), " p2=", round(object@p2,4)," info=", object@info, " min=", object@min, " max=", object@max, " truncated=", object@truncated, sep=""))})
setMethod("toString",signature("Empirical"), function(object) { return(paste("Empirical ", object@info, sep=""))})

#' Calculate Theoretical Mean of distribution.
#' min and max are not applied
#' @name TMean
#' @param object Distribution Object
#' @param ... Additional function arguments
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' TMean(xPareto)
#' @rdname TMean-methods
#' @export TMean
setGeneric("TMean", function(object, ...) standardGeneric("TMean"))

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("Normal"), function(object) { return (object@p1)})

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("Beta"), function(object) { return (object@p1/(object@p1+object@p2))})

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("Exponential"), function(object) { return (object@p1^(-1))})

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("Gamma"), function(object) { return (object@p1*object@p2)})

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("Geometric"), function(object) { return (1/object@p1)})

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("Lognormal"), function(object) { return (exp(object@p1+object@p2^2/2))})

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("NegativeBinomial"), function(object) { return (object@p1*(1-object@p2)/object@p2)})

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("Pareto"), function(object) { return (ifelse(object@p2>1, object@p1*object@p2/(object@p2-1), Inf))})

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("Poisson"), function(object) { return (object@p1)})

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("Uniform"), function(object) { return ((object@p1+object@p2)/2)})

#' @rdname TMean-methods
#' @aliases TMean,ANY-method
setMethod("TMean",signature("Weibull"), function(object) { return (object@p2*gamma(1+1/object@p1))})

#' Calculate Theoretical Standard Deviation of distribution.
#' min and max are not applied
#' @name TSD
#' @param object Distribution Object
#' @param ... Additional function arguments
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=3)
#' TSD(xPareto)
#' @rdname TSD-methods
#' @exportMethod TSD
setGeneric("TSD", function(object, ...) standardGeneric("TSD"))

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("Normal"), function(object) { return (object@p2)})

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("Beta"), function(object) { return (sqrt(object@p1*object@p2/(object@p1+object@p2+1))/(object@p1+object@p2))})

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("Exponential"), function(object) { return ((object@p1)^(-1))})

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("Gamma"), function(object) { return (object@p1^0.5*object@p2)})

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("Geometric"), function(object) { return ((1-object@p1)^0.5/object@p1)})

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("Lognormal"), function(object) { return (sqrt((exp(object@p2^2)-1)*exp(2*object@p1+object@p2^2)))})

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("NegativeBinomial"), function(object) { return (sqrt(object@p1*(1-object@p2))/object@p2)})

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("Pareto"), function(object) { return (ifelse(object@p2>2, object@p1/(object@p2-1)*sqrt(object@p2/(object@p2-2)), Inf))})

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("Poisson"), function(object) { return (object@p1)})

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("Uniform"), function(object) { return ((object@p2-object@p1)/sqrt(12))})

#' @rdname TSD-methods
#' @aliases TSD,ANY-method
setMethod("TSD",signature("Weibull"), function(object) { return (object@p2*sqrt(gamma(1+2/object@p1)-gamma(1+1/object@p1)^2))})


#' Calculate Theoretical Skewness of distribution.
#' min and max are not applied
#' @name TSkewness
#' @param object Distribution Object
#' @param ... Additional function arguments
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=4)
#' TSkewness(xPareto)
#' @rdname TSkewness-methods
#' @exportMethod TSkewness
setGeneric("TSkewness", function(object, ...) standardGeneric("TSkewness"))

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("Normal"), function(object) { return (0)})

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("Beta"), function(object) { return (2*(object@p2-object@p1)*sqrt(object@p1+object@p2+1)/(object@p1+object@p2+2)/sqrt(object@p1*object@p2))})

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("Exponential"), function(object) { return (2)})

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("Gamma"), function(object) { return (2/object@p1^(-0.5))})

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("Geometric"), function(object) { return ((2-object@p1)/(1-object@p1)^(-0.5))})

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("Lognormal"), function(object) { return ((exp(object@p2^2)+2)*sqrt(exp(object@p2^2)-1))})

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("NegativeBinomial"), function(object) { return ((2-object@p2)/sqrt(object@p1*(1-object@p2)))})

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("Pareto"), function(object) { return (ifelse(object@p2>3, (2+2*object@p2)/(object@p2-3)*sqrt((object@p2-2)/object@p2), Inf))})

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("Poisson"), function(object) { return (object@p1^(-0.5))})

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("Uniform"), function(object) { return (0)})

#' @rdname TSkewness-methods
#' @aliases TSkewness,ANY-method
setMethod("TSkewness",signature("Weibull"), function(object) { return ((gamma(1+3/object@p1)*object@p2^3-3*TMean(object)*TSD(object)^2-TMean(object)^3)/TSD(object)^3)})


#' Calculate Theoretical Excessive Kurtosis of distribution.
#' min and max are not applied
#' @name TEKurt
#' @param object Distribution Object
#' @param ... Additional function arguments
#' @examples
#' xPareto <- new("Pareto",p1=20,p2=5)
#' TEKurt(xPareto)
#' @rdname TEKurt-methods
#' @exportMethod TEKurt
setGeneric("TEKurt", function(object, ...) standardGeneric("TEKurt"))

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("Normal"), function(object) { return (0)})

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("Beta"), function(object) { return (6*((object@p1-object@p2)*(object@p1-object@p2)*(object@p1+object@p2+1)-object@p1*object@p2*(object@p1+object@p2+2))/(object@p1*object@p2*(object@p1+object@p2+2)*(object@p1+object@p2+3)))})

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("Exponential"), function(object) { return (6)})

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("Gamma"), function(object) { return (6/object@p1)})

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("Geometric"), function(object) { return (6+object@p1^2/(1-object@p1))})

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("Lognormal"), function(object) { return ((exp(4*object@p2^2)+2*exp(3*object@p2^2)+3*exp(2*object@p2^2)-6))})

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("NegativeBinomial"), function(object) { return (6/object@p1+object@p2^2/((1-object@p2)*object@p1))})

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("Pareto"), function(object) { return (ifelse(object@p2>4, 6*(object@p2^3+object@p2^2-6*object@p2-2)/(object@p2*(object@p2-3)*(object@p2-4)), Inf))})

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("Poisson"), function(object) { return (object@p1^(-1))})

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("Uniform"), function(object) { return (-6/5)})

#' @rdname TEKurt-methods
#' @aliases TEKurt,ANY-method
setMethod("TEKurt",signature("Weibull"), function(object) { return ((gamma(1+4/object@p1)*object@p2^4-4*TSkewness(object)*TMean(object)*TSD(object)^3-6*TMean(object)^2*TSD(object)^2-TMean(object)^4)/TSD(object)^4-3)})

setGeneric("objName", function(object, ...) standardGeneric("objName"))

setMethod("objName",signature("Normal"), function(object) { return ("Normal")})
setMethod("objName",signature("Beta"), function(object) { return ("Beta")})
setMethod("objName",signature("Exponential"),  function(object) { return ("Exponential")})
setMethod("objName",signature("Gamma"), function(object) { return ("Gamma")})
setMethod("objName",signature("Geometric"), function(object) { return ("Geometric")})
setMethod("objName",signature("Lognormal"), function(object) { return ("Lognormal")})
setMethod("objName",signature("NegativeBinomial"), function(object) { return ("NegativeBinomial")})
setMethod("objName",signature("Pareto"), function(object) { return ("Pareto")})
setMethod("objName",signature("Poisson"), function(object) { return ("Poisson")})
setMethod("objName",signature("Uniform"), function(object) { return ("Uniform")})
setMethod("objName",signature("Weibull"), function(object) { return ("Weibull")})
setMethod("objName",signature("Empirical"), function(object) { return ("Empirical")})

setGeneric("fitClassName", function(object, ...) standardGeneric("fitClassName"))

setMethod("fitClassName",signature("Normal"), function(object) {
	if (object@truncated == FALSE) {
		return ("norm")
	}else{
		return ("tnorm")
	}
})

setMethod("fitClassName",signature("Beta"), function(object) {
	if (object@truncated == FALSE) {
		return ("beta")
	}else{
		return ("tbeta")
	}
})


setMethod("fitClassName",signature("Exponential"),  function(object) {
	if (object@truncated == FALSE) {
		return ("exp")
	}else{
		return ("texp")
	}
})
setMethod("fitClassName",signature("Gamma"), function(object) {
	if (object@truncated == FALSE) {
		return ("gamma")
	}else{
		return ("tgamma")
	}
})

setMethod("fitClassName",signature("Geometric"), function(object) {
	if (object@truncated == FALSE) {
		return ("geom")
	}else{
		return ("tgeom")
	}
})

setMethod("fitClassName",signature("Lognormal"), function(object) {
	if (object@truncated == FALSE) {
		return ("lnorm")
	}else{
		return ("tlnorm")
	}
})

setMethod("fitClassName",signature("NegativeBinomial"), function(object) {
	if (object@truncated == FALSE) {
		return ("nbinom")
	}else{
		return ("tnbinom")
	}
})

setMethod("fitClassName",signature("Pareto"), function(object) {
	if (object@truncated == FALSE) {
		return ("pareto")
	}else{
		return ("tpareto")
	}
})

setMethod("fitClassName",signature("Poisson"), function(object) {
	if (object@truncated == FALSE) {
		return ("pois")
	}else{
		return ("tpois")
	}
})

setMethod("fitClassName",signature("Uniform"), function(object) {return ("unif")})
setMethod("fitClassName",signature("Weibull"), function(object) {
	if (object@truncated == FALSE) {
		return ("weibull")
	}else{
		return ("tweibull")
	}
})

setMethod("fitClassName",signature("Empirical"), function(object) {
	return ("empirical")
})

setGeneric("fitStartValue", function(object, ...) standardGeneric("fitStartValue"))

setMethod("fitStartValue",signature("Normal"), function(object, n) {return (list(mean=object@p1, sd=max(0.1,object@p2)))})
setMethod("fitStartValue",signature("Beta"), function(object, n) { return (list(shape1=object@p1, shape2=object@p2, ncp=object@p3))})
setMethod("fitStartValue",signature("Exponential"),  function(object, n) {return (list(rate=max(0.01,object@p1)))})
setMethod("fitStartValue",signature("Gamma"), function(object, n) { return (list(shape=max(0.1,object@p1), scale=max(0.1,object@p2)))})
setMethod("fitStartValue",signature("Geometric"), function(object, n) {return (list(prob=min(0.99,max(0.01,object@p1))))})
setMethod("fitStartValue",signature("Lognormal"), function(object, n) {return (list(meanlog=object@p1, sdlog=max(0.1,object@p2)))})
setMethod("fitStartValue",signature("NegativeBinomial"), function(object, n) { return (list(size=round(max(2,object@p1)), prob=min(0.99,max(0.01,object@p2))))})
setMethod("fitStartValue",signature("Pareto"), function(object, n) { return (list(xm=max(0.1,object@p1), alpha=max(0.1,object@p2)))})
setMethod("fitStartValue",signature("Poisson"), function(object, n) {return (list(lambda=max(1,object@p1)))})
setMethod("fitStartValue",signature("Uniform"), function(object, n) {})
setMethod("fitStartValue",signature("Weibull"), function(object, n) {return (list(shape=max(0.1,object@p1), scale=max(0.1,object@p2)))})

setGeneric("nParameter", function(object, ...) standardGeneric("nParameter"))

setMethod("nParameter",signature("Normal"), function(object) { return (2) })
setMethod("nParameter",signature("Beta"), function(object) { return (3)})
setMethod("nParameter",signature("Exponential"),  function(object) { return (1) })
setMethod("nParameter",signature("Gamma"), function(object) { return (2)})
setMethod("nParameter",signature("Geometric"), function(object) { return (1)})
setMethod("nParameter",signature("Lognormal"), function(object) { return (2)})
setMethod("nParameter",signature("NegativeBinomial"), function(object) { return (2) })
setMethod("nParameter",signature("Pareto"), function(object) { return (2)})
setMethod("nParameter",signature("Poisson"), function(object) { return (1) })
setMethod("nParameter",signature("Uniform"), function(object) { return (2) })
setMethod("nParameter",signature("Weibull"), function(object) { return (2) })

setGeneric("params", function(object, ...) standardGeneric("params"))

setMethod("params",signature("Normal"), function(object) {return (list(mean=object@p1, sd=object@p2))})
setMethod("params",signature("Beta"), function(object) { return (list(rate=object@p1))})
setMethod("params",signature("Exponential"),  function(object) {return (list(rate=object@p1))})
setMethod("params",signature("Gamma"), function(object) { return (list(shape=object@p1, scale=object@p2))})
setMethod("params",signature("Geometric"), function(object) {return (list(prob=object@p1))})
setMethod("params",signature("Lognormal"), function(object) {return (list(meanlog=object@p1, sdlog=object@p2))})
setMethod("params",signature("NegativeBinomial"), function(object) { return (list(size=object@p1, prob=object@p2))})
setMethod("params",signature("Pareto"), function(object) { return (list(xm=object@p1, alpha=object@p2))})
setMethod("params",signature("Poisson"), function(object) {return (list(lambda=object@p1))})
setMethod("params",signature("Uniform"), function(object) {return (list(min=object@p1, max=object@p2))})
setMethod("params",signature("Weibull"), function(object) {return (list(shape=object@p1, scale=object@p2))})
