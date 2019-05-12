#' Moment function of Pareto Distribution (PDF: alpha*xm^alpha/x^(alpha+1))
#' @param order Order of moment
#' @param xm Threshold value
#' @param alpha Default=3
#' @examples
#' mpareto(1,1000,2)
#' @rdname pareto
#' @export
mpareto <- function(order, xm, alpha = 3) {
	if (order == 1 && alpha > 1) {
		return(alpha*xm/(alpha-1))
	} else if (order == 1 && alpha <= 1) {
		return(Inf)
	} else if (order == 2 && alpha > 2) {
		return(xm^2*alpha/(alpha-1)^2/(alpha-2)+(alpha*xm/(alpha-1))^2)
	} else {
		return(Inf)
	}
}

#' Density function of Pareto Distribution (PDF: alpha*xm^alpha/x^(alpha+1))
#' @param x Value of the variable
#' @examples
#' dpareto(1500,1000,2)
#' @rdname pareto
#' @export
dpareto <- function(x, xm, alpha = 3) ifelse(x > xm , alpha*xm^alpha/(x^(alpha+1)), 0)

#' Cumulative probability function of Pareto Distribution (CDF: 1-(xm/x)^alpha)
#' @param q Value of the variable
#' @examples
#' ppareto(1500,1000,2)
#' @rdname pareto
#' @export
ppareto <- function(q, xm, alpha = 3) ifelse(q > xm , 1 - (xm/q)^alpha, 0 )

#' Quantile function of Pareto Distribution
#' @param p Value of the probability
#' @examples
#' qpareto(0.5,1000,2)
#' @rdname pareto
#' @export
qpareto <- function(p, xm, alpha = 3) ifelse(p < 0 | p > 1, NaN, xm*(1-p)^(-1/alpha))

#' Random generation of Pareto Distribution
#' @param n Number of samples
#' @examples
#' rpareto(100,1000,2)
#' @rdname pareto
#' @export
rpareto <- function(n, xm, alpha = 3) qpareto(runif(n), xm, alpha)


#' Truncate a numeric vector
#' @param x A numeric vector
#' @param lower Lower bound
#' @param upper Upper bound
#' @examples
#' trunc(rnorm(100,3,6),0,7)
#' @rdname truncate
#' @export
truncate <- function(x, lower, upper) {
	x[x>upper] <- upper
	x[x<lower] <- lower
	return(x)
}

#' Cumulative probability function of empirical distribution using linear interpolation
#' @param q Value of the variable
#' @param cdf empirical distribution (cdf for continuous distribution and pmf for discrete distribution)
#' @examples
#' #discrete distribution
#' pempirical(c(3,5,10),matrix(c(0.1,0.2,0.3,0.05,0.05,0.2,0.1,1:6,10),7,2))
#' #continuous distribution
#' pempirical(350,matrix(c(seq(0.01,1,0.01),cumprod(c(1,rep(1.1,99)))),100,2))
#' @rdname empirical
#' @export
pempirical <- function(q, cdf) {
	if (length(unique(cdf[,2]))==1) {
		return(runif(length(q)))
	} else if (sum(cdf[,1])>1 & cdf[nrow(cdf),1] ==1) { #cumulative probability function for continuous distribution
		return(approx(cdf[,2],cdf[,1],xout=q, rule=2)$y)
	} else if (sum(cdf[,1])>1 && cdf[nrow(cdf),1] <1) {
		warning("cdf input is not complete. The last row does not have a cumulative probability of 1. It will be extrapolated to 1.")
		nx <- nrow(cdf)
		xend = (cdf[nx,2]-cdf[nx-1,2])/(cdf[nx,1]-cdf[nx-1,1])*(1-cdf[nx-1,1])+cdf[nx-1,2]
		cdfnew <- rbind(cdfnew,c(1,xend))
		return(approx(cdfnew[,2],cdfnew[,1],xout=q, rule=2)$y)
	} else { #probability mass function for discrete distribution
		ps <- cdf[,1]/sum(cdf[,1])
		y <- as.vector(q)
		l <- length(y)
		z <- rep(0, l)
		for (i in 1:l) z[i] <- sum(ps[cdf[,2] <= y[i]])
		z <- as.numeric(z)
		if (is.array(q))
			dim(z) <- dim(q)
		return(z)
	}
}

#' Quantile function of Empirical Distribution
#' @param p Value of the probability
#' @examples
#' #discrete distribution
#' qempirical(c(0.3,0.65,1),matrix(c(0.1,0.2,0.3,0.05,0.05,0.2,0.1,1:6,10),7,2))
#' #continuous distribution
#' qempirical(c(0.3,0.65,0.8),matrix(c(seq(0.01,1,0.01),cumprod(c(1,rep(1.1,99)))),100,2))
#' @rdname empirical
#' @export
qempirical <- function(p, cdf) {
	if (sum(cdf[,1])>1 && cdf[nrow(cdf),1] ==1) { #cumulative probability function for continuous distribution
		return(approx(cdf[,1],cdf[,2],xout=p, rule=2)$y)
	} else if (sum(cdf[,1])>1 && cdf[nrow(cdf),1] <1) {
		warning("cdf input is not complete. The last row does not have a cumulative probability of 1. It will be extrapolated to 1.")
		nx <- nrow(cdf)
		xend = (cdf[nx,2]-cdf[nx-1,2])/(cdf[nx,1]-cdf[nx-1,1])*(1-cdf[nx-1,1])+cdf[nx-1,2]
		cdfnew <- rbind(cdfnew,c(1,xend))
		return(approx(cdfnew[,1],cdfnew[,2],xout=p, rule=2)$y)
	} else { #probability mass function for discrete distribution
		ps <- cumsum(cdf[,1])/sum(cdf[,1])
		y <- as.vector(p)
		l <- length(y)
		z <- rep(0, l)
		for (i in 1:l) z[i] <- length(cdf[,2]) - sum(y[i] <= ps) + 1
		z <- as.numeric(z)
		z <- cdf[,2][z]
		if (is.array(q))
			dim(z) <- dim(q)
		return(z)
	}
}

#' Random generation function of Empirical Distribution
#' @param n Number of samples
#' @examples
#' #discrete distribution
#' rempirical(100,matrix(c(0.1,0.2,0.3,0.05,0.05,0.2,0.1,1:6,10),7,2))
#' #continuous distribution
#' rempirical(100,matrix(c(seq(0.01,1,0.01),cumprod(c(1,rep(1.1,99)))),100,2))
#' @rdname empirical
#' @export
rempirical <- function(n, cdf) {
	if (sum(cdf[,1])>1 && cdf[nrow(cdf),1] ==1) { #cumulative probability function for continuous distribution
		return(qempirical(runif(n), cdf))
	} else if (sum(cdf[,1])>1 && cdf[nrow(cdf),1] <1) {
		warning("cdf input is not complete. The last row does not have a cumulative probability of 1. It will be extrapolated to 1.")
		nx <- nrow(cdf)
		xend = (cdf[nx,2]-cdf[nx-1,2])/(cdf[nx,1]-cdf[nx-1,1])*(1-cdf[nx-1,1])+cdf[nx-1,2]
		cdfnew <- rbind(cdfnew,c(1,xend))
		return(qempirical(runif(n), cdfnew))
	} else { #probability mass function for discrete distribution
		ps <- cdf[,1]/sum(cdf[,1])
		return(sample(x=cdf[,2], size = n, replace = TRUE, prob = ps))
	}
}

#' Density function of Empirical Distribution based on simulation
#' @param x Value of the variable
#' @examples
#' #discrete distribution
#' dempirical(3,matrix(c(0.1,0.2,0.3,0.05,0.05,0.2,0.1,1:6,10),7,2))
#' #continuous distribution
#' dempirical(30,matrix(c(seq(0.01,1,0.01),qnorm(seq(0.01,1,0.01),30,20)),100,2))
#' @rdname empirical
#'
#' @import stats
#'
#' @export
dempirical <- function(x, cdf) {
	sdf <- approxfun(density(rempirical(100000,cdf)))
	return(sdf(x))
}

#' Density function of Truncated Normal Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param mean Mean of the untruncated Normal distribution
#' @param sd Standard deviation of the untruncated Normal distribution
#' @param min Left truncation (like deductible)
#' @param max Right truncation (like limit)
#' @examples
#' dtnorm(0.5,1,2)
#' @rdname tnorm
#' @export
dtnorm <- function(x,mean,sd,min=0,max=1e+9) {
	ifelse(x==0, pnorm(min, mean, sd),
			ifelse(x>=(max-min-1e-10), 1 - pnorm(max, mean, sd),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dnorm(x+min, mean=mean, sd=sd), 0))))
}

#' Cumulative probability function of Truncated Normal Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptnorm(0.5,1,2)
#' @rdname tnorm
#' @export
ptnorm <- function(q,mean,sd,min=0,max=1e+9) {
	ifelse(q==0, pnorm(min, mean, sd),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), pnorm(q+min, mean=mean, sd=sd), 0))))
}

#' Quantile function of Truncated Normal Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtnorm(0.5,1,2)
#' @rdname tnorm
#' @export
qtnorm <- function(p,mean,sd,min=0,max=1e+9) {
	ifelse(p<=pnorm(min, mean, sd), 0,
			ifelse(p>=pnorm(max, mean, sd), max-min, qnorm(p, mean=mean, sd=sd)-min))
}

#' Random generation of Truncated Normal Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtnorm(100,1,2)
#' @rdname tnorm
#' @export
rtnorm <- function(n,mean,sd,min=0,max=1e+9) {qtnorm(runif(n),mean=mean, sd=sd,min,max)}



#' Density function of Truncated Beta Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param shape1 distribution parameter
#' @param shape2 distribution parameter
#' @param ncp non-centrality parameter (Default: 0)
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' dtbeta(0.6,1,2)
#' @rdname tbeta
#' @export
dtbeta <- function(x, shape1, shape2, ncp = 0, min=0,max=1) {
	ifelse(x==0, pbeta(min, shape1, shape2, ncp),
			ifelse(x>=(max-min-1e-10), 1 - pbeta(max, shape1, shape2, ncp),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dbeta(x+min, shape1, shape2, ncp), 0))))
}

#' Cumulative probability function of Truncated Beta Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptbeta(0.5,1,2)
#' @rdname tbeta
#' @export
ptbeta <- function(q, shape1, shape2, ncp = 0, min=0,max=1) {
	ifelse(q==0, pbeta(min, shape1, shape2, ncp),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), pbeta(q+min, shape1, shape2, ncp), 0))))
}

#' Quantile function of Truncated Beta Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtbeta(0.5,1,2)
#' @rdname tbeta
#' @export
qtbeta <- function(p, shape1, shape2, ncp = 0, min=0,max=1) {
	ifelse(p<=pbeta(min, shape1, shape2, ncp), 0,
			ifelse(p>=pbeta(max, shape1, shape2, ncp), max-min, qbeta(p, shape1, shape2, ncp)-min))
}

#' Random generation of Truncated Beta Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtbeta(100,1,2)
#' @rdname tbeta
#' @export
rtbeta <- function(n, shape1, shape2, ncp = 0, min=0,max=1) {qtbeta(runif(n),shape1, shape2, ncp,min,max)}



#' Density function of Truncated Exponential Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param rate Distribution parameter
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' dtexp(5,0.1)
#' @rdname texp
#' @export
dtexp <- function(x,rate,min=0,max=1e+9) {
	ifelse(x==0, pexp(min, rate),
			ifelse(x>=(max-min-1e-10), 1 - pexp(max, rate),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dexp(x+min, rate), 0))))
}

#' Cumulative probability function of Truncated Exponential Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptexp(5,0.1)
#' @rdname texp
#' @export
ptexp <- function(q,rate,min=0,max=1e+9) {
	ifelse(q==0, pexp(min, rate),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), pexp(q+min, rate), 0))))
}

#' Quantile function of Truncated Exponential Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtexp(0.5,0.1)
#' @rdname texp
#' @export
qtexp <- function(p,rate,min=0,max=1e+9) {
	ifelse(p<=pexp(min, rate), 0,
			ifelse(p>=pexp(max, rate), max-min, qexp(p, rate)-min))
}

#' Random generation of Truncated Exponential Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtexp(100,0.1)
#' @rdname texp
#' @export
rtexp <- function(n,rate,min=0,max=1e+9) {qtexp(runif(n),rate,min,max)}



#' Density function of Truncated Gamma Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param shape Shape parameter
#' @param scale Scale parameter
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' dtgamma(2,3,2)
#' @rdname tgamma
#' @export
dtgamma <- function(x,shape,scale,min=0,max=1e+9) {
	ifelse(x==0, pgamma(min, shape,scale),
			ifelse(x>=(max-min-1e-10), 1 - pgamma(max, shape,scale),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dgamma(x+min, shape,scale), 0))))
}

#' Cumulative probability function of Truncated Gamma Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptgamma(2,3,2)
#' @rdname tgamma
#' @export
ptgamma <- function(q,shape,scale,min=0,max=1e+9) {
	ifelse(q==0, pgamma(min, shape,scale),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), pgamma(q+min, shape,scale), 0))))
}

#' Quantile function of Truncated Gamma Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtgamma(0.5,3,2)
#' @rdname tgamma
#' @export
qtgamma <- function(p,shape,scale,min=0,max=1e+9) {
	ifelse(p<=pgamma(min, shape,scale), 0,
			ifelse(p>=pgamma(max, shape,scale), max-min, qgamma(p, shape,scale)-min))
}

#' Random generation of Truncated Gamma Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtgamma(100,3,2)
#' @rdname tgamma
#' @export
rtgamma <- function(n,shape,scale,min=0,max=1e+9) {qtgamma(runif(n),shape,scale,min,max)}



#' Density function of Truncated Geometric Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param prob Distribution parameter
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' dtgeom(3,0.3)
#' @rdname tgeom
#' @export
dtgeom <- function(x,prob,min=0,max=1e+9) {
	ifelse(x==0, pgeom(min, prob),
			ifelse(x>=(max-min-1e-10), 1 - pgeom(max, prob),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dgeom(x+min, prob), 0))))
}

#' Cumulative probability function of Truncated Geometric Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptgeom(3,0.3)
#' @rdname tgeom
#' @export
ptgeom <- function(q,prob,min=0,max=1e+9) {
	ifelse(q==0, pgeom(min, prob),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), pgeom(q+min, prob), 0))))
}

#' Quantile function of Truncated Geometric Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtgeom(0.7,0.3)
#' @rdname tgeom
#' @export
qtgeom <- function(p,prob,min=0,max=1e+9) {
	ifelse(p<=pgeom(min, prob), 0,
			ifelse(p>=pgeom(max, prob), max-min, qgeom(p, prob)-min))
}

#' Random generation of Truncated Geometric Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtgeom(100,0.3)
#' @rdname tgeom
#' @export
rtgeom <- function(n,prob,min=0,max=1e+9) {qtgeom(runif(n),prob,min,max)}



#' Density function of Truncated Lognormal Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param meanlog Mean of the log of the distribution
#' @param sdlog Standard deviation of the log of the distribution
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' dtlnorm(20,3,0.5)
#' @rdname tlnorm
#' @export
dtlnorm <- function(x,meanlog,sdlog,min=0,max=1e+9) {
	ifelse(x==0, plnorm(min, meanlog,sdlog),
			ifelse(x>=(max-min-1e-10), 1 - plnorm(max, meanlog,sdlog),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dlnorm(x+min, meanlog,sdlog), 0))))
}

#' Cumulative probability function of Truncated Lognormal Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptlnorm(20,3,0.5)
#' @rdname tlnorm
#' @export
ptlnorm <- function(q,meanlog,sdlog,min=0,max=1e+9) {
	ifelse(q==0, plnorm(min, meanlog,sdlog),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), plnorm(q+min, meanlog,sdlog), 0))))
}

#' Quantile function of Truncated Lognormal Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtlnorm(0.5,3,0.5)
#' @rdname tlnorm
#' @export
qtlnorm <- function(p,meanlog,sdlog,min=0,max=1e+9) {
	ifelse(p<=plnorm(min, meanlog, sdlog), 0,
			ifelse(p>=plnorm(max, meanlog, sdlog), max-min, qlnorm(p, meanlog, sdlog)-min))
}

#' Random generation of Truncated Lognormal Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtlnorm(100,3,0.5)
#' @rdname tlnorm
#' @export
rtlnorm <- function(n,meanlog,sdlog,min=0,max=1e+9) {qtlnorm(runif(n),meanlog, sdlog,min,max)}



#' Density function of Truncated Negative Binomial Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param size Number of successful trials
#' @param prob Probability of success in each trial
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' dtnbinom(230,100,0.3)
#' @rdname tnbinom
#' @export
dtnbinom <- function(x,size,prob,min=0,max=1e+9) {
	ifelse(x==0, pnbinom(min, size,prob),
			ifelse(x>=(max-min-1e-10), 1 - pnbinom(max, size,prob),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dnbinom(x+min, size,prob), 0))))
}

#' Cumulative probability function of Truncated Negative Binomial Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptnbinom(230,100,0.3)
#' @rdname tnbinom
#' @export
ptnbinom <- function(q,size,prob,min=0,max=1e+9) {
	ifelse(q==0, pnbinom(min, size,prob),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), pnbinom(q+min, size,prob), 0))))
}

#' Quantile function of Truncated Negative Binomial Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtnbinom(0.5,100,0.3)
#' @rdname tnbinom
#' @export
qtnbinom <- function(p,size,prob,min=0,max=1e+9) {
	ifelse(p<=pnbinom(min, size,prob), 0,
			ifelse(p>=pnbinom(max, size,prob), max-min, qnbinom(p, size,prob)-min))
}

#' Random generation of Truncated Negative Binomial Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtnbinom(500,100,0.3)
#' @rdname tnbinom
#' @export
rtnbinom <- function(n,size,prob,min=0,max=1e+9) {qtnbinom(runif(n),size,prob,min,max)}



#' Density function of Truncated Pareto Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param xm Threshold value
#' @param alpha Model parameter
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' dtpareto(500,1000,2)
#' @rdname tpareto
#' @export
dtpareto <- function(x,xm,alpha,min=xm,max=1e+9) {
	ifelse(x==0, ppareto(min,xm,alpha),
			ifelse(x>=(max-min-1e-10), 1 - ppareto(max,xm,alpha),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dpareto(x+min,xm,alpha), 0))))
}

#' Cumulative probability function of Truncated Pareto Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptpareto(500,1000,2)
#' @rdname tpareto
#' @export
ptpareto <- function(q,xm,alpha,min=xm,max=1e+9) {
	ifelse(q==0, ppareto(min,xm,alpha),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), ppareto(q+min,xm,alpha), 0))))
}

#' Quantile function of Truncated Pareto Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtpareto(0.5,1000,2)
#' @rdname tpareto
#' @export
qtpareto <- function(p,xm,alpha,min=xm,max=1e+9) {
	ifelse(p<=ppareto(min,xm,alpha), 0,
			ifelse(p>=ppareto(max,xm,alpha), max-min, qpareto(p,xm,alpha)-min))
}

#' Random generation of Truncated Pareto Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtpareto(100,1000,2)
#' @rdname tpareto
#' @export
rtpareto <- function(n,xm,alpha,min=xm,max=1e+9) {qtpareto(runif(n),xm,alpha,min,max)}



#' Density function of Truncated Poisson Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param lambda Distribution parameter
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' dtpois(3,5)
#' @rdname tpois
#' @export
dtpois <- function(x,lambda,min=0,max=1e+9) {
	ifelse(x==0, ppois(min,lambda),
			ifelse(x>=(max-min-1e-10), 1 - ppois(max,lambda),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dpois(x+min,lambda), 0))))
}

#' Cumulative probability function of Truncated Poisson Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptpois(3,5)
#' @rdname tpois
#' @export
ptpois <- function(q,lambda,min=0,max=1e+9) {
	ifelse(q==0, ppois(min,lambda),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), ppois(q+min,lambda), 0))))
}

#' Quantile function of Truncated Poisson Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtpois(0.6,5)
#' @rdname tpois
#' @export
qtpois <- function(p,lambda,min=0,max=1e+9) {
	ifelse(p<=ppois(min,lambda), 0,
			ifelse(p>=ppois(max,lambda), max-min, qpois(p,lambda)-min))
}

#' Random generation of Truncated Poisson Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtpois(100,5)
#' @rdname tpois
#' @export
rtpois <- function(n,lambda,min=0,max=1e+9){qtpois(runif(n),lambda,min,max)}



#' Density function of Truncated Weibull Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param shape Shape parameter
#' @param scale Scale parameter
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' dtweibull(2.5,2,3)
#' @rdname tweibull
#' @export
dtweibull <- function(x,shape,scale,min=0,max=1e+9){
	ifelse(x==0, pweibull(min,shape,scale),
			ifelse(x>=(max-min-1e-10), 1 - pweibull(max,shape,scale),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dweibull(x+min,shape,scale), 0))))
}

#' Cumulative probability function of Truncated Weibull Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptweibull(2.5,2,3)
#' @rdname tweibull
#' @export
ptweibull <- function(q,shape,scale,min=0,max=1e+9) {
	ifelse(q==0, pweibull(min,shape,scale),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), pweibull(q+min,shape,scale), 0))))
}

#' Quantile function of Truncated Weibull Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtweibull(0.5,2,3)
#' @rdname tweibull
#' @export
qtweibull <- function(p,shape,scale,min=0,max=1000000000) {
	ifelse(p<=pweibull(min,shape,scale), 0,
			ifelse(p>=pweibull(max,shape,scale), max-min, qweibull(p,shape,scale)-min))
}

#' Random generation of Truncated Weibull Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtweibull(100,2,3)
#' @rdname tweibull
#' @export
rtweibull <- function(n,shape,scale,min=0,max=1e+9) {qtweibull(runif(n),shape,scale,min,max)}



#' Density function of truncated empirical distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param cdf empirical distribution (cdf for continuous distribution and pmf for discrete distribution)
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' #discrete distribution
#' dtempirical(3,matrix(c(0.1,0.2,0.3,0.05,0.05,0.2,0.1,1:6,10),7,2),3,100)
#' #continuous distribution
#' dtempirical(30,matrix(c(seq(0.01,1,0.01),qnorm(seq(0.01,1,0.01),30,20)),100,2),200,10000000)
#' @rdname tempirical
#' @export
dtempirical <- function(x,cdf,min=0,max=1e+9) {
	ifelse(x==0, pempirical(min,cdf),
			ifelse(x>=(max-min-1e-10), 1 - pempirical(max,cdf),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dempirical(x+min,cdf), 0))))
}

#' Cumulative probability function of truncated empirical distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' #discrete distribution
#' ptempirical(c(3,5,10),matrix(c(0.1,0.2,0.3,0.05,0.05,0.2,0.1,1:6,10),7,2),3,100)
#' #continuous distribution
#' ptempirical(350,matrix(c(seq(0.01,1,0.01),cumprod(c(1,rep(1.1,99)))),100,2),200,10000000)
#' @rdname tempirical
#' @export
ptempirical <- function(q,cdf,min=0,max=100000) {
	ifelse(q==0, pempirical(min,cdf),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), pempirical(q+min,cdf), 0))))
}

#' Quantile function of truncated empirical distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' #discrete distribution
#' qtempirical(c(0.3,0.65,1),matrix(c(0.1,0.2,0.3,0.05,0.05,0.2,0.1,1:6,10),7,2),3,100)
#' #continuous distribution
#' qtempirical(c(0.3,0.65,0.8),matrix(c(seq(0.01,1,0.01),
#' cumprod(c(1,rep(1.1,99)))),100,2),200,10000000)
#' @rdname tempirical
#' @export
qtempirical <- function(p,cdf,min=0,max=100000) {
	ifelse(p<=pempirical(min,cdf), 0,
			ifelse(p>=pempirical(max,cdf), max-min, qempirical(p,cdf)-min))
}

#' Random generation of Truncated empirical distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' #discrete distribution
#' rtempirical(100,matrix(c(0.1,0.2,0.3,0.05,0.05,0.2,0.1,1:6,10),7,2),3,100)
#' #continuous distribution
#' rtempirical(100,matrix(c(seq(0.01,1,0.01),cumprod(c(1,rep(1.1,99)))),100,2),200,10000000)
#' @rdname tempirical
#' @export
rtempirical <- function(n,cdf,min=0,max=100000) {qtempirical(runif(n),cdf,min,max)}


#' Plot text content
#' @param content A string to plot
#' @examples
#' plotText("You are awesome!")
#' @rdname plotText
#'
#' @import graphics
#' @export
plotText <- function(content){
	par(font = 2, ps=10, mar=c(4, 4, 1, 1), cex.main=0.7, cex.sub=0.8, cex.lab=0.8, cex.axis=0.8)#mfrow = c(1,1),
	plot(0:100, 0:100, type = "n", xlab = "", ylab = "", axes=FALSE)
	text(45,45, content)
}

#' Calculate ultimate development factor based on current development year, a mean development factor schedule and its volatility. It is used to simulate the ultimate loss for open claims.
#' @param Years Include two columns: Current development year and Settlement Year
#' @param meanDevFac A vector that contains the expected development factor schedule for Normal distribution. It is mu for Lognormal distribution and shape for Gamma distribution.
#' @param sdDevFac A vector that contains the standard deviation of expected development factor schedule for Normal distribution. It is sigma for Lognormal distribution and scale for Gamma distribution.
#' @param distType distribution type for development factor. It can be "normal", "lognormal" or "gamma".
#' @examples
#' meanfac<-c(1.1,1.08,1.05,1.03,1.01,1)
#' volfac<-rep(0.02,6)
#' years<-matrix(c(1:6),3,2)
#' ultiDevFac(years,meanfac,volfac)
#' @rdname ultiDevFac
#' @export
ultiDevFac <- function(Years,meanDevFac,sdDevFac=rep(0,length(meanDevFac)),distType = "normal"){
	nDevFac<-pmin(length(meanDevFac),Years[,2]-1)
	n<-length(meanDevFac)
	result<-vector()
	for (i in c(1:length(nDevFac))) {
		if (is.na(nDevFac[i]) == TRUE){
			result <- c(result,NA)
		} else {
			DevFac <- vector()
			if(distType == "normal") {
				DevFac<-meanDevFac+rnorm(n)*sdDevFac
			} else if (distType == "lognormal"){
				for (j in 1:n) {DevFac <- c(DevFac, rlnorm(1,meanlog=meanDevFac[j],sdlog=sdDevFac[j]))}
			} else if (distType == "gamma"){
				for (j in 1:n) {DevFac <- c(DevFac, rgamma(1,shape=meanDevFac[j],scale=sdDevFac[j]))}
			} else {
				DevFac<-rep(1,n)
			}

			if(Years[i,1]==Years[i,2]){
				result <- c(result,1)
			} else {
				result <- c(result,prod(DevFac[pmin(Years[i,1],nDevFac[i]):nDevFac[i]]))
			}
		}
	}
	result
}

#' Simulate whether closed claims will be reopened or not.
#' @param closeYear Years after claim closure. It could be a number or a numeric vector.
#' @param reopenProb A vector that contains the reopen probability based on closeYear.
#' @examples
#' reopenprob<-c(0.02,0.01,0.005,0.005,0.003,0)
#' rreopen(rep(2,1000),reopenprob)
#' @rdname rreopen
#' @export
rreopen <- function(closeYear,reopenProb){
	nDevFac<-length(reopenProb)
	ifelse(runif(length(closeYear))<=reopenProb[pmin(closeYear,nDevFac)],1,0)
}

#' Simulate whether claims will have zero payment.
#' @param devYear Development Year. It could be a number or a numeric vector.
#' @param zeroProb A vector that contains the probability of zero payment based on development year.
#' @examples
#' zeroprob<-c(0.02,0.01,0.005,0.005,0.003,0)
#' simP0(rep(2,1000),zeroprob)
#' @rdname simP0
#' @export
simP0 <- function(devYear,zeroProb){
	nDevFac<-length(zeroProb)
	ifelse(runif(length(devYear))<=zeroProb[pmin(devYear,nDevFac)],0,1)
}

#' Get the expected P0 based on settlement/close year.
#' @param closeYear Development years that claims are settled. It could be a number or a numeric vector.
#' @param zeroProb A vector that contains the P(0) based on development year.
#' @examples
#' zeroprob<-c(0.02,0.01,0.005,0.005,0.003,0)
#' expectZeros(c(2,3,6,9,100,1,2,3,4),zeroprob)
#' @rdname expectZeros
#' @export
expectZeros <- function(closeYear,zeroProb){
	nProb<-length(zeroProb)
	zeroProb[pmin(closeYear,nProb)]
}

#' Negative Loglikelihood.
#' @param paras A vector contain distribution parameters.
#' @param dist A Distribution Object.
#' @param fitdata A vector of loss data for fitting.
#' @param deductible A vector of deductible data for all loss data.
#' @param limit A vector of limit data for all loss data.
#' @examples
#' paras<-c(1,1)
#' dist<-new("Normal")
#' fitdata<-rtnorm(1000,3,2,1,10)
#' deductible<-rep(1,1000)
#' limit<-rep(9,1000)
#' nloglik(paras,dist,fitdata,deductible,limit)
#' paras<-c(3,2)
#' nloglik(paras,dist,fitdata,deductible,limit)

#' @rdname loglik
#' @export
nloglik <- function(paras,dist,fitdata,deductible,limit){

	if (length(paras) == 1){
		dist@p1 <- paras[1]
	} else if (length(paras) == 2) {
		dist@p1 <- paras[1]
		dist@p2 <- paras[2]
	} else if (length(paras) == 3) {
		dist@p1 <- paras[1]
		dist@p2 <- paras[2]
		dist@p3 <- paras[3]
	}

	loglik <- rep(0,length(fitdata))

	withinlimit <- ifelse((is.na(limit) | (fitdata < limit)),TRUE,FALSE)

	loglik <- ifelse(withinlimit,log(Density(dist, (fitdata+deductible))),(1-Probability(dist, (limit+deductible))))
	loglik <- loglik - log(1-Probability(dist, deductible))

	return(-sum(loglik))

}


#' Convert US date mm/dd/yyyy to yyyy-mm-dd format
#' @param d vector of dates in possible US format
#' @examples
#' toDate("3/5/2017")
#' @rdname toDate
#' @export
toDate<-function(d)
{
	test<-d[1]
	if (Find(length, list(grep("/", test), 0)) >0) {
		return (as.Date(d, "%m/%d/%Y"))
	}

	return (as.Date(d))
}
