#' An S4 class to represent a claim type.
#'
#' @slot simno The simulation index.
#' @slot line A string to identify the business line that the claim belongs to.
#' @slot claimType A string to identify the type of the claim. It further classifies the claims within a business line. For example, the type could be based on the size of the loss. 
#' @slot iRBNER A Boolean variable to indicate whether RBNER (open claims) should be simulated.
#' @slot iROPEN A Boolean variable to indicate whether claim reopen should be simulated.
#' @slot iIBNR A Boolean variable to indicate whether IBNR claims should be simulated.
#' @slot iUPR A Boolean variable to indicate whether future claims should be simulated.
#' @slot fIBNER IBNER development factor.
#' @slot severity Severity distribution.
#' @slot frequency Frequency distribution.
#' @slot reportLag Report lag distribution.
#' @slot settlementLag Settlement lag distribution.
#' @slot reopen Claim reopen probability based on the number of years after settlement till valuation date.
#' @slot reopenLag Reopen lag distribution.
#' @slot resettleLag Resettlement lag distribution.
#' @slot roDevFac Reopened claim development factor.
#' @slot ioDevFac A numeric variable to indicate the method of loss development for open claim severity. 1: Conditional distribution based on paid loss; 2: conditional distribution based on incurred loss; 3: year-to-year development factors
#' @slot irDevFac A numeric variable to indicate the method of loss development for claim reopen severity simulation. 1: Conditional distribution based on paid loss; 2: conditional distribution based on incurred loss; 3: year-to-year development factors
#' @slot freqIndex Frequency distribution time index.
#' @slot severityIndex Severity distribution time index.
#' @slot exposureIndex Exposure time index for IBNR or UPR.
#' @slot iCopula Whether copula is used to model severity, report lag and settlement lag.
#' @slot ssrCopula Copula object used for severity, report lag and settlement lag.
#' @slot sdata Indicating whether only closed claims (CLOSED) or closed + open claims (ALL) will be used for severity fitting.
#' @slot p0 An yearly table that controls the probability of invalid claim, excluding these valid claims less than deductible based on development year. It is based on the DevFac class.
setClass("ClaimType", 
	slots=c(
			simno="character",
			line="character",
			claimType="character",
#			claimClass="character",
			iRBNER="logical",
			iROPEN="logical",
			iIBNR="logical",
			iUPR="logical",
			fIBNER="DevFac",
			severity="Distribution",
			frequency="Distribution",
			reportLag="Distribution",
			settlementLag="Distribution",
			reopen="DevFac",
			reopenLag="Distribution",
			resettleLag="Distribution",
			roDevFac="DevFac",
			ioDevFac="numeric",
			irDevFac="numeric",
			IBNRfreqIndex="Index",
			UPRfreqIndex="Index",
			severityIndex="Index",
			exposureIndex="Index",
			iCopula="logical",
			ssrCopula="CopulaObj",
			laeDevFac="DevFac",
			deductible="Distribution",
			limit="Distribution",
			sdata="character",
			p0="DevFac"
	),
	prototype=list(	
			simno="XXX",
			line="Line1",
			claimType="Normal",
#			claimClass="RBNER",
			iRBNER=TRUE,
			iROPEN=TRUE,
			iIBNR=TRUE,
			iUPR=TRUE,
			fIBNER=new("DevFac",meanList=c(1.2,1.15,1.1,1.05,1),volList=c(runif(4)/10,0)),
			severity=new("Lognormal",p1=3,p2=2),
			frequency=new("Poisson",p1=100),
			reportLag=new("Exponential",p1=0.25),
			settlementLag=new("Exponential",p1=0.1),
			reopen=new("DevFac",meanList=c(0.01,0.005,0.002,0.001,0),volList=rep(0,5)),
			reopenLag=new("Exponential",p1=0.005),
			resettleLag=new("Exponential",p1=0.05),
			roDevFac=new("DevFac",meanList=c(1.05,1.1,1,1,1),volList=c(runif(4)/100,0)),
			ioDevFac=3,
			irDevFac=3,
			IBNRfreqIndex=new("Index",monthlyIndex=rep(0,360)),
			UPRfreqIndex=new("Index",monthlyIndex=rep(0,360)),
			severityIndex=new("Index",monthlyIndex=rep(1,360)),
			exposureIndex=new("Index",monthlyIndex=rep(1,360)),
			iCopula=FALSE,
			ssrCopula=new("CopulaObj",param=c(0,0,0),marginal=list(dist1=new("Lognormal",p1=3,p2=2),dist2=new("Exponential",p1=0.25),dist3=new("Exponential",p1=0.1)),dimension=3),
			laeDevFac=new("DevFac",FacModel=TRUE,paras=c(0.1,0.01,0.005,0.002,5)),
			deductible=new("Empirical",empirical=matrix(c(0,1,0,0),2,2)),
			limit=new("Empirical",empirical=matrix(c(0,1,1e10,1e10),2,2)),
			sdata="CLOSED",
			p0=new("DevFac",meanList=c(0,0),volList=c(0,0))
	)
)

#' Claim simulation at line/type/status level
#' @name claimSample
#' @param object ClaimType object
#' @param ... Additional parameters that may or may not be used. 
#' @rdname claimSample-methods
#' @exportMethod claimSample
setGeneric("claimSample", function(object, ...) standardGeneric("claimSample"))
#' @param claimData claim data including existing claims for RBNER and claim reopenness analysis;
#' @param startDate Date from which claim data is included in the analysis;
#' @param evaluationDate Date of evaluation.
#' @rdname claimSample-methods
#' @aliases claimSample,ANY-method
setMethod("claimSample", signature("ClaimType"), function(object, claimData=data.frame(), startDate=as.Date("2012-01-01"), evaluationDate=as.Date("2016-12-31"))
{
	tryCatch({
		startDate <- toDate(startDate)
		evaluationDate <- toDate(evaluationDate)
		if (nrow(claimData)>0) {
			claimData <- claimData[claimData[,"Type"] == object@claimType & claimData[,"LoB"] == object@line,]
			claimData[,"status"] = toupper(claimData[,"status"])
			claimData[,"claimLiability"] = toupper(claimData[,"claimLiability"])
			claimData[,"occurrenceDate"] = as.character(toDate(claimData[,"occurrenceDate"]))
			claimData[,"reportDate"] = as.character(toDate(claimData[,"reportDate"]))
			claimData[,"settlementDate"] = as.character(toDate(claimData[,"settlementDate"]))
			claimData <- claimData[as.Date(as.character(claimData[,"occurrenceDate"]))>=as.Date(as.character(startDate)),]
			claimData <- claimData[as.Date(claimData[,"reportDate"])<=as.Date(evaluationDate),]
		}

		nid <- 0



		sobj <- object@severity
		#sobj@truncated <- FALSE

		tmpdata = data.frame(ClaimID=character(),
						LoB=character(),
						Type=character(),
						status=character(),
						occurrenceDate=character(),
						reportDate=character(),
						incurredLoss=double(),
						osRatio=double(),
						settlementDate=character(),
						totalLoss=double(),
						ultimateLoss=double(),
						Deductible=double(),
						Limit=double(),
						Paid=double(),
						LAE=double(),
						claimLiability=character(),
						expectedLoss=double(),
						ultimateLAE=double(),
						expectedLAE=double(),
						reopenDate=character(),
						resettleDate=character(),
						reopenLoss=double(),
						Sim=double(),
						stringsAsFactors=FALSE)

		if (object@iRBNER == TRUE) {
			if(nrow(claimData)==0){
				nobs<-0
			} else {
				tmpdata <- claimData[claimData[,"status"] == "OPEN" | as.Date(claimData[,"settlementDate"])>as.Date(evaluationDate),]
				nobs <- nrow(tmpdata)
			}

			if (nobs > 0){
				occurrenceDates<-as.character(tmpdata[,"occurrenceDate"])
				reportDates<-as.character(tmpdata[,"reportDate"])
				osRatios<-as.numeric(tmpdata[,"osRatio"])
				incurredLosses<-as.numeric(tmpdata[,"incurredLoss"])
				settlementDates<-as.character(as.Date(reportDates) + as.numeric(lapply(setMin(object@settlementLag, as.numeric(as.Date(evaluationDate)-as.Date(reportDates))),doSample, n=1)) + as.numeric(as.Date(evaluationDate+1)-as.Date(reportDates))+1)
				sindex<-getIndex(object@severityIndex,settlementDates)/getIndex(object@severityIndex,evaluationDate)
				tmpdata[,"settlementDate"]<-settlementDates
				developmentYears<-ceiling(as.numeric(as.Date(evaluationDate) - as.Date(tmpdata[,"occurrenceDate"]))/365)
				settlementYears<-pmax(1,ceiling(as.numeric(as.Date(settlementDates) - as.Date(tmpdata[,"occurrenceDate"]))/365))
				if(object@fIBNER@FacModel==TRUE & object@fIBNER@fun == "exponential" & object@ioDevFac==3){
					eibnerdev<-object@fIBNER@paras[1]+object@fIBNER@paras[2]*developmentYears+object@fIBNER@paras[3]*incurredLosses+object@fIBNER@paras[4]*osRatios
					ibnerdev<-eibnerdev+rnorm(nobs,0,object@fIBNER@paras[length(object@fIBNER@paras)])
					if (length(object@fIBNER@xname)>0){
						for (i in c(1:length(object@fIBNER@xname))){
							ibnerdev<-ibnerdev+object@fIBNER@paras[4+i]*as.numeric(tmpdata[,object@fIBNER@xname[i]])
							eibnerdev<-eibnerdev+object@fIBNER@paras[4+i]*as.numeric(tmpdata[,object@fIBNER@xname[i]])
						}
					}
					ultimateLosses<-as.numeric(tmpdata[,"incurredLoss"])*pmax(log(ibnerdev),0)
					expectedLosses<-as.numeric(tmpdata[,"incurredLoss"])*pmax(log(eibnerdev),0)
				} else if(object@fIBNER@FacModel==TRUE & object@fIBNER@fun == "log" & object@ioDevFac==3){
					eibnerdev<-object@fIBNER@paras[1]+object@fIBNER@paras[2]*developmentYears+object@fIBNER@paras[3]*incurredLosses+object@fIBNER@paras[4]*osRatios
					ibnerdev<-eibnerdev+rnorm(nobs,0,object@fIBNER@paras[length(object@fIBNER@paras)])
					if (length(object@fIBNER@xname)>0){
						for (i in c(1:length(object@fIBNER@xname))){
							ibnerdev<-ibnerdev+object@fIBNER@paras[4+i]*as.numeric(tmpdata[,object@fIBNER@xname[i]])
							eibnerdev<-eibnerdev+object@fIBNER@paras[4+i]*as.numeric(tmpdata[,object@fIBNER@xname[i]])
						}
					}
					ultimateLosses<-as.numeric(tmpdata[,"incurredLoss"])*pmax(exp(ibnerdev),0)
					expectedLosses<-as.numeric(tmpdata[,"incurredLoss"])*pmax(exp(eibnerdev),0)
				} else if(object@fIBNER@FacModel==TRUE & object@fIBNER@fun == "inverse" & object@ioDevFac==3){
					eibnerdev<-object@fIBNER@paras[1]+object@fIBNER@paras[2]*developmentYears+object@fIBNER@paras[3]*incurredLosses+object@fIBNER@paras[4]*osRatios
					ibnerdev<-eibnerdev+rnorm(nobs,0,object@fIBNER@paras[length(object@fIBNER@paras)])
					if (length(object@fIBNER@xname)>0){
						for (i in c(1:length(object@fIBNER@xname))){
							ibnerdev<-ibnerdev+object@fIBNER@paras[4+i]*as.numeric(tmpdata[,object@fIBNER@xname[i]])
							eibnerdev<-eibnerdev+object@fIBNER@paras[4+i]*as.numeric(tmpdata[,object@fIBNER@xname[i]])
						}
					}
					ultimateLosses<-as.numeric(tmpdata[,"incurredLoss"])*pmax(1/ibnerdev,0)
					expectedLosses<-as.numeric(tmpdata[,"incurredLoss"])*pmax(1/eibnerdev,0)
				} else if (object@fIBNER@FacModel==TRUE & object@fIBNER@fun == "identity" & object@ioDevFac==3) {
					eibnerdev<-object@fIBNER@paras[1]+object@fIBNER@paras[2]*developmentYears+object@fIBNER@paras[3]*incurredLosses+object@fIBNER@paras[4]*osRatios
					ibnerdev<-eibnerdev+rnorm(nobs,0,object@fIBNER@paras[length(object@fIBNER@paras)])
					if (length(object@fIBNER@xname)>0){
						for (i in c(1:length(object@fIBNER@xname))){
							ibnerdev<-ibnerdev+object@fIBNER@paras[4+i]*as.numeric(tmpdata[,object@fIBNER@xname[i]])
							eibnerdev<-eibnerdev+object@fIBNER@paras[4+i]*as.numeric(tmpdata[,object@fIBNER@xname[i]])
						}
					}
					ultimateLosses<-as.numeric(tmpdata[,"incurredLoss"])*pmax(ibnerdev,0)
					expectedLosses<-as.numeric(tmpdata[,"incurredLoss"])*pmax(eibnerdev,0)
				} else if (object@ioDevFac==3){
					years <- cbind(developmentYears, settlementYears)
					ibnerdev<-ultiDevFac(years,meanDevFac=object@fIBNER@meanList,sdDevFac=object@fIBNER@volList,distType=object@fIBNER@distType)
					eibnerdev<-ultiDevFac(years,meanDevFac=object@fIBNER@meanList,distType=object@fIBNER@distType)
					ultimateLosses<-as.numeric(tmpdata[,"incurredLoss"])*pmax(ibnerdev,0)
					expectedLosses<-as.numeric(tmpdata[,"incurredLoss"])*pmax(eibnerdev,0)
				} else if (object@ioDevFac==2){
					tmpdata[,"Limit"] <- ifelse(is.na(tmpdata[,"Limit"]),1e10,tmpdata[,"Limit"])
					tmpdata[,"Deductible"] <- ifelse(is.na(tmpdata[,"Deductible"]),0,tmpdata[,"Deductible"])
					ultimateLosses<-pmax(as.numeric(tmpdata[,"incurredLoss"]),pmax((pmin(tmpdata[,"Limit"]+tmpdata[,"Deductible"],doSample(sobj,nobs))-tmpdata[,"Deductible"]),0))
					basicmean<-sampleMean(object@severity)
					expectedLosses<-rep(basicmean,nobs)
				} else {
					tmpdata[,"Limit"] <- ifelse(is.na(tmpdata[,"Limit"]),1e10,tmpdata[,"Limit"])
					tmpdata[,"Deductible"] <- ifelse(is.na(tmpdata[,"Deductible"]),0,tmpdata[,"Deductible"])
					if(object@iCopula==FALSE){
						probs <- Probability(sobj,tmpdata[,"Paid"])
						newprobs <- probs + (1-probs)*runif(nobs)
						ultimateLosses<-pmax(as.numeric(tmpdata[,"Paid"]),pmax((pmin(tmpdata[,"Limit"]+tmpdata[,"Deductible"],Quantile(sobj,newprobs))-tmpdata[,"Deductible"]),0))
					} else {
						for (j in c(1:nrow(tmpdata))) {
							settlemin <- as.numeric(as.Date(evaluationDate)-as.Date(tmpdata[j,"reportDate"]))
							sobjtrunc <- object@ssrCopula@marginal[[1]]@truncated
							sobjmin <- object@ssrCopula@marginal[[1]]@min
							robjtrunc <- object@ssrCopula@marginal[[2]]@truncated
							robjmin <- object@ssrCopula@marginal[[2]]@min
							object@ssrCopula@marginal[[1]]@min <- tmpdata[j,"Paid"]
							object@ssrCopula@marginal[[1]]@truncated <- TRUE
							object@ssrCopula@marginal[[2]]@min <- settlemin
							object@ssrCopula@marginal[[2]]@truncated <- TRUE
							samples <- copulaSample(object@ssrCopula,1)
							tmpdata[j,"settlementDate"] <- as.character(as.Date(as.Date(tmpdata[j,"reportDate"]) + round(samples[,2])))
							tmpdata[j,"ultimateLoss"] <- samples[,1]
						}

						object@ssrCopula@marginal[[1]]@min <- sobjmin
						object@ssrCopula@marginal[[1]]@truncated <- sobjtrunc
						object@ssrCopula@marginal[[2]]@min <- robjmin
						object@ssrCopula@marginal[[2]]@truncated <- robjtrunc

						settlementDates <- tmpdata[,"settlementDate"]
						ultimateLosses <- tmpdata[,"ultimateLoss"]

					}
					basicmean<-sampleMean(object@severity)
					expectedLosses<-rep(basicmean,nobs)

				}

				tmpdata[,"Limit"] <- ifelse(is.na(tmpdata[,"Limit"]),1e10,tmpdata[,"Limit"])
				tmpdata[,"ultimateLoss"]<-pmin(round(ultimateLosses*sindex,2),tmpdata[,"Limit"])

				zeros<-simP0(settlementYears,object@p0@meanList)
				exzeros<-expectZeros(settlementYears,object@p0@meanList)

				tmpdata[,"expectedLoss"]<-pmin(round(expectedLosses*sindex,2),tmpdata[,"Limit"])*(1-exzeros)

				tmpdata[,"claimLiability"] <- ifelse(zeros==0,FALSE,TRUE)
				tmpdata[,"ultimateLoss"]<- ifelse(tmpdata[,"claimLiability"] == FALSE, 0, tmpdata[,"ultimateLoss"])

				if(object@fIBNER@FacModel==TRUE & object@fIBNER@fun == "exponential" & object@ioDevFac==3){
					laes<-as.numeric(tmpdata[,"LAE"])*pmax(log(ibnerdev)*sindex,0)
					elaes<-as.numeric(tmpdata[,"LAE"])*pmax(log(eibnerdev)*sindex,0)
				} else if(object@fIBNER@FacModel==TRUE & object@fIBNER@fun == "log" & object@ioDevFac==3){
					laes<-as.numeric(tmpdata[,"LAE"])*pmax(exp(ibnerdev)*sindex,0)
					elaes<-as.numeric(tmpdata[,"LAE"])*pmax(exp(eibnerdev)*sindex,0)
				} else if(object@fIBNER@FacModel==TRUE & object@fIBNER@fun == "inverse" & object@ioDevFac==3){
					laes<-as.numeric(tmpdata[,"LAE"])*pmax(1/ibnerdev*sindex,0)
					elaes<-as.numeric(tmpdata[,"LAE"])*pmax(1/eibnerdev*sindex,0)
				} else if (object@fIBNER@FacModel==TRUE & object@fIBNER@fun == "identity" & object@ioDevFac==3) {
					laes<-as.numeric(tmpdata[,"LAE"])*pmax(ibnerdev*sindex,0)
					elaes<-as.numeric(tmpdata[,"LAE"])*pmax(eibnerdev*sindex,0)
				} else if (object@ioDevFac==3){
					laes<-as.numeric(tmpdata[,"LAE"])*pmax(ibnerdev*sindex,0)
					elaes<-as.numeric(tmpdata[,"LAE"])*pmax(eibnerdev*sindex,0)
				} else if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "exponential" & object@ioDevFac<3){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nobs,0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
					if (length(object@laeDevFac@xname)>0){
						for (i in c(1:length(object@laeDevFac@xname))){
							laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(tmpdata[,object@laeDevFac@xname[i]])
							elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(tmpdata[,object@laeDevFac@xname[i]])
						}
					}
					laes<-pmax(log(laedev),0)
					elaes<-pmax(log(elaedev),0)
				} else if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "log" & object@ioDevFac<3){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nobs,0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
					if (length(object@laeDevFac@xname)>0){
						for (i in c(1:length(object@laeDevFac@xname))){
							laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(tmpdata[,object@laeDevFac@xname[i]])
							elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(tmpdata[,object@laeDevFac@xname[i]])
						}
					}
					laes<-pmax(exp(laedev),0)
					elaes<-pmax(exp(elaedev),0)
				} else if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "inverse" & object@ioDevFac<3){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nobs,0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
					if (length(object@laeDevFac@xname)>0){
						for (i in c(1:length(object@laeDevFac@xname))){
							laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(tmpdata[,object@laeDevFac@xname[i]])
							elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(tmpdata[,object@laeDevFac@xname[i]])
						}
					}
					laes<-pmax(1/laedev,0)
					elaes<-pmax(1/elaedev,0)
				} else if (object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "identity" & object@ioDevFac<3) {
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nobs,0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
					if (length(object@laeDevFac@xname)>0){
						for (i in c(1:length(object@laeDevFac@xname))){
							laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(tmpdata[,object@laeDevFac@xname[i]])
							elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(tmpdata[,object@laeDevFac@xname[i]])
						}
					}
					laes<-pmax(laedev,0)
					elaes<-pmax(elaedev,0)
				} else {
					years <-cbind(developmentYears, settlementYears)
					laedev<-ultiDevFac(years,meanDevFac=object@laeDevFac@meanList,sdDevFac=object@laeDevFac@volList, distType=object@laeDevFac@distType)
					elaedev<-ultiDevFac(years,meanDevFac=object@laeDevFac@meanList, distType=object@laeDevFac@distType)
					laes<-as.numeric(tmpdata[,"ultimateLoss"])*pmax(laedev,0)
					elaes<-as.numeric(tmpdata[,"expectedLoss"])*pmax(elaedev,0)
				}


				tmpdata[,"ultimateLAE"]<-round(laes,2)#*sindex
				tmpdata[,"expectedLAE"]<-round(elaes,2)#*sindex
				tmpdata[,"reopenDate"]<-NA
				tmpdata[,"resettleDate"]<-NA
				tmpdata[,"reopenLoss"]<-NA
				tmpdata[,"Sim"]<-object@simno
				tmpdata<-tmpdata[,c("ClaimID","LoB","Type","status","occurrenceDate","reportDate","incurredLoss","osRatio","settlementDate","totalLoss","ultimateLoss","Deductible",
					"Limit","Paid","LAE","claimLiability","expectedLoss","ultimateLAE","expectedLAE","reopenDate","resettleDate","reopenLoss","Sim")]

				#return(claimData)
			} else {
				tmpdata = data.frame(ClaimID=character(),
								LoB=character(),
								Type=character(),
								status=character(),
								occurrenceDate=character(),
								reportDate=character(),
								incurredLoss=double(),
								osRatio=double(),
								settlementDate=character(),
								totalLoss=double(),
								ultimateLoss=double(),
								Deductible=double(),
								Limit=double(),
								Paid=double(),
								LAE=double(),
								claimLiability=character(),
								expectedLoss=double(),
								ultimateLAE=double(),
								expectedLAE=double(),
								reopenDate=character(),
								resettleDate=character(),
								reopenLoss=double(),
								Sim=double(),
								stringsAsFactors=FALSE)
				#return(noData)
			}
		}

		if (object@iROPEN == TRUE) {
			if(nrow(claimData)==0){
				nobs<-0
			} else {
				claimData <- claimData[claimData[,"status"] == "CLOSED",]# | as.Date(claimData[,"settlementDate"])<=as.Date(evaluationDate),]
				nobs <- nrow(claimData)
			}
			if(nobs>0){
				settlementDates<-as.Date(claimData[,"settlementDate"])
				occurrenceDates<-as.character(claimData[,"occurrenceDate"])
				reportDates<-as.character(claimData[,"reportDate"])
				osRatios<-as.numeric(claimData[,"osRatio"])
				incurredLosses<-as.numeric(claimData[,"incurredLoss"])
				closelags <- as.numeric(as.Date(evaluationDate) - as.Date(claimData[,"settlementDate"]))
				closeyears <- pmax(1,ceiling(closelags/365))
				developmentYears<-ceiling(as.numeric(as.Date(evaluationDate) - as.Date(claimData[,"occurrenceDate"]))/365)

				if(object@reopen@FacModel==TRUE & object@reopen@fun == "exponential"){
					reopenp<-object@reopen@paras[1]+object@reopen@paras[2]*developmentYears+object@reopen@paras[3]*incurredLosses+object@reopen@paras[4]*osRatios+rnorm(nobs,0,object@reopen@paras[length(object@reopen@paras)])
					if (length(object@reopen@xname)>0){
						for (i in c(1:length(object@reopen@xname))){
							reopenp<-reopenp+object@reopen@paras[4+i]*as.numeric(claimData[,object@reopen@paras[i]])
						}
					}
					reopens <- ifelse(runif(length(closeyears))<=log(reopenp),1,0)
					reopenDates <- ifelse(reopens == 0, NA, as.character(settlementDates + as.numeric(lapply(setMin(object@reopenLag, as.numeric(as.Date(evaluationDate)-as.Date(settlementDates))),doSample, n=1)) + as.numeric(as.Date(evaluationDate)-as.Date(settlementDates))) + 1)
					resettleDates <- ifelse(reopens == 0, NA, as.character(reopenDates + as.numeric(lapply(object@resettleLag,doSample, n=1))))
					sindex<-getIndex(object@severityIndex,resettleDates)/getIndex(object@severityIndex,as.character(settlementDates))
				} else if(object@reopen@FacModel==TRUE & object@reopen@fun == "log"){
					reopenp<-object@reopen@paras[1]+object@reopen@paras[2]*developmentYears+object@reopen@paras[3]*incurredLosses+object@reopen@paras[4]*osRatios+rnorm(nobs,0,object@reopen@paras[length(object@reopen@paras)])
					if (length(object@reopen@xname)>0){
						for (i in c(1:length(object@reopen@xname))){
							reopenp<-reopenp+object@reopen@paras[4+i]*as.numeric(claimData[,object@reopen@paras[i]])
						}
					}
					reopens <- ifelse(runif(length(closeyears))<=exp(reopenp),1,0)
					reopenDates <- ifelse(reopens == 0, NA, as.character(settlementDates + as.numeric(lapply(setMin(object@reopenLag, as.numeric(as.Date(evaluationDate)-as.Date(settlementDates))),doSample, n=1)) + as.numeric(as.Date(evaluationDate)-as.Date(settlementDates))) + 1)
					resettleDates <- ifelse(reopens == 0, NA, as.character(reopenDates + as.numeric(lapply(object@resettleLag,doSample, n=1))))
					sindex<-getIndex(object@severityIndex,resettleDates)/getIndex(object@severityIndex,as.character(settlementDates))
				} else if(object@reopen@FacModel==TRUE & object@reopen@fun == "inverse"){
					reopenp<-object@reopen@paras[1]+object@reopen@paras[2]*developmentYears+object@reopen@paras[3]*incurredLosses+object@reopen@paras[4]*osRatios+rnorm(nobs,0,object@reopen@paras[length(object@reopen@paras)])
					if (length(object@reopen@xname)>0){
						for (i in c(1:length(object@reopen@xname))){
							reopenp<-reopenp+object@reopen@paras[4+i]*as.numeric(claimData[,object@reopen@paras[i]])
						}
					}
					reopens <- ifelse(runif(length(closeyears))<=1/reopenp,1,0)
					reopenDates <- ifelse(reopens == 0, NA, as.character(settlementDates + as.numeric(lapply(setMin(object@reopenLag, as.numeric(as.Date(evaluationDate)-as.Date(settlementDates))),doSample, n=1)) + as.numeric(as.Date(evaluationDate)-as.Date(settlementDates))) + 1)
					resettleDates <- ifelse(reopens == 0, NA, as.character(reopenDates + as.numeric(lapply(object@resettleLag,doSample, n=1))))
					sindex<-getIndex(object@severityIndex,resettleDates)/getIndex(object@severityIndex,as.character(settlementDates))
				} else if (object@reopen@FacModel==TRUE & object@reopen@fun == "identity") {
					reopenp<-object@reopen@paras[1]+object@reopen@paras[2]*developmentYears+object@reopen@paras[3]*incurredLosses+object@reopen@paras[4]*osRatios+rnorm(nobs,0,object@reopen@paras[length(object@reopen@paras)])
					if (length(object@reopen@xname)>0){
						for (i in c(1:length(object@reopen@xname))){
							reopenp<-reopenp+object@reopen@paras[4+i]*as.numeric(claimData[,object@reopen@paras[i]])
						}
					}
					reopens <- ifelse(runif(length(closeyears))<=reopenp,1,0)
					reopenDates <- ifelse(reopens == 0, NA, as.character(settlementDates + as.numeric(lapply(setMin(object@reopenLag, as.numeric(as.Date(evaluationDate)-as.Date(settlementDates))),doSample, n=1))+as.numeric(as.Date(evaluationDate)-as.Date(settlementDates))+1))
					resettleDates <- ifelse(reopens == 0, NA, as.character(reopenDates + as.numeric(lapply(object@resettleLag,doSample, n=1))))
					sindex<-getIndex(object@severityIndex,resettleDates)/getIndex(object@severityIndex,as.character(settlementDates))
				} else {
					reopens<-rreopen(closeyears,object@reopen@meanList)
					reopenDates <- ifelse(reopens == 0, NA, as.character(settlementDates + as.numeric(lapply(setMin(object@reopenLag, as.numeric(as.Date(evaluationDate)-as.Date(settlementDates))),doSample, n=1))+as.numeric(as.Date(evaluationDate)-as.Date(settlementDates))+1))
					resettleDates <- ifelse(reopens == 0, NA, as.character(as.Date(reopenDates) + as.numeric(doSample(object@resettleLag, n=length(reopens)))))
					sindex<-getIndex(object@severityIndex,resettleDates)/getIndex(object@severityIndex,as.character(settlementDates))
				}

				settlementYears<-pmax(1,ceiling(as.numeric(as.Date(resettleDates) - as.Date(claimData[,"occurrenceDate"]))/365))

				if(object@roDevFac@FacModel==TRUE & object@roDevFac@fun == "exponential" & object@irDevFac==3){
					erodev<-object@roDevFac@paras[1]+object@roDevFac@paras[2]*developmentYears+object@roDevFac@paras[3]*incurredLosses+object@roDevFac@paras[4]*osRatios
					rodev<-erodev+rnorm(nobs,0,object@roDevFac@paras[length(object@roDevFac@paras)])
					if (length(object@roDevFac@xname)>0){
						for (i in c(1:length(object@roDevFac@xname))){
							rodev<-rodev+object@roDevFac@paras[4+i]*as.numeric(claimData[,object@roDevFac@xname[i]])
							erodev<-erodev+object@roDevFac@paras[4+i]*as.numeric(claimData[,object@roDevFac@xname[i]])
						}
					}
					ultimateLosses<-as.numeric(claimData[,"incurredLoss"])*pmax(log(rodev),0)
					expectedLosses<-as.numeric(claimData[,"incurredLoss"])*pmax(log(erodev),0)
					ultimateLosses<- ifelse(reopens == 0, NA, ultimateLosses*sindex)
					expectedLosses<- ifelse(reopens == 0, NA, expectedLosses*sindex)
				} else if(object@roDevFac@FacModel==TRUE & object@roDevFac@fun == "log" & object@irDevFac==3){
					erodev<-object@roDevFac@paras[1]+object@roDevFac@paras[2]*developmentYears+object@roDevFac@paras[3]*incurredLosses+object@roDevFac@paras[4]*osRatios
					rodev<-erodev+rnorm(nobs,0,object@roDevFac@paras[length(object@roDevFac@paras)])
					if (length(object@roDevFac@xname)>0){
						for (i in c(1:length(object@roDevFac@xname))){
							rodev<-rodev+object@roDevFac@paras[4+i]*as.numeric(claimData[,object@roDevFac@xname[i]])
							erodev<-erodev+object@roDevFac@paras[4+i]*as.numeric(claimData[,object@roDevFac@xname[i]])
						}
					}
					ultimateLosses<-as.numeric(claimData[,"incurredLoss"])*pmax(exp(rodev),0)
					expectedLosses<-as.numeric(claimData[,"incurredLoss"])*pmax(exp(erodev),0)
					ultimateLosses<- ifelse(reopens == 0, NA, ultimateLosses*sindex)
					expectedLosses<- ifelse(reopens == 0, NA, expectedLosses*sindex)
				} else if(object@roDevFac@FacModel==TRUE & object@roDevFac@fun == "inverse" & object@irDevFac==3){
					erodev<-object@roDevFac@paras[1]+object@roDevFac@paras[2]*developmentYears+object@roDevFac@paras[3]*incurredLosses+object@roDevFac@paras[4]*osRatios
					rodev<-erodev+rnorm(nobs,0,object@roDevFac@paras[length(object@roDevFac@paras)])
					if (length(object@roDevFac@xname)>0){
						for (i in c(1:length(object@roDevFac@xname))){
							rodev<-rodev+object@roDevFac@paras[4+i]*as.numeric(claimData[,object@roDevFac@xname[i]])
							erodev<-erodev+object@roDevFac@paras[4+i]*as.numeric(claimData[,object@roDevFac@xname[i]])
						}
					}
					ultimateLosses<-as.numeric(claimData[,"incurredLoss"])*pmax(1/rodev,0)
					expectedLosses<-as.numeric(claimData[,"incurredLoss"])*pmax(1/erodev,0)
					ultimateLosses<- ifelse(reopens == 0, NA, ultimateLosses*sindex)
					expectedLosses<- ifelse(reopens == 0, NA, expectedLosses*sindex)
				} else if (object@roDevFac@FacModel==TRUE & object@roDevFac@fun == "identity" & object@irDevFac==3) {
					erodev<-object@roDevFac@paras[1]+object@roDevFac@paras[2]*developmentYears+object@roDevFac@paras[3]*incurredLosses+object@roDevFac@paras[4]*osRatios
					rodev<-erodev+rnorm(nobs,0,object@roDevFac@paras[length(object@roDevFac@paras)])
					if (length(object@roDevFac@xname)>0){
						for (i in c(1:length(object@roDevFac@xname))){
							rodev<-rodev+object@roDevFac@paras[4+i]*as.numeric(claimData[,object@roDevFac@xname[i]])
							erodev<-erodev+object@roDevFac@paras[4+i]*as.numeric(claimData[,object@roDevFac@xname[i]])
						}
					}
					ultimateLosses<-as.numeric(claimData[,"incurredLoss"])*pmax(rodev,0)
					expectedLosses<-as.numeric(claimData[,"incurredLoss"])*pmax(erodev,0)
					ultimateLosses<- ifelse(reopens == 0, NA, ultimateLosses*sindex)
					expectedLosses<- ifelse(reopens == 0, NA, expectedLosses*sindex)
				} else if (object@irDevFac==3){
					years <- cbind(developmentYears, settlementYears)
					rodev<-ultiDevFac(years,meanDevFac=object@roDevFac@meanList,sdDevFac=object@roDevFac@volList,distType=object@roDevFac@distType)
					erodev<-ultiDevFac(years,meanDevFac=object@roDevFac@meanList,distType=object@roDevFac@distType)
					ultimateLosses<-as.numeric(claimData[,"incurredLoss"])*pmax(rodev,0)
					expectedLosses<-as.numeric(claimData[,"incurredLoss"])*pmax(erodev,0)
					ultimateLosses<- ifelse(reopens == 0, NA, ultimateLosses*sindex)
					expectedLosses<- ifelse(reopens == 0, NA, expectedLosses*sindex)
				} else if (object@irDevFac==2){
					claimData[,"Limit"] <- ifelse(is.na(claimData[,"Limit"]),1e10,claimData[,"Limit"])
					claimData[,"Deductible"] <- ifelse(is.na(claimData[,"Deductible"]),0,claimData[,"Deductible"])
					ultimateLosses<-pmax(as.numeric(claimData[,"incurredLoss"]),pmax((pmin(claimData[,"Limit"]+claimData[,"Deductible"],doSample(sobj,nobs)*sindex)-claimData[,"Deductible"]),0))
					basicmean<-sampleMean(object@severity)
					expectedLosses<-rep(basicmean,nobs)
					ultimateLosses<- ifelse(reopens == 0, NA, ultimateLosses)
					expectedLosses<- ifelse(reopens == 0, NA, expectedLosses)
				} else {
					claimData[,"Limit"] <- ifelse(is.na(claimData[,"Limit"]),1e10,claimData[,"Limit"])
					claimData[,"Deductible"] <- ifelse(is.na(claimData[,"Deductible"]),0,claimData[,"Deductible"])
					probs <- Probability(sobj,claimData[,"Paid"])
					newprobs <- probs + (1-probs)*runif(nobs)
					ultimateLosses<-pmax(as.numeric(claimData[,"Paid"]),pmax((pmin(claimData[,"Limit"]+claimData[,"Deductible"],Quantile(sobj,newprobs)*sindex)-claimData[,"Deductible"]),0))
					basicmean<-sampleMean(object@severity)
					expectedLosses<-rep(basicmean,nobs)
					ultimateLosses<- ifelse(reopens == 0, NA, ultimateLosses)
					expectedLosses<- ifelse(reopens == 0, NA, expectedLosses)
				}

				if(object@roDevFac@FacModel==TRUE & object@roDevFac@fun == "exponential" & object@irDevFac==3){
					laes<-as.numeric(claimData[,"LAE"])*pmax(log(rodev),0)
					elaes<-as.numeric(claimData[,"LAE"])*pmax(log(erodev),0)
					laes<- ifelse(reopens == 0, NA, laes*sindex)
					elaes<- ifelse(reopens == 0, NA, elaes*sindex)
				} else if(object@roDevFac@FacModel==TRUE & object@roDevFac@fun == "log" & object@irDevFac==3){
					laes<-as.numeric(claimData[,"LAE"])*pmax(exp(rodev),0)
					elaes<-as.numeric(claimData[,"LAE"])*pmax(exp(erodev),0)
					laes<- ifelse(reopens == 0, NA, laes*sindex)
					elaes<- ifelse(reopens == 0, NA, elaes*sindex)
				} else if(object@roDevFac@FacModel==TRUE & object@roDevFac@fun == "inverse" & object@irDevFac==3){
					laes<-as.numeric(claimData[,"LAE"])*pmax(1/rodev,0)
					elaes<-as.numeric(claimData[,"LAE"])*pmax(1/erodev,0)
					laes<- ifelse(reopens == 0, NA, laes*sindex)
					elaes<- ifelse(reopens == 0, NA, elaes*sindex)
				} else if (object@roDevFac@FacModel==TRUE & object@roDevFac@fun == "identity" & object@irDevFac==3) {
					laes<-as.numeric(claimData[,"LAE"])*pmax(rodev,0)
					elaes<-as.numeric(claimData[,"LAE"])*pmax(erodev,0)
					laes<- ifelse(reopens == 0, NA, laes*sindex)
					elaes<- ifelse(reopens == 0, NA, elaes*sindex)
				} else if (object@irDevFac==3){
					laes<-as.numeric(claimData[,"LAE"])*pmax(rodev,0)
					elaes<-as.numeric(claimData[,"LAE"])*pmax(erodev,0)
					laes<- ifelse(reopens == 0, NA, laes*sindex)
					elaes<- ifelse(reopens == 0, NA, elaes*sindex)
				} else if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "exponential" & object@irDevFac<3){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nobs,0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
					if (length(object@laeDevFac@xname)>0){
						for (i in c(1:length(object@laeDevFac@xname))){
							laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
							elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
						}
					}
					laes<-pmax(log(laedev),0)
					elaes<-pmax(log(elaedev),0)
				} else if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "log" & object@irDevFac<3){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nobs,0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
					if (length(object@laeDevFac@xname)>0){
						for (i in c(1:length(object@laeDevFac@xname))){
							laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
							elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
						}
					}
					laes<-pmax(exp(laedev),0)
					elaes<-pmax(exp(elaedev),0)
				} else if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "inverse" & object@irDevFac<3){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nobs,0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
					if (length(object@laeDevFac@xname)>0){
						for (i in c(1:length(object@laeDevFac@xname))){
							laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
							elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
						}
					}
					laes<-pmax(1/laedev,0)
					elaes<-pmax(1/elaedev,0)
				} else if (object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "identity") {
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nobs,0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
					if (length(object@laeDevFac@xname)>0){
						for (i in c(1:length(object@laeDevFac@xname))){
							laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
							elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
						}
					}
					laes<-pmax(laedev,0)
					elaes<-pmax(elaedev,0)
				} else {
					years <- cbind(developmentYears, settlementYears)
					laedev<-ultiDevFac(years,meanDevFac=object@laeDevFac@meanList,sdDevFac=object@laeDevFac@volList, distType=object@laeDevFac@distType)
					elaedev<-ultiDevFac(years,meanDevFac=object@laeDevFac@meanList, distType=object@laeDevFac@distType)
					laes<-ultimateLosses*pmax(laedev,0)
					elaes<-expectedLosses*pmax(elaedev,0)
				}

				claimData[,"ultimateLoss"]<-claimData[,"incurredLoss"]
				claimData[,"expectedLoss"]<-round(expectedLosses,2)
				claimData[,"ultimateLAE"]<-round(laes,2)#*sindex
				claimData[,"expectedLAE"]<-round(elaes,2)#*sindex
				claimData[,"reopenDate"]<-reopenDates
				claimData[,"resettleDate"]<-resettleDates
				claimData[,"Limit"] <- ifelse(is.na(claimData[,"Limit"]),1e10,claimData[,"Limit"])
				claimData[,"reopenLoss"]<-pmin(round(ultimateLosses,2),claimData[,"Limit"])
				claimData[,"Sim"]<-object@simno
				claimData<-claimData[,c("ClaimID","LoB","Type","status","occurrenceDate","reportDate","incurredLoss","osRatio","settlementDate","totalLoss","ultimateLoss","Deductible",
					"Limit","Paid","LAE","claimLiability","expectedLoss","ultimateLAE","expectedLAE","reopenDate","resettleDate","reopenLoss","Sim")]

				tmpdata <- rbind(tmpdata,claimData)
			}

		}

		if (object@iIBNR==TRUE) {
			basicmean<-sampleMean(object@severity)
			days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
			nid <- length(object@IBNRfreqIndex@monthlyIndex)
			startyear <- substr(as.character(object@IBNRfreqIndex@startDate),1,4)
			startmonth <- substr(as.character(object@IBNRfreqIndex@startDate),6,7)
			ibnrdata = data.frame(ClaimID=character(),
									LoB=character(),
									Type=character(),
									status=character(),
									occurrenceDate=character(),
									reportDate=character(),
									incurredLoss=double(),
									osRatio=double(),
									settlementDate=character(),
									totalLoss=double(),
									ultimateLoss=double(),
									Deductible=double(),
									Limit=double(),
									Paid=double(),
									LAE=double(),
									claimLiability=character(),
									expectedLoss=double(),
									ultimateLAE=double(),
									expectedLAE=double(),
									reopenDate=character(),
									resettleDate=character(),
									reopenLoss=double(),
									Sim=double(),
									stringsAsFactors=FALSE)
			if(object@iCopula==TRUE){
				ssindex<-vector()
				for(i in c(1:nid)){
					if(object@IBNRfreqIndex@monthlyIndex[i] > 0) {
						currentmonth <- (i-1) %% 12 + as.numeric(startmonth)
						currentyear <- floor((i-1)/12)+as.numeric(startyear)
						if(currentmonth>12) {
							currentmonth <- currentmonth-12
							currentyear <- currentyear+1
						}
						umax <- days[currentmonth]
						if(currentyear <10) {
							currentmonth <- paste0("0",currentmonth)
						}
						occurrenceDate<-as.Date(paste0(as.character(currentyear),"-",as.character(currentmonth),"-01"))+round(runif(object@IBNRfreqIndex@monthlyIndex[i],min=0,max=(umax-1)))
						ibnrs<-occurrenceDate
						sobjtrunc <- object@ssrCopula@marginal[[1]]@truncated
						robjtrunc <- object@ssrCopula@marginal[[3]]@truncated
						robjmin <- object@ssrCopula@marginal[[3]]@min
						object@ssrCopula@marginal[[1]]@truncated <- FALSE
						object@ssrCopula@marginal[[3]]@min <- as.numeric(as.Date(evaluationDate)-as.Date(paste0(as.character(currentyear),"-",as.character(currentmonth),"-01")))
						object@ssrCopula@marginal[[3]]@truncated <- TRUE
						samples <- copulaSample(object@ssrCopula,object@IBNRfreqIndex@monthlyIndex[i])
						object@ssrCopula@marginal[[1]]@truncated <- sobjtrunc
						object@ssrCopula@marginal[[3]]@truncated <- robjtrunc
						object@ssrCopula@marginal[[3]]@min <- robjmin
						reportDate<-as.Date(occurrenceDate + round(samples[,3]) + as.numeric(as.Date(evaluationDate)-as.Date(occurrenceDate))+1)
						settlementDate<-as.Date(reportDate + round(samples[,2]))
						sindex<-getIndex(object@severityIndex,settlementDate)/getIndex(object@severityIndex,evaluationDate)
						ssindex<-c(ssindex,sindex)
						deductibles<-round(doSample(object@deductible,nrow(samples)),-2)
						limits<-round(doSample(object@limit,nrow(samples)),-2)
						totalLoss<-samples[,1]*sindex
						ultimateLoss<-pmax(0,pmin(limits+deductibles,totalLoss)-deductibles)
						expectedLoss<-basicmean*sindex
						ibnrs<-cbind(as.character(occurrenceDate),as.character(reportDate),as.character(settlementDate),round(ultimateLoss,2),round(expectedLoss,2))
						ibnrs<-as.data.frame(ibnrs)
						colnames(ibnrs)<-c("occurrenceDate","reportDate","settlementDate","ultimateLoss","expectedLoss")
						ibnrs$totalLoss<-totalLoss
						ibnrs$ultimateLoss<-as.numeric(as.character(ibnrs$ultimateLoss))

						settlementYears<-pmax(1,ceiling(as.numeric(as.Date(settlementDate) - as.Date(occurrenceDate))/365))
						zeros<-simP0(settlementYears,object@p0@meanList)
						exzeros<-expectZeros(settlementYears,object@p0@meanList)

						ibnrs$claimLiability <- ifelse(zeros==0,FALSE,TRUE)
						ibnrs$ultimateLoss <- ifelse(ibnrs$claimLiability == FALSE, 0, ibnrs$ultimateLoss)
						ibnrs[,"Deductible"]<-deductibles
						ibnrs[,"Limit"]<-limits
						ibnrs[,"Paid"]<-ibnrs$ultimateLoss
						ibnrs$expectedLoss<-as.numeric(as.character(ibnrs$expectedLoss))*(1-exzeros)
						ibnrs[,"LoB"]<-object@line
						ibnrs[,"Type"]<-object@claimType
						ibnrs[,"status"]<-"IBNR"
						ibnrs[,"incurredLoss"]<-ibnrs$ultimateLoss
						ibnrs[,"osRatio"]<-0
						ibnrs[,"ClaimID"]<-as.character(paste0(i,"X",c(1:object@IBNRfreqIndex@monthlyIndex[i])))
						ibnrdata<-rbind(ibnrdata,ibnrs)
					}
				}
			} else {
				ssindex<-vector()
				for(i in c(1:nid)){
					if(object@IBNRfreqIndex@monthlyIndex[i] > 0) {
						currentmonth <- (i-1) %% 12 + as.numeric(startmonth)
						currentyear <- floor((i-1)/12)+as.numeric(startyear)
						if(currentmonth>12) {
							currentmonth <- currentmonth-12
							currentyear <- currentyear+1
						}
						umax <- days[currentmonth]
						if(currentyear <10) {
							currentmonth <- paste0("0",currentmonth)
						}
						occurrenceDate<-as.Date(paste0(as.character(currentyear),"-",as.character(currentmonth),"-01"))+round(runif(object@IBNRfreqIndex@monthlyIndex[i],min=0,max=(umax-1)))
						ibnrs<-occurrenceDate
						object@reportLag@min <- as.numeric(as.Date(evaluationDate)-as.Date(paste0(as.character(currentyear),"-",as.character(currentmonth),"-01")))
						object@reportLag@truncated <- TRUE
						reportDate<-as.Date(occurrenceDate + round(doSample(object@reportLag,object@IBNRfreqIndex@monthlyIndex[i])) + as.numeric(as.Date(evaluationDate)-as.Date(occurrenceDate))+1)
						settlementDate<-as.Date(reportDate + round(doSample(object@settlementLag,object@IBNRfreqIndex@monthlyIndex[i])))
						sindex<-getIndex(object@severityIndex,settlementDate)#/getIndex(object@severityIndex,evaluationDate)
						ssindex<-c(ssindex,sindex)
						deductibles<-round(doSample(object@deductible,object@IBNRfreqIndex@monthlyIndex[i]),-2)
						limits<-round(doSample(object@limit,object@IBNRfreqIndex@monthlyIndex[i]),-2)
						totalLoss<-doSample(sobj,object@IBNRfreqIndex@monthlyIndex[i])*sindex
						ultimateLoss<-pmax(0,pmin(limits+deductibles,totalLoss)-deductibles)
						expectedLoss<-basicmean*sindex
						ibnrs<-cbind(as.character(occurrenceDate),as.character(reportDate),as.character(settlementDate),round(ultimateLoss,2),round(expectedLoss,2))
						ibnrs<-as.data.frame(ibnrs)
						colnames(ibnrs)<-c("occurrenceDate","reportDate","settlementDate","ultimateLoss","expectedLoss")
						ibnrs$totalLoss<-totalLoss
						ibnrs$ultimateLoss<-as.numeric(as.character(ibnrs$ultimateLoss))

						settlementYears<-pmax(1,ceiling(as.numeric(as.Date(settlementDate) - as.Date(occurrenceDate))/365))
						zeros<-simP0(settlementYears,object@p0@meanList)
						exzeros<-expectZeros(settlementYears,object@p0@meanList)

						ibnrs$claimLiability <- ifelse(zeros==0,FALSE,TRUE)
						ibnrs$ultimateLoss <- ifelse(ibnrs$claimLiability == FALSE, 0, ibnrs$ultimateLoss)
						ibnrs[,"Deductible"]<-deductibles
						ibnrs[,"Limit"]<-limits
						ibnrs[,"Paid"]<-ibnrs$ultimateLoss
						ibnrs$expectedLoss<-as.numeric(as.character(ibnrs$expectedLoss))*(1-exzeros)
						ibnrs[,"LoB"]<-object@line
						ibnrs[,"Type"]<-object@claimType
						ibnrs[,"status"]<-"IBNR"
						ibnrs[,"incurredLoss"]<-ibnrs$ultimateLoss
						ibnrs[,"osRatio"]<-0
						ibnrs[,"ClaimID"]<-as.character(paste0(i,"X",c(1:object@IBNRfreqIndex@monthlyIndex[i])))
						ibnrdata<-rbind(ibnrdata,ibnrs)
					}
				}
			}

			if (nrow(ibnrdata)>0) {
				developmentYears<-ceiling(as.numeric(as.Date(ibnrdata[,"reportDate"]) - as.Date(ibnrdata[,"occurrenceDate"]))/365)
				settlementYears<-pmax(1,ceiling(as.numeric(as.Date(ibnrdata[,"settlementDate"]) - as.Date(ibnrdata[,"occurrenceDate"]))/365))
				incurredLosses<-ibnrdata[,"ultimateLoss"]
				osRatios<-ibnrdata[,"osRatio"]
				if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "exponential"){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nrow(ibnrdata),0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
				#	if (length(object@laeDevFac@xname)>0){
				#		for (i in c(1:length(object@laeDevFac@xname))){
				#			laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#			elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#		}
				#	}
					laes<-pmax(log(laedev),0)
					elaes<-pmax(log(elaedev),0)
				} else if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "log"){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nrow(ibnrdata),0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
				#	if (length(object@laeDevFac@xname)>0){
				#		for (i in c(1:length(object@laeDevFac@xname))){
				#			laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#			elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#		}
				#	}
					laes<-pmax(exp(laedev),0)
					elaes<-pmax(exp(elaedev),0)
				} else if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "inverse"){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nrow(ibnrdata),0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
				#	if (length(object@laeDevFac@xname)>0){
				#		for (i in c(1:length(object@laeDevFac@xname))){
				#			laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#			elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#		}
				#	}
					laes<-pmax(1/laedev,0)
					elaes<-pmax(1/elaedev,0)
				} else if (object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "identity") {
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nrow(ibnrdata),0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
				#	if (length(object@laeDevFac@xname)>0){
				#		for (i in c(1:length(object@laeDevFac@xname))){
				#			laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#			elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#		}
				#	}
					laes<-pmax(laedev,0)
					elaes<-pmax(elaedev,0)
				} else {
					years <- cbind(developmentYears,settlementYears)
					laedev<-ultiDevFac(years,meanDevFac=object@laeDevFac@meanList,sdDevFac=object@laeDevFac@volList, distType=object@laeDevFac@distType)
					elaedev<-ultiDevFac(years,meanDevFac=object@laeDevFac@meanList, distType=object@laeDevFac@distType)
					laes<-as.numeric(ibnrdata[,"ultimateLoss"])*pmax(laedev,0)
					elaes<-as.numeric(ibnrdata[,"expectedLoss"])*pmax(elaedev,0)
				}
				ibnrdata[,"ultimateLAE"]<-round(laes,2)#*ssindex
				ibnrdata[,"expectedLAE"]<-round(elaes,2)#*ssindex
				ibnrdata[,"reopenDate"]<-NA
				ibnrdata[,"resettleDate"]<-NA
				ibnrdata[,"reopenLoss"]<-NA
				ibnrdata[,"LAE"]<-ibnrdata[,"ultimateLAE"]*1
				ibnrdata[,"Sim"]<-object@simno
#				ibnrdata<-ibnrdata[,c(11,6,7,8,1,2,9,10,3,4,17,5,12:16,18)]
				ibnrdata<-ibnrdata[,c("ClaimID","LoB","Type","status","occurrenceDate","reportDate","incurredLoss","osRatio","settlementDate","totalLoss","ultimateLoss","Deductible",
									"Limit","Paid","LAE","claimLiability","expectedLoss","ultimateLAE","expectedLAE","reopenDate","resettleDate","reopenLoss","Sim")]
			tmpdata <- rbind(tmpdata,ibnrdata)
			}

		}

		if (object@iUPR == TRUE)  {
			basicmean<-sampleMean(object@severity)
			days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
			nid <- length(object@UPRfreqIndex@monthlyIndex)
			startyear <- substr(as.character(object@UPRfreqIndex@startDate),1,4)
			startmonth <- substr(as.character(object@UPRfreqIndex@startDate),6,7)
			uprdata = data.frame(ClaimID=character(),
										LoB=character(),
										Type=character(),
										status=character(),
										occurrenceDate=character(),
										reportDate=character(),
										incurredLoss=double(),
										osRatio=double(),
										settlementDate=character(),
										totalLoss=double(),
										ultimateLoss=double(),
										Deductible=double(),
										Limit=double(),
										Paid=double(),
										LAE=double(),
										claimLiability=character(),
										expectedLoss=double(),
										ultimateLAE=double(),
										expectedLAE=double(),
										reopenDate=character(),
										resettleDate=character(),
										reopenLoss=double(),
										Sim=double(),
										stringsAsFactors=FALSE)

			if(object@iCopula==TRUE){
				ssindex<-vector()
				for(i in c(1:nid)){
					if(object@UPRfreqIndex@monthlyIndex[i] > 0) {
						currentmonth <- (i-1) %% 12 + as.numeric(startmonth)
						currentyear <- floor((i-1)/12)+as.numeric(startyear)
						if(currentmonth>12) {
							currentmonth <- currentmonth-12
							currentyear <- currentyear+1
						}
						umax <- days[currentmonth]
						if(currentyear <10) {
							currentmonth <- paste0("0",currentmonth)
						}
						occurrenceDate<-as.Date(paste0(as.character(currentyear),"-",as.character(currentmonth),"-01"))+round(runif(object@UPRfreqIndex@monthlyIndex[i],min=0,max=(umax-1)))
						uprs<-occurrenceDate
						sobjtrunc <- object@ssrCopula@marginal[[1]]@truncated
						object@ssrCopula@marginal[[1]]@truncated <- FALSE
						samples <- copulaSample(object@ssrCopula,object@UPRfreqIndex@monthlyIndex[i])
						object@ssrCopula@marginal[[1]]@truncated <- sobjtrunc
						reportDate<-as.Date(occurrenceDate + round(samples[,3]))# + as.numeric(as.Date(evaluationDate)-as.Date(occurrenceDate))+1)
						settlementDate<-as.Date(reportDate + round(samples[,2]))
						sindex<-getIndex(object@severityIndex,settlementDate)#/getIndex(object@severityIndex,evaluationDate)
						ssindex<-c(ssindex,sindex)
						deductibles<-round(doSample(object@deductible,nrow(samples)),-2)
						limits<-round(doSample(object@limit,nrow(samples)),-2)
						totalLoss<-samples[,1]*sindex
						ultimateLoss<-pmax(0,pmin(limits+deductibles,totalLoss)-deductibles)
						expectedLoss<-basicmean*sindex
						uprs<-cbind(as.character(occurrenceDate),as.character(reportDate),as.character(settlementDate),round(ultimateLoss,2),round(expectedLoss,2))
						uprs<-as.data.frame(uprs)
						colnames(uprs)<-c("occurrenceDate","reportDate","settlementDate","ultimateLoss","expectedLoss")
						uprs$totalLoss<-totalLoss
						uprs$ultimateLoss<-as.numeric(as.character(uprs$ultimateLoss))

						settlementYears<-pmax(1,ceiling(as.numeric(as.Date(settlementDate) - as.Date(occurrenceDate))/365))
						zeros<-simP0(settlementYears,object@p0@meanList)
						exzeros<-expectZeros(settlementYears,object@p0@meanList)

						uprs$claimLiability <- ifelse(zeros==0,FALSE,TRUE)
						uprs$ultimateLoss <- ifelse(uprs$claimLiability == FALSE, 0, uprs$ultimateLoss)
						uprs[,"Deductible"]<-deductibles
						uprs[,"Limit"]<-limits
						uprs[,"Paid"]<- uprs$ultimateLoss
						uprs$expectedLoss<-as.numeric(as.character(uprs$expectedLoss))*(1-exzeros)
						uprs[,"LoB"]<-object@line
						uprs[,"Type"]<-object@claimType
						uprs[,"status"]<-"UPR"
						uprs[,"incurredLoss"]<-uprs$ultimateLoss
						uprs[,"osRatio"]<-0
						uprs[,"ClaimID"]<-as.character(paste0(i,"X",c(1:object@UPRfreqIndex@monthlyIndex[i])))
						uprdata<-rbind(uprdata,uprs)
					}
				}
			} else {
				ssindex<-vector()
				for(i in c(1:nid)){
					if(object@UPRfreqIndex@monthlyIndex[i] > 0) {
						currentmonth <- (i-1) %% 12 + as.numeric(startmonth)
						currentyear <- floor((i-1)/12)+as.numeric(startyear)
						if(currentmonth>12) {
							currentmonth <- currentmonth-12
							currentyear <- currentyear+1
						}
						umax <- days[currentmonth]
						if(currentyear <10) {
							currentmonth <- paste0("0",currentmonth)
						}
						occurrenceDate<-as.Date(paste0(as.character(currentyear),"-",as.character(currentmonth),"-01"))+round(runif(object@UPRfreqIndex@monthlyIndex[i],min=0,max=(umax-1)))
						uprs<-occurrenceDate
#						object@reportLag@min <- as.numeric(as.Date(evaluationDate)-as.Date(paste0(as.character(currentyear),"-",as.character(currentmonth),"-01")))
#						object@reportLag@truncated <- TRUE
						reportDate<-as.Date(occurrenceDate + round(doSample(object@reportLag,object@UPRfreqIndex@monthlyIndex[i])))
						settlementDate<-as.Date(reportDate + round(doSample(object@settlementLag,object@UPRfreqIndex@monthlyIndex[i])))
						sindex<-getIndex(object@severityIndex,settlementDate)/getIndex(object@severityIndex,evaluationDate)
						ssindex<-c(ssindex,sindex)
						deductibles<-round(doSample(object@deductible,object@UPRfreqIndex@monthlyIndex[i]),-2)
						limits<-round(doSample(object@limit,object@UPRfreqIndex@monthlyIndex[i]),-2)
						totalLoss<-doSample(sobj,object@UPRfreqIndex@monthlyIndex[i])*sindex
						ultimateLoss<-pmax(0,pmin(limits+deductibles,totalLoss)-deductibles)
						expectedLoss<-basicmean*sindex
						uprs<-cbind(as.character(occurrenceDate),as.character(reportDate),as.character(settlementDate),round(ultimateLoss,2),round(expectedLoss,2))
						uprs<-as.data.frame(uprs)
						colnames(uprs)<-c("occurrenceDate","reportDate","settlementDate","ultimateLoss","expectedLoss")
						uprs$totalLoss<-totalLoss
						uprs$ultimateLoss<-as.numeric(as.character(uprs$ultimateLoss))

						settlementYears<-pmax(1,ceiling(as.numeric(as.Date(settlementDate) - as.Date(occurrenceDate))/365))
						zeros<-simP0(settlementYears,object@p0@meanList)
						exzeros<-expectZeros(settlementYears,object@p0@meanList)

						uprs$claimLiability <- ifelse(zeros==0,FALSE,TRUE)
						uprs$ultimateLoss <- ifelse(uprs$claimLiability == FALSE, 0, uprs$ultimateLoss)
						uprs[,"Deductible"]<-deductibles
						uprs[,"Limit"]<-limits
						uprs[,"Paid"]<-uprs$ultimateLoss
						uprs$expectedLoss<-as.numeric(as.character(uprs$expectedLoss))*(1-exzeros)
						uprs[,"LoB"]<-object@line
						uprs[,"Type"]<-object@claimType
						uprs[,"status"]<-"UPR"
						uprs[,"incurredLoss"]<-uprs$ultimateLoss
						uprs[,"osRatio"]<-0
						uprs[,"ClaimID"]<-as.character(paste0(i,"X",c(1:object@UPRfreqIndex@monthlyIndex[i])))
						uprdata<-rbind(uprdata,uprs)
					}
				}
			}

			if (nrow(uprdata)>0) {
				developmentYears<-ceiling(as.numeric(as.Date(uprdata[,"reportDate"]) - as.Date(uprdata[,"occurrenceDate"]))/365)
				settlementYears<-pmax(1,ceiling(as.numeric(as.Date(uprdata[,"settlementDate"]) - as.Date(uprdata[,"occurrenceDate"]))/365))
				incurredLosses<-uprdata[,"ultimateLoss"]
				osRatios<-uprdata[,"osRatio"]
				if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "exponential"){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nrow(uprdata),0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
				#	if (length(object@laeDevFac@xname)>0){
				#		for (i in c(1:length(object@laeDevFac@xname))){
				#			laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#			elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#		}
				#	}
					laes<-pmax(log(laedev),0)
					elaes<-pmax(log(elaedev),0)
				} else if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "log"){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nrow(uprdata),0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
				#	if (length(object@laeDevFac@xname)>0){
				#		for (i in c(1:length(object@laeDevFac@xname))){
				#			laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#			elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#		}
				#	}
					laes<-pmax(exp(laedev),0)
					elaes<-pmax(exp(elaedev),0)
				} else if(object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "inverse"){
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nrow(uprdata),0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
				#	if (length(object@laeDevFac@xname)>0){
				#		for (i in c(1:length(object@laeDevFac@xname))){
				#			laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#			elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#		}
				#	}
					laes<-pmax(1/laedev,0)
					elaes<-pmax(1/elaedev,0)
				} else if (object@laeDevFac@FacModel==TRUE & object@laeDevFac@fun == "identity") {
					elaedev<-object@laeDevFac@paras[1]+object@laeDevFac@paras[2]*developmentYears+object@laeDevFac@paras[3]*incurredLosses+object@laeDevFac@paras[4]*osRatios
					laedev<-elaedev+rnorm(nrow(uprdata),0,object@laeDevFac@paras[length(object@laeDevFac@paras)])
				#	if (length(object@laeDevFac@xname)>0){
				#		for (i in c(1:length(object@laeDevFac@xname))){
				#			laedev<-laedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#			elaedev<-elaedev+object@laeDevFac@paras[4+i]*as.numeric(claimData[,object@laeDevFac@xname[i]])
				#		}
				#	}
					laes<-pmax(laedev,0)
					elaes<-pmax(elaedev,0)
				} else {
					years <- cbind(developmentYears,settlementYears)
					laedev<-ultiDevFac(years,meanDevFac=object@laeDevFac@meanList,sdDevFac=object@laeDevFac@volList, distType=object@laeDevFac@distType)
					elaedev<-ultiDevFac(years,meanDevFac=object@laeDevFac@meanList, distType=object@laeDevFac@distType)
					laes<-as.numeric(uprdata[,"ultimateLoss"])*pmax(laedev,0)
					elaes<-as.numeric(uprdata[,"expectedLoss"])*pmax(elaedev,0)
				}
				uprdata[,"ultimateLAE"]<-round(laes,2)#*ssindex
				uprdata[,"expectedLAE"]<-round(elaes,2)#*ssindex
				uprdata[,"reopenDate"]<-NA
				uprdata[,"resettleDate"]<-NA
				uprdata[,"reopenLoss"]<-NA
				uprdata[,"LAE"]<-uprdata[,"ultimateLAE"]*1
				uprdata[,"Sim"]<-object@simno
				uprdata<-uprdata[,c("ClaimID","LoB","Type","status","occurrenceDate","reportDate","incurredLoss","osRatio","settlementDate","totalLoss","ultimateLoss","Deductible",
									"Limit","Paid","LAE","claimLiability","expectedLoss","ultimateLAE","expectedLAE","reopenDate","resettleDate","reopenLoss","Sim")]
			tmpdata <- rbind(tmpdata,uprdata)
			}
		}

		return(tmpdata)

	}, error = function(err){
		print(paste0(">>>Critical Error for claim simulation: Line-", object@line, " Type-", object@claimType, " Sim No.-", object@simno, ", ",err))
		gc()
	})
})
