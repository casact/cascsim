#' An S4 class to represent a simulation task.
#'
#' @slot startNo The starting simulation index.
#' @slot simNo Number of simulation.
#' @slot lines A string vector to identify the business line(s) to be simulated.
#' @slot types A string vector to identify the claim types to be simulated.
#' @slot classes A string vector to identify the claim classes(RBNER, ROPEN, IBNR or UPR) to be simulated.
#' @slot claimobjs A list of claim objects.
#' @slot workingFolder A string to specify the working folder where the simulation results will be saved.
#' @slot iCopula A Boolean indicating whether to use copula for frequency simulation.
#' @slot freqCopula Frequency copula.
#' @slot iSummary A Boolean indicating whether to summarzie the simulation results.
#' @slot iReport A Boolean indicating whether to generate an HTML report.
#' @slot iFit A Boolean indicating whether to fit some simulation parameters based on claim data.
#' @slot ncores Number of cores used for simulation.
#' @slot tag A unique tag for the simulation object including date and a random ID.
#' @slot fitfile A string to set the distribution fitting file name. If omitted, a name based on tag will be used.
#' @slot copfile A string to set the copula fitting file name. If omitted, a name based on tag will be used.
#' @slot facfile A string to set the factor fitting file name. Factor table is development year dependant. It could be the probability of zero payment, reopen probability, or loss development factors. If omitted, a name based on tag will be used.
#' @slot fitRpt A string to set the distribution fitting html report file name. If omitted, a name based on tag will be used.
#' @slot simfile A string to set the simulation result file name. If omitted, a name based on tag will be used.
#' @slot sumfile A string to set the summary file name. If omitted, a name based on tag will be used.
#' @slot plog A string to set the parallel run log file name. If omitted, a name based on tag will be used.
#' @slot htmlRpt A string to set the html report name. If omitted, a name based on tag will be used.
#' @slot libpath A string to the R liabrary folder where required packages are installed.
setClass("Simulation", 
	slots=c(
			startNo="numeric",
			simNo="numeric",
			lines="vector",
			types="vector",
#			classes="vector",
			iRBNER="logical",
			iROPEN="logical",
			iIBNR="logical",
			iUPR="logical",
			claimobjs="list",
			workingFolder="character",
			iCopula="logical",
			freqCopula="CopulaObj",
			iSummary="logical",
			iReport="logical",
			iFit="logical",
			ncores="numeric",
			tag="character",
			fitfile="character",
			copfile="character",
			facfile="character",
			fitRpt="character",
			simfile="character",
			sumfile="character",
			plog="character",
			htmlRpt="character",
			libpath="character"
	),
	prototype=list(	
			startNo=1,
			simNo=1,
			lines=vector(),
			types=vector(),
#			classes=c("RBNER","ROPEN","IBNR","UPR"),
			iRBNER=TRUE,
			iROPEN=TRUE,
			iIBNR=TRUE,
			iUPR=TRUE,
			claimobjs=list(),
			workingFolder="",
			iCopula=FALSE,
			freqCopula=new("CopulaObj",param=c(0,0,0),dimension=3),
			iSummary=TRUE,
			iReport=TRUE,
			iFit=TRUE,
			ncores=1,
			tag=paste0(format(Sys.Date(),"%Y-%m-%d"),"-",round(runif(1,1,10000))),
			fitfile="",
			copfile="",
			facfile="",
			fitRpt="",
			simfile="",
			sumfile="",
			plog="",
			htmlRpt="",
			libpath=""
	)
)

#' Claim data fitting analysis at line/type/status level
#' @param object Simulation object
#' @param claimData claim data including existing claims for RBNER and claim reopenness analysis
#' @param startDate Date after which claims are analyzed
#' @param evaluationDate Date of evaluation for existing claims and IBNR
#' @param discreteDist List of discrete distributions to try fitting (report lag, settlemet lag, frequency)
#' @param continuousDist List of continuous distribution to try fitting (severity)
#' @param copulaList List of copula to try fitting
#' @param fReportLag Boolean variable to indicate whether report lag needs to be fitted.
#' @param fSettlementLag Boolean variable to indicate whether settlement lag needs to be fitted.
#' @param fFrequency Boolean variable to indicate whether monthly frequency needs to be fitted.
#' @param fSeverity Boolean variable to indicate whether severity needs to be fitted.
#' @param fSSRCorrelation Boolean variable to indicate whether copula among severity, report lag and settlement lag needs to be fitted.
#' @param fFreqCorrelation Boolean variable to indicate whether copula among frequencies of business lines needs to be fitted.																		
#' @param copulaTest Whether to test copula. The testing could take a very long time.
#' @param iTotalLoss Boolean variable to indicate whether total loss before deductible and limit is available for severity fitting
#' @param fDeductible Boolean variable to indicate whether deductible empirical distribution needs to be fitted.
#' @param fLimit Boolean variable to indicate whether limit empirical distribution needs to be fitted.
#' @param check Boolean variable to indicate whether graph of each tried distribution fitting needs to be generated and saved.																		
#' @examples
#' library(cascsim)
#' data(claimdata)
#' lines<-c("Auto","Property","Liab")
#' types<-c("N","H")
#' #exposure index
#' index1 <- new("Index",monthlyIndex=c(rep(1,11),cumprod(c(1,rep(1.5^(1/12),11))),cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),rep(1.4,301)))
#' #severity index
#' index2 <- new("Index",monthlyIndex=c(cumprod(c(1,rep(1.03^(1/12),59))),rep(1.03^(5),300)))
#' objan <- new("ClaimType", line="Auto",claimType="N",exposureIndex=index1,severityIndex=index2)
#' objah <- new("ClaimType", line="Auto",claimType="H",exposureIndex=index1,severityIndex=index2)
#' objpn <- new("ClaimType", line="Property",claimType="N",exposureIndex=index1,severityIndex=index2)
#' objph <- new("ClaimType", line="Property",claimType="H",exposureIndex=index1,severityIndex=index2)
#' objln <- new("ClaimType", line="Liab",claimType="N",exposureIndex=index1,severityIndex=index2)
#' objlh <- new("ClaimType", line="Liab",claimType="H",exposureIndex=index1,severityIndex=index2)
#' objlist <- list(objan,objah,objpn,objph,objln,objlh)
#' simobj <- new("Simulation",lines=lines,types=types,claimobjs=objlist,iFit=TRUE, iCopula=TRUE, iReport=TRUE)
#' simobj <- claimFitting(simobj,claimdata)
#' @rdname claimFitting
#' @export
setGeneric("claimFitting", function(object, claimData, ...) standardGeneric("claimFitting"))
setMethod("claimFitting", signature("Simulation", "data.frame"), function(object, claimData, startDate=as.Date("2012-01-01"),evaluationDate=as.Date("2016-12-31"),
																			lineList = object@lines,
																			typeList = object@types,
																			discreteDist = c("Poisson","NegativeBinomial","Geometric"),
																			continuousDist = c("Normal","Lognormal","Pareto","Weibull","Gamma","Uniform","Exponential"),
																			copulaList = c("normal"),#c("normal","clayton","gumbel","frank","joe", "t")
																			fReportLag = TRUE,
																			fSettlementLag = TRUE,
																			fFrequency = TRUE,
																			fSeverity = TRUE,
																			fSSRCorrelation = TRUE,
																			fFreqCorrelation = TRUE,																			
																			copulaTest = TRUE,
																			iTotalLoss = TRUE,
																			fDeductible = TRUE,
																			fLimit = TRUE,
																			check = TRUE)
{
	tryCatch({
		
		print("Fitting process started.")
		
		startDate <- toDate(startDate)
		evaluationDate <- toDate(evaluationDate)
		
		if (nrow(claimData) > 0) {
			claimData[,"occurrenceDate"] = toDate(claimData[,"occurrenceDate"])
			claimData[,"reportDate"] = toDate(claimData[,"reportDate"])
			claimData[,"settlementDate"] = toDate(claimData[,"settlementDate"])
			claimData[,"status"] = toupper(claimData[,"status"])
			claimData[,"claimLiability"] = toupper(claimData[,"claimLiability"])
			claimData <- claimData[as.Date(claimData[,"occurrenceDate"])>=as.Date(startDate),]
			claimData <- claimData[as.Date(claimData[,"reportDate"])<=as.Date(evaluationDate),]
		}

		if (as.numeric(as.Date(evaluationDate) - as.Date(startDate))<60 & fFrequency == TRUE) {
			fFrequency = FALSE
			fFreqCorrelation = FALSE
			print("Frequency and Frequency Copula Fitting are turned off due to insufficient data.")
			cat("\n")
		}
		
		
		if(length(object@lines)>0 & length(object@types)>0 & length(object@claimobjs)>0 & nrow(claimData)>0) {
			
			if(object@iFit==TRUE){

				if(nchar(object@workingFolder)>0 & dir.exists(object@workingFolder)){
					setwd(object@workingFolder)
				} else if(nchar(object@workingFolder)>0 ){
					dir.create(object@workingFolder)
					setwd(object@workingFolder)
				}
				
				if(object@iReport==TRUE){
					dir.create("fit")
					setwd(paste0(getwd(),"/fit"))
				}
				
				if(object@fitfile==""){
					fitname <- paste0("fit",object@tag,".csv")
				} else {
					fitname <- paste0(object@fitfile,".csv")
				}
				
				if(object@fitfile==""){
					empname <- paste0("emp",object@tag,".csv")
				} else {
					empname <- paste0("emp",object@fitfile,".csv")
				}

				if(object@copfile==""){
					copname <- paste0("cop",object@tag,".csv")
				} else {
					copname <- paste0(object@copfile,".csv")
				}

				if(object@facfile==""){
					facname <- paste0("fac",object@tag,".csv")
				} else {
					facname <- paste0(object@facfile,".csv")
				}

				#f <- c("reportLag", "settlementLag", "frequency", "severity", "ssrCorrelation", "freqCorrelation")
				
				empiricals <- data.frame(matrix(seq(0.001,1,0.001),1000,1))
				colnames(empiricals) <- "prob"
				
				fitsummary <- data.frame(LoB=character(),
							Type=character(),
							Fit=character(),
							Distribution=character(),
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
				rfit<-1
				fitsumcop <- data.frame(LoB=character(),
							Type=character(),
							Fit=character(),
							Copula=character(),
							Method=character(), 
							Parameter=character(), 
							SD=character(),
							DoF=integer(),
							Sn=double(), 
							p=double(), 
							stringsAsFactors=FALSE)
				rcop<-1
				
				fitsumfac <- data.frame(LoB=character(),
							Type=character(),
							Fit=character(),
							Year=character(), 
							MeanList=double(),
							VolList=double(),
							stringsAsFactors=FALSE)
				rfac<-1
							
				for(l in lineList){#object@lines
					for(t in object@types){#object@lines
						fitdata <- claimData[claimData[,"LoB"]==l,]
						fitdata <- fitdata[fitdata[,"Type"]==t,]
						obji<-1
						for(co in object@claimobjs){
							if(co@line==l & co@claimType==t) break
							obji <- obji + 1
						}

						if(nrow(fitdata)>5 & co@line==l & co@claimType==t){
							if(fReportLag==TRUE){
								f <- "reportLag"
								print(paste0("Start Fitting Line:",l," Type:",t," ",f))
								reportlags <- as.numeric(as.Date(fitdata[,"reportDate"])-as.Date(fitdata[,"occurrenceDate"]))
								reportlags <- ifelse(reportlags==0, runif(length(reportlags)),reportlags)

								if(TRUE){#(objName(co@reportLag)!="Empirical"){
												
									xFit <- new("FitDist", observation=as.data.frame(reportlags), method="mle",ifreq=FALSE)
									xFit <- setFitdata(xFit)
									bestBIC <- 1e10
									irlso <- 0
									for (i in c(1:length(continuousDist))){
										so <- tryCatch({
											setTrialDistErr(xFit) <- new(continuousDist[i])
											print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",continuousDist[i]," distribution fitted"))
											1
											}#,warning=function(w) {
											#	setTrialDist(xFit) <- new(continuousDist[i])
											#	return(1)
											#}
											,error=function(e) {
											print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",continuousDist[i]," distribution failed to fit"))
											-1
											}
										)
										if(so==1) {
											irlso <- 1
											fitsummary[rfit,] <- c(l,t,f,xFit@soutput)
											fitsummary[rfit,"p0"] <- NA
											if (check == TRUE){
												jpeg(filename = paste0(object@fitfile,l,t,f,continuousDist[i],".jpg"))
												fitPlot(xFit)
												dev.off()
											}
										} else {
											fitsummary[rfit,] <- c(l,t,f,continuousDist[i],"mle",rep(NA,11))
											if (check == TRUE){
												jpeg(filename = paste0(object@fitfile,l,t,f,continuousDist[i],".jpg"))
												par(mfrow = c(1, 1))
												plotText(paste0("Data cannot fit to ",continuousDist[i]," distribution"))
												dev.off()
											}
										}
										
										rfit <- rfit+1
										if (i==1) {
											bestfit <- xFit@fitted
											if (!is.na(xFit@soutput[1,12])) {bestBIC <- as.numeric(xFit@soutput[1,12])}
										} else if ((!is.na(xFit@soutput[1,12])) & (as.numeric(xFit@soutput[1,12])<bestBIC)){
											bestfit <- xFit@fitted
											bestBIC <- as.numeric(xFit@soutput[1,12])
										}
										#print(toString(xFit@fitted))
									}
									
									if (irlso == 1) {
										co@reportLag <- bestfit
										print(paste0("Line:",l," Type:",t," ",f, " best fit: ",toString(co@reportLag)))
										xFit@fitted<-bestfit
										png(filename = paste0("fit",object@tag,l,t,f,".png"))
										fitPlot(xFit)
										dev.off()
									} else {
										co@reportLag@fitsucc <- FALSE
										print(paste0("Line:",l," Type:",t," ",f, " best fit: ","No distribution is found appropriate."))
										png(filename = paste0("fit",object@tag,l,t,f,".png"))
										plotText(paste0("Data cannot fit to any tested distribution"))
										dev.off()									
									}

								} else {
									xFit <- new("FitDist", observation=as.data.frame(reportlags), method="mle",ifreq=FALSE)
									xFit <- setFitdata(xFit)
									setTrialDist(xFit) <- new("Empirical")
									co@reportLag <- xFit@fitted

									#xFit@fitted<-bestfit
									png(filename = paste0("fit",object@tag,l,t,f,".png"))
									fitPlot(xFit)
									dev.off()
									
								}
								
								if (check == TRUE) {
									xFit <- new("FitDist", observation=as.data.frame(reportlags), method="mle",ifreq=FALSE)
									xFit <- setFitdata(xFit)
									setTrialDist(xFit) <- new("Empirical")
									jpeg(filename = paste0(object@fitfile,l,t,f,"Empirical.jpg"))
									fitPlot(xFit)
									dev.off()
									cn <- paste0(l,t,f,"emp")
									empiricals[,cn]<-Quantile(xFit@fitted,seq(0.001,1,0.001))
								}
								cat("\n")
							}
							
							if(fFrequency==TRUE){
								f <- "frequency"
								print(paste0("Start Fitting Line:",l," Type:",t," ",f))
								rawdata <- as.data.frame(as.Date(fitdata$occurrenceDate))
								colnames(rawdata)<-"occurrenceDate"
								if(TRUE){#(objName(co@frequency)!="Empirical"){
									xFit <- new("FitDist", observation=rawdata, trend=co@exposureIndex,startDate = startDate, endDate= evaluationDate, method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly", iLag=TRUE, reportLag=co@reportLag)
									xFit <- setFitdata(xFit)
									bestBIC <- 1e10
									ifso <- 0
									for (i in c(1:length(discreteDist))){
										so <- tryCatch({
											setTrialDistErr(xFit) <- new(discreteDist[i])
											print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",discreteDist[i]," distribution fitted"))
											1
											}#,warning=function(w) {
											#	setTrialDist(xFit) <- new(continuousDist[i])
											#	return(1)
											#}
											,error=function(e) {
												print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",discreteDist[i]," distribution failed to fit"))
												-1
											}
										)
										if(so==1) {
											ifso <- 1
											fitsummary[rfit,] <- c(l,t,f,xFit@soutput)
											fitsummary[rfit,"p0"] <- NA
											if (check == TRUE){
												jpeg(filename = paste0(object@fitfile,l,t,f,discreteDist[i],".jpg"))
												fitPlot(xFit)
												dev.off()
											}
										} else {
											fitsummary[rfit,] <- c(l,t,f,discreteDist[i],"mle",rep(NA,11))
											if (check == TRUE){
												jpeg(filename = paste0(object@fitfile,l,t,f,discreteDist[i],".jpg"))
												par(mfrow = c(1, 1))
												plotText(paste0("Data cannot fit to ",discreteDist[i]," distribution"))
												dev.off()
											}
										}
										
										rfit <- rfit+1
										if (i==1) {
											bestfit <- xFit@fitted
											if (!is.na(xFit@soutput[1,12])) {bestBIC <- as.numeric(xFit@soutput[1,12])}
										} else if ((!is.na(xFit@soutput[1,12])) & (as.numeric(xFit@soutput[1,12])<bestBIC)){
											bestfit <- xFit@fitted
											bestBIC <- as.numeric(xFit@soutput[1,12])
										}
										#print(toString(xFit@fitted))
									}
									if (ifso == 1) {
										co@frequency <- bestfit
										print(paste0("Line:",l," Type:",t," ",f, " best fit: ",toString(co@frequency)))
										xFit@fitted<-bestfit
										png(filename = paste0("fit",object@tag,l,t,f,".png"))
										fitPlot(xFit)
										dev.off()
									} else {
										co@frequency@fitsucc <- FALSE
										print(paste0("Line:",l," Type:",t," ",f, " best fit: ","No distribution is found appropriate."))
										png(filename = paste0("fit",object@tag,l,t,f,".png"))
										plotText(paste0("Data cannot fit to any tested distribution"))
										dev.off()									
									}
								} else {
									xFit <- new("FitDist", observation=rawdata, trend=co@exposureIndex,startDate = startDate, method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly", iLag=TRUE, reportLag=co@reportLag)
									xFit <- setFitdata(xFit)
									setTrialDist(xFit) <- new("Empirical")
									co@frequency <- xFit@fitted
									#xFit@fitted<-bestfit
									png(filename = paste0("fit",object@tag,l,t,f,".png"))
									fitPlot(xFit)
									dev.off()
								}
								
								if (check == TRUE) {
									xFit <- new("FitDist", observation=rawdata, trend=co@exposureIndex,startDate = startDate, endDate= evaluationDate, method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly", iLag=TRUE, reportLag=co@reportLag)
									xFit <- setFitdata(xFit)
									setTrialDist(xFit) <- new("Empirical")
									jpeg(filename = paste0(object@fitfile,l,t,f,"Empirical.jpg"))
									fitPlot(xFit)
									dev.off()
									cn <- paste0(l,t,f,"emp")
									empiricals[,cn]<-Quantile(xFit@fitted,seq(0.001,1,0.001))
								}
								cat("\n")
							}
							
							if(fSettlementLag==TRUE){
								fitdatacls <- fitdata[fitdata$status=="CLOSED",]
								f <- "settlementLag"
								print(paste0("Start Fitting Line:",l," Type:",t," ",f))
								settlementlags <- as.numeric(as.Date(fitdatacls[,"settlementDate"])-as.Date(fitdatacls[,"reportDate"]))
								rm(fitdatacls)
								settlementlags <- settlementlags[!is.na(settlementlags)]								
								settlementlags <- ifelse(settlementlags==0, runif(length(settlementlags)),settlementlags)
								if(TRUE){#(objName(co@settlementLag)!="Empirical"){
												
									xFit <- new("FitDist", observation=as.data.frame(settlementlags), method="mle",ifreq=FALSE)
									xFit <- setFitdata(xFit)
									bestBIC <- 1e10
									islso <- 0
									for (i in c(1:length(continuousDist))){
										so <- tryCatch({
											setTrialDistErr(xFit) <- new(continuousDist[i])
											print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",continuousDist[i]," distribution fitted"))
											1
											}#,warning=function(w) {
											#	setTrialDist(xFit) <- new(continuousDist[i])
											#	return(1)
											#}
											,error=function(e) {
											print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",continuousDist[i]," distribution failed to fit"))
											-1
											}
										)
										if(so==1) {
											islso <- 1
											fitsummary[rfit,] <- c(l,t,f,xFit@soutput)
											fitsummary[rfit,"p0"] <- NA
											if (check == TRUE){
												jpeg(filename = paste0(object@fitfile,l,t,f,continuousDist[i],".jpg"))
												fitPlot(xFit)
												dev.off()
											}
										} else {
											fitsummary[rfit,] <- c(l,t,f,continuousDist[i],"mle",rep(NA,11))
											if (check == TRUE){
												jpeg(filename = paste0(object@fitfile,l,t,f,continuousDist[i],".jpg"))
												par(mfrow = c(1, 1))
												plotText(paste0("Data cannot fit to ",continuousDist[i]," distribution"))
												dev.off()
											}
										}
										
										rfit <- rfit+1
										if (i==1) {
											bestfit <- xFit@fitted
											if (!is.na(xFit@soutput[1,12])) {bestBIC <- as.numeric(xFit@soutput[1,12])}
										} else if ((!is.na(xFit@soutput[1,12])) & (as.numeric(xFit@soutput[1,12])<bestBIC)){
											bestfit <- xFit@fitted
											bestBIC <- as.numeric(xFit@soutput[1,12])
										}
									}
									
									if (islso == 1) {
										co@settlementLag <- bestfit
										print(paste0("Line:",l," Type:",t," ",f, " best fit: ",toString(co@settlementLag)))
										xFit@fitted<-bestfit
										png(filename = paste0("fit",object@tag,l,t,f,".png"))
										fitPlot(xFit)
										dev.off()
									} else {
										co@settlementLag@fitsucc <- FALSE
										print(paste0("Line:",l," Type:",t," ",f, " best fit: ","No distribution is found appropriate."))
										png(filename = paste0("fit",object@tag,l,t,f,".png"))
										plotText(paste0("Data cannot fit to any tested distribution"))
										dev.off()									
									}
								} else {
									xFit <- new("FitDist", observation=as.data.frame(settlementlags), method="mle",ifreq=FALSE)
									xFit <- setFitdata(xFit)
									setTrialDist(xFit) <- new("Empirical")
									co@settlementLag <- xFit@fitted
									#xFit@fitted<-bestfit
									png(filename = paste0("fit",object@tag,l,t,f,".png"))
									fitPlot(xFit)
									dev.off()
								}
								
								if (check == TRUE) {
									xFit <- new("FitDist", observation=as.data.frame(settlementlags), method="mle",ifreq=FALSE)
									xFit <- setFitdata(xFit)
									setTrialDist(xFit) <- new("Empirical")
									jpeg(filename = paste0(object@fitfile,l,t,f,"Empirical.jpg"))
									fitPlot(xFit)
									dev.off()
									cn <- paste0(l,t,f,"emp")
									empiricals[,cn]<-Quantile(xFit@fitted,seq(0.001,1,0.001))
								}
								cat("\n")
							}
							
							if(fDeductible==TRUE){
								f <- "Deductible"
								print(paste0("Start Fitting Line:",l," Type:",t," ",f))
								deductibles <- as.numeric(fitdata[,"Deductible"])
								deductibles <- ifelse(is.na(deductibles),0,deductibles)
								deductibles <- ifelse(deductibles<0,0,deductibles)
								
								xFit <- new("FitDist", observation=as.data.frame(deductibles), method="mle",ifreq=FALSE)
								xFit <- setFitdata(xFit)
								setTrialDist(xFit) <- new("Empirical")
								fitsummary[rfit,] <- c(l,t,f,xFit@soutput)
								fitsummary[rfit,"p0"] <- NA
								co@deductible <- xFit@fitted
								rfit <- rfit + 1
								png(filename = paste0("fit",object@tag,l,t,f,".png"),width=240,height=240)
								CDFPlot(xFit)
								dev.off()
									
								if (check == TRUE){
									jpeg(filename = paste0(object@fitfile,l,t,f,"Empirical",".jpg"))
									par(mfrow = c(1, 1))
									CDFPlot(xFit)
									dev.off()
								}

								cn <- paste0(l,t,f,"emp")
								empiricals[,cn]<-Quantile(xFit@fitted,seq(0.001,1,0.001))
								cat("\n")
							}

							if(fLimit==TRUE){
								f <- "Limit"
								print(paste0("Start Fitting Line:",l," Type:",t," ",f))
								
								limits <- as.numeric(fitdata[,"Limit"])
								limits <- ifelse(is.na(limits),1e10,limits)
								limits <- ifelse(limits<0,1e10,limits)
								xFit <- new("FitDist", observation=as.data.frame(limits), method="mle",ifreq=FALSE)
								xFit <- setFitdata(xFit)
								setTrialDist(xFit) <- new("Empirical")
								fitsummary[rfit,] <- c(l,t,f,xFit@soutput)
								fitsummary[rfit,"p0"] <- NA
								co@limit <- xFit@fitted
								rfit <- rfit + 1
								png(filename = paste0("fit",object@tag,l,t,f,".png"),width=240,height=240)
								CDFPlot(xFit)
								dev.off()
									
								if (check == TRUE){
									jpeg(filename = paste0(object@fitfile,l,t,f,"Empirical",".jpg"))
									par(mfrow = c(1, 1))
									CDFPlot(xFit)
									dev.off()
								}

								cn <- paste0(l,t,f,"emp")
								empiricals[,cn]<-Quantile(xFit@fitted,seq(0.001,1,0.001))
								cat("\n")
							}

							if(fSeverity==TRUE){
								f <- "severity"
								print(paste0("Start Fitting Line:",l," Type:",t," ",f))
								
								nclosed <- nrow(fitdata[fitdata[,"status"]=="CLOSED",])
								nvalid <- nrow(fitdata[fitdata[,"claimLiability"]==TRUE & fitdata[,"status"]=="CLOSED",])
								aggP0 <- 1-nvalid/nclosed
								
								p0Set <- fitdata[fitdata[,"status"]=="CLOSED",]
								p0Set[,"settlementYears"] <- ceiling(as.numeric(as.Date(p0Set[,"settlementDate"]) - as.Date(p0Set[,"occurrenceDate"]))/365)
								p0Set <- p0Set[,colnames(p0Set) %in% c("settlementYears","claimLiability")]
								#p0Set <- read.csv("C:/temp/CAS/pset.csv")
								yclosed <- aggregate(claimLiability ~ settlementYears, data=p0Set, FUN="length")
								if(nrow(p0Set[p0Set$claimLiability==FALSE,])>0){
									yinvalid <- aggregate(claimLiability ~ settlementYears, data=p0Set[p0Set$claimLiability==FALSE,], FUN="length")
									vp0 <- rep(0,max(yclosed$settlementYears))
									for (i in c(1:length(vp0))) {
										denominator <- yclosed[yclosed$settlementYears == i,]$claimLiability[1]
										numerator <- yinvalid[yinvalid$settlementYears == i,]$claimLiability[1]
										vp0[i] <- numerator/denominator
										vp0[i] <- min(1,vp0[i])
									}
									vp0[is.na(vp0)] <- 0
								} else {
									vp0 <- rep(0,max(yclosed$settlementYears))									
								}
								
								for (i in c(1:length(vp0))) {
									fitsumfac[rfac,] <- c(l,t,"p0",i,vp0[i],0)
									rfac <- rfac + 1
								}
								
								co@p0 <- new("DevFac",FacID=paste0(l,t,"p0"),FacModel= FALSE,meanList =vp0,volList =rep(0,length(vp0)))
								
								if(co@sdata == "CLOSED") {
									fitdata <- fitdata[fitdata[,"claimLiability"]==TRUE & fitdata[,"status"]=="CLOSED",]
								} else {
									fitdata <- fitdata[fitdata[,"claimLiability"]==TRUE,]								
								}
								
								if (iTotalLoss == TRUE) {
									incurredlosses <- fitdata[,colnames(fitdata) %in% c("settlementDate","totalLoss")][,c("settlementDate","totalLoss")]
								} else {
									incurredlosses <- fitdata[,colnames(fitdata) %in% c("settlementDate","incurredLoss","Deductible","Limit")][,c("settlementDate","incurredLoss","Deductible","Limit")]								
								}
								if (iTotalLoss == TRUE) {
									incurredlosses[,2] <- pmax(0.01,incurredlosses[,2])
								}
								if(TRUE){#objName(co@reportLag)!="Empirical"){
									
									xFit <- new("FitDist", observation=incurredlosses, trend=co@severityIndex, startDate=startDate, method="mle",ifreq=FALSE)
									
									if (iTotalLoss == FALSE) {
										xFit@iDL = TRUE
									}
									
									xFit <- setFitdata(xFit)
									losses <- xFit@fitdata
									bestBIC <- 1e10
									isso <- 0
									for (i in c(1:length(continuousDist))){
										so <- tryCatch({
											setTrialDistErr(xFit) <- new(continuousDist[i])
											print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",continuousDist[i]," distribution fitted"))
											1
											}#,warning=function(w) {
											#	setTrialDist(xFit) <- new(continuousDist[i])
											#	return(1)
											#}
											,error=function(e) {
											print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",continuousDist[i]," distribution failed to fit"))											
											-1
											}
										)
										if(so==1) {
											isso <- 1
											fitsummary[rfit,] <- c(l,t,f,xFit@soutput)
											fitsummary[rfit,"p0"] <- aggP0
											if (check == TRUE){
												jpeg(filename = paste0(object@fitfile,l,t,f,continuousDist[i],".jpg"))
												fitPlot(xFit)
												dev.off()
											}
										} else {
											fitsummary[rfit,] <- c(l,t,f,continuousDist[i],"mle",rep(NA,11))
											if (check == TRUE){
												jpeg(filename = paste0(object@fitfile,l,t,f,continuousDist[i],".jpg"))
												par(mfrow = c(1, 1))
												if (iTotalLoss == FALSE) {
													plotText(paste0("Data cannot fit to truncated ",continuousDist[i]," distribution"))												
												} else {
													plotText(paste0("Data cannot fit to ",continuousDist[i]," distribution"))
												}
												dev.off()
											}
										}
										
										rfit <- rfit+1
										if (i==1) {
											bestfit <- xFit@fitted
											if (!is.na(xFit@soutput[1,12])) {bestBIC <- as.numeric(xFit@soutput[1,12])}
										} else if ((!is.na(xFit@soutput[1,12])) & (as.numeric(xFit@soutput[1,12])<bestBIC)){
											bestfit <- xFit@fitted
											bestBIC <- as.numeric(xFit@soutput[1,12])
										}
									}
									if (isso == 1) {
										co@severity <- bestfit
										print(paste0("Line:",l," Type:",t," ",f, " best fit: ",toString(co@severity)))
										xFit@fitted<-bestfit
										png(filename = paste0("fit",object@tag,l,t,f,".png"))
										fitPlot(xFit)
										dev.off()
									} else {
										co@severity@fitsucc <- FALSE
										print(paste0("Line:",l," Type:",t," ",f, " best fit: ","No distribution is found appropriate."))
										png(filename = paste0("fit",object@tag,l,t,f,".png"))
										plotText(paste0("Data cannot fit to any tested distribution"))
										dev.off()									
									}
								} else {
									xFit <- new("FitDist", observation=incurredlosses, trend=co@severityIndex, startDate=startDate, method="mle",ifreq=FALSE)
									xFit <- setFitdata(xFit)
									setTrialDist(xFit) <- new("Empirical")
									co@severity <- xFit@fitted
									png(filename = paste0("fit",object@tag,l,t,f,".png"))
									fitPlot(xFit)
									dev.off()
								}
								
								if (check == TRUE) {
									xFit <- new("FitDist", observation=incurredlosses, trend=co@severityIndex, startDate=startDate, method="mle",ifreq=FALSE)
									xFit <- setFitdata(xFit)
									setTrialDist(xFit) <- new("Empirical")
									jpeg(filename = paste0(object@fitfile,l,t,f,"Empirical.jpg"))
									fitPlot(xFit)
									dev.off()
									cn <- paste0(l,t,f,"emp")
									empiricals[,cn]<-Quantile(xFit@fitted,seq(0.001,1,0.001))
								}
								
								fitdata <- fitdata[fitdata[,"claimLiability"]==TRUE & fitdata[,"status"]=="CLOSED",]
								reportlags <- as.numeric(as.Date(fitdata[,"reportDate"])-as.Date(fitdata[,"occurrenceDate"]))
								reportlags <- ifelse(reportlags==0, runif(length(reportlags)),reportlags)
								settlementlags <- as.numeric(as.Date(fitdata[,"settlementDate"])-as.Date(fitdata[,"reportDate"]))
								settlementlags <- ifelse(settlementlags==0, runif(length(settlementlags)),settlementlags)
								if (iTotalLoss == TRUE) {
									incurredlosses <- fitdata[,colnames(fitdata) %in% c("settlementDate","totalLoss")][,c("settlementDate","totalLoss")]
								} else {
									incurredlosses <- fitdata[,colnames(fitdata) %in% c("settlementDate","incurredLoss","Deductible","Limit")][,c("settlementDate","incurredLoss","Deductible","Limit")]								
								}
								if (iTotalLoss == TRUE) {
									incurredlosses[,2] <- pmax(0.01,incurredlosses[,2])
								}
								xFit <- new("FitDist", observation=incurredlosses, trend=co@severityIndex, startDate=startDate, method="mle",ifreq=FALSE)
								if (iTotalLoss == FALSE) {
									xFit@iDL = TRUE
								}
									
								xFit <- setFitdata(xFit)
								losses <- xFit@fitdata

								cat("\n")
								
							}

							if(co@iCopula==TRUE & fReportLag==TRUE & fSettlementLag==TRUE & fSeverity==TRUE & fSSRCorrelation==TRUE){
								f <- "ssrCorrelation"
								print(paste0("Start Fitting Line:",l," Type:",t," ","Correlation among severity, report lag and settlement lag"))
								x <- cbind(losses,settlementlags,reportlags)
								if (nrow(x) > 2000) {
									x <- x[sample(nrow(x), 2000), ]
								}
								
								if(nrow(x)>10) {
									dist1 <- co@severity
									dist2 <- co@settlementLag
									dist3 <- co@reportLag
									bestP <- 0
									issrso <- 0
									for (i in c(1:length(copulaList))){
										if(copulaList[i]=="normal"){
											nom.cop <- new("CopulaObj", type="normal",param=c(0.5,0.5,0.5),marginal=list(dist1=dist1,dist2=dist2,dist3=dist3),dimension=3,observation=x,fittest=copulaTest)
										} else if(copulaList[i]=="t"){
											nom.cop <- new("CopulaObj", type="t",param=c(0.5,0.5,0.5),df=5, marginal=list(dist1=dist1,dist2=dist2,dist3=dist3),dimension=3,observation=x,fittest=copulaTest)
										} else {
											nom.cop <- new("CopulaObj", type=copulaList[i],param=c(3),marginal=list(dist1=dist1,dist2=dist2,dist3=dist3),dimension=3,observation=x,fittest=copulaTest)
										}

										so <- tryCatch({
											if(nom.cop@type=="t") {nom.cop@fittest=TRUE} else {nom.cop@fittest=TRUE}#FALSE
											nom.cop <- copulaFitErr(nom.cop)
											print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",copulaList[i]," copula fitted"))
											1
											}#,warning=function(w) {
											#	setTrialDist(xFit) <- new(continuousDist[i])
											#	return(1)
											#}
											,error=function(e) {
											print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",copulaList[i]," copula failed to fit"))
											-1
											}
										)
										if(so==1) {
											issrso <- 1
											fitsumcop[rcop,] <- c(l,t,f,nom.cop@coutput)
											if (check == TRUE){
												jpeg(filename = paste0(object@fitfile,l,t,f,copulaList[i],".jpg"),width=480,height=240)
												copulaFitPlot(nom.cop)
												dev.off()
											}
										} else {
											fitsumcop[rcop,] <- c(l,t,f,copulaList[i],"mpl",rep(NA,5))
											nom.cop@coutput <- data.frame(matrix(c(copulaList[i],"mpl",rep(NA,5)),1,7))
											if (check == TRUE){
												jpeg(filename = paste0(object@fitfile,l,t,f,copulaList[i],".jpg"),width=480,height=480)
												par(mfrow = c(1, 1))
												plotText(paste0("Data cannot fit to ",copulaList[i]," copula"))
												dev.off()
											}
										}
										
										rcop <- rcop+1
										if (i==1) {
											bestfit <- nom.cop
											if (!is.na(nom.cop@coutput[1,7])) {bestP <- as.numeric(nom.cop@coutput[1,7])}
										} else if ((!is.na(nom.cop@coutput[1,7])) & (as.numeric(nom.cop@coutput[1,7])>bestP)){
											bestfit <- nom.cop
											bestP <- as.numeric(nom.cop@coutput[1,7])
										}
									}
									if (issrso == 1) {
										bestfit@fitsucc = TRUE
										co@ssrCopula <- bestfit
										print(paste0("Line:",l," Type:",t," ",f, " best fit: ",toString(co@ssrCopula)))
										png(filename = paste0("fit",object@tag,l,t,f,".png"),width=480,height=240)
										copulaFitPlot(bestfit)
										dev.off()
									} else {
										nom.cop <- new("CopulaObj", type="normal",param=c(0,0,0),marginal=list(dist1=dist1,dist2=dist2,dist3=dist3),dimension=3,fittest=copulaTest)
										nom.cop@coutput = data.frame(Copula=character(),
															Method=character(), 
															Parameter=character(), 
															SD=character(),
															DoF=integer(),
															Sn=double(), 
															p=double(), 
															stringsAsFactors=FALSE)
										nom.cop@coutput[1,] = c("normal", "mpl", NA, NA, 3, NA, NA)
										co@ssrCopula <- nom.cop
										print(paste0("Line:",l," Type:",t," ",f, " best fit: ","No copula is found appropriate."))
										png(filename = paste0("fit",object@tag,l,t,f,".png"),width=480,height=240)
										plotText(paste0("Data cannot fit to any tested copula"))
										dev.off()																	
									}
									cat("\n")
								} else {
									dist1 <- co@severity
									dist2 <- co@settlementLag
									dist3 <- co@reportLag
									nom.cop <- new("CopulaObj", type="normal",param=c(0,0,0),marginal=list(dist1=dist1,dist2=dist2,dist3=dist3),dimension=3,fittest=copulaTest)
									nom.cop@coutput = data.frame(Copula=character(),
														Method=character(), 
														Parameter=character(), 
														SD=character(),
														DoF=integer(),
														Sn=double(), 
														p=double(), 
														stringsAsFactors=FALSE)
									nom.cop@coutput[1,] = c("normal", "mpl", NA, NA, 3, NA, NA)
									co@ssrCopula <- nom.cop
									print(paste0("Severity, settlement lag and report lag copula fitting is turned off due to insufficient data for line " , l, " type " , t))
									png(filename = paste0("fit",object@tag,l,t,f,".png"),width=480,height=240)
									plotText(paste0("Data are insufficient fit to any tested copula"))
									dev.off()																	
									cat("\n")
								}
							}
							if(co@line==l & co@claimType==t) {object@claimobjs[[obji]]<-co}
						} else {
							print (paste0("Nothing to fit for line " , l, " type " , t))
							cat("\n")
						}
					}
				}
				
				if(fFreqCorrelation==TRUE & object@iCopula==TRUE){

					tmp <- cbind(as.character(claimData$LoB),as.character(claimData$Type),claimData$occurrenceDate,as.character(substr(as.character(claimData$occurrenceDate),1,7)))
					colnames(tmp) <- c("line","type","od","ym")
					tmp <- aggregate(od ~ ym+line+type, data=tmp, FUN="length")
					tmp <- cbind(tmp,as.numeric(substr(tmp$ym,1,4)),as.numeric(substr(tmp$ym,6,7)))
					colnames(tmp) <- c("ym","line","type","od","year","mth")
					for(l in object@lines){
						for(t in object@types){
							for(co in object@claimobjs){
								if(co@line==l & co@claimType==t) break
							}
							if(co@line==l & co@claimType==t){
								startYear <- as.numeric(substr(as.character(co@exposureIndex@startDate),1,4))
								startMonth <- as.numeric(substr(as.character(co@exposureIndex@startDate),6,7))
								endYear <- as.numeric(substr(as.character(evaluationDate),1,4))
								endMonth <- as.numeric(substr(as.character(evaluationDate),6,7))
								tmp$od <- ifelse((tmp$line==l & tmp$type==t),tmp$od/Probability(co@reportLag,(endYear-tmp$year)*365+(endMonth-tmp$mth)*30+15),tmp$od)
								di <- (tmp$year - startYear)*12+(tmp$mth - startMonth)+1
								di <- ifelse(di>360,360,ifelse(di<1,1,di))
								tmp$od <- ifelse((tmp$line==l & tmp$type==t),tmp$od/co@exposureIndex@monthlyIndex[di],tmp$od)
								tmp$od <- round(tmp$od)		
							}
						}
					}

					tmp <- aggregate(od ~ ym+line, data=tmp, FUN="sum")
					
					uym<-as.data.frame(as.character(unique(tmp$ym)))
					colnames(uym)<-"uym"
					for(l in object@lines){
							uym[,l] <- 0
					}
					
					for(j in c(1:nrow(uym))){
						for(l in object@lines){
							uym[j,l] <- if(nrow(tmp[tmp$ym==uym$uym[j] & tmp$line==l,])==0) {0} else {tmp[tmp$ym==uym$uym[j] & tmp$line==l,]$od}
						}
					}
					
					uym <- na.omit(uym[uym[2:ncol(uym)]>0,])
					
					f <- "freqCorrelation"
					l <- "Total"
					t <- "Total"
					print(paste0("Start Fitting frequency correlation among business lines"))
					x <- data.matrix(uym[,-1])
					if (nrow(x)>=10) {
						bestP <- 0
						ifcso <- 0
						for (i in c(1:length(copulaList))){
							if(copulaList[i]=="normal"){
								nom.cop <- new("CopulaObj", type="normal",param=rep(0.5,(length(object@lines)-1)*length(object@lines)/2),dimension=length(object@lines),observation=x,fittest=copulaTest)
							} else if(copulaList[i]=="t"){
								nom.cop <- new("CopulaObj", type="t",param=rep(0.5,(length(object@lines)-1)*length(object@lines)/2),df=5, dimension=length(object@lines),observation=x,fittest=copulaTest)
							} else {
								nom.cop <- new("CopulaObj", type=copulaList[i],param=c(3),dimension=length(object@lines),observation=x,fittest=copulaTest)
							}

							so <- tryCatch({
								nom.cop <- copulaFitErr(nom.cop)
								print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",copulaList[i]," copula fitted"))
								1
								}#,warning=function(w) {
								#	setTrialDist(xFit) <- new(continuousDist[i])
								#	return(1)
								#}
								,error=function(e) {
								print(paste0("Line-", l, " Type-", t, " Fitting-", f,": ",copulaList[i]," copula failed to fit"))
								-1
								}
							)
							if(so==1) {
								ifcso <- 1
								fitsumcop[rcop,] <- c(l,t,f,nom.cop@coutput)
								if (check == TRUE){
									jpeg(filename = paste0(object@fitfile,l,t,f,copulaList[i],".jpg"),width=480,height=240)
									copulaFitPlot(nom.cop)
									dev.off()
								}
							} else {
								fitsumcop[rcop,] <- c(l,t,f,copulaList[i],"mpl",rep(NA,5))
								nom.cop@coutput <- data.frame(matrix(c(copulaList[i],"mpl",rep(NA,5)),1,7))
								if (check == TRUE){
									jpeg(filename = paste0(object@fitfile,l,t,f,copulaList[i],".jpg"),width=480,height=480)
									par(mfrow = c(1, 1))
									plotText(paste0("Data cannot fit to ",copulaList[i]," copula"))
									dev.off()
								}
							}
							
							rcop <- rcop+1
							if (i==1) {
								bestfit <- nom.cop
								if (!is.na(nom.cop@coutput[1,7])) {bestP <- as.numeric(nom.cop@coutput[1,7])}
							} else if ((!is.na(nom.cop@coutput[1,7])) & (as.numeric(nom.cop@coutput[1,7])>bestP)){
								bestfit <- nom.cop
								bestP <- as.numeric(nom.cop@coutput[1,7])
							}
						}

						if (ifcso == 1) {
							object@freqCopula <- bestfit
							print(paste0("Line:",l," Type:",t," ",f, " best fit: ",toString(object@freqCopula)))
							png(filename = paste0("fit",object@tag,f,".png"),width=480,height=240)
							copulaFitPlot(bestfit)
							dev.off()
						} else {
							nom.cop <- new("CopulaObj", type="normal",param=rep(0,(length(object@lines)-1)*length(object@lines)/2),dimension=length(object@lines),fittest=copulaTest)
							nom.cop@coutput <- data.frame(matrix(c("normal","mpl",rep(NA,5)),1,7))
							object@freqCopula <- nom.cop
							print(paste0("Line:",l," Type:",t," ",f, " best fit: ","No copula is found appropriate."))
							png(filename = paste0("fit",object@tag,l,t,f,".png"),width=480,height=240)
							plotText(paste0("Data cannot fit to any tested copula"))
							dev.off()																	
						}
					} else {
						nom.cop <- new("CopulaObj", type="normal",param=rep(0,(length(object@lines)-1)*length(object@lines)/2),dimension=length(object@lines),fittest=copulaTest)
						nom.cop@coutput <- data.frame(matrix(c("normal","mpl",rep(NA,5)),1,7))
						object@freqCopula <- nom.cop
						print(paste0("Frequency copula fitting is turned off due to insufficient data"))
						png(filename = paste0("fit",object@tag,l,t,f,".png"),width=480,height=240)
						plotText(paste0("Data is insufficient to fit to any tested copula"))
						dev.off()																	
						fFreqCorrelation = FALSE
					}


				}
				
				if (check == TRUE) {
					jpeg(filename = paste0(object@fitfile,"na.jpg"),width=480,height=480)
					par(mfrow = c(1, 1))
					plotText(paste0("Not Fitted"))
					dev.off()
					jpeg(filename = paste0(object@fitfile,"halfna.jpg"),width=480,height=240)
					par(mfrow = c(1, 1))
					plotText(paste0("Not Fitted"))
					dev.off()
				}

				if(object@iReport==TRUE) {setwd("..")}
				
				if(nrow(fitsummary)>=1) {write.table(fitsummary,fitname,row.names=FALSE, sep=",")}
				if(ncol(fitsumfac)>=1) {write.table(fitsumfac,facname,row.names=FALSE, sep=",")}
				if(nrow(fitsumcop)>=1) {write.table(fitsumcop,copname,row.names=FALSE, sep=",")}
				if(ncol(empiricals)>=1) {write.table(empiricals,empname,row.names=FALSE, sep=",")}

				
				if(object@iReport==TRUE){
				
					require(R2HTML)
					pckgdir <- find.package("cascsim")
					pckgdir <- paste0(pckgdir,"/doc/")
					dir.create("fit")
					setwd(paste0(getwd(),"/fit"))
					copied <- file.copy(file.path(pckgdir, "report.css"), file.path(getwd(),"report.css"))
					if(object@fitRpt==""){
						fitrpt <- paste0("sim",object@tag)
					} else {
						fitrpt <- paste0(object@fitRpt)
					}
					if (file.exists(paste0(fitrpt,".html"))) {file.remove(paste0(fitrpt,".html"))}
					
					ReportBegin <- function(file, title) {
						cat(paste("<html><head><title>", title, "</title></head>", "<body bgcolor=#D0D0D0>", sep = ""), file = file, append = FALSE)
					}

					ReportEnd <- function(file) {
						cat("<hr size=1></body></html>", file = file, append = TRUE)
					}
					
					RptFetch<-function(line,type,dist){
						if(dist=="ssrCorrelation" | dist=="freqCorrelation"){
							result <- fitsumcop[fitsumcop$LoB==line & fitsumcop$Type==type & fitsumcop$Fit==dist,]					
						}else{
							result <- fitsummary[fitsummary$LoB==line & fitsummary$Type==type & fitsummary$Fit==dist,]
						}
						rownames(result)<-NULL
						return(result)
					}
			
					vecFlatten<-function(vec){
						result<-""
						if(length(vec)>1){
							for(i in c(1:(length(vec)-1))){
								result<-paste0(result,vec[i],", ")
							}
							result <- paste0(result,"and ",vec[length(vec)])
						} else {
							result <- vec[1]
						}
						return(result)
					}
			
					getCopulaInfo<-function(c){
						out <- paste0(c@info," ",c@type," copula, dimension=",c@dimension)
						if(c@type=="t"){out<-paste0(out,", df=",c@df)}
						if((c@type=="t" | c@type=="normal") & length(c@param)==(c@dimension*(c@dimension-1)/2)){
							outm<-matrix(1,c@dimension,c@dimension)
							irow<-2
							icol<-1
							for(cor in c@param){
								outm[irow,icol]<-cor
								outm[icol,irow]<-cor
								if(irow<c@dimension){
									irow<-irow+1
								}else{
									icol<-icol+1
									irow<-icol+1
								}
							}
							outm<-as.data.frame(outm)
							colnames(outm)<-object@lines
							return(list(out=out,outm=outm))
						}else{
							out<-paste0(out,", parameter=",vecFlatten(c@param))
							return(list(out=out))
						}
					}
			
					getCopulaSR<-function(c){
						out <- paste0(c@info," ",c@type," copula, dimension=",c@dimension)
						if(c@type=="t"){out<-paste0(out,", df=",c@df)}
						if((c@type=="t" | c@type=="normal") & length(c@param)==(c@dimension*(c@dimension-1)/2)){
							outm<-matrix(1,c@dimension,c@dimension)
							irow<-2
							icol<-1
							for(cor in c@param){
								outm[irow,icol]<-cor
								outm[icol,irow]<-cor
								if(irow<c@dimension){
									irow<-irow+1
								}else{
									icol<-icol+1
									irow<-icol+1
								}
							}
							outm<-as.data.frame(outm)
							colnames(outm)<-c("Severity","ReportLag","SettlementLag")
							return(list(out=out,outm=outm))
						}else{
							out<-paste0(out,", parameter=",vecFlatten(c@param))
							return(list(out=out))
						}
					}

					findClaimObj <- function(line,type,objpool){
						for(co in objpool){
							if(co@line==line & co@claimType==type) {
								result <- co
								break
							} else {
								result <- NULL
							}
						}
						return(result)
					}

					HTMLcontent <- function(file, append = TRUE, directory = getwd()) {
						file = file.path(directory, file)
						cat("\n", file = file, append = append)
						HTML.title("Claim Data Fitting Report", file=file, HR=1)
						HTML(paste0("Claim Data Start Date: ",startDate,"; Claim Data Ending Date: ",evaluationDate), file=file)
						HTML(paste0("Simulation Task ID: ",object@tag), file=file)
						HTML(paste0("Report generated at ",date()), file=file)
						HTMLhr(file = file)
						HTML.title("Links",file = file, HR=4)
						HTML(paste0("<a href=\"#tp\">Total Portfolio</a>"),file=file)
						for(l in object@lines){
							#HTML(paste0("<a href=\"#",l,"\">",l,"-Total","</a>"),file=file)
							for(t in object@types){
								if(!is.null(findClaimObj(l,t,object@claimobjs))){
									s<-paste0("<a href=\"#",l,t,"\">",l,"-",t,"</a>")
									HTML(s,file=file)
								}
							}
						}
						HTMLhr(file = file)
						HTML(paste0("<a name=\"tp\"></a>"), file = file)
						HTML.title("Total Portfolio",file = file, HR=2)
						HTML(paste0("The portfolio includes ",length(object@lines)," business line(s): ",vecFlatten(object@lines),"."),file=file)#,". Each business line may contain ",length(object@types)," type(s): ",vecFlatten(object@types)
						if(fFreqCorrelation==TRUE & object@iCopula==TRUE){
							HTML.title("Frequency Copula Among Business Lines",file = file, HR=3)
							if (ifcso == 1) {
								HTML(paste0("Best Copula: ",getCopulaInfo(object@freqCopula)$out), file = file)
								if (!is.null(getCopulaInfo(object@freqCopula)$outm)){
									HTML(getCopulaInfo(object@freqCopula)$outm, file = file, innerBorder=0.5, row.names=FALSE)
								}
								HTMLInsertGraph(paste0("fit",object@tag,"freqCorrelation.png"),Caption = "Copula Fitting Plot",file = file)
							} else {
								HTML(paste0("No tested copula is found appropriate."),file=file)									
							}
							HTML("Tested Copulas", file = file)
							HTML(RptFetch("Total","Total","freqCorrelation")[,-c(1:3)], file = file, innerBorder=0.5, row.names=FALSE)
						}

						HTMLhr(file = file)
						
						for(l in object@lines){
							HTML(paste0("<a name=\"",l,"\"></a>"), file = file)
							for(t in object@types){
								if (!is.null(findClaimObj(l,t,object@claimobjs))){
									if(fFrequency==TRUE){
										HTML(paste0("<a name=\"",l,t,"\"></a>"), file = file)
										HTML.title(paste0(l,"-",t),file = file, HR=2)
										HTML.title(paste0("Frequency"),file = file, HR=3)
										#if (ifso == 1) {
										if (findClaimObj(l,t,object@claimobjs)@frequency@fitsucc == TRUE) {
											HTML(paste0("Best Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@frequency)),file=file)
											HTMLInsertGraph(paste0("fit",object@tag,l,t,"frequency.png"),Caption = "",file = file)
											HTML("Tested Distributions", file = file)
											HTML(RptFetch(l,t,"frequency")[,-c(1:3)], file = file, innerBorder=0.5, row.names=FALSE)
										} else {
											HTML(paste0("No tested distribution is found appropriate."),file=file)									
										}
										HTMLhr(file = file)
									}

									if(fSeverity==TRUE){
										HTML.title(paste0("Severity"),file = file, HR=3)
										#if (isso == 1) {
										if (findClaimObj(l,t,object@claimobjs)@severity@fitsucc == TRUE) {
											HTML(paste0("Best Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@severity)),file=file)
											HTMLInsertGraph(paste0("fit",object@tag,l,t,"severity.png"),Caption = "",file = file)
											HTML("Tested Distributions", file = file)
											HTML(RptFetch(l,t,"severity")[,-c(1:3)], file = file, innerBorder=0.5, row.names=FALSE)
										} else {
											HTML(paste0("No tested distribution is found appropriate."),file=file)									
										}
										HTMLhr(file = file)

										if(!is.null(findClaimObj(l,t,object@claimobjs))){
											HTML(paste0("Probability of Zero Payment by Development Year(p0): ",findClaimObj(l,t,object@claimobjs)@p0@FacID),file=file)
											HTML(toString(findClaimObj(l,t,object@claimobjs)@p0), file = file, innerBorder=0.5, row.names=TRUE)
											HTMLhr(file = file)								
										}
									}
									
									if(fReportLag==TRUE){
										HTML.title(paste0("Report Lag"),file = file, HR=3)
										#if (irlso == 1) {
										if (findClaimObj(l,t,object@claimobjs)@reportLag@fitsucc == TRUE) {
											HTML(paste0("Best Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@reportLag)),file=file)
											HTMLInsertGraph(paste0("fit",object@tag,l,t,"reportLag.png"),Caption = "",file = file)
											HTML("Tested Distributions", file = file)
											HTML(RptFetch(l,t,"reportLag")[,-c(1:3)], file = file, innerBorder=0.5, row.names=FALSE)
										} else {
											HTML(paste0("No tested distribution is found appropriate."),file=file)									
										}
										HTMLhr(file = file)
									}
									
									if(fSettlementLag==TRUE){
										HTML.title(paste0("Settlement Lag"),file = file, HR=3)
										#if (islso == 1) {
										if (findClaimObj(l,t,object@claimobjs)@settlementLag@fitsucc == TRUE) {
											HTML(paste0("Best Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@settlementLag)),file=file)
											HTMLInsertGraph(paste0("fit",object@tag,l,t,"settlementLag.png"),Caption = "",file = file)
											HTML("Tested Distributions", file = file)
											HTML(RptFetch(l,t,"settlementLag")[,-c(1:3)], file = file, innerBorder=0.5, row.names=FALSE)
										} else {
											HTML(paste0("No tested distribution is found appropriate."),file=file)									
										}
										HTMLhr(file = file)
									}

									if(fDeductible==TRUE){
										HTML.title(paste0("Deductible Empirical Distribution"),file = file, HR=3)
										HTMLInsertGraph(paste0("fit",object@tag,l,t,"Deductible.png"),Caption = "",file = file,WidthHTML=250)
										HTMLhr(file = file)
									}

									if(fLimit==TRUE){
										HTML.title(paste0("Limit Empirical Distribution"),file = file, HR=3)
										HTMLInsertGraph(paste0("fit",object@tag,l,t,"Limit.png"),Caption = "",file = file,WidthHTML=250)
										HTMLhr(file = file)
									}

									if(findClaimObj(l,t,object@claimobjs)@iCopula==TRUE & fReportLag==TRUE & fSettlementLag==TRUE & fSeverity==TRUE & fSSRCorrelation==TRUE){
										HTML.title(paste0("Severity, Report Lag and Settlement Lag Copula"),file = file, HR=3)
										#if (issrso == 1) {
										if (findClaimObj(l,t,object@claimobjs)@ssrCopula@fitsucc == TRUE) {
											HTML(paste0("Best Copula: ",getCopulaSR(findClaimObj(l,t,object@claimobjs)@ssrCopula)$out), file = file)
											HTML(getCopulaSR(findClaimObj(l,t,object@claimobjs)@ssrCopula)$outm, file = file, innerBorder=0.5, row.names=FALSE)
											HTMLInsertGraph(paste0("fit",object@tag,l,t,"ssrCorrelation.png"),Caption = "",file = file)
											HTML("Tested Copulas", file = file)
											HTML(RptFetch(l,t,"ssrCorrelation")[,-c(1:3)], file = file, innerBorder=0.5, row.names=FALSE)
										} else {
											HTML(paste0("No tested copula is found appropriate."),file=file)									
										}
										HTMLhr(file = file)
									}
								}
							}
						}
						cat(paste("Report generated: ",file," at ",date(), "\n", sep = ""))
					}
			
					Report <- function(file) {
						ReportBegin(file,"Claim Data Fitting Report")
						if(copied == TRUE | file.exists("report.css")) {
							HTMLCSS(file = file, CSSfile = "report.css")
						}
						HTMLcontent(file)
						ReportEnd(file)
					}
					
					options("R2HTML.format.big.mark"=",")
					options("R2HTML.format.decimal.mark"=".")
					Report(paste0(fitrpt,".html"))
					setwd("..")
				}
				
			} else {
				stop("Reset iFit of the simulation object to TRUE to fit frequency, severity, report lag, settlement lag and frequency correlation to claim data.")
			}
			print("Claim data fitting done")
			return(object)

		} else {
			print("Nothing to fit or no claim data.")
			print("Claim data fitting done")
		}
		
	}, error = function(err){
		print("Something is wrong. Please check the error messages")
		if (exists("l") & exists("t") & exists("f")){
			print(paste0(">>>Critical Error for claim distribution fitting: ",err," Line-", l, " Type-", t, " Fitting-", f))
		} else {
			print(paste0(">>>Critical Error for claim distribution fitting: ",err))		
		}
		print("Claim data fitting done")
		gc()
		return(-1)
	})
})


#' Claim simulation at line/type/status level
#' @param object Simulation object
#' @param claimData claim data including existing claims for RBNER and claim reopenness analysis
#' @param startDate Date after which claims are analyzed
#' @param evaluationDate Date of evaluation for existing claims and IBNR
#' @param futureDate Date of evaluation for UPR (future claims)
#' @examples
#' library(cascsim)
#' data(claimdata)
#' lines<-c("Auto","Property","Liab")
#' types<-c("N","H")
#' #IBNR Only
#' library(cascsim)
#' data(claimdata)
#' lines<-c("Auto","Property","Liab")
#' types<-c("N","H")
#' objan <- new("ClaimType", line="Auto",claimType="N")
#' objah <- new("ClaimType", line="Auto",claimType="H")
#' objpn <- new("ClaimType", line="Property",claimType="N")
#' objph <- new("ClaimType", line="Property",claimType="H")
#' objln <- new("ClaimType", line="Liab",claimType="N")
#' objlh <- new("ClaimType", line="Liab",claimType="H")
#' objlist <- list(objan,objah,objpn,objph,objln,objlh)
#' simobj <- new("Simulation",lines=lines,types=types,iRBNER=FALSE,iROPEN=FALSE,iIBNR=TRUE,iUPR=FALSE,claimobjs=objlist,simNo=2)
#' simdata <- claimSimulation(simobj)
#' simSummary <- simSummary(simobj,simdata)
#' simobj@iReport <- TRUE
#' simReport(simobj,simSummary)
#'
#' #IBNR and UPR Only
#' library(cascsim)
#' data(claimdata)
#' lines<-c("Auto","Property","Liab")
#' types<-c("N","H")
#' objan <- new("ClaimType", line="Auto",claimType="N")
#' objah <- new("ClaimType", line="Auto",claimType="H")
#' objpn <- new("ClaimType", line="Property",claimType="N")
#' objph <- new("ClaimType", line="Property",claimType="H")
#' objln <- new("ClaimType", line="Liab",claimType="N")
#' objlh <- new("ClaimType", line="Liab",claimType="H")
#' objlist <- list(objan,objah,objpn,objph,objln,objlh)
#' simobj <- new("Simulation",lines=lines,types=types,iRBNER=FALSE,iROPEN=FALSE,iIBNR=TRUE,iUPR=TRUE,claimobjs=objlist,simNo=2)
#' simdata <- claimSimulation(simobj)
#' simSummary <- simSummary(simobj,simdata)
#' simobj@iReport <- TRUE
#' simReport(simobj,simSummary)
#'
#' #All four claim classes: RBNER, ROPEN, IBNR and UPR
#' library(cascsim)
#' data(claimdata)
#' lines<-c("Auto","Property","Liab")
#' types<-c("N","H")
#' objan <- new("ClaimType", line="Auto",claimType="N")
#' objah <- new("ClaimType", line="Auto",claimType="H")
#' objpn <- new("ClaimType", line="Property",claimType="N")
#' objph <- new("ClaimType", line="Property",claimType="H")
#' objln <- new("ClaimType", line="Liab",claimType="N")
#' objlh <- new("ClaimType", line="Liab",claimType="H")
#' objlist <- list(objan,objah,objpn,objph,objln,objlh)
#' simobj <- new("Simulation",lines=lines,types=types,iRBNER=TRUE,iROPEN=TRUE,iIBNR=TRUE,iUPR=TRUE,claimobjs=objlist,simNo=2)
#' simdata <- claimSimulation(simobj,claimData=claimdata)
#' simSummary <- simSummary(simobj,simdata)
#' simTriangle <- simTriangle(simobj,claimdata,simdata)
#' simobj@iReport <- TRUE
#' simReport(simobj,simSummary,simTriangle)
#' #multicore computing 
#' simobj@ncores<-4
#' simobj@simNo<-9
#' simdata <- claimSimulation(simobj,claimdata)
#' simSummary <- simSummary(simobj,simdata)
#' simTriangle <- simTriangle(simobj,claimdata,simdata)
#' simobj@iReport <- TRUE
#' simReport(simobj,simSummary)
#' @rdname claimSimulation
#' @export
setGeneric("claimSimulation", function(object, ...) standardGeneric("claimSimulation"))
setMethod("claimSimulation", signature("Simulation"), function(object, claimData=data.frame(), startDate=as.Date("2012-01-01"),evaluationDate=as.Date("2016-12-31"),futureDate=as.Date("2017-12-31"),append=TRUE)
{
	tryCatch({
		startDate <- toDate(startDate)
		evaluationDate <- toDate(evaluationDate)
		futureDate <- toDate(futureDate)
		#rn<-round(runif(1,1,10000))
		if(object@simfile==""){
			filename <- paste0("sim",object@tag,".csv")
		} else {
			filename <- paste0(object@simfile,".csv")
		}
				
		if(object@plog==""){
			mcfile <- paste0("mcsim",object@tag,".txt")
		} else {
			mcfile <- paste0(object@plog,".txt")
		}
		
		
		simdata = data.frame(Sim=character(),
							ClaimID=character(),
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
							reopenDate=character(),
							resettleDate=character(),
							reopenLoss=double(),
							expectedLoss=double(),
							LAE=double(),
							ultimateLAE=double(),
							expectedLAE=double(),
							stringsAsFactors=FALSE)

		iclass <- object@iRBNER | object@iROPEN | object@iIBNR | object@iUPR
		if(length(object@lines)>0 & length(object@types)>0 & length(object@claimobjs)>0 & iclass) {
			
			if(nchar(object@workingFolder)>0 & dir.exists(object@workingFolder)){
				setwd(object@workingFolder)
			} else if(nchar(object@workingFolder)>0 ){
				dir.create(object@workingFolder)
				setwd(object@workingFolder)
			}
			
			if (file.exists(mcfile)) {file.remove(mcfile)}

			if (append==FALSE & file.exists(filename)) {file.remove(filename)}
			
			if(evaluationDate < startDate | futureDate < startDate){
				stop("Evaluation date cannot be earlier than the start date.")
			} else {
				nmonths <- (as.numeric(substr(as.character(futureDate),1,4))-as.numeric(substr(as.character(startDate),1,4)))*12+as.numeric(substr(as.character(futureDate),6,7))-as.numeric(substr(as.character(startDate),6,7))+1
			}
			
			monthlyfreq <- function(mthfreq,startDate,endDate,objRptLag,iRptLag=FALSE) {
				startyear<-as.numeric(substr(as.character(startDate),1,4))
				startmonth<-as.numeric(substr(as.character(startDate),6,7))
				endyear<-as.numeric(substr(as.character(endDate),1,4))
				endmonth<-as.numeric(substr(as.character(endDate),6,7))
				nmth<-(endyear-startyear)*12+endmonth-startmonth+1
				istart <- max(1,(startyear-as.numeric(substr(as.character(startDate),1,4)))*12+startmonth-as.numeric(substr(as.character(startDate),6,7))+1)
				iend <- min(istart + nmth - 1,nmonths)
				result <- mthfreq[istart:iend]
				if (iRptLag == FALSE) {
					return(round(result))
				} else {
					for (i in c(1:(iend-istart+1))){
						prob <- mean(Probability(objRptLag,(nmth-istart-i+1)*30+c(1:30)))
						result[i] <- result[i]*(1-prob)
					}
					return(round(result))
				}
			}
			
			if(object@ncores>1 & object@simNo >1){
				require(parallel)
				
				pC <- makeCluster(object@ncores, outfile = mcfile)
				vsim <- rep(0,object@ncores)
				vsims <- rep(0,object@ncores)
				vobj<-list()
				if (object@ncores > object@simNo) {
					object@ncores <- max(object@simNo,2)
				}
				for (j in c(1:object@ncores)){
					vobj<- c(vobj,object)
					vsim[j] <- floor(object@simNo/object@ncores)*j
					vsims[j] <- floor(object@simNo/object@ncores)*(j-1)+1
				}
				vsim[object@ncores] <- object@simNo
				#set.seed(123)
				clusterSetRNGStream (cl = pC)
				clusterExport(cl=pC, varlist=c("vsim", "vsims", "vobj", "claimData", "nmonths", "startDate", "evaluationDate", "futureDate", "monthlyfreq", "filename"), envir=environment())
				msim <- function(i){
				
					if(nchar(object@libpath)>0 & dir.exists(object@libpath)){
						.libPaths(object@libpath)
						library(cascsim,lib.loc=object@libpath)
					}
					
					simdata = data.frame(Sim=character(),
										ClaimID=character(),
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
										reopenDate=character(),
										resettleDate=character(),
										reopenLoss=double(),
										expectedLoss=double(),
										LAE=double(),
										ultimateLAE=double(),
										expectedLAE=double(),
										stringsAsFactors=FALSE)
					if(object@simfile==""){
						filenamei <- paste0("sim",object@tag,"c",i,".csv")
					} else {
						filenamei <- paste0(object@simfile,"c",i,".csv")
					}
					for(isim in c(vsims[i]:vsim[i])){
						object<-vobj[[i]]
						print(paste0("Worker ",i,",Simulation ",isim+object@startNo-1," started at ",date()))
						if(object@iCopula==TRUE) {
							freqs <- copulaSample(object@freqCopula,nmonths)
						}
						
						for(l in object@lines){
							for(t in object@types){
								for(co in object@claimobjs){
									if(co@line==l & co@claimType==t) break						
								}
								
								if(co@line==l & co@claimType==t){
									co@simno<-as.character(isim+object@startNo-1)
									if(object@iIBNR==TRUE){
										if(object@iCopula==TRUE){
											co@IBNRfreqIndex@startDate <- startDate
											lid <- match(l,object@lines)
											co@IBNRfreqIndex@monthlyIndex <- round(shiftIndex(co@exposureIndex, startDate, evaluationDate)* monthlyfreq(Quantile(co@frequency,freqs[,lid]),startDate,evaluationDate,co@reportLag,TRUE))
										} else {
											co@IBNRfreqIndex@startDate <- startDate
											mthfreq <- doSample(co@frequency,nmonths)
											co@IBNRfreqIndex@monthlyIndex <- round(shiftIndex(co@exposureIndex, startDate, evaluationDate)* monthlyfreq(mthfreq,startDate,evaluationDate,co@reportLag,TRUE))
										}
									}
									
									if(object@iUPR==TRUE){
										if(object@iCopula==TRUE){
											co@UPRfreqIndex@startDate <- as.Date(evaluationDate)+15
											lid <- match(l,object@lines)
											co@UPRfreqIndex@monthlyIndex <- round(shiftIndex(co@exposureIndex, as.Date(evaluationDate)+15, futureDate)* monthlyfreq(Quantile(co@frequency,freqs[,lid]),as.Date(evaluationDate)+15,futureDate,co@reportLag,FALSE))
										} else {
											co@UPRfreqIndex@startDate <- as.Date(evaluationDate)+15
											mthfreq <- doSample(co@frequency,nmonths)
											co@UPRfreqIndex@monthlyIndex <- round(shiftIndex(co@exposureIndex, as.Date(evaluationDate)+15, futureDate)* monthlyfreq(mthfreq,as.Date(evaluationDate)+15,futureDate,co@reportLag,FALSE))										
										}										
									}
									
									stmp = data.frame(Sim=character(),
														ClaimID=character(),
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
														reopenDate=character(),
														resettleDate=character(),
														reopenLoss=double(),
														expectedLoss=double(),
														LAE=double(),
														ultimateLAE=double(),
														expectedLAE=double(),
														stringsAsFactors=FALSE)

									co@iRBNER = object@iRBNER
									co@iROPEN = object@iROPEN
									co@iIBNR = object@iIBNR
									co@iUPR = object@iUPR
									
									stmp<-rbind(stmp,claimSample(co,claimData,startDate,evaluationDate)) #futureDate											
									if(file.exists(filenamei)){
										write.table(stmp,filenamei,row.names=FALSE, sep=",",append = TRUE, col.names = FALSE)
									}else{
										write.table(stmp,filenamei,row.names=FALSE, sep=",",append = TRUE, col.names = TRUE)
									}
									simdata <- rbind(simdata,stmp)
									
								}
							}
						}
						#	if (isim %% 10 == 0) {
						print(paste0("Worker",i,",Simulation ",isim+object@startNo-1," ended at ",date()))
						print(">>>Simulation is finished successfully<<<")
					}
					#file.rename("simdatatmp.csv", filename)
					return(simdata)
				}
				#ptm<-proc.time()
				#clusterExport(pC, "ptm")
				msimdata <- parLapply(pC, 1:(vobj[[1]]@ncores), msim)
				simdata = data.frame(Sim=character(),
									ClaimID=character(),
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
									reopenDate=character(),
									resettleDate=character(),
									reopenLoss=double(),
									expectedLoss=double(),
									LAE=double(),
									ultimateLAE=double(),
									expectedLAE=double(),
									stringsAsFactors=FALSE)
				
				for (k in c(1:length(msimdata))){
					simdata<-rbind(simdata,msimdata[[k]])
				}
				stopCluster (pC)
				rm(msimdata)
				gc()
				if(file.exists(filename)){
					write.table(simdata,filename,row.names=FALSE, sep=",",append = TRUE, col.names = FALSE)
				}else{
					write.table(simdata,filename,row.names=FALSE, sep=",",append = TRUE, col.names = TRUE)
				}
				for(i in c(1:object@ncores)){
					if(object@simfile==""){
						if(file.exists(paste0("sim",object@tag,"c",i,".csv"))) file.remove(paste0("sim",object@tag,"c",i,".csv"))
					} else {
						if(file.exists(paste0(object@simfile,"c",i,".csv"))) file.remove(paste0(object@simfile,"c",i,".csv"))
					}
				}
			} else {
				for(isim in c(1:object@simNo)){
					if(object@iCopula==TRUE) {
						freqs <- copulaSample(object@freqCopula,nmonths)
					}
					print(paste0("Simulation ",isim+object@startNo-1," started at ",date()))					
					for(l in object@lines){
						for(t in object@types){
							for(co in object@claimobjs){
								if(co@line==l & co@claimType==t) break						
							}
							
							if(co@line==l & co@claimType==t){
								co@simno<-as.character(isim+object@startNo-1)

								if(object@iIBNR==TRUE){
									if(object@iCopula==TRUE){
										co@IBNRfreqIndex@startDate <- startDate
										lid <- match(l,object@lines)
										co@IBNRfreqIndex@monthlyIndex <- round(shiftIndex(co@exposureIndex, startDate, evaluationDate)* monthlyfreq(Quantile(co@frequency,freqs[,lid]),startDate,evaluationDate,co@reportLag,TRUE))
									} else {
										co@IBNRfreqIndex@startDate <- startDate
										mthfreq <- doSample(co@frequency,nmonths)
										co@IBNRfreqIndex@monthlyIndex <- round(shiftIndex(co@exposureIndex, startDate, evaluationDate)* monthlyfreq(mthfreq,startDate,evaluationDate,co@reportLag,TRUE))										
									}
								}
								
								if(object@iUPR==TRUE){
									if(object@iCopula==TRUE){
										co@UPRfreqIndex@startDate <- as.Date(evaluationDate)+15
										lid <- match(l,object@lines)
										co@UPRfreqIndex@monthlyIndex <- round(shiftIndex(co@exposureIndex, as.Date(evaluationDate)+15, futureDate)* monthlyfreq(Quantile(co@frequency,freqs[,lid]),as.Date(evaluationDate)+15,futureDate,co@reportLag,FALSE))
									} else {
										co@UPRfreqIndex@startDate <- as.Date(evaluationDate)+15
										mthfreq <- doSample(co@frequency,nmonths)
										co@UPRfreqIndex@monthlyIndex <- round(shiftIndex(co@exposureIndex, as.Date(evaluationDate)+15, futureDate)* monthlyfreq(mthfreq,as.Date(evaluationDate)+15,futureDate,co@reportLag,FALSE))										
									}										
								}
								
								stmp = data.frame(Sim=character(),
													ClaimID=character(),
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
													reopenDate=character(),
													resettleDate=character(),
													reopenLoss=double(),
													expectedLoss=double(),
													LAE=double(),
													ultimateLAE=double(),
													expectedLAE=double(),
													stringsAsFactors=FALSE)
													
								co@iRBNER = object@iRBNER
								co@iROPEN = object@iROPEN
								co@iIBNR = object@iIBNR
								co@iUPR = object@iUPR

								stmp<-rbind(stmp,claimSample(co,claimData,startDate,evaluationDate)) #futureDate											
								if(file.exists(filename)){
									write.table(stmp,filename,row.names=FALSE, sep=",",append = TRUE, col.names = FALSE)
								}else{
									write.table(stmp,filename,row.names=FALSE, sep=",",append = TRUE, col.names = TRUE)
								}
								simdata <- rbind(simdata,stmp)

								#	if (isim %% 10 == 0) {
								#	print(paste0("Simulation ",isim,": ",proc.time()[3]," elapsed"))
							}
						}
					}
					#write.csv(simdata,filename,row.names=FALSE)
					print(paste0("Simulation ",isim+object@startNo-1," ended at ",date()))					
					print(">>>Simulation is finished successfully<<<")
				}
#				write.csv(simdata,filename,row.names=FALSE)
				#file.rename("simdatatmp.csv", filename)
			}
			gc()
			return(simdata)
		} else {
			stop("Nothing to simulate.")
		}
		

	}, error = function(err){
		if (exists("l") & exists("t") & exists("c") & exists("isim")){
			print(paste0(">>>Critical Error for claim simulation:  ",err," Line-", l, " Type-", t, " Class-", c, " Sim No.-", isim+object@startNo-1))
		} else {
			print(paste0(">>>Critical Error for claim simulation:  ",err))			
		}
		
		print(">>>Simulation is finished with error")
		gc()
		return(-1)
	})
})

#' Claim simulation result summary
#' @param object Simulation object
#' @param simdata simulation data generated by claimSimulation
#' @param startDate Date after which claims are analyzed
#' @param evaluationDate Date of evaluation for existing claims and IBNR
#' @param futureDate Date of evaluation for UPR (future claims)
#' @rdname simSummary
#' @export
setGeneric("simSummary", function(object, simdata, ...) standardGeneric("simSummary"))
setMethod("simSummary", signature("Simulation", "data.frame"), function(object, simdata, startDate=as.Date("2012-01-01"),evaluationDate=as.Date("2016-12-31"),futureDate=as.Date("2017-12-31"))
{
	tryCatch({
		startDate <- toDate(startDate)
		evaluationDate <- toDate(evaluationDate)
		futureDate <- toDate(futureDate)
		if(object@sumfile==""){
			summaryname <- paste0("sum",object@tag,".csv")
		} else {
			summaryname <- paste0(object@sumfile,".csv")
		}
		if(nchar(object@workingFolder)>0 & dir.exists(object@workingFolder)){
			setwd(object@workingFolder)
		} else if(nchar(object@workingFolder)>0 ){
			dir.create(object@workingFolder)
			setwd(object@workingFolder)
		}

		if(nrow(simdata)==0) {stop("No simulation data to summarize")}
		if (object@iSummary==TRUE){
			print(paste0("Starting summarizing the simulation results: ",date()))					
#			if(length(object@lines)>1) {
				idx1<-c(object@lines,"Total")
#			} else {
#				idx1<-object@lines
#			}
			
#			if(length(object@types)>1) {
				idx2<-c(object@types,"Total")
#			} else {
#				idx2<-object@types
#			}
			
#			if(length(object@classes)>1) {

			idx3 <- vector()
			if (object@iRBNER==TRUE) {idx3 <- c(idx3,"IBNER")}
			if (object@iROPEN==TRUE) {idx3 <- c(idx3,"ROPEN")}
			if (object@iIBNR==TRUE) {idx3 <- c(idx3,"IBNR")}
			if (object@iUPR==TRUE) {idx3 <- c(idx3,"UPR")}
			idx3<-c(idx3,"Total")
#			} else {
#				idx3<-object@classes
#			}
			
			idx4<-c(as.numeric(substr(as.character(startDate),1,4)):as.numeric(substr(as.character(futureDate),1,4)))
			idx4<-c(idx4,"Total")
			idx5<-c("avg","sd","min",paste0("percentile ",c(10, 30, 50, 75, 80, 95, 99)),"max")
			
			simSummary<-vector()
			summaryNames <- c("LoB","Type","Class","AccidentYear","Measure","Frequency","AvgIncurredLoss","TotalIncurredLoss","AvgUltimateLoss","TotalUltimateLoss","TotalPaid","TotalLAE","PaidLAE","ReopenProb")
			
			simdata$occurrenceYear <- as.numeric(substr(as.character(as.Date(simdata$occurrenceDate)),1,4))
			transStatus<-function(class){
				if(class=="IBNER"){
					return("OPEN")
				} else if (class=="ROPEN") {
					return("CLOSED")
				} else {
					return(class)
				}
			}
			
			for(i1 in idx1){
				l<-i1
				for(i2 in idx2){
					t<-i2
					for(co in object@claimobjs){
						if(co@line==l & co@claimType==t) break						
					}
					
					if((co@line==l & co@claimType==t) | (i1 == "Total") | (i2 == "Total")){
						for(i3 in idx3){
							c<-i3
							#isim<--10000
							for(i4 in idx4){
								if(i1 != "Total"){
									datatmp <- simdata[simdata[,"LoB"] == i1,]
									if(i2 != "Total") {datatmp <- datatmp[datatmp[,"Type"] == i2,]}
									if(i3 != "Total") {datatmp <- datatmp[datatmp[,"status"] == transStatus(i3),]}
									if(i3 == "ROPEN") {
										closeCount <- nrow(datatmp)/length(unique(datatmp$Sim))
										datatmp <- datatmp[!is.na(datatmp$reopenLoss),]
									}
	#								if(i3 == "Total") {
	#									datatmp <- datatmp[!(datatmp$status==transStatus("ROPEN") & is.na(datatmp$reopenLoss)),]
	#								}
									if(i4 != "Total") {datatmp <- datatmp[datatmp[,"occurrenceYear"] == as.numeric(i4),]}
								}else{
									datatmp <- simdata
									if(i4 != "Total") {datatmp <- datatmp[datatmp[,"occurrenceYear"] == as.numeric(i4),]}
									#datatmp <- datatmp[!(datatmp$status==transStatus("ROPEN") & is.na(datatmp$reopenLoss)),]
								}
								if (nrow(datatmp)>0){
									colreserve <- c("Sim","status","incurredLoss","osRatio","ultimateLoss","reopenLoss","LAE","ultimateLAE")
									datatmp <- datatmp[,colreserve]
									datatmp$paid <- datatmp$incurredLoss*(1-datatmp$osRatio)
									datatmp$ultimateLoss <- ifelse(datatmp$status=="CLOSED" & !is.na(datatmp$reopenLoss),datatmp$reopenLoss,datatmp$ultimateLoss)
									datatmp$ultimateLAE <- ifelse(datatmp$status=="CLOSED" & is.na(datatmp$reopenLoss),datatmp$LAE,datatmp$ultimateLAE)
									datatmp$status<-NULL
									datatmp$reopenLoss<-NULL
									frequency<-aggregate(incurredLoss ~ Sim, data = datatmp, length)[,2]
									avgIncurredLoss<-aggregate(incurredLoss ~ Sim, data = datatmp, mean)[,2]
									totalIncurredLoss<-aggregate(incurredLoss ~ Sim, data = datatmp, sum)[,2]
									avgUltimateLoss<-aggregate(ultimateLoss ~ Sim, data = datatmp, mean)[,2]
									totalUltimateLoss<-aggregate(ultimateLoss ~ Sim, data = datatmp, sum)[,2]
									totalPaidLoss<-aggregate(paid ~ Sim, data = datatmp, sum)[,2]
									totalLAE<-aggregate(ultimateLAE ~ Sim, data = datatmp, sum)[,2]
									totalPaidLAE<-aggregate(LAE ~ Sim, data = datatmp, sum)[,2]
									if(i3=="ROPEN") reopenProb<-aggregate(incurredLoss ~ Sim, data = datatmp, length)[,2]/closeCount
									gc()
									
									for(i5 in idx5){
										if(i5 == "avg"){
											tl<- c(i1,i2,i3,i4,i5)
											tl <- c(tl,mean(frequency),mean(avgIncurredLoss),mean(totalIncurredLoss),mean(avgUltimateLoss),mean(totalUltimateLoss),mean(totalPaidLoss),mean(totalLAE),mean(totalPaidLAE))
											if(i3=="ROPEN"){
												tl <- c(tl,mean(reopenProb))
											} else {
												tl <- c(tl,NA)
											}
											names(tl)<-summaryNames
										} else if(i5 == "sd"){
											tl<- c(i1,i2,i3,i4,i5)
											tl <- c(tl,sd(frequency),sd(avgIncurredLoss),sd(totalIncurredLoss),sd(avgUltimateLoss),sd(totalUltimateLoss),sd(totalPaidLoss),sd(totalLAE),sd(totalPaidLAE))
											if(i3=="ROPEN"){
												tl <- c(tl,sd(reopenProb))
											} else {
												tl <- c(tl,NA)
											}
											names(tl)<-summaryNames
										} else if(i5 == "min"){
											tl<- c(i1,i2,i3,i4,i5)
											tl <- c(tl,min(frequency),min(avgIncurredLoss),min(totalIncurredLoss),min(avgUltimateLoss),min(totalUltimateLoss),min(totalPaidLoss),min(totalLAE),min(totalPaidLAE))
											if(i3=="ROPEN"){
												tl <- c(tl,min(reopenProb))
											} else {
												tl <- c(tl,NA)
											}
											names(tl)<-summaryNames
										} else if(i5 == "max"){
											tl<- c(i1,i2,i3,i4,i5)
											tl <- c(tl,max(frequency),max(avgIncurredLoss),max(totalIncurredLoss),max(avgUltimateLoss),max(totalUltimateLoss),max(totalPaidLoss),max(totalLAE),max(totalPaidLAE))
											if(i3=="ROPEN"){
												tl <- c(tl,max(reopenProb))
											} else {
												tl <- c(tl,NA)
											}										
											names(tl)<-summaryNames
										} else {
											pc <- as.numeric(substr(i5,12,nchar(i5)))/100
											tl<- c(i1,i2,i3,i4,i5)
											tl <- c(tl,quantile(frequency,pc),quantile(avgIncurredLoss,pc),quantile(totalIncurredLoss,pc),quantile(avgUltimateLoss,pc),quantile(totalUltimateLoss,pc),quantile(totalPaidLoss,pc),quantile(totalLAE,pc),quantile(totalPaidLAE,pc))
											if(i3=="ROPEN"){
												tl <- c(tl,quantile(reopenProb,pc))
											} else {
												tl <- c(tl,NA)
											}										
											names(tl)<-summaryNames
										}
										simSummary<-rbind(simSummary,tl)
									}
								}
							}
						}
					}
				}
				print(paste0("Business Line ",i1," summarized: ",date()))					
			}
			rownames(simSummary)<-NULL
			simSummary<-as.data.frame(simSummary)
			#simSummary$AccidentYear=round(as.numeric(as.character(simSummary$AccidentYear)))
			simSummary$Frequency=round(as.numeric(as.character(simSummary$Frequency)))
			simSummary$AvgIncurredLoss=round(as.numeric(as.character(simSummary$AvgIncurredLoss)),2)
			simSummary$TotalIncurredLoss=round(as.numeric(as.character(simSummary$TotalIncurredLoss)),2)
			simSummary$AvgUltimateLoss=round(as.numeric(as.character(simSummary$AvgUltimateLoss)),2)
			simSummary$TotalUltimateLoss=round(as.numeric(as.character(simSummary$TotalUltimateLoss)),2)
			simSummary$TotalPaid=round(as.numeric(as.character(simSummary$TotalPaid)),2)
			simSummary$TotalLAE=round(as.numeric(as.character(simSummary$TotalLAE)),2)
			simSummary$PaidLAE=round(as.numeric(as.character(simSummary$PaidLAE)),2)
			simSummary$ReopenProb <- ifelse(is.na(simSummary$ReopenProb),NA,round(as.numeric(as.character(simSummary$ReopenProb)),4))
			write.table(simSummary,summaryname,row.names=FALSE, sep=",")
			return(simSummary)
		} else {
			stop("Reset iSummary of the simulation object to TRUE to start summarizing the result.")
		}
	}, error = function(err){
		if (exists("l") & exists("t") & exists("c")){
			print(paste0(">>>Critical Error for claim simulation summarization: ",err," Line-", l, " Type-", t, " Class-", c))
		} else {
			print(paste0(">>>Critical Error for claim simulation summarization: ",err))		
		}
		return(-1)
		gc()
	})
})

#' Claim simulation result triangles
#' @param object Simulation object
#' @param claimdata claim data used as basis for simulation
#' @param simdata simulation data generated by claimSimulation
#' @param frequency triangle frequency, either "yearly" or "quarterly"
#' @param startDate Date after which claims are analyzed
#' @param evaluationDate Date of evaluation for existing claims and IBNR
#' @param futureDate Date of evaluation for UPR (future claims)
#' @rdname simTriangle
#' @export
setGeneric("simTriangle", function(object, claimdata, simdata, ...) standardGeneric("simTriangle"))
setMethod("simTriangle", signature("Simulation", "data.frame", "data.frame"), function(object, claimdata, simdata, frequency="yearly", startDate=as.Date("2012-01-01"),evaluationDate=as.Date("2016-12-31"),futureDate=as.Date("2017-12-31"))
{
	tryCatch({
		startDate <- toDate(startDate)
		evaluationDate <- toDate(evaluationDate)
		futureDate <- toDate(futureDate)
		if(object@sumfile==""){
			summaryname <- paste0("tri",object@tag,".csv")
		} else {
			summaryname <- paste0("tri",object@sumfile,".csv")
		}
		if(nchar(object@workingFolder)>0 & dir.exists(object@workingFolder)){
			setwd(object@workingFolder)
		} else if(nchar(object@workingFolder)>0 ){
			dir.create(object@workingFolder)
			setwd(object@workingFolder)
		}
		
		if (nrow(claimdata)>0){
			claimdata <- claimdata[claimdata[,"LoB"] %in% object@lines & claimdata[,"Type"] %in% object@types,]
			claimdata[,"status"] = toupper(claimdata[,"status"])		
			claimdata[,"occurrenceDate"] = toDate(claimdata[,"occurrenceDate"])
			claimdata[,"reportDate"] = toDate(claimdata[,"reportDate"])
			claimdata[,"settlementDate"] = toDate(claimdata[,"settlementDate"])
		}

		if (object@iSummary==TRUE){
			print(paste0("Starting creating triangles from the simulation results: ",date()))					
#			if(length(object@lines)>1) {
				idx1<-c(object@lines,"Total")
#			} else {
#				idx1<-object@lines
#			}
			
#			if(length(object@types)>1) {
				idx2<-c(object@types,"Total")
#			} else {
#				idx2<-object@types
#			}
			
			idx3<-c("reportedCount","closedCount","incurredLoss")
#			idx4<-c(as.numeric(substr(as.character(startDate),1,4)):as.numeric(substr(as.character(futureDate),1,4)))
			idx5<-c("upper","avg",paste0("percentile ",c(10,50,90,99)))
			if (object@simNo < 10) {
				idx5<-c("upper","avg")
			}
			
			if (nrow(claimdata)==0 & object@simNo >=10) {
				idx5<-c("avg",paste0("percentile ",c(10,50,90,99)))
			}

			if (nrow(claimdata)==0 & object@simNo <10) {
				idx5<-c("avg")
			}
#
			xTri <- new("Triangle", triID = "TRI1", type = "closedCount", startDate=startDate, frequency="yearly", sim=0, iRBNER=object@iRBNER, iROPEN=object@iROPEN)
			if (nrow(claimdata)>0) {
				xTri<-setUpperTriangle(xTri,claimdata,evaluationDate=evaluationDate)
				xTri<-setUpperKeep(xTri,claimdata,evaluationDate=evaluationDate)
			}
			xTri<-setRectangle(xTri,simdata,evaluationDate=evaluationDate,futureDate=futureDate)
			dynames <- colnames(xTri@rectangle)		
			simSummary<-vector()
			summaryNames <- c("LoB","Type","Triangle","Measure","AccidentPeriod",dynames)
			
			for(i1 in idx1){
				l<-i1
				for(i2 in idx2){
					t<-i2
					for(co in object@claimobjs){
						if(co@line==l & co@claimType==t) break						
					}
					
					if((co@line==l & co@claimType==t) | (i1 == "Total") | (i2 == "Total")){
						for(i3 in idx3){
							c<-i3
							xTri@type <- c
							for(i5 in idx5){
								if (i5 == "upper"){
									xTri<-setUpperTriangle(xTri,claimdata,lob=i1,ctype=i2,evaluationDate=evaluationDate)
									cname <- colnames(xTri@upper)
									rname <- rownames(xTri@upper)
									tl<- data.frame(xTri@upper)
									colnames(tl) <- cname
									for (iname in dynames) {
										if(!(iname %in% cname)){
											tl[,iname]<-0
										}
									}
									tl$AccidentPeriod <- rname
								} else if(i5 == "avg"){
									xTri@sim <- 0
									if (nrow(claimdata)>0) {
										xTri<-setUpperTriangle(xTri,claimdata,lob=i1,ctype=i2,evaluationDate=evaluationDate)
										xTri<-setUpperKeep(xTri,claimdata,lob=i1,ctype=i2,evaluationDate=evaluationDate)
									}
									xTri<-setRectangle(xTri,simdata,lob=i1,ctype=i2,evaluationDate=evaluationDate,futureDate=futureDate)
									cname <- colnames(xTri@rectangle)
									rname <- rownames(xTri@rectangle)
									tl<- data.frame(xTri@rectangle)
									colnames(tl) <- cname
									for (iname in dynames) {
										if(!(iname %in% cname)){
											tl[,iname]<-0
										}
									}
									tl$AccidentPeriod <- rname
								} else if(i5 == "min"){
									xTri@sim <- NaN
									xTri@percentile <- 0
									if (nrow(claimdata)>0) {
										xTri<-setUpperTriangle(xTri,claimdata,lob=i1,ctype=i2,evaluationDate=evaluationDate)
										xTri<-setUpperKeep(xTri,claimdata,lob=i1,ctype=i2,evaluationDate=evaluationDate)
									}
									xTri<-setRectangle(xTri,simdata,lob=i1,ctype=i2,evaluationDate=evaluationDate,futureDate=futureDate)
									cname <- colnames(xTri@rectangle)
									rname <- rownames(xTri@rectangle)
									tl<- data.frame(xTri@rectangle)
									colnames(tl) <- cname
									for (iname in dynames) {
										if(!(iname %in% cname)){
											tl[,iname]<-0
										}
									}
									tl$AccidentPeriod <- rname
								} else if(i5 == "max"){
									xTri@sim <- NaN
									xTri@percentile <- 100
									if (nrow(claimdata)>0) {
										xTri<-setUpperTriangle(xTri,claimdata,lob=i1,ctype=i2,evaluationDate=evaluationDate)
										xTri<-setUpperKeep(xTri,claimdata,lob=i1,ctype=i2,evaluationDate=evaluationDate)
									}
									xTri<-setRectangle(xTri,simdata,lob=i1,ctype=i2,evaluationDate=evaluationDate,futureDate=futureDate)
									cname <- colnames(xTri@rectangle)
									rname <- rownames(xTri@rectangle)
									tl<- data.frame(xTri@rectangle)
									colnames(tl) <- cname
									for (iname in dynames) {
										if(!(iname %in% cname)){
											tl[,iname]<-0
										}
									}
									tl$AccidentPeriod <- rname
								} else {
									xTri@sim <- NaN
									xTri@percentile <- as.numeric(substr(i5,12,nchar(i5)))
									if (nrow(claimdata)>0) {
										xTri<-setUpperTriangle(xTri,claimdata,lob=i1,ctype=i2,evaluationDate=evaluationDate)
										xTri<-setUpperKeep(xTri,claimdata,lob=i1,ctype=i2,evaluationDate=evaluationDate)
									}
									xTri<-setRectangle(xTri,simdata,lob=i1,ctype=i2,evaluationDate=evaluationDate,futureDate=futureDate)
									cname <- colnames(xTri@rectangle)
									rname <- rownames(xTri@rectangle)
									tl<- data.frame(xTri@rectangle)
									colnames(tl) <- cname
									for (iname in dynames) {
										if(!(iname %in% cname)){
											tl[,iname]<-0
										}
									}
									tl$AccidentPeriod <- rname
								}
								
								tl$LoB <- i1
								tl$Type <- i2
								tl$Triangle <- i3
								tl$Measure <- i5
								tl <- tl[,summaryNames]
								simSummary<-rbind(simSummary,tl)
							}
						}
					}
				}
				print(paste0("Business Line ",i1," triangle constructed: ",date()))					
			}
			rownames(simSummary)<-NULL
			write.table(simSummary,summaryname,row.names=FALSE, sep=",")
			return(simSummary)
		} else {
			stop("Reset iSummary of the simulation object to TRUE to start summarizing the triangles.")
		}
	}, error = function(err){
		if (exists("l") & exists("t") & exists("c") & exists("i5")){
			print(paste0(">>>Critical Error for constructing claim simulation triangles: ",err," Line-", l, " Type-", t, " Class-", c, " Measure-",i5))
		} else {
			print(paste0(">>>Critical Error for constructing claim simulation triangles: ",err))		
		}
		return(-1)
		gc()
	})
})	
			
#' Generate claim simulation result report in html
#' @param object ClaimType object
#' @param simSummary simulation result summary generated by simSummary
#' @param simTriangle triangle summary generated by simTriangle
#' @param startDate Date after which claims are analyzed
#' @param evaluationDate Date of evaluation for existing claims and IBNR
#' @param futureDate Date of evaluation for UPR (future claims)
#' @param iYear Boolean that indicates whether summary by accident year should be produced in the report
#' @rdname simReport
#' @export
setGeneric("simReport", function(object, simSummary, ...) standardGeneric("simReport"))
setMethod("simReport", signature("Simulation", "data.frame"), function(object, simSummary, simTriangle=NA, startDate=as.Date("2012-01-01"),evaluationDate=as.Date("2016-12-31"),futureDate=as.Date("2017-12-31"),iYear=FALSE)
{
	tryCatch({
		startDate <- toDate(startDate)
		evaluationDate <- toDate(evaluationDate)
		futureDate <- toDate(futureDate)
		if (object@iReport==TRUE){

			if(nchar(object@workingFolder)>0 & dir.exists(object@workingFolder)){
				setwd(object@workingFolder)
			} else if(nchar(object@workingFolder)>0 ){
				dir.create(object@workingFolder)
				setwd(object@workingFolder)
			}
			
			classes <- vector()
			if (object@iRBNER==TRUE) {classes <- c(classes,"IBNER")}
			if (object@iROPEN==TRUE) {classes <- c(classes,"REOPEN")}
			if (object@iIBNR==TRUE) {classes <- c(classes,"IBNR")}
			if (object@iUPR==TRUE) {classes <- c(classes,"UPR")}

			require(R2HTML)
			pckgdir <- find.package("cascsim")
			pckgdir <- paste0(pckgdir,"/doc/")
			dir.create("report")
			setwd(paste0(getwd(),"/report"))
			copied <- file.copy(file.path(pckgdir, "report.css"), file.path(getwd(),"report.css"))
			if(object@htmlRpt==""){
				htmlfile <- paste0("sim",object@tag)
			} else {
				htmlfile <- paste0(object@htmlRpt)
			}
			if (file.exists(paste0(htmlfile,".html"))) {file.remove(paste0(htmlfile,".html"))}
			
			ReportBegin <- function(file, title) {
				cat(paste("<html><head><title>", title, "</title></head>", "<body bgcolor=#D0D0D0>", sep = ""), file = file, append = FALSE)
			}

			ReportEnd <- function(file) {
				cat("<hr size=1></body></html>", file = file, append = TRUE)
			}
			
			haveData<-function(line,type,class){
				result <- simSummary[simSummary$LoB==line & simSummary$Type==type & simSummary$Class==class,]
				if (nrow(result) > 0) {
					result <- TRUE
				} else {
					result <- FALSE
				}
				return (result)
			}

			RptFetch<-function(line,type,class,accYear=TRUE){
				result <- simSummary[simSummary$LoB==line & simSummary$Type==type & simSummary$Class==class,]
				if (nrow(result) > 0) {
					rowR<-c("avg","sd","min","percentile 10","percentile 30","percentile 50","percentile 75","percentile 90","percentile 95","percentile 99","max")#"percentile 99.5"
					result <- subset(result, result$Measure %in% rowR)
					if (accYear==FALSE) {result <- result[result$AccidentYear=="Total",]}
					rownames(result)<-NULL
					if(accYear==TRUE){
						result <- result[,colnames(result) %in% c("AccidentYear","Measure","Frequency","TotalUltimateLoss")]
						yrs <- unique(result$AccidentYear)
						seps <- list()
						coln <- vector()
						for(y in c(1:length(yrs))){
							tmp <- result[result$AccidentYear==yrs[y],]
							if(y==1){
								tmp <- tmp[,colnames(tmp) %in% c("Measure","Frequency","TotalUltimateLoss")]
								coln <- c(coln,"Measure",paste0(yrs[y],"Count"),paste0(yrs[y],"Loss"))
								seps<-c(seps,tmp)
							} else {
								tmp <- tmp[,colnames(tmp) %in% c("Frequency","TotalUltimateLoss")]
								coln <- c(coln,paste0(yrs[y],"Count"),paste0(yrs[y],"Loss"))
								seps<-c(seps,tmp)
							}
						}
						for(y in c(1:length(seps))){
							if(y==1){
								result<-seps[[y]]
							}else{
								result<-cbind(result,seps[[y]])
							}
						}
						result<-as.data.frame(result)
						result[,1]<-rowR
						colnames(result)<-coln
					} else {
						result <- result[,!colnames(result) %in% c("LoB","Type","Class","AccidentYear","AvgIncurredLoss","TotalIncurredLoss","TotalPaid","PaidLAE","ReopenProb")]				
						colnames(result) <- c("Measure","Count","AvgUltimateLoss","TotalUltimateLoss","TotalLAE")
					}
				} else {
					result <- paste0("No ", class, " claims are simulated.")
				}
				return(result)
			}
			
			triangleFetch<-function(line,type,triangle,measure){
				result <- simTriangle[simTriangle$LoB==line & simTriangle$Type==type & simTriangle$Triangle==triangle & simTriangle$Measure==measure,]
				if (nrow(result) > 0) {
					colremove <- c("LoB","Type","Triangle","Measure")
					colkeep <- colnames(result)
					colkeep <- colkeep[!colkeep %in% colremove]
					if (measure == "upper") {colkeep <- colkeep[1:(nrow(result)+1)]}
					result <- result[,colnames(result) %in% colkeep]

					if (triangle=="incurredLoss" | triangle=="paidLoss"){
						result[,!colnames(result) %in% c("AccidentPeriod")] <- round(result[,!colnames(result) %in% c("AccidentPeriod")]/1000)
					}

					for (i in c(2:ncol(result))){
						result[,i] <- formatC(result[,i], format="d", big.mark=",")
					}
					if (measure == "upper") {
						for (c in range(1:ncol(result))){
							result[,c] = as.character(result[,c])
						}
						#result <- apply(result,2,as.character)
						for(i in c(1:nrow(result))){
							for(j in c(1:ncol(result))){
								if(result[i,j]=="0"){result[i,j]=""}
							}
						}
					} else {
						result$AccidentPeriod <- as.character(result$AccidentPeriod)
					}
					
					
					colnames(result) <- colkeep
					rownames(result) <- NULL
				} else {
					result <- NULL
				}
				return(result)
			}

			RptPlot<-function(line,type,class,accYear){
				result <- simSummary[simSummary$LoB==line & simSummary$Type==type & simSummary$Class==class & simSummary$AccidentYear==accYear,]
				##c("avg","sd","min",paste0("percentile ",c(10, 30, 50, 75, 80, 95, 99)),"max")
				rowR<-paste0("percentile ",c(10, 30, 50, 75, 80, 95, 99))
				result <- subset(result, result$Measure %in% rowR)
				perc <- c(10, 30, 50, 75, 80, 95, 99)/100
				par(mfrow = c(2, 2)) 
				plot(result$TotalUltimateLoss, perc, type = "p", xlab = "TotalUltimateLoss", ylab = "Cumulative Probability", main = "Total Ultimate Loss CDF") 
				plot(result$AvgUltimateLoss, perc, type = "p", xlab = "AvgUltimateLoss", ylab = "Cumulative Probability", main = "Avg. Ultimate Loss CDF") 
				plot(result$Frequency, perc, type = "p", xlab = "Frequency", ylab = "Cumulative Probability", main = "Count CDF") 
				plot(result$TotalLAE, perc, type = "p", xlab = "TotalLAE", ylab = "Cumulative Probability", main = "Total LAE CDF") 
			}

			distPlot<-function(claimobj){
				par(mfrow = c(2, 2)) 
				x<-doSample(claimobj@frequency, 1000)
				p<-seq(0,1, by=0.005)
				q<-Quantile(claimobj@frequency, p)
				plot(q, Density(claimobj@frequency, q),type="l",  main = "Frequency", xlab="q", ylab="density", col = "red") 

				x<-doSample(claimobj@severity, 1000)
				p<-seq(0,1, by=0.005)
				q<-Quantile(claimobj@severity, p)
				plot(q, Density(claimobj@severity, q),type="l",  main = "Severity", xlab="q", ylab="density", col = "red") 

				x<-doSample(claimobj@reportLag, 1000)
				p<-seq(0,1, by=0.005)
				q<-Quantile(claimobj@reportLag, p)
				plot(q, Density(claimobj@reportLag, q),type="l",  main = "Report Lag", xlab="q", ylab="density", col = "red") 

				x<-doSample(claimobj@settlementLag, 1000)
				p<-seq(0,1, by=0.005)
				q<-Quantile(claimobj@settlementLag, p)
				plot(q, Density(claimobj@settlementLag, q),type="l",  main = "Settlement Lag", xlab="q", ylab="density", col = "red") 
			}

			idxPlot<-function(claimobj){
				par(mfrow = c(2, 2)) 
				plot(claimobj@exposureIndex@monthlyIndex, main = "Exposure Index", xlab="Time", ylab="Index") 
				plot(claimobj@severityIndex@monthlyIndex, main = "Severity Index", xlab="Time", ylab="Index") 
				if(claimobj@iCopula==TRUE){
					samples<-copulaSample(claimobj@ssrCopula, 1000)
					if (claimobj@ssrCopula@dimension==2)
					{
						plot(samples, col="blue")
					}
					else if (claimobj@ssrCopula@dimension>=2)
					{
						require(scatterplot3d)
						scatterplot3d(samples[,1:3], color="blue")
					}
				} else {
					plotText("Severity and lags are independent.")				
				}
				if (claimobj@laeDevFac@FacModel==TRUE){
					plotText("LAE development factor is formula based.")
				} else {
					plot(claimobj@laeDevFac@meanList, main = "LAE Development", xlab="Duration", ylab="Factor")
				}
			}

			roPlot<-function(claimobj){
				par(mfrow = c(2, 2)) 
				if (claimobj@reopen@FacModel==TRUE){
					plotText("Reopen probability is formula based.")
				} else {
					plot(claimobj@reopen@meanList, main = "Reopen probability", xlab="Duration", ylab="Probability")
				}

				if (claimobj@roDevFac@FacModel==TRUE){
					plotText("Reopen loss development is formula based.")
				} else if (claimobj@irDevFac==3 & claimobj@roDevFac@distType == "normal"){
					plot(claimobj@roDevFac@meanList, main = "Reopen loss development", xlab="Duration", ylab="Factor")
				} else if (claimobj@irDevFac==3 & claimobj@roDevFac@distType != "normal"){
					plotText(paste0("LDF based on ", claimobj@roDevFac@distType, " dist."))
				} else {
					plotText("Loss based on truncated severity dist.")				
				}

				x<-doSample(claimobj@reopenLag, 1000)
				p<-seq(0,1, by=0.005)
				q<-Quantile(claimobj@reopenLag, p)
				plot(q, Density(claimobj@reopenLag, q),type="l",  main = "Reopen Lag", xlab="q", ylab="density", col = "red") 

				x<-doSample(claimobj@resettleLag, 1000)
				p<-seq(0,1, by=0.005)
				q<-Quantile(claimobj@resettleLag, p)
				plot(q, Density(claimobj@resettleLag, q),type="l",  main = "Resettlement Lag", xlab="q", ylab="density", col = "red") 

			}

			dlPlot<-function(claimobj){
				par(mfrow = c(1, 2)) 
				
				y<-claimobj@deductible
				
				ysample<-doSample(y, 1000)
				d1<-ecdf(ysample)
				plot(d1, main = "CDF: Deductible", col="blue")

				y<-claimobj@limit
				
				ysample<-doSample(y, 1000)
				d1<-ecdf(ysample)
				plot(d1, main = "CDF: Limit", col="blue")

			}

			fibnerPlot<-function(claimobj){
				par(mfrow = c(1, 1)) 
				if (claimobj@fIBNER@FacModel==TRUE){
					plotText("IBNER loss development is formula based.")
				} else {
					plot(claimobj@fIBNER@meanList, main = "RBNER Loss Development", xlab="Duration", ylab="Factor")
				}
			}

			p0Plot<-function(claimobj){
				par(mfrow = c(1, 1)) 
				plot(claimobj@p0@meanList, main = "Probability of Zero Payment", xlab="Duration", ylab="Factor")
			}

			vecFlatten<-function(vec){
				result<-""
				if(length(vec)>1){
					for(i in c(1:(length(vec)-1))){
						result<-paste0(result,vec[i],", ")
					}
					result <- paste0(result,"and ",vec[length(vec)])
				} else {
					result <- vec[1]
				}
				return(result)
			}
			
			getCopulaInfo<-function(c){
				out <- paste0(c@info," ",c@type," copula, dimension=",c@dimension)
				if(c@type=="t"){out<-paste0(out,", df=",c@df)}
				if((c@type=="t" | c@type=="normal") & length(c@param)==(c@dimension*(c@dimension-1)/2)){
					outm<-matrix(1,c@dimension,c@dimension)
					irow<-2
					icol<-1
					for(cor in c@param){
						outm[irow,icol]<-cor
						outm[icol,irow]<-cor
						if(irow<c@dimension){
							irow<-irow+1
						}else{
							icol<-icol+1
							irow<-icol+1
						}
					}
					outm<-as.data.frame(outm)
					colnames(outm)<-object@lines
					return(list(out=out,outm=outm))
				}else{
					out<-paste0(out,", parameter=",vecFlatten(c@param))
					return(list(out=out))
				}
			}
			
			getCopulaSR<-function(c){
				out <- paste0(c@info," ",c@type," copula, dimension=",c@dimension)
				if(c@type=="t"){out<-paste0(out,", df=",c@df)}
				if((c@type=="t" | c@type=="normal") & length(c@param)==(c@dimension*(c@dimension-1)/2)){
					outm<-matrix(1,c@dimension,c@dimension)
					irow<-2
					icol<-1
					for(cor in c@param){
						outm[irow,icol]<-cor
						outm[icol,irow]<-cor
						if(irow<c@dimension){
							irow<-irow+1
						}else{
							icol<-icol+1
							irow<-icol+1
						}
					}
					outm<-as.data.frame(outm)
					colnames(outm)<-c("Severity","SettlementLag","ReportLag")
					return(list(out=out,outm=outm))
				}else{
					out<-paste0(out,", parameter=",vecFlatten(c@param))
					return(list(out=out))
				}
			}

			findClaimObj <- function(line,type,objpool){
				for(co in objpool){
					if(co@line==line & co@claimType==type) {
						result <- co
						break
					} else {
						result <- NULL
					}
				}
				return(result)
			}

			HTMLcontent <- function(file, append = TRUE, directory = getwd()) {
				file = file.path(directory, file)
				cat("\n", file = file, append = append)
				HTML.title("Claim Simulation Report", file=file, HR=1)
				HTML(paste0("Claim Data Start Date: ",startDate,"; Claim Data Ending Date: ",evaluationDate,"; Future Claim Ending Date: ",futureDate), file=file)
				HTML(paste0("Simulation Task ID: ",object@tag), file=file)
				HTML(paste0("Report generated at ",date()), file=file)
				HTMLhr(file = file)
				HTML.title("Notation",file = file, HR=4)
				HTML(paste0("AccidentPeriod: Accident Year or Quarter."),file=file)
				HTML(paste0("Mxx: Development period. For example, M12 is the first development year and M24 is the second development year."),file=file)
				HTML(paste0("Count: Number of simulated claims."),file=file)
				HTML(paste0("AvgUltimateLoss: Ultimate Loss per claim. It is the average loss of all claims in one portfolio simulation. Each simulation has an average ultimate loss and multiple simulations allow the construction of the distribution."),file=file)
				HTML(paste0("TotalUltimateLoss: Total Ultimate Loss for all simulated claims in one portfolio simulation. Multiple simulations allow the construction of its distribution."),file=file)
				HTML(paste0("TotalLAE: Total Loss Adjustment Expense for all simulated claims in one portfolio simulation(if modeled separately from indemnity). Multiple simulations allow the construction of its distribution."),file=file)
				HTML(paste0("Measure: Statistical measure such as avg(average), sd(standard deviation), min, max and a certain percentile."),file=file)
				HTMLhr(file = file)
				HTML.title("Links",file = file, HR=4)
				HTML(paste0("<a href=\"#tp\">Total Portfolio</a>"),file=file)
				for(l in object@lines){
					HTML(paste0("<a href=\"#",l,"\">",l,"-Total","</a>"),file=file)
					for(t in object@types){
						if(!is.null(findClaimObj(l,t,object@claimobjs))){
							s<-paste0("<a href=\"#",l,t,"\">",l,"-",t,"</a>")
							for(c in classes){
								if(!is.null(findClaimObj(l,t,object@claimobjs))){
									s<-paste0(s," ","<a href=\"#",l,t,c,"\">",c,"</a>")
								}
							}
							HTML(s,file=file)
						}
					}
				}
				HTMLhr(file = file)
				HTML(paste0("<a name=\"tp\"></a>"), file = file)
				HTML.title("Total Portfolio",file = file, HR=2)
				HTML(paste0("The portfolio includes ",length(object@lines)," business line(s): ",vecFlatten(object@lines),". Each business line contains ",length(object@types)," type(s): ",vecFlatten(object@types),". ",length(classes)," class(es) of claims are simulated: ",vecFlatten(classes),"."),file=file)
				if(!is.na(simTriangle)){
					if (is.null(triangleFetch("Total","Total","reportedCount","upper"))) {
						HTML.title("Average Simulated Reported Count Rectangle",file = file, HR=3)
						HTML(triangleFetch("Total","Total","reportedCount","avg"), file = file, innerBorder=0.5, row.names=FALSE)
						HTMLhr(file = file)
						HTML.title("Average Simulated Closed Count Rectangle",file = file, HR=3)
						HTML(triangleFetch("Total","Total","closedCount","avg"), file = file, innerBorder=0.5, row.names=FALSE)
						HTMLhr(file = file)
						HTML.title("Average Simulated Data Incurred Loss Rectangle (Thousands)",file = file, HR=3)
						HTML(triangleFetch("Total","Total","incurredLoss","avg"), file = file, innerBorder=0.5, row.names=FALSE)
					} else {
						HTML.title("Claim Data Reported Count Triangle",file = file, HR=3)
						HTML(triangleFetch("Total","Total","reportedCount","upper"), file = file, innerBorder=0.5, row.names=FALSE)
						HTML.title("Average Simulated Reported Count Rectangle",file = file, HR=3)
						HTML(triangleFetch("Total","Total","reportedCount","avg"), file = file, innerBorder=0.5, row.names=FALSE)
						HTMLhr(file = file)
						HTML.title("Claim Data Closed Count Triangle",file = file, HR=3)
						HTML(triangleFetch("Total","Total","closedCount","upper"), file = file, innerBorder=0.5, row.names=FALSE)
						HTML.title("Average Simulated Closed Count Rectangle",file = file, HR=3)
						HTML(triangleFetch("Total","Total","closedCount","avg"), file = file, innerBorder=0.5, row.names=FALSE)
						HTMLhr(file = file)
						HTML.title("Claim Data Incurred Loss Triangle (Thousands)",file = file, HR=3)
						HTML(triangleFetch("Total","Total","incurredLoss","upper"), file = file, innerBorder=0.5, row.names=FALSE)
						HTML.title("Average Simulated Data Incurred Loss Rectangle (Thousands)",file = file, HR=3)
						HTML(triangleFetch("Total","Total","incurredLoss","avg"), file = file, innerBorder=0.5, row.names=FALSE)
					}
					HTMLhr(file = file)
				}
				HTML.title("Key Statistics - Total Portfolio, All Accident Years",file = file, HR=3)
				HTML(RptFetch("Total","Total","Total",FALSE), file = file, innerBorder=0.5, row.names=FALSE)
				HTMLhr(file = file)
				HTML.title("CDF - Total Portfolio, All Accident Years",file = file, HR=3)
				png(filename = paste0("sim",object@tag,"tp.png"))
				RptPlot("Total","Total","Total","Total")
				dev.off()
				HTMLInsertGraph(paste0("sim",object@tag,"tp.png"),Caption = "CDF",file = file)
				HTMLhr(file = file)
				if (iYear==TRUE) {
					HTML.title("Key Statistics - Total Portfolio, By Accident Year",file = file, HR=3)
					HTML(RptFetch("Total","Total","Total",TRUE), file = file, innerBorder=0.5, row.names=FALSE)
					HTMLhr(file = file)
				}
				HTML.title("Annual Frequency Correlation",file = file, HR=3)
				if(object@iCopula==FALSE){
					HTML("Frequencies are assumed to be independent for different lines/types", file = file)
				} else {
					HTML(paste0("Frequencies are assumed to follow ",getCopulaInfo(object@freqCopula)$out), file = file)
					HTML(getCopulaInfo(object@freqCopula)$outm, file = file, innerBorder=0.5, row.names=FALSE)
				}
				HTMLhr(file = file)
				for(l in object@lines){
					HTML(paste0("<a name=\"",l,"\"></a>"), file = file)
					HTML.title(l,file = file, HR=2)
					HTML(RptFetch(l,"Total","Total",FALSE), file = file, innerBorder=0.5, row.names=FALSE)
					HTMLhr(file = file)
					HTML.title(paste0("CDF - ",l,", All Accident Years"),file = file, HR=3)
					png(filename = paste0("sim",object@tag,l,".png"))
					RptPlot(l,"Total","Total","Total")
					dev.off()
					HTMLInsertGraph(paste0("sim",object@tag,l,".png"),Caption = "CDF",file = file)
					HTMLhr(file = file)
					if (iYear==TRUE) {
						HTML.title(paste0("Key Statistics - ",l,", By Accident Year"),file = file, HR=3)
						HTML(RptFetch(l,"Total","Total",TRUE), file = file, innerBorder=0.5, row.names=FALSE)
						HTMLhr(file = file)
					}
					for(t in object@types){
						if(!is.null(findClaimObj(l,t,object@claimobjs))){
							HTML(paste0("<a name=\"",l,t,"\"></a>"), file = file)
							HTML.title(paste0(l,"-",t),file = file, HR=2)
							HTML(RptFetch(l,t,"Total",FALSE), file = file, innerBorder=0.5, row.names=FALSE)
							HTMLhr(file = file)
							HTML.title(paste0("CDF - ",l,"-",t,", All Accident Years"),file = file, HR=3)
							png(filename = paste0("sim",object@tag,l,t,".png"))
							RptPlot(l,t,"Total","Total")
							dev.off()
							HTMLInsertGraph(paste0("sim",object@tag,l,t,".png"),Caption = "CDF",file = file)
							HTMLhr(file = file)
							if (iYear==TRUE) {
								HTML.title(paste0("Key Statistics - ",l,"-",t,", By Accident Year"),file = file, HR=3)
								HTML(RptFetch(l,t,"Total",TRUE), file = file, innerBorder=0.5, row.names=FALSE)
								HTMLhr(file = file)
							}
							
							if(!is.null(findClaimObj(l,t,object@claimobjs))){
								HTML(paste0("Probability of Zero Payment by Development Year(p0): ",findClaimObj(l,t,object@claimobjs)@p0@FacID),file=file)
								HTML(toString(findClaimObj(l,t,object@claimobjs)@p0), file = file, innerBorder=0.5, row.names=TRUE)
								HTMLhr(file = file)								
							}
							
							for(c in classes){
								HTML(paste0("<a name=\"",l,t,c,"\"></a>"), file = file)
								HTML.title(paste0(l,"-",t,"-",c),file = file, HR=3)
								if (c=="IBNER"){
									c2 = "IBNER"
								} else if (c=="REOPEN") {
									c2 = "ROPEN"
								} else {
									c2 = c
								}
								HTML(RptFetch(l,t,c2,FALSE), file = file, innerBorder=0.5, row.names=FALSE)
								HTMLhr(file = file)
								if (haveData(l,t,c2) == TRUE) {
									HTML.title(paste0("CDF - ",l,"-",t,"-",c,", All Accident Years"),file = file, HR=5)
									png(filename = paste0("sim",object@tag,l,t,c,".png"))
									RptPlot(l,t,c2,"Total")
									dev.off()
									HTMLInsertGraph(paste0("sim",object@tag,l,t,c,".png"),Caption = "CDF",file = file)
									HTMLhr(file = file)
									if (iYear==TRUE) {
										HTML.title(paste0("Key Statistics - ",l,"-",t,"-",c,", By Accident Year"),file = file, HR=5)
										HTML(RptFetch(l,t,c,TRUE), file = file, innerBorder=0.5, row.names=FALSE)
										HTMLhr(file = file)	
									}
									HTML.title("Assumptions",file = file, HR=5)
									
									if(c=="IBNER"){
										if(!is.null(findClaimObj(l,t,object@claimobjs))){
											if(findClaimObj(l,t,object@claimobjs)@fIBNER@FacModel==TRUE & findClaimObj(l,t,object@claimobjs)@ioDevFac==3){
												HTML(paste0("IBNER Loss Development: ",findClaimObj(l,t,object@claimobjs)@fIBNER@FacID," ",toString(findClaimObj(l,t,object@claimobjs)@fIBNER)),file=file)
											} else if (findClaimObj(l,t,object@claimobjs)@ioDevFac==3){
												HTML(paste0("IBNER Loss Development: ",findClaimObj(l,t,object@claimobjs)@fIBNER@FacID, ". Distribution type: ", findClaimObj(l,t,object@claimobjs)@fIBNER@distType),file=file)
												HTML(toString(findClaimObj(l,t,object@claimobjs)@fIBNER), file = file, innerBorder=0.5, row.names=TRUE)										
											} else if (findClaimObj(l,t,object@claimobjs)@ioDevFac==2){
												HTML(paste0("IBNER Loss Development: Severity distribution floored by incurred loss is used. ",toString(findClaimObj(l,t,object@claimobjs)@severity)),file=file)
											} else {
												HTML(paste0("IBNER Loss Development: Severity distribution conditioned on paid loss is used. ",toString(findClaimObj(l,t,object@claimobjs)@severity)),file=file)											
											}
											if (findClaimObj(l,t,object@claimobjs)@ioDevFac==3 & findClaimObj(l,t,object@claimobjs)@fIBNER@distType == "normal"){
												png(filename = paste0("sim",object@tag,l,t,c,"fibner.png"),width=240,height=240)
												fibnerPlot(findClaimObj(l,t,object@claimobjs))
												dev.off()
												HTMLInsertGraph(paste0("sim",object@tag,l,t,c,"fibner.png"),Caption = "",file = file,WidthHTML=250)
											}
										}
									}else if (c=="REOPEN"){
										if(!is.null(findClaimObj(l,t,object@claimobjs))){
											if(findClaimObj(l,t,object@claimobjs)@reopen@FacModel==TRUE){
												HTML(paste0("Reopen Probability: ",findClaimObj(l,t,object@claimobjs)@reopen@FacID," ",toString(findClaimObj(l,t,object@claimobjs)@reopen)),file=file)
											} else {
												HTML(paste0("Reopen Probability: ",findClaimObj(l,t,object@claimobjs)@reopen@FacID),file=file)
												HTML(toString(findClaimObj(l,t,object@claimobjs)@reopen), file = file, innerBorder=0.5, row.names=TRUE)										
											}
											if(findClaimObj(l,t,object@claimobjs)@roDevFac@FacModel==TRUE & findClaimObj(l,t,object@claimobjs)@irDevFac==3){
												HTML(paste0("Reopen Loss Development: ",findClaimObj(l,t,object@claimobjs)@roDevFac@FacID," ",toString(findClaimObj(l,t,object@claimobjs)@roDevFac)),file=file)
											} else if (findClaimObj(l,t,object@claimobjs)@irDevFac==3){
												HTML(paste0("Reopen Loss Development: ",findClaimObj(l,t,object@claimobjs)@roDevFac@FacID, ". Distribution type: ", findClaimObj(l,t,object@claimobjs)@roDevFac@distType),file=file)
												HTML(toString(findClaimObj(l,t,object@claimobjs)@roDevFac), file = file, innerBorder=0.5, row.names=TRUE)										
											} else if (findClaimObj(l,t,object@claimobjs)@irDevFac==2){
												HTML(paste0("Reopen Loss Development: Severity distribution floored by incurred loss is used. ",toString(findClaimObj(l,t,object@claimobjs)@severity)),file=file)					
											} else {
												HTML(paste0("Reopen Loss Development: Severity distribution conditioned on paid loss is used. ",toString(findClaimObj(l,t,object@claimobjs)@severity)),file=file)																
											}
											HTML(paste0("Reopen Lag Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@reopenLag)),file=file)
											HTML(paste0("Resettlement Lag Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@resettleLag)),file=file)
											png(filename = paste0("sim",object@tag,l,t,c,"ro.png"))
											roPlot(findClaimObj(l,t,object@claimobjs))
											dev.off()
											HTMLInsertGraph(paste0("sim",object@tag,l,t,c,"ro.png"),Caption = "",file = file)								
										}
									}else if (c=="IBNR"){
										if(!is.null(findClaimObj(l,t,object@claimobjs))){
											HTML(paste0("Frequency Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@frequency)),file=file)
											HTML(paste0("Severity Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@severity)),file=file)
											HTML(paste0("Report Lag Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@reportLag)),file=file)
											HTML(paste0("Settlement Lag Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@settlementLag)),file=file)
											png(filename = paste0("sim",object@tag,l,t,c,"dist.png"))
											distPlot(findClaimObj(l,t,object@claimobjs))
											dev.off()
											HTMLInsertGraph(paste0("sim",object@tag,l,t,c,"dist.png"),Caption = "",file = file)
											HTML(paste0("Severity Index: ",toString(findClaimObj(l,t,object@claimobjs)@severityIndex)),file=file)
											HTML(paste0("Exposure Index: ",toString(findClaimObj(l,t,object@claimobjs)@exposureIndex)),file=file)
											HTML(paste0("Correlation between Severity, settlement Lag and Report Lag: "),file=file)
											if(findClaimObj(l,t,object@claimobjs)@iCopula==FALSE){
												HTML("Severity, Settlement Lag and Report Lag are assumed to be independent.", file = file)
											} else {
												HTML(paste0("Severity, Settlement Lag and Report Lag are assumed to follow ",getCopulaSR(findClaimObj(l,t,object@claimobjs)@ssrCopula)$out), file = file)
												HTML(getCopulaSR(findClaimObj(l,t,object@claimobjs)@ssrCopula)$outm, file = file, innerBorder=0.5, row.names=FALSE)
											}
											if(findClaimObj(l,t,object@claimobjs)@laeDevFac@FacModel==TRUE){
												HTML(paste0("LAE: ",findClaimObj(l,t,object@claimobjs)@laeDevFac@FacID," ",toString(findClaimObj(l,t,object@claimobjs)@laeDevFac)),file=file)
											} else {
												HTML(paste0("LAE Development Schedule: ",findClaimObj(l,t,object@claimobjs)@laeDevFac@FacID),file=file)
												HTML(toString(findClaimObj(l,t,object@claimobjs)@laeDevFac), file = file, innerBorder=0.5, row.names=TRUE)										
											}
											png(filename = paste0("sim",object@tag,l,t,c,"idx.png"))
											idxPlot(findClaimObj(l,t,object@claimobjs))
											dev.off()
											HTMLInsertGraph(paste0("sim",object@tag,l,t,c,"idx.png"),Caption = "",file = file)
											HTML(paste0("Deductible Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@deductible)),file=file)
											HTML(paste0("Limit Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@limit)),file=file)
											png(filename = paste0("sim",object@tag,l,t,c,"dl.png"),width=480,height=240)
											dlPlot(findClaimObj(l,t,object@claimobjs))
											dev.off()
											HTMLInsertGraph(paste0("sim",object@tag,l,t,c,"dl.png"),Caption = "",file = file)
										}
									}else if (c=="UPR" & haveData(l,t,"IBNR")==FALSE){ #sum("IBNR"==classes)==0 &
										if(!is.null(findClaimObj(l,t,object@claimobjs))){
											HTML(paste0("Frequency Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@frequency)),file=file)
											HTML(paste0("Severity Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@severity)),file=file)
											HTML(paste0("Report Lag Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@reportLag)),file=file)
											HTML(paste0("Settlement Lag Distribution: ",toString(findClaimObj(l,t,object@claimobjs)@settlementLag)),file=file)
											png(filename = paste0("sim",object@tag,l,t,c,"dist.png"))
											distPlot(findClaimObj(l,t,object@claimobjs))
											dev.off()
											HTMLInsertGraph(paste0("sim",object@tag,l,t,c,"dist.png"),Caption = "",file = file)
											HTML(paste0("Severity Index: ",toString(findClaimObj(l,t,object@claimobjs)@severityIndex)),file=file)
											HTML(paste0("Exposure Index: ",toString(findClaimObj(l,t,object@claimobjs)@exposureIndex)),file=file)
											HTML(paste0("Correlation between Severity, settlement Lag and Report Lag: "),file=file)
											if(findClaimObj(l,t,object@claimobjs)@iCopula==FALSE){
												HTML("Severity, settlement Lag and Report Lag are assumed to be independent.", file = file)
											} else {
												HTML(paste0("Severity, settlement Lag and Report Lag are assumed to follow ",getCopulaSR(findClaimObj(l,t,object@claimobjs)@ssrCopula)$out), file = file)
												HTML(getCopulaSR(findClaimObj(l,t,object@claimobjs)@ssrCopula)$outm, file = file, innerBorder=0.5, row.names=FALSE)
											}
											if(findClaimObj(l,t,object@claimobjs)@laeDevFac@FacModel==TRUE){
												HTML(paste0("LAE: ",findClaimObj(l,t,object@claimobjs)@laeDevFac@FacID," ",toString(findClaimObj(l,t,object@claimobjs)@laeDevFac)),file=file)
											} else {
												HTML(paste0("LAE Development Schedule: ",findClaimObj(l,t,object@claimobjs)@laeDevFac@FacID),file=file)
												HTML(toString(findClaimObj(l,t,object@claimobjs)@laeDevFac), file = file, innerBorder=0.5, row.names=TRUE)										
											}
											png(filename = paste0("sim",object@tag,l,t,c,"idx.png"))
											idxPlot(findClaimObj(l,t,object@claimobjs))
											dev.off()
											HTMLInsertGraph(paste0("sim",object@tag,l,t,c,"idx.png"),Caption = "",file = file)
										}
									} else {
										HTML(paste0("Same as assumptions used for IBNR."),file=file)														
									}
								}
								HTMLhr(file = file)						
							}
						}
					}
				}
				cat(paste("Report generated: ",file," at ",date(), "\n",sep = ""))
			}
			
			Report <- function(file) {
				ReportBegin(file,"Claim Simulation Report")
				if(copied == TRUE | file.exists("report.css")) {
					HTMLCSS(file = file, CSSfile = "report.css")
				}
				HTMLcontent(file)
				ReportEnd(file)
			}
			
			options("R2HTML.format.big.mark"=",")
			options("R2HTML.format.decimal.mark"=".")
			Report(paste0(htmlfile,".html"))
			setwd("..")
		} else {
			stop("Set iReport of simulation object to TRUE to generate html report.")
		}
	}, error = function(err){
		print(paste0(">>>Critical Error for generating claim simulation report: ",err))
		gc()
	})
})
