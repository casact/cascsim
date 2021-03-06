# From Type.R
#' @examples
#' library(cascsim)
#' data(claimdata)
#' #RBNER simulation
#' claimobj <- new("ClaimType", line="Auto",claimType="N",iRBNER=TRUE,iROPEN=FALSE,
#' iIBNR=FALSE,iUPR=FALSE)
#' rbnerdata <- claimSample(claimobj,claimdata)
#' #claim reopen simulation
#' #claimobj <- new("ClaimType", line="Auto",claimType="N",iRBNER=FALSE,iROPEN=TRUE,
#' #iIBNR=FALSE,iUPR=FALSE)
#' #reopendata <- claimSample(claimobj,claimdata)
#' #na.omit(reopendata)
#' #IBNR simulation
#' claimobj <- new("ClaimType", line="Auto",claimType="N",iRBNER=FALSE,iROPEN=FALSE,
#' iIBNR=TRUE,iUPR=FALSE,
#' IBNRfreqIndex=new("Index",startDate=as.Date("2016-01-01"),
#' monthlyIndex=rep(30,12)),iCopula=TRUE)
#' ibnrdata <- claimSample(claimobj,claimdata)
#' #UPR simulation
#' #claimobj <- new("ClaimType", line="Auto",claimType="N",
#' #iRBNER=FALSE,iROPEN=FALSE,iIBNR=FALSE,iUPR=TRUE,
#' #UPRfreqIndex=new("Index",startDate=as.Date("2017-01-01"),
#' #monthlyIndex=rep(30,12)),iCopula=TRUE)
#' #uprdata <- claimSample(claimobj,claimdata)

# From Sim.R
#' @examples
#' library(cascsim)
#' data(claimdata)
#' lines<-c("Auto","Property","Liab")
#' types<-c("N","H")
#' #exposure index
#' index1 <- new("Index",monthlyIndex=c(rep(1,11),cumprod(c(1,rep(1.5^(1/12),11))),
#' cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),rep(1.4,301)))
#' #severity index
#' index2 <- new("Index",monthlyIndex=c(cumprod(c(1,rep(1.03^(1/12),59))),rep(1.03^(5),300)))
#' objan <- new("ClaimType", line="Auto",claimType="N",exposureIndex=index1,severityIndex=index2)
#' objah <- new("ClaimType", line="Auto",claimType="H",exposureIndex=index1,severityIndex=index2)
#' objpn <- new("ClaimType", line="Property",claimType="N",exposureIndex=index1,severityIndex=index2)
#' objph <- new("ClaimType", line="Property",claimType="H",exposureIndex=index1,severityIndex=index2)
#' objln <- new("ClaimType", line="Liab",claimType="N",exposureIndex=index1,severityIndex=index2)
#' objlh <- new("ClaimType", line="Liab",claimType="H",exposureIndex=index1,severityIndex=index2)
#' objlist <- list(objan,objah,objpn,objph,objln,objlh)
#' simobj <- new("Simulation",lines=lines,types=types,claimobjs=objlist,iFit=TRUE,
#' iCopula=TRUE, iReport=TRUE)
#' simobj <- claimFitting(simobj,claimdata)

#' @examples
#' library(cascsim)
#' data(claimdata)
#' lines<-c("Auto","Property","Liab")
#' types<-c("N","H")
#' #exposure index
#' index1 <- new("Index",monthlyIndex=c(rep(1,11),cumprod(c(1,rep(1.5^(1/12),11))),
#' cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),rep(1.4,301)))
#' #severity index
#' index2 <- new("Index",monthlyIndex=c(cumprod(c(1,rep(1.03^(1/12),59))),rep(1.03^(5),300)))
#' objan <- new("ClaimType", line="Auto",claimType="N",exposureIndex=index1,severityIndex=index2)
#' objah <- new("ClaimType", line="Auto",claimType="H",exposureIndex=index1,severityIndex=index2)
#' objpn <- new("ClaimType", line="Property",claimType="N",exposureIndex=index1,severityIndex=index2)
#' objph <- new("ClaimType", line="Property",claimType="H",exposureIndex=index1,severityIndex=index2)
#' objln <- new("ClaimType", line="Liab",claimType="N",exposureIndex=index1,severityIndex=index2)
#' objlh <- new("ClaimType", line="Liab",claimType="H",exposureIndex=index1,severityIndex=index2)
#' objlist <- list(objan,objah,objpn,objph,objln,objlh)
#' simobj <- new("Simulation",lines=lines,types=types,claimobjs=objlist,iFit=TRUE,
#' iCopula=TRUE, iReport=TRUE)
#' simobj <- claimFitting(simobj,claimdata)

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
#' simobj <- new("Simulation",lines=lines,types=types,iRBNER=FALSE,iROPEN=FALSE,iIBNR=TRUE,
#' iUPR=FALSE,claimobjs=objlist,simNo=2)
#' simdata <- claimSimulation(simobj)
#' simSummary <- simSummary(simobj,simdata)
#' simobj@iReport <- TRUE
#' simReport(simobj,simSummary)
#'
#' #IBNR and UPR Only
#' #library(cascsim)
#' #data(claimdata)
#' #lines<-c("Auto","Property","Liab")
#' #types<-c("N","H")
#' #objan <- new("ClaimType", line="Auto",claimType="N")
#' #objah <- new("ClaimType", line="Auto",claimType="H")
#' #objpn <- new("ClaimType", line="Property",claimType="N")
#' #objph <- new("ClaimType", line="Property",claimType="H")
#' #objln <- new("ClaimType", line="Liab",claimType="N")
#' #objlh <- new("ClaimType", line="Liab",claimType="H")
#' #objlist <- list(objan,objah,objpn,objph,objln,objlh)
#' #simobj <- new("Simulation",lines=lines,types=types,iRBNER=FALSE,iROPEN=FALSE,iIBNR=TRUE,
#' #iUPR=TRUE,claimobjs=objlist,simNo=2)
#' #simdata <- claimSimulation(simobj)
#' #simSummary <- simSummary(simobj,simdata)
#' #simobj@iReport <- TRUE
#' #simReport(simobj,simSummary)
#'
#' #All four claim classes: RBNER, ROPEN, IBNR and UPR
#' #library(cascsim)
#' #data(claimdata)
#' #lines<-c("Auto","Property","Liab")
#' #types<-c("N","H")
#' #objan <- new("ClaimType", line="Auto",claimType="N")
#' #objah <- new("ClaimType", line="Auto",claimType="H")
#' #objpn <- new("ClaimType", line="Property",claimType="N")
#' #objph <- new("ClaimType", line="Property",claimType="H")
#' #objln <- new("ClaimType", line="Liab",claimType="N")
#' #objlh <- new("ClaimType", line="Liab",claimType="H")
#' #objlist <- list(objan,objah,objpn,objph,objln,objlh)
#' #simobj <- new("Simulation",lines=lines,types=types,iRBNER=TRUE,iROPEN=TRUE,iIBNR=TRUE,
#' #iUPR=TRUE,claimobjs=objlist,simNo=2)
#' #simdata <- claimSimulation(simobj,claimData=claimdata)
#' #simSummary <- simSummary(simobj,simdata)
#' #simTriangle <- simTriangle(simobj,claimdata,simdata)
#' #simobj@iReport <- TRUE
#' #simReport(simobj,simSummary,simTriangle)
#' #multicore computing
#' #simobj@ncores<-4
#' #simobj@simNo<-2
#' #simdata <- claimSimulation(simobj,claimdata)
#' #simSummary <- simSummary(simobj,simdata)
#' #simTriangle <- simTriangle(simobj,claimdata,simdata)
#' #simobj@iReport <- TRUE
#' #simReport(simobj,simSummary)

# From fitDist.R

#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting example 1
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
#'
#' #frequecy fitting example 2
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Liab" & 
#' claimdata[,"Type"]=="N"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = as.Date("2012-01-01"),
#' method="qme",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit@probs <- c(0.5,0.9)
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("NegativeBinomial",p1=20,p2=0.5)
#' xFit@soutput
#' observationPlot(xFit)
#' fitPlot(xFit)
#'
#' #severity fitting example 1
#' sindex <- new("Index", startDate = as.Date("2012-01-01"), 
#' monthlyIndex = cumprod(c(1,rep(1.03^(1/12),59))))
#' sdate <- as.Date("2012-01-01")
#' rawdata <- as.data.frame(claimdata[(claimdata[,"LoB"]=="Liab" & claimdata[,"Type"]=="H" & 
#' claimdata[,"status"]=="Closed" & as.Date(claimdata[,"occurrenceDate"])>=sdate),][,c(9,7)])
#' xFit <- new("FitDist", observation=rawdata, trend=sindex,startDate = sdate,method="qme",
#' probs=c(0.7,0.5),ifreq=FALSE)
#' #xFit <- new("FitDist", observation=rawdata, trend=sindex,
#' #startDate = sdate,method="mme",ifreq=FALSE)
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Pareto",p1=500,p2=2.5)
#' xFit@soutput
#' observationPlot(xFit)
#' fitPlot(xFit)
#'
#' #severity fitting example 2 Creating empirical distribution
#' sindex <- new("Index", startDate = as.Date("2012-01-01"), 
#' monthlyIndex = cumprod(c(1,rep(1.03^(1/12),59))))
#' sdate <- as.Date("2012-01-01")
#' rawdata <- as.data.frame(claimdata[(claimdata[,"LoB"]=="Liab" & 
#' claimdata[,"Type"]=="H" & claimdata[,"status"]=="Closed" & 
#' as.Date(claimdata[,"occurrenceDate"])>=sdate),][,c(9,7)])
#' xFit <- new("FitDist", observation=rawdata, trend=sindex, ifreq=FALSE)
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Empirical")
#' xFit@soutput
#' xFit@fitted@empirical
#' observationPlot(xFit)
#' fitPlot(xFit)
#'
#' #report lag fitting
#' lindex <- new("Index", startDate = as.Date("2012-01-01"), monthlyIndex = rep(1,360))
#' rawdata <- as.data.frame(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),][,c(5,6)])
#' rawdata <- as.numeric(as.Date(rawdata$reportDate)-as.Date(rawdata$occurrenceDate))
#' rawdata <- as.data.frame(rawdata)
#' xFit <- new("FitDist", observation=rawdata, trend=lindex,
#' startDate = as.Date("2012-01-01"),method="mle",ifreq=FALSE)
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Exponential",p1=1)
#' xFit@soutput
#' fitPlot(xFit)
#' observationPlot(xFit)
#' observationPlot(xFit)
#' fitPlot(xFit)


#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, 
#' monthlyIndex = c(rep(1,11),cumprod(c(1,rep(1.5^(1/12),11))),
#' cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,startDate = 
#' as.Date("2012-01-01"),method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Poisson")
#' xFit@soutput
#' fitPlot(xFit)


#' @examples
#' library(cascsim)
#' data(claimdata)
#'
#' #frequecy fitting
#' findex <- new("Index", startDate = as.Date("2012-01-01"), tabulate=TRUE, 
#' monthlyIndex = c(rep(1,11),cumprod(c(1,rep(1.5^(1/12),11))),
#' cumprod(c(1.5,rep((1.3/1.5)^(1/12),11))),
#' cumprod(c(1.3,rep((1.35/1.3)^(1/12),11))),cumprod(c(1.35,rep((1.4/1.35)^(1/12),11))),1.4))
#' rawdata <- as.data.frame(as.Date(claimdata[(claimdata[,"LoB"]=="Auto" & 
#' claimdata[,"Type"]=="H"),]$occurrenceDate))
#' colnames(rawdata)<-"occurrenceDate"
#' xFit <- new("FitDist", observation=rawdata, trend=findex,
#' startDate = as.Date("2012-01-01"),method="mle",ifreq=TRUE,idate=TRUE, freq="Monthly")
#' xFit <- setFitdata(xFit)
#' setTrialDist(xFit) <- new("Poisson")
#' xFit@soutput
#' observationPlot(xFit)
