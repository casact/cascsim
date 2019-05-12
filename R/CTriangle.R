#' An S4 class to represent a triangle or rectangle object.
#'
#' @slot triID A character string to identify the triangle object.
#' @slot type A character string that indicates the triangle type, such as reportedCount, closedCount, paidLoss, and incurredLoss.
#' @slot startDate The start date for the accident year or Quarter.
#' @slot frequency A character that indicates the frequency of the triangle, "yearly" or "quarterly".
#' @slot sim A number that indicates the simulation number used to complete the rectangle. Zero means using the average value.
#' @slot percentile A number that indicates the percentile used to complete the rectangle. It is only used when sim is NA.
#' @slot iRBNER A Boolean that indicates whether open claims are simulated. If not, current information will be used for constructing rectangles. Otherwise, simulated data will be used.
#' @slot iROPEN A Boolean that indicates whether claim reopen are simulated. If not, current information will be used for constructing rectangles. Otherwise, simulated data will be used.
#' @slot percentile A number that indicates the percentile used to complete the rectangle. It is only used when sim is NA.
#' @slot upper A matrix that contains the upper triangle based on claim data.
#' @slot upperkeep A matrix that contains the upper triangle that are not simulated. It will be used to construct the rectangle for the non-simulated part.
#' @slot rectangle A matrix that contains the entire rectangle based on simulation data.

setClass("Triangle",
	slots=c(
			triID="character",
			type="character",
			startDate="Date",
			frequency="character",
			sim="numeric",
			percentile="numeric",
			iRBNER="logical",
			iROPEN="logical",
			upper="matrix",
			upperkeep="matrix",
			rectangle="matrix"
	),
	prototype=list(
			triID="XXXXXX",
			type="reportedCount",
			startDate=as.Date("2012-01-01"),
			frequency="yearly",
			sim=1,
			percentile=50,
			iRBNER=TRUE,
			iROPEN=TRUE,
			upper=matrix(),
			upperkeep=matrix(),
			rectangle=matrix()
	)
)

#' @title Set up the upper triangle based on claim data.
#' @description
#' \code{setUpperTriangle} sets up the upper triangle based on a data file.
#' @name setUpperTriangle
#' @param object Triangle Object
#' @param data Claim Data
#' @param ... Additional function arguments.
#' @examples
#' library(cascsim)
#' data(claimdata)
#' xTri <- new("Triangle", triID = "TRI1", type = "reportedCount", startDate=as.Date("2012-01-01"),
#' frequency="yearly", sim=1, percentile=50)
#' xTri<-setUpperTriangle(xTri,claimdata)
#' xTri@upper
#'
#' xTri <- new("Triangle", triID = "TRI1", type = "closedCount", startDate=as.Date("2012-01-01"),
#' frequency="quarterly", sim=1, percentile=50)
#' xTri<-setUpperTriangle(xTri,claimdata)
#' xTri@upper
#'
#' xTri <- new("Triangle", triID = "TRI1", type = "incurredLoss", startDate=as.Date("2012-01-01"),
#' frequency="yearly", sim=1, percentile=50)
#' xTri<-setUpperTriangle(xTri,claimdata,lob="Auto",ctype="H")
#' xTri@upper
#'
#' xTri <- new("Triangle", triID = "TRI1", type = "paidLoss", startDate=as.Date("2012-01-01"),
#' frequency="yearly", sim=1, percentile=50)
#' xTri<-setUpperTriangle(xTri,claimdata,lob="Auto",ctype="H")
#' xTri@upper
#'
#' @rdname setUpperTriangle-methods
#' @exportMethod setUpperTriangle
setGeneric("setUpperTriangle", function(object,data,...) standardGeneric("setUpperTriangle"))
#' @param evaluationDate Evaluation Date;
#' @param lob Line of Business;
#' @param ctype Claim Type.
#' @rdname setUpperTriangle-methods
#' @aliases setUpperTriangle,ANY-method
setMethod("setUpperTriangle",signature("Triangle","data.frame"), function(object,data,evaluationDate=as.Date("2016-12-31"),lob="Total",ctype="Total") {
	tryCatch({

		if (lob != "Total") {data <- data[data[,"LoB"]==lob,]}
		if (ctype != "Total") {data <- data[data[,"Type"]==ctype,]}

		startYear <- as.numeric(substr(as.character(object@startDate),1,4))
		endYear <- as.numeric(substr(as.character(evaluationDate),1,4))
		startMonth <- as.numeric(substr(as.character(object@startDate),6,7))
		endMonth <- as.numeric(substr(as.character(evaluationDate),6,7))
		startQuarter <- ceiling(startMonth/3)
		endQuarter <- ceiling(endMonth/3)

		if (object@frequency=="yearly") {
			ncols <- endYear - startYear + 1
			rowname <- c(startYear:endYear)
			colname <- paste0("M",seq(12,(12*ncols),12))
		} else {
			ncols <- (endYear - startYear)*4 + endQuarter - startQuarter + 1

			rowname <- vector()
			for (i in c(startYear:endYear)){
				if (i==startYear){
					for (j in c(startQuarter:4)){
						rowname <- c(rowname,paste0(j,"Q",i))
					}
				} else if (i == endYear){
					for (j in c(1:endQuarter)){
						rowname <- c(rowname,paste0(j,"Q",i))
					}
				} else {
					for (j in c(1:4)){
						rowname <- c(rowname,paste0(j,"Q",i))
					}
				}
			}

			colname <- paste0("M",seq(3,(3*ncols),3))
		}

		rec <- matrix(0,ncols,ncols,dimnames = list(rowname,colname))

		if ("Sim" %in% colnames(data)) {
			sim <- data$Sim[1]
			data <- data[data[,"Sim"]==sim,]
		}

		data <- data[,colnames(data) %in% c("occurrenceDate","reportDate","settlementDate","incurredLoss","Paid")]

		accidentYears <- as.numeric(substr(as.character(data$occurrenceDate),1,4))
		accidentMonths <- as.numeric(substr(as.character(data$occurrenceDate),6,7))
		accidentQuarters <- ceiling(accidentMonths/3)

		if (object@frequency=="yearly") {
			data$rowno <- (accidentYears - startYear) + 1
		} else {
			data$rowno <- (accidentYears - startYear) * 4 + (accidentQuarters - startQuarter) + 1
		}

		if (object@type == "reportedCount"){
			reportYears <- as.numeric(substr(as.character(data$reportDate),1,4))
			reportMonths <- as.numeric(substr(as.character(data$reportDate),6,7))
			reportQuarters <- ceiling(reportMonths/3)
				if (object@frequency=="yearly") {
					data$colno <- (reportYears - accidentYears) + 1
				} else {
					data$colno <- (reportYears - accidentYears) * 4 + (reportQuarters - accidentQuarters) + 1
				}
		} else if (object@type == "closedCount"){
			settleYears <- as.numeric(substr(as.character(data$settlementDate),1,4))
			settleYears <- ifelse(is.na(settleYears), endYear+1, settleYears)
			settleMonths <- as.numeric(substr(as.character(data$settlementDate),6,7))
			settleMonths <- ifelse(is.na(settleMonths), endMonth, settleMonths)
			settleQuarters <- ceiling(settleMonths/3)
				if (object@frequency=="yearly") {
					data$colno <- (settleYears - accidentYears) + 1
				} else {
					data$colno <- (settleYears - accidentYears) * 4 + (settleQuarters - accidentQuarters) + 1
				}
		}else {
			settleYears <- as.numeric(substr(as.character(data$settlementDate),1,4))
			settleYears <- ifelse(is.na(settleYears), endYear, settleYears)
			settleMonths <- as.numeric(substr(as.character(data$settlementDate),6,7))
			settleMonths <- ifelse(is.na(settleMonths), endMonth, settleMonths)
			settleQuarters <- ceiling(settleMonths/3)
				if (object@frequency=="yearly") {
					data$colno <- (settleYears - accidentYears) + 1
				} else {
					data$colno <- (settleYears - accidentYears) * 4 + (settleQuarters - accidentQuarters) + 1
				}
		}

		if (object@type == "reportedCount" | object@type == "closedCount") {
			agg<-aggregate(occurrenceDate ~ rowno + colno, data = data, length)
		} else if (object@type == "incurredLoss") {
			agg<-aggregate(incurredLoss ~ rowno + colno, data = data, sum)
		} else {
			agg<-aggregate(Paid ~ rowno + colno, data = data, sum)
		}

		for (i in c(1:length(rowname))){
			rowsum <- 0
			for (j in c(1:(ncols-i+1))){
				tmp <- agg[agg$rowno == i & agg$colno == j,][1,3]
				if (!is.na(tmp)) {
					rowsum <- rowsum + tmp
				}

				rec[i,j] <- rowsum
			}
		}


		object@upper <- rec
		gc()
		object
	}, error = function(err){
		message(paste0(">>>Critical Error: ", err))
		gc()
		return(-1)
	})

})

#' @title Set up the upper triangle for non-simulated data.
#' @description
#' \code{setUpperKeep} sets up the upper triangle for non-simulated data.
#' @name setUpperKeep
#' @param object Triangle Object
#' @param data Claim Data
#' @param ... Additional function arguments.
#' @examples
#' library(cascsim)
#' data(claimdata)
#' xTri <- new("Triangle", triID = "TRI1", type = "reportedCount", startDate=as.Date("2012-01-01"),
#' frequency="yearly", sim=1, percentile=50, iRBNER=TRUE, iROPEN=TRUE)
#' xTri<-setUpperTriangle(xTri,claimdata)
#' xTri<-setUpperKeep(xTri,claimdata)
#' xTri@upperkeep
#'
#' xTri <- new("Triangle", triID = "TRI1", type = "closedCount", startDate=as.Date("2012-01-01"),
#' frequency="quarterly", sim=1, percentile=50, iRBNER=FALSE, iROPEN=TRUE)
#' xTri<-setUpperTriangle(xTri,claimdata)
#' xTri<-setUpperKeep(xTri,claimdata)
#' xTri@upperkeep
#'
#' xTri <- new("Triangle", triID = "TRI1", type = "incurredLoss", startDate=as.Date("2012-01-01"),
#' frequency="yearly", sim=1, percentile=50, iRBNER=TRUE, iROPEN=FALSE)
#' xTri<-setUpperTriangle(xTri,claimdata)
#' xTri<-setUpperKeep(xTri,claimdata,lob="Auto",ctype="H")
#' xTri@upperkeep
#'
#' @rdname setUpperKeep-methods
#' @exportMethod setUpperKeep
setGeneric("setUpperKeep", function(object,data,...) standardGeneric("setUpperKeep"))
#' @param evaluationDate Evaluation Date;
#' @param lob Line of Business;
#' @param ctype Claim Type.
#' @rdname setUpperKeep-methods
#' @aliases setUpperKeep,ANY-method
setMethod("setUpperKeep",signature("Triangle","data.frame"), function(object,data,evaluationDate=as.Date("2016-12-31"),lob="Total",ctype="Total") {
	tryCatch({

		if (lob != "Total") {data <- data[data[,"LoB"]==lob,]}
		if (ctype != "Total") {data <- data[data[,"Type"]==ctype,]}
#		if(object@iRBNER == TRUE & object@iROPEN == FALSE){
#			data <- data[data[,"status"]=="CLOSED",]
#		} else if(object@iRBNER == FALSE & object@iROPEN == TRUE){
#			data <- data[data[,"status"]=="OPEN",]
#		}

		startYear <- as.numeric(substr(as.character(object@startDate),1,4))
		endYear <- as.numeric(substr(as.character(evaluationDate),1,4))
		startMonth <- as.numeric(substr(as.character(object@startDate),6,7))
		endMonth <- as.numeric(substr(as.character(evaluationDate),6,7))
		startQuarter <- ceiling(startMonth/3)
		endQuarter <- ceiling(endMonth/3)

		if (object@frequency=="yearly") {
			ncols <- endYear - startYear + 1
			rowname <- c(startYear:endYear)
			colname <- paste0("M",seq(12,(12*ncols),12))
		} else {
			ncols <- (endYear - startYear)*4 + endQuarter - startQuarter + 1

			rowname <- vector()
			for (i in c(startYear:endYear)){
				if (i==startYear){
					for (j in c(startQuarter:4)){
						rowname <- c(rowname,paste0(j,"Q",i))
					}
				} else if (i == endYear){
					for (j in c(1:endQuarter)){
						rowname <- c(rowname,paste0(j,"Q",i))
					}
				} else {
					for (j in c(1:4)){
						rowname <- c(rowname,paste0(j,"Q",i))
					}
				}
			}

			colname <- paste0("M",seq(3,(3*ncols),3))
		}

		rec <- matrix(0,ncols,ncols,dimnames = list(rowname,colname))

		if ("Sim" %in% colnames(data)) {
			sim <- data$Sim[1]
			data <- data[data[,"Sim"]==sim,]
		}

	#	if (object@iRBNER == FALSE & object@iROPEN == FALSE) {
	#		rec <- object@upper
		#} else if (object@iRBNER == TRUE & object@iROPEN == TRUE) {
		#
	#	} else {
			data <- data[,colnames(data) %in% c("occurrenceDate","reportDate","settlementDate","incurredLoss","Paid")]

			accidentYears <- as.numeric(substr(as.character(data$occurrenceDate),1,4))
			accidentMonths <- as.numeric(substr(as.character(data$occurrenceDate),6,7))
			accidentQuarters <- ceiling(accidentMonths/3)

			if (object@frequency=="yearly") {
				data$rowno <- (accidentYears - startYear) + 1
			} else {
				data$rowno <- (accidentYears - startYear) * 4 + (accidentQuarters - startQuarter) + 1
			}

			if (object@type == "reportedCount"){
				reportYears <- as.numeric(substr(as.character(data$reportDate),1,4))
				reportMonths <- as.numeric(substr(as.character(data$reportDate),6,7))
				reportQuarters <- ceiling(reportMonths/3)
					if (object@frequency=="yearly") {
						data$colno <- (reportYears - accidentYears) + 1
					} else {
						data$colno <- (reportYears - accidentYears) * 4 + (reportQuarters - accidentQuarters) + 1
					}
			} else if (object@type == "closedCount"){
				settleYears <- as.numeric(substr(as.character(data$settlementDate),1,4))
				settleYears <- ifelse(is.na(settleYears), endYear+1, settleYears)
				settleMonths <- as.numeric(substr(as.character(data$settlementDate),6,7))
				settleMonths <- ifelse(is.na(settleMonths), endMonth, settleMonths)
				settleQuarters <- ceiling(settleMonths/3)
					if (object@frequency=="yearly") {
						data$colno <- (settleYears - accidentYears) + 1
					} else {
						data$colno <- (settleYears - accidentYears) * 4 + (settleQuarters - accidentQuarters) + 1
					}
			} else {
				settleYears <- as.numeric(substr(as.character(data$settlementDate),1,4))
				settleYears <- ifelse(is.na(settleYears), endYear, settleYears)
				settleMonths <- as.numeric(substr(as.character(data$settlementDate),6,7))
				settleMonths <- ifelse(is.na(settleMonths), endMonth, settleMonths)
				settleQuarters <- ceiling(settleMonths/3)
					if (object@frequency=="yearly") {
						data$colno <- (settleYears - accidentYears) + 1
					} else {
						data$colno <- (settleYears - accidentYears) * 4 + (settleQuarters - accidentQuarters) + 1
					}
			}

			if (object@type == "reportedCount" | object@type == "closedCount") {
				agg<-aggregate(occurrenceDate ~ rowno + colno, data = data, length)
			} else if(object@type == "incurredLoss"){
				agg<-aggregate(incurredLoss ~ rowno + colno, data = data, sum)
			} else {
				agg<-aggregate(Paid ~ rowno + colno, data = data, sum)
			}

			for (i in c(1:length(rowname))){
				rowsum <- 0
				for (j in c(1:(ncols-i+1))){
					tmp <- agg[agg$rowno == i & agg$colno == j,][1,3]
					if (!is.na(tmp)) {
						rowsum <- rowsum + tmp
					}

					rec[i,j] <- rowsum
				}
			}
	#	}

		object@upperkeep <- rec
		gc()
		object
	}, error = function(err){
		message(paste0(">>>Critical Error: ", err))
		gc()
		return(-1)
	})

})

#' @title Set up the rectangle based on simulated data.
#' @description
#' \code{setRectangle} sets up the rectangle based on a data file.
#' @name setRectangle
#' @param object Triangle Object
#' @param data Simulated Data
#' @param ... Additional function arguments.
#' @rdname setRectangle-methods
setGeneric("setRectangle", function(object,data,...) standardGeneric("setRectangle"))
#' @param evaluationDate Evaluation Date;
#' @param futureDate End of projection date;
#' @param lob Line of Business;
#' @param ctype Claim Type.
#' @rdname setRectangle-methods
#' @aliases setRectangle,ANY-method
setMethod("setRectangle",signature("Triangle","data.frame"), function(object,data,evaluationDate=as.Date("2016-12-31"),futureDate=as.Date("2017-12-31"),lob="Total",ctype="Total") {
	tryCatch({

		if (lob != "Total") {data <- data[data[,"LoB"]==lob,]}
		if (ctype != "Total") {data <- data[data[,"Type"]==ctype,]}
		if (!is.nan(object@sim) & object@sim > 0) {
			data <- data[data[,"Sim"]==object@sim,]
		} else {
			ultAgg <- aggregate(ultimateLoss ~ Sim, data = data, sum)
			ultAgg <- ultAgg[order(ultAgg[,2]),]
			nsim <- nrow(ultAgg)
			lsim <- max(1,floor(nsim*object@percentile/100))
			usim <- max(1,ceiling(nsim*object@percentile/100))
			lambda <- usim - nsim*object@percentile/100
			lsim <- ultAgg[lsim,1]
			usim <- ultAgg[usim,1]
		}

		if (nrow(data)<=0){
			stop("No data available for rectangle construction.")
		} else {
			settleYears <- as.numeric(substr(as.character(data$settlementDate),1,4))
			resettleYears <- as.numeric(substr(as.character(data$resettleDate),1,4))
			settleMonths <- as.numeric(substr(as.character(data$settlementDate),6,7))
			resettleMonths <- as.numeric(substr(as.character(data$resettleDate),6,7))
			settleYears <- ifelse(is.na(resettleYears), settleYears, resettleYears)
			settleMonths <- ifelse(is.na(resettleMonths), settleMonths, resettleMonths)
			settleQuarters <- ceiling(settleMonths/3)

			startYear <- as.numeric(substr(as.character(object@startDate),1,4))
			endYear <- max(settleYears)
			evaluationYear <- as.numeric(substr(as.character(evaluationDate),1,4))
			futureYear <- as.numeric(substr(as.character(futureDate),1,4))
			startMonth <- as.numeric(substr(as.character(object@startDate),6,7))
			endMonth <- max(settleMonths)
			evaluationMonth <- as.numeric(substr(as.character(evaluationDate),6,7))
			futureMonth <- as.numeric(substr(as.character(futureDate),6,7))
			startQuarter <- ceiling(startMonth/3)
			endQuarter <- ceiling(endMonth/3)
			evaluationQuarter <- ceiling(evaluationMonth/3)
			futureQuarter <- ceiling(futureMonth/3)

			if (object@frequency=="yearly") {
				ncols <- endYear - startYear + 1
				nevals <- evaluationYear - startYear + 1
				nrows <- futureYear - startYear + 1
				rowname <- c(startYear:futureYear)
				colname <- paste0("M",seq(12,(12*ncols),12))
			} else {
				ncols <- (endYear - startYear)*4 + endQuarter - startQuarter + 1
				nevals <- (evaluationYear - startYear)*4 + evaluationQuarter - startQuarter	+ 1
				nrows <- (futureYear - startYear)*4 + futureQuarter - startQuarter	+ 1
				rowname <- vector()
				for (i in c(startYear:futureYear)){
					if (i==startYear){
						for (j in c(startQuarter:4)){
							rowname <- c(rowname,paste0(j,"Q",i))
						}
					} else if (i == futureYear){
						for (j in c(1:futureQuarter)){
							rowname <- c(rowname,paste0(j,"Q",i))
						}
					} else {
						for (j in c(1:4)){
							rowname <- c(rowname,paste0(j,"Q",i))
						}
					}
				}

				colname <- paste0("M",seq(3,(3*ncols),3))
			}

			rec <- matrix(0,nrows,ncols,dimnames = list(rowname,colname))
			nsims <- 1
			if (!is.nan(object@sim)) {
				if (object@sim > 0) {
					data <- data[data[,"Sim"]==object@sim,]
					nsims <- 1
				} else {
					nsims <- length(unique(data[,"Sim"]))
				}

				data <- data[,colnames(data) %in% c("occurrenceDate","reportDate","settlementDate","ultimateLoss","Paid")]

				accidentYears <- as.numeric(substr(as.character(data$occurrenceDate),1,4))
				accidentMonths <- as.numeric(substr(as.character(data$occurrenceDate),6,7))
				accidentQuarters <- ceiling(accidentMonths/3)

				if (object@frequency=="yearly") {
					data$rowno <- (accidentYears - startYear) + 1
				} else {
					data$rowno <- (accidentYears - startYear) * 4 + (accidentQuarters - startQuarter) + 1
				}

				if (object@type == "reportedCount"){
					reportYears <- as.numeric(substr(as.character(data$reportDate),1,4))
					reportMonths <- as.numeric(substr(as.character(data$reportDate),6,7))
					reportQuarters <- ceiling(reportMonths/3)
						if (object@frequency=="yearly") {
							data$colno <- (reportYears - accidentYears) + 1
						} else {
							data$colno <- (reportYears - accidentYears) * 4 + (reportQuarters - accidentQuarters) + 1
						}
				} else {#if (object@type == "settledCount"){
					settleYears <- as.numeric(substr(as.character(data$settlementDate),1,4))
					settleYears <- ifelse(is.na(settleYears), endYear, settleYears)
					settleMonths <- as.numeric(substr(as.character(data$settlementDate),6,7))
					settleMonths <- ifelse(is.na(settleMonths), endMonth, settleMonths)
					settleQuarters <- ceiling(settleMonths/3)
						if (object@frequency=="yearly") {
							data$colno <- (settleYears - accidentYears) + 1
						} else {
							data$colno <- (settleYears - accidentYears) * 4 + (settleQuarters - accidentQuarters) + 1
						}
				}

				if (object@type == "reportedCount" | object@type == "closedCount") {
					agg<-aggregate(occurrenceDate ~ rowno + colno, data = data, length)
				} else {
					agg<-aggregate(ultimateLoss ~ rowno + colno, data = data, sum)
				}

				for (i in c(1:length(rowname))){
					rowsum <- 0
					for (j in c(1:length(colname))){
						tmp <- agg[agg$rowno == i & agg$colno == j,][1,3]
						if (!is.na(tmp)) {
							rowsum <- rowsum + tmp
						}
						rec[i,j] <- rowsum
					}
				}

				for (i in c(1:length(rowname))){
					for (j in c(1:length(colname))){
						rec[i,j] <- rec[i,j]/nsims
					}
				}

				if(any(is.na(object@upperkeep))==FALSE){
					for (i in c(1:length(rowname))){
						if(i <= nrow(object@upperkeep)){
							addtmp <- object@upperkeep[i,ncol(object@upperkeep)-i+1]-rec[i,ncol(object@upperkeep)-i+1]
						} else {
							addtmp <- 0
						}
						for (j in c(1:length(colname))){
							rec[i,j] <- rec[i,j] + addtmp
						}
					}
				}
			} else {

				data <- data[data[,"Sim"]==lsim | data[,"Sim"]==usim,]
				data <- data[,colnames(data) %in% c("occurrenceDate","reportDate","settlementDate","ultimateLoss","Sim")]

				accidentYears <- as.numeric(substr(as.character(data$occurrenceDate),1,4))
				accidentMonths <- as.numeric(substr(as.character(data$occurrenceDate),6,7))
				accidentQuarters <- ceiling(accidentMonths/3)

				if (object@frequency=="yearly") {
					data$rowno <- (accidentYears - startYear) + 1
				} else {
					data$rowno <- (accidentYears - startYear) * 4 + (accidentQuarters - startQuarter) + 1
				}

				if (object@type == "reportedCount"){
					reportYears <- as.numeric(substr(as.character(data$reportDate),1,4))
					reportMonths <- as.numeric(substr(as.character(data$reportDate),6,7))
					reportQuarters <- ceiling(reportMonths/3)
						if (object@frequency=="yearly") {
							data$colno <- (reportYears - accidentYears) + 1
						} else {
							data$colno <- (reportYears - accidentYears) * 4 + (reportQuarters - accidentQuarters) + 1
						}
				} else {#if (object@type == "settledCount"){
					settleYears <- as.numeric(substr(as.character(data$settlementDate),1,4))
					settleYears <- ifelse(is.na(settleYears), endYear, settleYears)
					settleMonths <- as.numeric(substr(as.character(data$settlementDate),6,7))
					settleMonths <- ifelse(is.na(settleMonths), endMonth, settleMonths)
					settleQuarters <- ceiling(settleMonths/3)
						if (object@frequency=="yearly") {
							data$colno <- (settleYears - accidentYears) + 1
						} else {
							data$colno <- (settleYears - accidentYears) * 4 + (settleQuarters - accidentQuarters) + 1
						}
				}

				if (object@type == "reportedCount" | object@type == "closedCount") {
					lagg<-aggregate(occurrenceDate ~ rowno + colno, data = data[data[,"Sim"]==lsim,], length)
					uagg<-aggregate(occurrenceDate ~ rowno + colno, data = data[data[,"Sim"]==usim,], length)
				} else {
					lagg<-aggregate(ultimateLoss ~ rowno + colno, data = data[data[,"Sim"]==lsim,], sum)
					uagg<-aggregate(ultimateLoss ~ rowno + colno, data = data[data[,"Sim"]==usim,], sum)
				}

				for (i in c(1:length(rowname))){
					rowsum <- 0
					for (j in c(1:length(colname))){
						ltmp <- lagg[lagg$rowno == i & lagg$colno == j,][1,3]
						utmp <- uagg[uagg$rowno == i & uagg$colno == j,][1,3]
						tmp <- 0
						if(!is.na(ltmp)){tmp<-lambda*ltmp}
						if(!is.na(utmp)){tmp<-tmp+(1-lambda)*utmp}
						if (!is.na(tmp)) {
							rowsum <- rowsum + tmp
						}
						rec[i,j] <- rowsum
					}
				}

				if(any(is.na(object@upperkeep))==FALSE){
					for (i in c(1:length(rowname))){
						if(i <= nrow(object@upperkeep)){
							addtmp <- object@upperkeep[i,ncol(object@upperkeep)-i+1]-rec[i,ncol(object@upperkeep)-i+1]
						} else {
							addtmp <- 0
						}
						for (j in c(1:length(colname))){
							rec[i,j] <- rec[i,j] + addtmp
						}
					}
				}
			}


			if(any(is.na(object@upperkeep))==FALSE){
				for (i in c(1:min(nrows,nrow(object@upper)))){
					for (j in c(1:(min(ncol(object@upper),ncols)-i+1))){
						rec[i,j] <- object@upper[i,j]
					}
				}
			}
			object@rectangle <- rec
			gc()
			object
		}
	}, error = function(err){
		message(paste0(">>>Critical Error: ", err))
		gc()
		return(-1)
	})

})

setMethod("toString",signature("Triangle"), function(object) {
	return(object@rectangle)
})
