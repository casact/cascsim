#' Sample Claim Data
#'
#' A dataset containing about 10,000 simulated claim records from 2012 to 2016 for illustration.
#' The variables are as follows:
#'
#' \itemize{
#'   \item ClaimID. Claim ID
#'   \item LoB. Line of Business (Auto, Liab, Property)
#'   \item Type. Claim Type (N: Normal, H: High)
#'   \item status. Current Claim Status (Closed, Open)
#'   \item occurrenceDate. Claim Occurrence Date
#'   \item reportDate. Claim Report Date
#'   \item incurredLoss. Incurred Loss. For closed claim, it is the ultimate loss. For open claim, it is the estimated or booked loss.
#'   \item osRatio. Outstanding Ratio
#'   \item settlementDate. Claim Settlement Date.
#'   \item Paid. Paid Loss by the valuation date. It equals incurredLoss * (1-osRatio)
#'   \item totalLoss. Total loss before deductible and limit. If not available, it will be set as incurredLoss and not used for fitting.
#'   \item Deductible. Deductible applied to the claim.
#'   \item Limit. Limit applied to the claim.
#'   \item LAE. Loss adjustment expense at the claim level. It can be omitted if idemnity and LAE are modeled together as incurred loss.
#'   \item claimLiability. Indicating whether the claim is invalid and leads to zero payment. It excludes valid claims that are smaller than deductibles.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name claimdata
#' @usage data(claimdata)
#' @format A data frame with 10030 rows and 15 variables
NULL
