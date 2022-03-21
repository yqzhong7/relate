#' Simulated Dataset with three clusters
#'
#' Simulated multivariate continuous samples from three cohort clusters (1-3 ,4-6, 7-10) with different mean vectors.
#' Each cohort has 100 observations, 15 simuolated covariates, 1 cohort id indicator, and 1 true cluster (distribution) indicator.
#' Four cohorts have two complete missing variables. Cohort 3: X3 and X10, Cohort 5: X4 and X12, Cohort 7: X9 & X11, Cohort 9: X3 & X5
#'
#'
#' @docType data
#'
#' @usage data(cohort_na_df)
#'
#' @format An object of class data.frame with 1000 rows and 17 columns:
#' \describe{
#'   \item{X1-X15}{numeric, simulated continuous variables from multivariate joint distributions}
#'   \item{cohortid}{numeric, cohort id}
#'   \item{distribution}{numeric, true distribution indicator}
#' }
"cohort_na_df"
