% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recursive-test.R
\name{recursive.test}
\alias{recursive.test}
\title{Recursive Multivariate Testing}
\usage{
recursive.test(
  dend,
  df,
  cateVar = NULL,
  ordinalVar = NULL,
  cohortid.var = "cohortid",
  alpha.level = 0.05,
  verbose = T,
  saveIntermediate = F,
  BG.method = "asymptotic",
  n_perm = 200,
  N_auto = 50,
  impute = T,
  miceArgs = list(method = "mean", maxit = 1)
)
}
\arguments{
\item{dend}{dendrogram object from the \code{stat::as.as.dendrogram()}}

\item{df}{data.frame}

\item{cateVar}{string or vector, names of variables are categorical, which will be converted into dummy variables}

\item{ordinalVar}{string or vector, names of variables are ordinal factors, which will be converted into numeric variables}

\item{cohortid.var}{string, name of the cohort id indicator in the \code{relate::df}}

\item{alpha.level}{numeric, alpha level for statistical significance of the BG test}

\item{verbose}{boolean, whether to print the intermediate test results at each round}

\item{saveIntermediate}{boolean, whether to save the intermediate test result in the final output}

\item{BG.method}{string, "asymptotic","permutation","automatic", see \code{relate::BGcompare} above}

\item{n_perm}{number of permutation if method is permutation}

\item{N_auto}{integer, the sample size for automatically choosing between asymptotic and permutation, below is permutation, above is asymptotic}

\item{impute}{boolean, whether to conduct MICE imputation within clusters before testing}

\item{miceArgs}{list, a list of arguments parse into \code{mice::mice}}
}
\description{
This recursion function conducts the Biswas and Ghosh multivariate test
based on the dendrogram from the hierarchical clustering of the average distance matrix of the unsupervised random forest.
}
