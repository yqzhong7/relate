#' @title Create average distance matrix
#'
#' @description Convert individual-level distance matrix to average distance matrix at cohort level
#'
#' @param dist_mat distance matrix
#' @param cohortid.vec a numeric vector of cohort/group id for all observations e.g.,[rep(1:4,each=100)]
#'
#' @export
avg_dist_func <- function(dist_mat, cohortid.vec){
  #change matrix row and column names
  colnames(dist_mat) <- rownames(dist_mat) <- cohortid.vec

  #convert to average distance matrix
  dist_df <- as.data.frame(as.table(dist_mat)) %>%
    dplyr::filter(!is.na(Freq)) %>%
    dplyr::rename(X = Var1, Y = Var2, Dist= Freq) %>%
    dplyr::group_by(X,Y) %>%
    dplyr::summarise(avg_dist = mean(Dist)) %>%
    tidyr::pivot_wider(names_from = Y, values_from = avg_dist) %>%
    tibble::column_to_rownames("X") %>%
    as.matrix()

  return(dist_df)
}


#' @title Multiple imputation by group
#'
#' @description Multiple imputation using `mice::mice` by group
#'
#' @param df data.frame for MICE imputation
#' @param clusterid  string, the name of cohort id indicator in the dataframe
#' @param miceArg a list of arguments parse into `mice::mice`
#'
#' @export
mice_group_impute <- function(df, clusterid, miceArg = list(method = 'mean', maxit = 1), ...){
  group_imp <- df %>%
    dplyr::group_by(vars(clusterid)) %>%
    dplyr::group_map(~mice::complete(do.call(mice::mice, args= append(list(data=.x),miceArg)))) %>%
    dplyr::bind_rows() %>%
    dplyr::ungroup()
  return(group_imp)
}


#' @title Wrapper function of BG test
#'
#' @description This is a wrapper function of Biswas and Ghosh multivariate test implemented in `SHT::eqdist.2014BG`
#'
#' @param X,Y two dataframes for comparisons
#' @param BG.method  string, "asymptotic","permutation","automatic"
#' @param .n_perm number of permuation if method is permutation
#' @param N_auto integer, the sample size for automatically choosing between asymptotic and permutation, below is permutation, above is asymptotic
#'
#' @export
#' @noRd
BGcompare <- function(X=cohort1,Y=cohort2, BG.method = c("asymptotic","permutation","automatic"),
                      .n_perm=n_perm, N_auto = N_auto){

  if (BG.method == "automatic"){
    if (nrow(X) <=N_auto | nrow(Y) <= N_auto){
      BG.method = "permutation"
    } else {
      BG.method = "asymptotic"
    }
  }
  # need to add an output for the method used
  if (BG.method == "asymptotic"){
    return(SHT::eqdist.2014BG(X,Y,BG.method)$p.value)
  } else if (BG.method == "permutation"){
    return(SHT::eqdist.2014BG(X,Y,BG.method,nreps = .n_perm)$p.value)
  }
}

