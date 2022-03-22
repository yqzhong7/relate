#' @title Create average distance matrix
#'
#' @description Convert individual-level distance matrix to average distance matrix at cohort level
#'
#' @param dist_mat distance matrix
#' @param cohortid.vec a numeric vector of cohort/group id for all observations e.g., `rep(1:4,each=100)`
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @export
avg_dist_func <- function(dist_mat, cohortid.vec){
  #change matrix row and column names
  colnames(dist_mat) <- rownames(dist_mat) <- cohortid.vec

  #convert to average distance matrix
  dist_df <- as.data.frame(as.table(dist_mat)) %>%
    dplyr::filter(!is.na(.data$Freq)) %>%
    dplyr::rename(X = .data$Var1, Y = .data$Var2, Dist= .data$Freq) %>%
    dplyr::group_by(.data$X,.data$Y) %>%
    dplyr::summarise(avg_dist = mean(.data$Dist)) %>%
    tidyr::pivot_wider(names_from = .data$Y, values_from = .data$avg_dist) %>%
    tibble::column_to_rownames("X") %>%
    as.matrix()

  return(dist_df)
}

#' @title Examine missing proportion by group
#'
#' @description A wrapper function for examining missingness level by cohort/cluster
#'
#' @param df data.frame
#' @param clusterid  string, the name of cohort id indicator in the dataframe
#'
#' @importFrom magrittr "%>%"
#' @export
find_na_level <- function(df, clusterid){
  df <- df %>%
    dplyr::group_by(dplyr::across(clusterid)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(is.na(.x)/dplyr::n())))

  colnames(df)[1] <- "ClusterID"
  return(df)
}

#' @title Multiple imputation by cohort/cluster
#'
#' @description Multiple imputation using `mice::mice` by cohort/cluster
#'
#' @param df data.frame for MICE imputation
#' @param clusterid  string, the name of cohort id indicator in the dataframe
#' @param imputeLevel binary, 0 (no imputation) and 1 (impute everything)
#' @param miceArg a list of arguments parse into `mice::mice`
#'
#' @importFrom magrittr "%>%"
#' @export
mice_group_impute <- function(df, clusterid, imputeLevel = 1, miceArg = list(method = 'mean', maxit = 1)){
  #--no imputation--#
  if (imputeLevel==0){
    return(df)
  }

  #--impute everything--#
  if (imputeLevel==1){
    group_imp <- df %>%
      dplyr::group_by(dplyr::all_of(clusterid)) %>%
      dplyr::group_map(~mice::complete(do.call(mice::mice, args= append(list(data=.x),miceArg)))) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup()

    return(group_imp)
  }

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
#' @importFrom magrittr "%>%"
#'
#' @export
BGcompare <- function(X,Y, BG.method = c("asymptotic","permutation","automatic"),
                      .n_perm, N_auto){

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

