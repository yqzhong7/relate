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


#' @title Multiple imputation by group
#'
#' @description Multiple imputation using `mice::mice` by group
#'
#' @param df data.frame for MICE imputation
#' @param clusterid  string, the name of cohort id indicator in the dataframe
#' @param imputeLevel numeric, only variables with missingness less than this level will be imputed, ranges from 0 (no imputation) to 1 (impute everything), e.g., imputeLevel = 0.3 meaning variables <= 30% missingness will be imputed
#' @param miceArg a list of arguments parse into `mice::mice`
#'
#' @importFrom magrittr "%>%"
#' @export
mice_group_impute <- function(df, clusterid, imputeLevel = 1, miceArg = list(method = 'mean', maxit = 1)){
  #no imputation
  if (imputeLevel==0){
    return(df)
  }

  #impute everything
  if (imputeLevel==1){
    group_imp <- df %>%
      dplyr::group_by(dplyr::all_of(clusterid)) %>%
      dplyr::group_map(~mice::complete(do.call(mice::mice, args= append(list(data=.x),miceArg)))) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup()

    return(group_imp)
  }

  #impute under imputeLevel
  df_col <- colnames(df)
  #calculate the proportion of missingness
  missing_lvl <- df %>%
    dplyr::select(-dplyr::all_of(clusterid)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(is.na(.x))/dplyr::n())) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::mutate(impute = .data$value <= imputeLevel)

  imputed_vars <- missing_lvl[missing_lvl$impute,]$name
  not_imputed_vars <- missing_lvl[!missing_lvl$impute,]$name

  #imputation by clusterid
  group_imp <- df %>%
    dplyr::select(dplyr::all_of(clusterid), dplyr::all_of(imputed_vars)) %>%
    dplyr::group_by(dplyr::all_of(clusterid)) %>%
    dplyr::group_map(~mice::complete(do.call(mice::mice, args= append(list(data=.x),miceArg)))) %>%
    dplyr::bind_rows() %>%
    dplyr::ungroup() %>%
    dplyr::bind_cols(df %>% dplyr::select(dplyr::all_of(not_imputed_vars))) %>%
    dplyr::select(dplyr::all_of(df_col))

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

