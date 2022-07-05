#' @title Recursive Multivariate Testing
#'
#' @description This recursion function conducts the Biswas and Ghosh multivariate test
#' based on the dendrogram from the hierarchical clustering of the average distance matrix of the unsupervised random forest.
#'
#' @param dend dendrogram object from the `stat::as.as.dendrogram()`
#' @param df  data.frame
#' @param cateVar string or vector, names of variables are categorical, which will be converted into dummy variables
#' @param ordinalVar string or vector, names of variables are ordinal factors, which will be converted into numeric variables
#' @param cohortid.var string, name of the cohort id indicator in the `relate::df`
#' @param alpha.level numeric, alpha level for statistical significance of the BG test
#' @param verbose boolean, whether to print the intermediate test results at each round
#' @param saveIntermediate boolean, whether to save the intermediate test result in the final output
#' @param BG.method string, "asymptotic","permutation","automatic", see `relate::BGcompare` above
#' @param n_perm number of permutation if method is permutation
#' @param N_auto integer, the sample size for automatically choosing between asymptotic and permutation, below is permutation, above is asymptotic
#' @param impute boolean, whether to conduct MICE imputation within clusters before testing
#' @param miceArgs list, a list of arguments parse into `mice::mice`
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom stats is.leaf
#'
#' @export
recursive.test <- function(dend , df, cateVar = NULL, ordinalVar = NULL,
                           cohortid.var = "cohortid", alpha.level = 0.05,
                           verbose = T,saveIntermediate = F,
                           BG.method = 'asymptotic', n_perm = 200, N_auto = 50,
                           impute = T, miceArgs = list(method = 'mean', maxit = 1)){
  ##---data preprocessing---#
  imputed_df <- df %>%
    dplyr::rename(cohortid = dplyr::all_of(cohortid.var)) %>%
    dplyr::mutate(clusterid = .data$cohortid)

  # dummy variables
  if (!is.null(cateVar)){
    imputed_df <- fastDummies::dummy_cols(imputed_df, select_columns = cateVar,
                                          remove_selected_columns	= T)
  }
  # ordinal to numeric
  if (!is.null(ordinalVar)){
    imputed_df <- imputed_df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(ordinalVar), as.numeric))
  }

  #standardization
  imputed_df <- imputed_df %>%
    dplyr::mutate(dplyr::across(-.data$clusterid, ~as.numeric(scale(.x))))


  ##---wrapper for pairwise comparison---#
  compareDF_func <- function(X, Y, verbose , BG.method){
    # create cartesian product of the cohort id
    compareDF = purrr::cross_df(list(X = unlist(X), Y = unlist(Y))) %>%
      dplyr::mutate(dplyr::across(.data$X:.data$Y, as.character))
    # print the comparison at each traversal
    if (verbose){
      cat("Start testing: \n")
      print(compareDF)
    }

    #pairwise comparison
    pairwise_p <- rep(NA,nrow(compareDF))
    for (i in 1:nrow(compareDF)){
      X <- compareDF[[i,1]]
      Y <- compareDF[[i,2]]

      #select and impute
      testing_cohorts_df <- imputed_df %>%
        dplyr::filter(.data$clusterid %in% c(X,Y))

      #mean imputation by cluster
      if(impute){
        imputed_cohorts <- mice_group_impute(df = testing_cohorts_df, clusterid=.data$clusterid, miceArg = miceArgs)
      } else {
        imputed_cohorts <- testing_cohorts_df
      }

      #select non-missing variables only (after imputation)
      cohorts <- imputed_cohorts %>%  dplyr::select_if(~ !any(is.na(.)))

      #BG test
      pairwise_p[i] <- BGcompare(cohorts %>%
                                   dplyr::filter(.data$clusterid %in% X) %>%
                                   dplyr::select(-.data$cohortid,-.data$clusterid) %>%
                                   as.matrix(),
                                 cohorts %>%
                                   dplyr::filter(.data$clusterid %in% Y) %>%
                                   dplyr::select(-.data$cohortid,-.data$clusterid) %>%
                                   as.matrix(),
                                 BG.method = BG.method,
                                 .n_perm = n_perm)



      # save intermediate dataset (imputed before testing)
      if(saveIntermediate){
        intermediate_imputation <<- append(intermediate_imputation, list(imputed_cohorts))
      }
    }
    compareDF = compareDF %>%
      dplyr::mutate(Pairwise.p = pairwise_p,
                    Significant = pairwise_p<alpha.level) %>%
      tibble::tibble()
    if (verbose){
      cat("Results: \n")
      print(compareDF[3:4])
      cat("--------------------------\n")
    }

    return(compareDF)
  }
  ##---recursion---##
  procDendro <- function(dend,verbose){
    if(is.leaf(dend)){
      return(dend %>% dendextend::get_leaves_attr('label') %>% as.character())
    }

    left = procDendro(dend[[1]], verbose = verbose)
    right = procDendro(dend[[2]], verbose = verbose)

    compareDF <- compareDF_func(X=left, Y=right, verbose = verbose, BG.method = BG.method)
    order_testing <<- append(order_testing, list(compareDF))

    #update cluster id and imputed variable
    mergeClusterID <- compareDF %>%
      #create indicator for merging
      dplyr::mutate(X_merged = F,
                    Y_merged = F) %>%
      #merge by larger p values
      dplyr::arrange(dplyr::desc(.data$Pairwise.p))

    for (i in 1:nrow(mergeClusterID)){
      # only merge highest p-value pairs
      if(!mergeClusterID$Significant[i] & !mergeClusterID$X_merged[i] & !mergeClusterID$Y_merged[i]){
        newclusterid = paste(compareDF$X[i],compareDF$Y[i], sep = ",")
        mergeClusterID <<- mergeClusterID %>%
          #only retain those unmerged
          dplyr::filter(!.data$X_merged & !.data$Y_merged) %>%
          dplyr::mutate(X_merged = (.data$X==mergeClusterID$X[i]),
                        Y_merged = (.data$Y==mergeClusterID$Y[i]) )

        imputed_df$clusterid <<-ifelse(imputed_df$clusterid %in% c(compareDF$X[i],compareDF$Y[i]),
                                           newclusterid,imputed_df$clusterid)
      }
    }

    combined_leaf <- compareDF %>%
      dplyr::filter(!.data$Significant) %>%
      dplyr::mutate(combin = paste(.data$X,.data$Y,sep=";")) %>%
      dplyr::pull(.data$combin)
    dend_list <- lapply(list(left,right), function(x) gsub(",$", "",x)) %>%
      unlist()

    not_in_combined <- dend_list[!dend_list %in% unlist(stringr::str_split(combined_leaf,";"))]


    output <- gsub(";", ",",c(combined_leaf,not_in_combined))
    return(output)


  }

  #output
  intermediate_imputation <- list()
  order_testing <- list()
  finalClusters <- procDendro(dend=dend, verbose = verbose)

  res <- list(clusters = unlist(finalClusters), test_results = order_testing,
              intermediate_imputated_df = intermediate_imputation)

  return(res)
}
