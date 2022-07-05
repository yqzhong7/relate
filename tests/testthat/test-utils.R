#' @title Testing avg_dist_func
#' @section Last Updated By:
#' Yongqi Zhong
test_that("avg_dist_func: arg checks", {
  cohortid = rep(1:3, each=10)
  mean_dist_vec = c(0.1,1,10)
  dist_vec = rep(mean_dist_vec, each=100)
  test_dist_df <- matrix(dist_vec,ncol = 30,nrow = 30)
})
