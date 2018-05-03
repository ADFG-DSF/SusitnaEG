#' Get Posterior Summaries/Plot
#'
#' @description This function allows you to pull out variables from a mcmc.list or matrix.
#'    Modifided version of oriiginal written by Ben Stanton (bas0041@auburn.edu).
#'
#' @param post.samp: an object of class 'mcmc.list' or 'matrix'
#' @param var: the variable you wish to view. Must be in "quotes". To pull out a vector specify thru the opening bracket e.g. "N["
#'
#' @examples get.post(post_er, var="N[")
#' @examples get.post(post_er, var="lnalpha")
#'
#' @export
get_post=function(post_dat, var){
  #coerce to matrix if mcmc.list
  if (!coda::is.mcmc.list(post_dat) & !is.matrix(post_dat)) stop("post_dat is not of class mcmc.list or matrix")
  if (coda::is.mcmc.list(post_dat)) post_dat=as.matrix(post_dat)

  #pull out posteriors for requested variable
  if(substr(var,nchar(var), nchar(var))=="[") post=post_dat[,substr(colnames(post_dat), 1, nchar(var))==var] else post=post_dat[,var]
  post
}

