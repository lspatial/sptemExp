
#' @title rmse
#'
#' @description This function is to caltulate rmse.
#'
#' @param obs: a vector of observed values
#'
#' @param pre: a vector of predicted values
#'
#' @return rmse value
#'
#' @examples  rmse(obs=c(1,2,3,4),pre=c(1,2,3,4)+rnorm(4))
#'
#' @export rmse
#'
rmse=function(obs,pre){
  error=obs-pre
  ret=sqrt(mean(error^2))
  return(ret)
}


#' @title rSquare
#'
#' @description This function is to calculate rSquare.
#'
#' @param obs: a vector of observed values
#'
#' @param res: residual (=observed values-predicted values)
#'
#' @return rSquare value
#'
#' @examples  rSquared(obs=c(1,2,3,4),res=rnorm(4))
#'
#' @export rSquared
#'
rSquared=function(obs, res){
  yy = obs - matrix(mean(obs), nrow = nrow(array(obs)))
  r2 = 1 - (t(res) %*% res)/(t(yy) %*% yy)
  return(r2)
}
