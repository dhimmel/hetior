#' Extract glmnet model coefficients.
#'
#' Extract standardized and unstandardized coefficients.
#'
#' @param cv_glmnet a \code{\link[glmnet]{cv.glmnet}} object to extract
#'     coefficients for.
#' @param X matrix of features used to train \code{cv_glmnet}
#' @param y outcome used to train \code{cv_glmnet}
#' @param s Value(s) of the penalty parameter lambda at which predictions are required
#' @param prepend text to prepend to the \code{coef} and \code{zcoef} column names
#' @export
glmnet_coefs <- function(cv_glmnet, X, y, s='lambda.1se', prepend='', ...) {
  lambda <- cv_glmnet[[s]]
  coef.vec <- coef(cv_glmnet, s=lambda)[, 1]
  zcoef.vec <- c(0, coef.vec[-1] * apply(X, 2, sd) / sd(y))
  coef_df <- data.frame('feature'=c('intercept', colnames(X)), ...,
    'coef'=coef.vec, 'zcoef'=zcoef.vec, row.names=NULL, stringsAsFactors=FALSE)
  colnum <- ncol(coef_df)
  colnames(coef_df)[c(colnum - 1, colnum)] <- paste0(prepend, c('coef', 'zcoef'))
  return(coef_df)
}

#' Predictions from a glmnet object
#'
#' @param cv_glmnet a \code{\link[glmnet]{cv.glmnet}} object.
#' @param X matrix of features to predict from.
#' @param s Value(s) of the penalty parameter lambda at which predictions are required.
#' @export
glmnet_predict <- function(cv_glmnet, X, s = 'lambda.1se') {
  y_pred <- as.numeric(
    predict(cv_glmnet, s=cv_glmnet[[s]], newx=X, type='response'))
  return(y_pred)
}