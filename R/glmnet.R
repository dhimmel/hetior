#' Train a regularized logistic regression model.
#'
#' @export
glmnet_train <- function(X, y, w, alpha=1, s='lambda.1se', cores=7, seed=0) {
  # Fit a regularized logistic regression model using the glmnet package.
  # alpha is the regularization parameter (0 for ridge, 1 for lasso).

  if (missing(w)) {
    w <- rep(1, length(y))
  }

  fit <- list(X=X, y=y, w=w, s=s, alpha=alpha, seed=seed)

  # train model
  doMC::registerDoMC(cores=cores)
  set.seed(seed)
  fit$cv_model <- glmnet::cv.glmnet(x = X, y = y, weights = w, family='binomial',
    alpha=alpha, standardize=TRUE, parallel=TRUE)
  fit$lambda <- fit$cv_model[[s]]

  # model information and performance
  fit$coef_df <- glmnet_coefs(fit$cv_model, X, y, s=s)
  fit$y_pred <- glmnet_predict(fit$cv_model, X, s=s)
  fit$vtm <- hetior::calc_vtms(y_true=y, y_pred=fit$y_pred)

  return(fit)
}

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
