#' Compute variable threshold metrics for classifier performance.
#'
#' Compute roc and prc curves as well as AUCs (area under the curves).
#'
#' @param y_true true binary labels in binary label indicators
#' @param y_pred target scores, can either be probability estimates of
#'     the positive class, confidence values, or binary decisions.
#' @param is_prob whether predictions (y_pred) are probabilities. If so
#'     additional metrics are computed
#' @export
calc_vtms <- function(y_true, y_pred, is_prob = FALSE) {
  if(length(unique(y_true)) < 2) {
    metrics <- list(
      'auroc'=NA,
      'auprc'=NA
    )
    if (is_prob) {
      metrics$tjur = NA
    }
    return(metrics)
  }

  rocr_pred <- ROCR::prediction(predictions=y_pred, labels=y_true)

  threshold_df <- data.frame(
    'threshold'=rocr_pred@cutoffs[[1]],
    'fpr'=ROCR::performance(rocr_pred, measure='fpr')@y.values[[1]],
    'recall'=ROCR::performance(rocr_pred, measure='rec')@y.values[[1]],
    'precision'=ROCR::performance(rocr_pred, measure='prec')@y.values[[1]],
    'lift'=ROCR::performance(rocr_pred, measure='lift')@y.values[[1]],
    stringsAsFactors=FALSE
  )

  auroc <- ROCR::performance(rocr_pred, 'auc')@y.values[[1]]
  roc_df <- prune_roc(threshold_df[, c('fpr', 'recall')])

  trapz_df <- na.omit(threshold_df[, c('recall', 'precision')])
  auprc <- caTools::trapz(trapz_df$recall, trapz_df$precision)

  metrics <- list(
    'auroc'=auroc,
    'auprc'=auprc,
    'threshold_df'=threshold_df,
    'roc_df'=roc_df
  )

  if (is_prob) {
    metrics$tjur = get_tjur(y_true, y_pred)
  }

  return(metrics)
}

#' Prune points from a ROC curve.
#'
#' Reduces the number of points in an ROC curve when many observations
#'
#' @param roc_df a dataframe containing a ROC curve
#'     created by \code{\link{calc_vtms}}.
prune_roc <- function(roc_df) {
  stopifnot(all(c('fpr', 'recall') %in% colnames(roc_df)))
  for (measure in c('fpr', 'recall')) {
    not.dup <- ! duplicated(roc_df$recall)
    not.dup <- not.dup | c(not.dup[-1], TRUE)
    roc_df <- roc_df[not.dup, ]
  }
  return(roc_df)
}

#' Prune points from a precision-recall curve.
#'
#' Reduces the number of points in a PRC when many observations.
#'
#' @param prc_df a dataframe containing a precision-recall curve
#'     created by \code{\link{calc_vtms}}.
#' @param min_distance minimum distance between two retained points
#' @export
prune_prc <- function(prc_df, min_distance=0.0005) {
  stopifnot(all(c('precision', 'recall') %in% colnames(prc_df)))
  dist_df <- prc_df[, c('precision', 'recall')]
  keep_row <- rowSums(is.na(dist_df)) == 0
  prc_df <- prc_df[keep_row, ]
  dist_df <- dist_df[keep_row, ]
  pointer <- 1
  as_index <- sapply(2:nrow(dist_df), function(i) {
    distance <- dist(dist_df[c(pointer, i), 1:2])[1]
    if (distance > min_distance) {
      pointer <<- i
      return(i)
    } else {return(pointer)}
  })
  prc_df <- prc_df[c(1, unique(as_index)), ]
  return(prc_df)
}


#' Compute the coefficient of discrimination.
#'
#' Compute Tjur's R-squared. See https://doi.org/10.1198/tast.2009.08210
#'
#' @param y_true true binary labels in binary label indicators
#' @param y_pred target scores, can either be probability estimates of
#'     the positive class, confidence values, or binary decisions.
#' @export
get_tjur <- function(y_true, y_pred) {
  is_pos = as.logical(y_true)
  tjur = mean(y_pred[is_pos]) - mean(y_pred[! is_pos])
  return(tjur)
}
