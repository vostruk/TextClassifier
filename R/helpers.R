#' Function metric.euclid calculates euclidean distance between two vectors
#'
#' @param v1 vector first
#' @param v2 vector second
#'
#' @return distance between two vectors
#'
#' @examples
#' metric.euclid(c(1,2,3), c(2,3,4))
#'
#' @export
metric.euclid <- function(v1,v2) {
  return (sqrt (sum ((v2 - v1)^2)))
}

#' Function metric.euclid calculates cosine distance between two vectors
#'
#' @param v1 vector first
#' @param v2 vector second
#'
#' @return distance between two vectors
#'
#' @examples
#' metric.cos(c(1,2,3), c(2,3,4))
#'
#' @export
metric.cos <- function(v1,v2) {
  return ((sum (v1*v2)) / (sqrt (sum (v1^2)) * sqrt (sum (v2^2))))
}


vec.norm <- function(v) {
  return( v / sqrt(sum(v*v)) );
}

#' Function compute.metrixes given confusion matrix, computes precision, accuracy and recall metrix for a whole dataset(averaging through the classes)
#'
#'
#' @param t matrix - confusion matrix returned by table function
#'
#' @return list containing precision, recall and accuracy for the whole dataset
#'
#' @export
compute.metrixes <- function(t){
  precs <- diag(t)/colSums(t)
  rec <- diag(t)/rowSums(t)
  accu <-sum(diag(t))/sum(t)
  return(list(sum(precs)/length(precs), sum(rec)/length(rec), accu))
}
