
#' Function transform.tfidf trainsforms bag-of-words values (count of word in the document) to tf-idf values (logarithmic)
#'
#' tf-idf value is calculated as tf/idf where tf is the number of times that term t occurs in document d
#' The inverse document frequency is a measure of how much information the word provides, that is,
#' whether the term is common or rare across all documents idf(term, Doc) = log(|Docs|/||)
#'
#' @param data data.frame contains values of our training set (either count of words or binary )
#'
#' @return data.frame with tf-idf values
#'
#' @examples
#' transform.df.tfidf(df)
#'
#' @export
transform.df.tfidf <-
  function(data) {
  tf <- data
  idf <- log(nrow(data)/colSums(data))
  tfidf <- data

  for(word in names(idf)){
    tfidf[,word] <- tf[,word] * idf[word]
  }
  return (tfidf);
}


#' Function transform.df.binary trainsforms bag-of-words values (count of word in the document) to binary values (present or not)
#'
#' @param data data.frame contains values of our training set
#'
#' @return data.frame with 0-1 values
#'
#' @examples
#' transform.df.binary(df)
#'
#' @export
transform.df.binary <-
  function(df) {
  for(i in 1:ncol(df)) {
    df[,i] <- sign(df[,i]);
  }
  return(df);
}

