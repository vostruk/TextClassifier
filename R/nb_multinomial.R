#' Function fit.nb.multinomial implements model building algorithm of Naive Bayes using bernoulli equasion
#'
#' Multinomial Naive Bayes is a specialized version of Naive Bayes that is designed more for text documents
#' Multinomial model generates one term from the vocabulary in each position of the document
#'  Whereas simple naive Bayes would model a document as the presence and absence of particular words,
#'  multinomial naive bayes explicitly models the word counts relevance in particular class / per all words in a class
#'
#' More information about the method can be found http://www.cs.cmu.edu/~knigam/papers/multinomial-aaaiws98.pdf
#' More information about algorithm can be found in the book Introduction to Information Retrieval (p.253-265)
#'
#' @param data data.frame contains values of our training set (bag-of-words format of text)
#' @param fact  factor containing proper classification of the training samples in data set (one per observation)
#'
#' @return object of the class mcTmnb to use in predict function
#'
#' @examples
#' fit.nb.multinomial(train.data, label)
#' @export
fit.nb.multinomial <- function(data, fact) {
  lvl         <- levels(fact);
  class.count <- length(lvl);
  words.count <- ncol(data);

  prior <- rep(NA, class.count);
  condprob <- matrix(nrow=words.count, ncol=class.count);

  N <- nrow(data);
  for(c in 1:class.count) {
    Nc <- sum(fact == lvl[c]); # number of samples of c
    prior[c] <- Nc / N;

    tmp.df <- data[which(fact == lvl[c]),];
    S <- words.count + sum(tmp.df);
    for(t in 1:words.count) {

      tmp <- data[, t];
      condprob[t, c] <-
        (1 + sum(tmp[which(fact == lvl[c])])) / S;
    }
  }

  l <- list(prior=prior,condprob=condprob);
  class(l) <- 'mcTmnb';
  return(l);
}


#' Function predict.mcTmnb extends generic function predict for Naive Bayes bernoulli model
#'
#'
#' @param model list containing two fields $prior and $condprob which are vector and matrix
#' @param data data.frame contains values of our testing set (identical structure that was use in training set for building model)
#'
#' @return result matrix where for each sample from data set probabilities of each class are given
#'
#' @examples
#' predict.mcTmnb(model, data)
#'
#' @export
predict.mcTmnb <- function(model, data) {
  result <- matrix(nrow=nrow(data), ncol=ncol(model$condprob));

  for(c in 1:ncol(model$condprob)) {
    for(i in 1:nrow(data)) {

        tmp <- log( model$prior[c] );
        tmp <- tmp + sum(
        data[i,] * log(model$condprob[,c])
      );

      result[i, c] <- tmp;
    }
  }
  result <- apply(result, 1, function(r) which(r==max(r)))
  return(result);
}


#' Function predict.mcTmnb.v2 extends generic function predict for Naive Bayes bernoulli model
#'
#' function is different approach of predicting from the same model
#'
#' @param model list containing two fields $prior and $condprob which are vector and matrix
#' @param data data.frame contains values of our testing set (identical structure that was use in training set for building model)
#'
#' @return result matrix where for each sample from data set probabilities of each class are given
#'
#' @examples
#' predict.mcTmnb(model, data)
#'
#' @export
predict.mcTmnb.v2 <- function(model, data) {
  result <- matrix(nrow=nrow(data), ncol=ncol(model$condprob));
  log.prior <- log(model$prior);
  log.condprob <- log(model$condprob);

  for(i in 1:nrow(data)) {
    d <- data[i, ];
    for(c in 1:ncol(model$condprob)) {
      tmp <- log.prior[c];
      tmp <- tmp + sum(
        d * log.condprob[,c]
      );

      result[i, c] <- tmp;
    }
  }

  result <- apply(result, 1, function(r) which(r==max(r)));
  return(result);
}
