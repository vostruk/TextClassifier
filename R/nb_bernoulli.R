#' Function fit.nb.bernoulli implements model building algorithm of Naive Bayes using bernoulli equasion
#'
#' Bernoulli model is equivalent to the binary independence model, which generates an indicator
#' for each term of the vocabulary, either 1 indicatingpresence of the term in the document or 0 indicating absence.
#' (we're using bernoulli distribution for conditional probabilities p(F|c) calculation )
#' More information about algorithm can be found in the book Introduction to Information Retrieval (p.253-265)
#'
#' @param data data.frame contains values of our training set
#' @param fact  Factor categories list pointing the right classification of the data (one per sample)
#'
#' @return object klasy mcTbnb do uzycia w funkcji predict
#'
#' @examples
#' fit.nb.bernoulli(train.data, label)
#'
#' @export
fit.nb.bernoulli <-
  function(data, fact) {
  lvl         <- levels(fact);
  class.count <- length(lvl);
  words.count <- ncol(data);

  prior <- rep(NA, class.count);
  condprob <- matrix(nrow=words.count, ncol=class.count);

  #1.  calculating prior probabilities
  #1.1 number of all records
  N <- nrow(data);
  for(c in 1:class.count) {
  #1.2 number of samples of particular class
    Nc <- sum(fact == lvl[c]);
  #1.3 Apriori probability for the class
    prior[c] <- Nc / N;

  #2. Now calculating conditional probabilities
    for(t in 1:words.count) {
  #2.1 Calculating conditional probabilities of each class
      Nct <- sum(data[,t] > 0 & fact == lvl[c]);
      condprob[t, c] <- (Nct + 1) / (Nc + 2);
    }
  }

  l <- list(prior=prior,condprob=condprob);
  class(l) <- 'mcTbnb';
  return(l);
}


#' Function predict.nb.bernoulli extends generic function predict for Naive Bayes bernoulli model
#'
#'
#' @param model list containing two fields $prior and $condprob which are vector and matrix
#' @param data data.frame contains values of our testing set (identical structure that was use in training set for building model)
#'
#' @return result matrix where for each sample from data set probabilities of each class are given
#'
#' @examples
#' predict.nb.bernoulli(model, data)
#'
#' @export
predict.mcTbnb <-
  function(model, data) {
  result <- matrix(nrow=nrow(data), ncol=ncol(model$condprob));

  for(c in 1:ncol(model$condprob)) {
    #cat(sprintf("\nClass nr %d:\n", c));
    for(i in 1:nrow(data)) {
      #cat(sprintf("\tSample: %d\t\t\r", i));
      #flush(stdout());

      tmp <- log( model$prior[c] );
      tmp <- tmp + sum(
        log( ifelse(data[i,] > 0, model$condprob[,c], 1 - model$condprob[,c]) )
      );

      result[i, c] <- tmp;
    }
  }
  result <- apply(result, 1, function(r) which(r==max(r)));
  return(result);
}

