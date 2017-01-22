#' Function fit.nb implements model building algorithm of Naive Bayes
#'
#' Bernoulli model is equivalent to the binary independence model, which generates an indicator
#' for each term of the vocabulary, either 1 indicatingpresence of the term in the document or 0 indicating absence.
#' (we're using bernoulli distribution for conditional probabilities p(F|c) calculation )
#' More information about algorithm can be found in the book Introduction to Information Retrieval (p.253-265)
#'
#' @param data data.frame contains values of our training set
#' @param fact  Factor categories list pointing the right classification of the data (one per sample)
#' @param p m-estimation constrant
#'
#' @return object klasy mcTbnb do uzycia w funkcji predict
#'
#' @examples
#' fit.nb(train.data, label)
#' fit.nb(train.data, label, p=5)
#'
#' @export
fit.nb <- function(data, fact, p=-1) {
  #only binary data is allowed
	data = sign(data);

	lvl <- levels(fact);
	CN  <- length(lvl);
	A   <- ncol(data);

	prior <- rep(0, CN);
	N     <- nrow(data);
	condprob <- matrix(ncol=ncol(data), nrow=2*CN);

	for(c in 1:CN) {
		Nc       <- sum(fact == lvl[c]);
		prior[c] <- Nc / N;
		pc <- ifelse(p == -1, 1/Nc, p);

		class.rows <- which(fact == lvl[c]);
		for(a in 1:A) {
			condprob[2*c-1, a] =
				(sum(data[class.rows, a] == 0) + pc*Nc) / (2*Nc);
			condprob[2*c, a] =
				(sum(data[class.rows, a] == 1) + pc*Nc) / (2*Nc);
		}
	}

	model <- list(prior=prior, condprob=condprob);
	class(model) <- 'mcNb';
	return(model);
}


#' Function predict.mcNb extends generic function predict for Naive Bayes model
#'
#'
#' @param model list containing two fields $prior and $condprob which are vector and matrix
#' @param data data.frame contains values of our testing set (identical structure that was use in training set for building model)
#'
#' @return result matrix where for each sample from data set probabilities of each class are given
#'
#' @examples
#' predict.mcNb(model, data)
#'
#' @export
predict.mcNb <- function(model, data) {
	data <- sign(as.matrix(data));

	CN <- length(model$prior);
	A  <- ncol(data);
	result <- matrix(nrow=nrow(data), ncol=CN);

	N <- nrow(data);
	log.cp <- log(model$condprob);
	log.prior <- log(model$prior);

	for(r in 1:N) {
		for(c in 1:CN) {
			tmp <- log.cp[c];
			tmp <- tmp +
				sum( ifelse(data[r,] == 0, log.cp[2*c-1,], log.cp[2*c,]) );
			result[r, c] <- tmp;
		}
	}
  result <- apply(result, 1, function(r) which(r==max(r)))
	return(result);
}




