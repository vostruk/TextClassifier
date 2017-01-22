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

# FUNC predict.mcNb(model, data)
#
# Rozszerzenie funkcji generycznej predict.
# Obiekt model musi byc lista zawierajaca
# dwa pola $prior $condprob bedace odpowiednio
# wektorem oraz macierza.
# data musi zawierac dane niezbene do klasyfikacji
# (data MUSI zawierac miec IDENTYCZNA strukture co
# dane uzyte przy nauce klasyfikatora)
#
# RETURN macierz ktora dla kazdego wiersza danych zwraca
# wiersz okreslajacy p-stwo przynaleznosci do danej ktegorii
# Kolumny sa uporzadkowoane zgodnie z porzadkiem leveli  faktora
# zwracanym przez levels
#
predict.mcNb <- function(model, data) {
	data <- sign(as.matrix(data));

	CN <- length(model$prior);
	A  <- ncol(data);
	result <- matrix(nrow=nrow(data), ncol=CN);

	N <- nrow(data);
	log.cp <- log(model$condprob);
	log.prior <- log(model$prior);

	for(r in 1:N) {
		cat(sprintf("Wiersz %d/%d\t\t\r", r, N));

		for(c in 1:CN) {
			tmp <- log.cp[c];
			tmp <- tmp +
				sum( ifelse(data[r,] == 0, log.cp[2*c-1,], log.cp[2*c,]) );
			result[r, c] <- tmp;
		}
	}

	cat('\n');
	return(result);
}




