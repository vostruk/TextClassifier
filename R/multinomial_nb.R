
#' Function fit.nb.multinomial implements model building algorithm of Naive Bayes using bernoulli equasion
#'
#' Multinomial Naive Bayes is a specialized version of Naive Bayes that is designed more for text documents
#' Multinomial model generates one term from the vocabulary in each position of the document
#'  Whereas simple naive Bayes would model a document as the presence and absence of particular words,
#'  multinomial naive bayes explicitly models the word counts and adjusts the underlying calculations to deal with in.
#'
#' More information about the method can be found http://www.cs.cmu.edu/~knigam/papers/multinomial-aaaiws98.pdf
#' More information about algorithm can be found in the book Introduction to Information Retrieval (p.253-265)
#'
#' @param data data.frame contains values of our training set (bag-of-words format of text)
#' @param fact  factor containing proper classification of the training samples in data set (one per observation)
#'
#' @return object klasy mcTmnb do uzycia w funkcji predict
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

  # Budowa modelu
  N <- nrow(data);
  for(c in 1:class.count) {
    cat(sprintf("\nKlasa %s:\n", lvl[c]));
    Nc <- sum(fact == lvl[c]); # Ilosc wystapien klasy c
    prior[c] <- Nc / N;

    tmp.df <- data[which(fact == lvl[c]),];
    S <- words.count + sum(tmp.df);
    for(t in 1:words.count) {
      cat(sprintf("\tKolumna: %d\t\t\r", t));
      flush(stdout());

      tmp <- data[, t];
      condprob[t, c] <-
        (1 + sum(tmp[which(fact == lvl[c])])) / S;
    }
  }

  cat('\n');
  l <- list(prior=prior,condprob=condprob);
  class(l) <- 'mcTmnb';
  return(l);
}




# FUNC predict.mcTmnb(model, data)
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
predict.mcTmnb <- function(model, data) {
  result <- matrix(nrow=nrow(data), ncol=ncol(model$condprob));

  for(c in 1:ncol(model$condprob)) {
    cat(sprintf("\nKlasa nr %d:\n", c));
    for(i in 1:nrow(data)) {
      cat(sprintf("\tWiersz: %d\t\t\r", i));
      flush(stdout());

      tmp <- log( model$prior[c] );
      tmp <- tmp + sum(
        # ifelse(data[i,] > 0, log(model$condprob[,c]), 0)
        data[i,] * log(model$condprob[,c])
      );

      #for(t in 1:ncol(data)) {
      #	if(data[i, t] > 0)
      #		tmp <- tmp + log( model$condprob[t,c] );
      #}

      result[i, c] <- tmp;
    }
  }

  return(result);
}


# Format danych wejsciowych:
# I) Ramka danych zawierajaca teksty w fomacie bag-of-word
# II) Faktor zawierajcy kategrie dla kazdego wiersza zbioru
#  uczacego (1 kategoria na wiersz)


predict.mcTmnb.v2 <- function(model, data) {
  result <- matrix(nrow=nrow(data), ncol=ncol(model$condprob));
  log.prior <- log(model$prior);
  log.condprob <- log(model$condprob);

  for(i in 1:nrow(data)) {
    cat(sprintf("Wiersz: %d\t\t\r", i));
    flush(stdout());

    d <- data[i, ];
    for(c in 1:ncol(model$condprob)) {
      tmp <- log.prior[c];
      tmp <- tmp + sum(
        d * log.condprob[,c]
      );

      result[i, c] <- tmp;
    }
  }

  cat('\n');
  return(result);
}
