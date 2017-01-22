#' Function split.categories changes dataframe from the one with many categories for one sample to many samples with one category
#'
#' Such shape of data is required by naiveBayes function and generally recognized. Additionaly some categories
#' defined by used can be dropped from the data frame by passing a list invalid.cats.hash
#'
#' @param df data.frame input data frame with values and categories
#' @param min.n int minimalna liczba wystapien danej kategorii
#' @param invalid.cats.hash hashtable nazwy kategori ktore powinny zostac odrzucone
#' @param min.w int minimalna liczba wystapien danego slowa
#'
#' @return splitted_vectors list list that contains $data - output data $fact - output factor corresponding to the data
#'
#' @examples
#' split.categories(mydataframe, 100, create.hash(c("c.OtherApplicationsNEC", "c.OtherSciencesNEC")), 2)
#'
#' @export
split.categories <- function(df, min.n=500, invalid.cats.hash=emptyenv(), min.w=2) {
  cats.names    <- colnames(df);
  cats.indexes  <- get.df.column.indexes(df, 'c');
  allow.in.data <- filter.categories(df, min.n, invalid.cats.hash);

  max.words.index <- min(cats.indexes) - 1;

  rows.vector <- integer(0);
  cats.vector <- character(0);

  # first row is different (statistics)
  N = nrow(df);

  for(i in 2:N){
    for(ci in cats.indexes) {
      if(allow.in.data[ci] && df[i, ci] > 0) {
        rows.vector <- c(rows.vector, i);
        cats.vector <- c(cats.vector, cats.names[ci]);
      }
    }
  }

  # remove words that are not frequent
  words.count  <- integer(max.words.index);
  for(wi in 1:max.words.index) {
    words.count[wi] = sum(df[rows.vector,wi]);
  }
  words <- (1:max.words.index)[words.count >= min.w];

  used.columns <- which(allow.in.data == TRUE);

  return(list(data=df[rows.vector, words],
              fact=factor(cats.vector),
              rows.indexes=rows.vector,
              org.classes=df[rows.vector,used.columns]));
}


is.column.name <- function(s) {
	return(length( grep('^c\\.', s) ) > 0);
}


is.invalid <- function(column.name, invalid.categories.hash) {
	return(exists(column.name, envir=invalid.categories.hash, inherits=FALSE));
}



# @return logic vector such that l[column.n] == T if category should stay in the final data
filter.categories <- function(text.data.frame, min.n, invalid.cats.hash) {
	column.names <- colnames(text.data.frame);
	result       <- logical( ncol(text.data.frame) );

	for(i in 1:ncol(text.data.frame)) {
		column.name <- column.names[i];
		if(is.column.name(column.name) && !is.invalid(column.name, invalid.cats.hash)) {
			number.of.arts <- text.data.frame[1, i];
			if(number.of.arts >= min.n) {
				result[i] = TRUE;
			}
		}
	}

	return(result);
}


create.hash <- function(str.vector) {
	e <- new.env(hash=TRUE, parent=emptyenv());

	for(s in str.vector) {
		assign(s, TRUE, envir=e);
	}

	return(e);
}

#' cs may be 'c' for categories, 'w' for words
get.df.column.indexes <- function(df, cs) {
	column.names  <- colnames(df);
	class.columns <- grep(paste("^",cs,"\\.",sep=''), column.names);

	return( (1:ncol(df)) [class.columns] );
}

#' Function data.compact removes columns with 0 value (eg. is used in knn algorithm)
#'
#' @param data data.frame
#'
#' @return data.frame with removed columns
#'
#' @examples
#' data.compact(data)
#'
#' @export
data.compact <- function (data){
  vectors <- list()

  for (i in 1:nrow(data)){
    zeroes <- data[i,] == 0
    id <- (1:ncol(data))[!zeroes]
    count <- data[i,][!zeroes]
    vec <- data.frame (id=id, count=count)
    vectors <- append (vectors, list(vec))
  }
  return (vectors)
}
