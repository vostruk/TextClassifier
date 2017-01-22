
Sys.setlocale(category="LC_ALL", locale="C");
library('Rstem');

#' Function create.articles.data.frame creates dataframe from preprocesses files lokated in one directory
#'
#' Files should have flat structure and should have format returned by preprocess function from the package
#'
#' @param input.path string directory with preprocessed files
#' @param output.path string parametr is the name (location+name) where the dataframe should be saved
#'
#' @examples
#' create.articles.data.frame("C:\\Users\\vostruk001\\Projects\\Studia\\MOW\\Projekt\\MOW_PROJEKT\\prepr\\", "C:\\Users\\vostruk001\\Projects\\Studia\\MOW\\Projekt\\MOW_PROJEKT\\data\\")
#'
#' @export
create.articles.data.frame <- function(articles.directory, output.df.filename) {
  init.stop.words.dictionary();
  art.stats <- compute.global.stats(articles.directory);
  df.file   <- file(output.df.filename, "wt");

  add.df.header(df.file, art.stats);
  add.df.stats.row(df.file, art.stats);

  add.article.rows(art.stats, df.file, articles.directory);

  close(df.file);
}
Sys.setlocale(category="LC_ALL", locale="C");


# given the filename function reads and returns all its lines
read.all.lines <- function(file.name) {
	handle <- file(file.name, "rt");
	lines  <- readLines(handle);
	close(handle);

	return(lines);
}


# function returns vector of categories given the string of categories
get.article.categories <- function(category.line) {
	category.vector <- strsplit(category.line, "[ \t]*,[ \t]*");
	return( category.vector[[1]] );
}


# given filename function reads preprocessed file and
# returns the list with $abstract (text) and $classes (categories)
# returns NA if something went wrong
read.preprocessed.article <- function(file.name) {
	lines <- read.all.lines(file.name);
	if( length(lines) != 3 ) {
		stop(paste('invalid article file:', file.name));
	}

	categories <- get.article.categories(lines[1]);
	abstract   <- lines[3];

	return(list(abstract=abstract, categories=categories));
}


#function preprocess text including lowercasing all words, removing non-literal symbols,
#stemming using Porter algorithm, function returns words vector
stem.abstract.text <- function(abstract.text) {
	abstract.text <- gsub("[^A-Za-z]+", " ", abstract.text);
	abstract.text <- tolower(abstract.text);

	# dzielimy tekst na poszczegolne slowa
	# wynik jest zwracany w postaci listy wektorow
	abstract.text <- strsplit(abstract.text, "[ \t]+");
	abstract.text <- abstract.text[[1]];

	# stemujemy poszczegolne slowa
	# wordStem obsluguje slowa do dlugosci 255
	# dluzsze usuwamy, dodatkowo podczas splitu
	# moga pojawic sie napis o 0 dlugosci
	MAX.WORD.LENGTH <- 255;
	abstract.text   <- abstract.text[
		nchar(abstract.text) < MAX.WORD.LENGTH &  # NIE MOZE BYC &&
		nchar(abstract.text) > 0
	];

	return(wordStem(abstract.text, language="english"));
}



#function removes English stop words (like the, you etc) from the words vector

remove.stop.words <- function(wordsVector) {
	return( wordsVector[!is.stop.word(wordsVector)] );
}


#function reads the article and returns list of two vectors: $abstract word vector after stemming
# and $categories vector of the labels (categories) for that abstract
load.article <- function(file.name) {
	art <- read.preprocessed.article(file.name);

	abstract <- stem.abstract.text(art$abstract);
	abstract <- remove.stop.words(abstract);

	categories <- art$categories;

	return( list(abstract=abstract, categories=categories) );
}


#function adds words from wordsWector to the dictionary env (to create df later)
# env[word] contains number of times word occures in all texts
insert.words <- function(env, words) {
	for(w in words) {
		if(!exists(w, envir=env)) {
			assign(w, 1, envir=env);
		}
		else {
			assign(w, 1+get(w, envir=env), envir=env);
		}
	}
}



# function counts how many times each word occures in the corpus (for ex. can be useful for tf-idf)
# returning $words - words vocabulary $categories - categories vocabulary, $files.count - number of files preprocessed
compute.global.stats <- function(articles.directory) {
	words 	 <- new.env(hash=TRUE, parent=emptyenv());
	categories <- new.env(hash=TRUE, parent=emptyenv());
	count 	 <- 0;

	articles <- list.files(
		path=articles.directory,
		pattern="*.txt",
		full.names=TRUE,
		ignore.case=TRUE
		);

	for(a in articles) {
		cat(sprintf("[%s]\n", a));
		tmp <- load.article(a);

		insert.words(words, tmp$abstract);
		insert.words(categories, tmp$categories);
		count <- count + 1;
	}

	return( list(count=count, words=words, categories=categories) );
}

META.COLUMNS <- c('filename', 'wordcount');

# function writes to the files column names of the dataframe
add.df.header <- function(df.file, art.stats) {
	word.col.names <- paste('w.', sort(ls(envir=art.stats$words)), sep='');
	cats.col.names <- paste('c.', sort(ls(envir=art.stats$categories)), sep='');

	meta.col.names <- paste('meta.', META.COLUMNS, sep='');

	cat( c(word.col.names, cats.col.names, meta.col.names),
		file=df.file, sep='\t' );
	cat( '\n', file=df.file, sep='' );
}


write.env.cols <- function(envir, file) {
	col.names <- sort(ls(envir=envir));

	for(c in col.names) {
		tmp <- get(c, envir=envir);
		cat(tmp, file=file, sep='');
		cat('\t', file=file, sep='');
	}
}

# function adds first row with statistics of text corpus to the dataframe
add.df.stats.row <- function(df.file, art.stats) {
	write.env.cols(art.stats$words, df.file);
	write.env.cols(art.stats$categories, df.file);

	for(  i in 1:(length(META.COLUMNS)-1)  ) {
		cat('na\t', file=df.file, sep='');
	}
	cat('na\n', file=df.file, sep='');
}


write.art.row.cols <- function(aenv, genv, file) {
	col.names <- sort(ls(envir=genv));

	for(c in col.names) {
		if( exists(c, envir=aenv, inherits=FALSE) ) {
			tmp <- get(c, envir=aenv);

		}
		else {

		}

	}
}


# function adds data row of words from one source file
add.article.row <- function(art, stats, file) {
	art.env <- new.env(hash=TRUE, parent=emptyenv());
	insert.words(art.env, art$abstract);

	category.env <- new.env(hash=TRUE, parent=emptyenv());
	insert.words(category.env, art$categories);

	write.art.row.cols(art.env, stats$words, file);
	write.art.row.cols(category.env, stats$categories, file);

}

# function adds rows representing particular articles to dataframe
add.article.rows <- function(art.stats, file, articles.directory) {
	articles <- list.files(
		path=articles.directory,
		pattern="*.txt",
		full.names=TRUE,
		ignore.case=TRUE
		);

	for(a in articles) {
		tmp <- load.article(a);
		tmp$file.name <- a;
		add.article.row(tmp, art.stats, file);
	}
}



STOP.WORDS.VECTOR <- c(
  "a", "about", "above", "above", "across", "after",
  "afterwards", "again", "against", "all", "almost",
  "alone", "along", "already", "also","although","always",
  "am","among", "amongst", "amoungst", "amount",  "an", "and",
  "another", "any","anyhow","anyone","anything","anyway", "anywhere",
  "are", "around", "as",  "at", "back","be","became", "because",
  "become","becomes", "becoming", "been", "before", "beforehand",
  "behind", "being", "below", "beside", "besides", "between", "beyond",
  "bill", "both", "bottom","but", "by", "call", "can", "cannot",
  "cant", "co", "con", "could", "couldnt", "cry", "de", "describe",
  "detail", "do", "done", "down", "due", "during", "each", "eg",
  "eight", "either", "eleven","else", "elsewhere", "empty",
  "enough", "etc", "even", "ever", "every", "everyone", "everything",
  "everywhere", "except", "few", "fifteen", "fify", "fill", "find",
  "fire", "first", "five", "for", "former", "formerly", "forty",
  "found", "four", "from", "front", "full", "further", "get", "give",
  "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here",
  "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him",
  "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc",
  "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last",
  "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me",
  "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly",
  "move", "much", "must", "my", "myself", "name", "namely", "neither", "never",
  "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not",
  "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one",
  "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves",
  "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re",
  "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she",
  "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some",
  "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still",
  "such", "system", "take", "ten", "than", "that", "the", "their", "them",
  "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore",
  "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this",
  "those", "though", "three", "through", "throughout", "thru", "thus", "to",
  "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un",
  "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well",
  "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter",
  "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which",
  "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will",
  "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself",
  "yourselves");

STOP.WORDS.ENV <- NA;

init.stop.words.dictionary <- function() {
  STOP.WORDS.ENV <<- new.env(hash=TRUE, parent=emptyenv(), size=length(STOP.WORDS.VECTOR));

  for(word in STOP.WORDS.VECTOR) {
    assign(word, TRUE, envir=STOP.WORDS.ENV);
  }
}

is.stop.word <- function(word) {
  return(
    sapply(word, function(w) exists(w, envir=STOP.WORDS.ENV, inherits=FALSE))
  );
}



