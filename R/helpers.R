# Funkcja liczy odległość pomiędzy wektorami zgodnie z metryką euklidesową.
#
# v1 - pierwszy wektor
# v2 - drugi wektor
#
# RETURN odległość pomiędzy wektorami
#
metric.euclid <- function(v1,v2) {
  return (sqrt (sum ((v2 - v1)^2)))
}

# Funkcja liczy odległość pomiędzy wektorami zgodnie z metryką kosinusową.
#
# v1 - pierwszy wektor
# v2 - drugi wektor
#
# RETURN odległość pomiędzy wektorami wyrażona kontem
#
metric.cos <- function(v1,v2) {
  return ((sum (v1*v2)) / (sqrt (sum (v1^2)) * sqrt (sum (v2^2))))
}

# Funkcja kompresuje dane (usuwa kolumny z wartością zero) na potrzeby
# algorytmu knn.
#
# data - tablica danych
#
# RETURN lista ramek reprezentująca indeksy słów i ich liczność.
#
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

vec.norm <- function(v) {
  return( v / sqrt(sum(v*v)) );
}
