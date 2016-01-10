library(tm)
library(wordcloud)
library(memoise)

# The list of valid books
books <<- list("Kradetsat na praskovi" = "Emilijan_Stanev_-_Kradetsyt_na_praskovi-4127.txt",
               "Maminoto detentse" = "Ljuben_Karavelov_-_Maminoto_detentse-3784.txt",
               "Tartjuf" = "Moliere_-_Tartjuf-2763.txt")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf(book), encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

