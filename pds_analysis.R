# install.packages("pdftools")
library(pdftools)
# install.packages("tm")
library(tm)
# install.packages("ggplot2")
library(ggplot2)

#create object with pdf names
files <- list.files(path = ".\\papers", pattern = "pdf$")

#create corpus (may take a while with large number of papers)
corp <- Corpus(URISource(paste(".\\papers\\", files, sep = "")),
               readerControl = list(reader = readPDF))

#issues with removing punctualtion from pdf encoded letters
corp <- tm_map(corp, removePunctuation, ucp = TRUE)

#create term document matrix 
pdf_tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(stopwords = TRUE, #remove common words
                                          tolower = TRUE, #make all words lower case
                                          stemming = TRUE, #stem words to their root i.e. hiking and hiker both become hike
                                          removeNumbers = TRUE, #remove numers
                                          bounds = list(global = c(3, Inf)))) #keep words that appear more than 3 times

#list of keywords to find (this search will be determined at a later date)
keywords <- c("ethic", "social")

#find the number of times the ket words appearences in each pdf
keyword_per_paper <- as.matrix(pdf_tdm[keywords,])

#calculate how many papers mention each keyword
keyword_usage <- data.frame(paper_count = rowSums(keyword_per_paper > 0))

#fix row names as col so they can be plotted
keyword_usage$keyword <- rownames(keyword_usage)

#plot the number of papers each keyword is found in
ggplot(data = keyword_usage, aes(x = keyword, 
                                 y = paper_count,
                                 fill = keyword)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(legend.position = "none") 
