
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries & auxiliary functions -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rm(list=ls())

library("RMySQL")
library("dplyr")
library("plotly")
library("lubridate")
library("reshape")
library("reshape2")
library("knitr")
library("readr")
library("gtools")
library("tm")
library("tokenizers")
library("data.table")

get_ascii <- function(x, invert = FALSE) {
        i <- grep("\\+", iconv(x, "latin1", "ASCII", "+"),
                  invert = !invert)
        if (all(vapply(i, length, double(1)) == 0))
                return(NA_character_)
        x <- iconv(x, "latin1", "utf-8", " ")
        x[i]
}

get_utf8 <- function(x) iconv(x, "latin1", "utf-8", "")

wtoken <- function(q) {
        ## remove periods, comas, question marks
        q <- gsub("\\.|\\,|\\?|\\:|\\;|\\(|\\)|\\[|\\]|\\+|\\*|\\{|\\}",
                  " ", tolower(q))
        ## remove spaces
        q <- gsub("[ ]{2,}", " ", gsub("[ ]${1,}", "", q))
        ## remove dashes (assume one word)
        q <- gsub("\u2014|\\-", "", q)
        ## split into words
        q <- strsplit(q, " ")
        ## convert to utf-8 and return
        lapply(q, get_utf8)
}

## filter for stopwords
fullstop <- function(q, fullstop = TRUE) {
        ## if non-null rm stopwords
        if (fullstop) {
                ## remove stopwords
                q <- lapply(q, function(x) x[!x %in% stopwords])
        }
        ## remove remaining punctuation
        q <- lapply(q, function(x) gsub("[[:punct:]]", "", x))
        ## replace empty with blank
        q[vapply(q, length, double(1)) == 0] <- NA_character_
        ## return words
        q
}

## word counts (corrected)
nword <- function(q) {
  sapply(gregexpr("\\W+", q), length) + 1
}
## word count diff
nword_diff <- function(q1, q2) {
  if (any(is.na(c(q1, q2)))) return(0)
  abs(as.numeric(nword(q1) - nword(q2)))
}
## char count diff
nchar_diff <- function(q1, q2) {
  if (any(is.na(c(q1, q2)))) return(0)
  abs(as.numeric(nchar(q1) - nchar(q2)))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load date --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
train <- fread(file = "~/Projects/Git/Kaggle_QuoraQuestionPairs/data/train.csv")
test <- fread(file = "~/Projects/Git/Kaggle_QuoraQuestionPairs/data/test.csv")
sample_submission <- fread(file = "~/Projects/Git/Kaggle_QuoraQuestionPairs/data/sample_submission.csv")

countries <- read.delim(file = "~/Projects/Git/Kaggle_QuoraQuestionPairs/countryNames.txt")
countries <- lapply(countries, tolower)$countryName

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# stopword dictionary -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stopwords = c("a", "about", "above", "above", "across", "after", "afterwards", "again",
              "against", "all", "almost", "alone", "along", "already", "also","although",
              "always","am","among", "amongst", "amoungst", "amount",  "an", "and",
              "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are",
              "around", "as",  "at", "back","be","became", "because","become","becomes",
              "becoming", "been", "before", "beforehand", "behind", "being", "below",
              "beside", "besides", "between", "beyond", "bill", "both", "bottom","but",
              "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry",
              "de", "describe", "detail", "do", "done", "down", "due", "during", "each",
              "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough",
              "etc", "even", "ever", "every", "everyone", "everything", "everywhere",
              "except", "few", "fifteen", "fify", "fill", "find", "fire", "first",
              "five", "for", "former", "formerly", "forty", "found", "four", "from",
              "front", "full", "further", "get", "give", "go", "had", "has", "hasnt",
              "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein",
              "hereupon", "hers", "herself", "him", "himself", "his", "how", "however",
              "hundred", "i", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it",
              "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd",
              "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more",
              "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name",
              "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody",
              "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off",
              "often", "on", "once", "one", "only", "onto", "or", "other", "others",
              "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per",
              "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed",
              "seeming", "seems", "serious", "several", "she", "should", "show", "side",
              "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone",
              "something", "sometime", "sometimes", "somewhere", "still", "such", "system",
              "take", "ten", "than", "that", "the", "their", "them", "themselves", "then",
              "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon",
              "these", "they", "thickv", "thin", "third", "this", "those", "though", "three",
              "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward",
              "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us",
              "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence",
              "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon",
              "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole",
              "whom", "whose", "why", "will", "with", "within", "without", "would", "yet",
              "you", "your", "yours", "yourself", "yourselves", "the")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# playground -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aux <- train %>%
        dplyr::select(question1, question2) %>%
        slice(1:10) %>%
        mutate(q1_token = wtoken(question1),
               q2_token = wtoken(question2)
        )


lapply(aux, strsplit(split = " "))

removeWords(aux[1,1], stopwords)
