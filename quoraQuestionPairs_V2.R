# ideas: https://www.kaggle.com/tanlikesmath/quora-question-pairs/xgb-starter-12357/code

rm(list=ls())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries & auxiliary functions -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library("dplyr")
library("tokenizers")
library("data.table")
library("stringr")
library("xgboost")
library("tictoc")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tic()

# set flag for directory choice
homeflag <- TRUE

if (homeflag == TRUE) {
        train_ <- fread(file = "/Users/benwo/Dropbox/DataScience/Kaggle_QuoraQuestionPairs_local/data_set/train.csv")
        test_ <- fread(file = "/Users/benwo/Dropbox/DataScience/Kaggle_QuoraQuestionPairs_local/data_set/test.csv")
        sample_submission_ <- fread(file = "/Users/benwo/Dropbox/DataScience/Kaggle_QuoraQuestionPairs_local/data_set/sample_submission.csv")
        
        ## load cleaned data set
        #train_c <- fread(file="test_c.csv")
        #train_c <- fread(file="train_c.csv")
} else {
        train_ <- fread(file = "/Users/bwolter/PhD/private/data/Kaggle_QQP_data/train.csv")
        test_ <- fread(file = "/Users/bwolter/PhD/private/data/Kaggle_QQP_data/test.csv")
        sample_submission_ <- fread(file = "/Users/bwolter/PhD/private/data/Kaggle_QQP_data/sample_submission.csv")
}


########################################
####### User defined functions  ########
########################################

## utf8 encoded
get_utf8 <- function(x) iconv(x, "latin1", "utf-8", "")

## tokenize by word
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

## get weight funciton for tf-idf metric

#if one word appears it gets thrown out as it might contain a typo
#epsilon is a smoothing constant, which minimizes the effect of extremely small words

get_weight <- function(count, eps=10000, min_count =2) {
        ifelse(count < min_count, 0 , 1/(count+eps))
}

## create function, that splits the words of individual questions and calculates their individual shares and their ratios

word_shares <- function(q1,q2) {
        foo <- function(q1,q2) {
                q1words <- setdiff(q1,stopwords)
                q2words <- setdiff(q2,stopwords)
                if(length(q1words) == 0) return(paste(c(0,0,0,0,0),collapse = ":"))
                if(length(q2words) == 0) return(paste(c(0,0,0,0,0),collapse = ":"))
                
                q1stops <- intersect(q1,stopwords)
                q2stops <- intersect(q2,stopwords)
                
                shared_words <- intersect(q1words,q2words)
                
                shared_weights <- weights_train[word %in% shared_words,sum(weight)]
                #shared_weights <- sum(unlist(sapply(shared_words,function(x) weights_train[weights_train$word == x,]$weight)))
                total_weights <- weights_train[word %in% q1words,sum(weight)] + weights_train[word %in% q2words,sum(weight)]
                #total_weights <- sum(unlist(sapply(q1words,function(x) weights_train[weights_train$word == x,]$weight))) +
                                #sum(unlist(sapply(q2words,function(x) weights_train[weights_train$word == x,]$weight)))
                        
                A <- shared_weights/total_weights
                B <- length(shared_words)/(length(q1words) + length(q2words))
                C <- length(shared_words)
                D1 <- length(q1stops) / length(q1words)
                D2 <- length(q2stops) / length(q2words) 
                
                return (paste(c(A,B,C,D1,D2),collapse = ":"))
        }
        mapply(foo,q1,q2)
}

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
####### Parse text/create variables  #######
############################################
train <- train_
test <- test_

## prepare data sets
train <- train %>%
        dplyr::slice(1:101) %>%
        mutate(question1 = fullstop(wtoken(question1), FALSE)
               ,question2 = fullstop(wtoken(question2), FALSE)
        
        )

test <- test %>%
        dplyr::slice(1:1001) %>%
        mutate(question1 = fullstop(wtoken(question1), FALSE)
               ,question2 = fullstop(wtoken(question2), FALSE)
        )

## create weights for all words in data sets
weights_train <- train %>%
                select(question1,question2) %>%
                unlist() %>%
                table() %>%
                as.data.frame() %>%
                mutate(weight = get_weight(Freq)) %>%
                select(-Freq)
colnames(weights_train) <- c("word","weight")
weights_train <- weights_train %>%
                mutate(word = as.character(word)) %>%
                as.data.table()
#setkey(weights_train,word)

weights_test <- test %>%
        select(question1,question2) %>%
        unlist() %>%
        table() %>%
        as.data.frame() %>%
        mutate(weight = get_weight(Freq)) %>%
        select(-Freq)%>%
        as.data.table()
colnames(weights_test) <- c("word","weight")

weights_test <- weights_test %>%
        mutate(word = as.character(word))

                
tic()
## calculate vars for training data
train_c <- train %>%
        select(question1,question2) %>%
        mutate(word_shares = word_shares(question1,question2),
               tfidf_word_match = as.numeric(unlist(lapply(word_shares, function(x) unlist(stringr::str_split(x,pattern=":"))[1]))),
               word_match = as.numeric(unlist(lapply(word_shares, function(x) unlist(stringr::str_split(x,pattern=":"))[2]))),
               shared_count = as.numeric(unlist(lapply(word_shares, function(x) unlist(stringr::str_split(x,pattern=":"))[3]))),
               stops1_ratio = as.numeric(unlist(lapply(word_shares, function(x) unlist(stringr::str_split(x,pattern=":"))[4]))),
               stops2_ratio = as.numeric(unlist(lapply(word_shares, function(x) unlist(stringr::str_split(x,pattern=":"))[5]))),
               diff_stops_r = stops1_ratio-stops2_ratio)
        


toc()
