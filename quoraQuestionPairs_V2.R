# ideas: https://www.kaggle.com/tanlikesmath/quora-question-pairs/xgb-starter-12357/code
#        https://www.kaggle.com/axelius/quora-question-pairs/blehhhh/code same as https://www.kaggle.com/hyoung/quora-question-pairs/xgb-with-whq-jaccard

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
library("wordnet")
setDict("/usr/local/Cellar/wordnet/3.1")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

# create an outersect function
outersect <- function(x, y) {
        sort(c(setdiff(x, y),
               setdiff(y, x)))
}

# create a 2gram function
twograms_question <- function(q) {
        twograms <- as.character(1:(length(q)-1))
        for(i in 1:(length(q)-1)) {
                twograms[i] <- paste(c(q[i],q[i+1]),collapse = "")
        }
        return(twograms)
}

compare_synonyms <- function(q1,q2) {
        foo <- function(q1,q2) {
                for(i in length(q1)) {
                        bar <- synonyms(q1[i],"NOUN")
                        sum(bar %in% q2)
                }
        }        
        mapply(foo,q1,q2)
}

## create function, that splits the words of individual questions and calculates their individual shares and their ratios
word_shares <- function(q1,q2) {
        foo <- function(q1,q2) {
                q1words <- setdiff(q1,stopwords)
                q2words <- setdiff(q2,stopwords)
                if(length(q1words) == 0) return(paste(c(0,0,0,0,0,0,0,0,0),collapse = ":"))
                if(length(q2words) == 0) return(paste(c(0,0,0,0,0,0,0,0,0),collapse = ":"))
                
                q1stops <- intersect(q1,stopwords)
                q2stops <- intersect(q2,stopwords)
                
                # 2grams - pairs of 2 subsequent words in the sentences
                q1_2gram <- twograms_question(q1)
                q2_2gram <- twograms_question(q2)
                shared_2grams <- intersect(q1_2gram,q2_2gram)
                
                shared_words <- intersect(q1words,q2words)
                non_shared_words <- outersect(q1words,q2words)
                
                # include a Hamming distance (https://en.wikipedia.org/wiki/Hamming_distance) type word comparison
                words_hamming <- sum(q1[1:min(length(q1), length(q2))] == q2[1:min(length(q1), length(q2))])/max(length(q1),length(q2))
                
                #synonyms of wordnet, pos can be either "ADJECTIVE", "ADVERB", "NOUN", or "VERB"
                non_shared_q1 <- setdiff(q1words,q2words)
                non_shared_q2 <- setdiff(q2words,q1words)
                
                # for every of the non_shared_q1, get all synomyms with synonyms(word, pos) [check for all four categories]
                # and check if they are in non_shared_q2. Sum up the hits. do it the other way around two. maybe write a function for that.
                # calculate again for both the factor over the sum of both non_shared, which is non_shared_words
                
                shared_weights <- weights_train[word %in% shared_words,sum(weight)]
                non_shared_weights <- weights_train[word %in% non_shared_words,sum(weight)]
                q1_weights <- weights_train[word %in% q1words,sum(weight)]
                q2_weights <- weights_train[word %in% q2words,sum(weight)]
                total_weights <- q1_weights + q2_weights
                
                if(total_weights == 0) {
                        A <- 0
                        E <- 0
                } else {
                        A <- shared_weights/total_weights
                        E <- non_shared_weights/total_weights
                }
                if((length(q1words) + length(q2words)) == 0) {
                        B <- 0
                } else {
                        B <- length(shared_words)/(length(q1words) + length(q2words))
                }
                C <- length(shared_words)
                D1 <- length(q1stops) / length(q1words)
                D2 <- length(q2stops) / length(q2words)
                F_ <- words_hamming
                if( (length(q1_2gram)+length(q2_2gram)) == 0 ) {
                        G <- 0
                } else {
                        G <- length(shared_2grams) / (length(q1_2gram) + length(q2_2gram))
                }
                if (q1_weights == 0 | q2_weights==0) {
                        H <- 0
                } else {
                        H <- shared_weights^2/q1_weights/q2_weights
                }

                return (paste(c(A,B,C,D1,D2,E,F_,G,H),collapse = ":"))
        }
        mapply(foo,q1,q2)
}

phrase_length <- function(q) {
        foo <- function(q) {
                length_q <- nchar(q[[1]])
        }
        mapply(foo,q)
}

avg_word_length <- function(q) {
        foo <- function(q) {
                length_words <- sum(nchar(q))
                count_words <- length(q)
                avg_word_length <- length_words / count_words
                return(paste(c(length_words,count_words,avg_word_length),collapse = ":"))
        }
        mapply(foo,q)
}

## function to check for same interrogatives
check_interrogative <- function(q,interrogative) {
        foo <- function(q,interrogative) {
                check_q <- (interrogative %in% q)*1
        }
        mapply(foo,q,interrogative)
}

## predictor function
pred_thres <- function(x,thres=0.5) {
        ifelse(x > thres,1,0)
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
## saving the original
train <- train_
test <- test_

#choice to slice and number of test examples
slice_flag <- FALSE
ex <- 1001

## looking at the original questions as one string
if(slice_flag) {
        train <- train %>%
                dplyr::slice((1:ex)) %>%
                mutate(len_q1 = phrase_length(question1),
                       len_q2 = phrase_length(question2),
                       diff_len = len_q1 - len_q2)
        test <- test %>%
                dplyr::slice((1:ex)) %>%
                mutate(len_q1 = phrase_length(question1),
                       len_q2 = phrase_length(question2),
                       diff_len = len_q1 - len_q2)
} else {
        train <- train %>%
                mutate(len_q1 = phrase_length(question1),
                       len_q2 = phrase_length(question2),
                       diff_len = len_q1 - len_q2)
        test <- test %>%
                mutate(len_q1 = phrase_length(question1),
                       len_q2 = phrase_length(question2),
                       diff_len = len_q1 - len_q2)
        }

## prepare data sets with split up words
train <- train %>%
        mutate(question1 = fullstop(wtoken(question1), FALSE)
               ,question2 = fullstop(wtoken(question2), FALSE)
        )

test <- test %>%
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
        select(is_duplicate,question1,question2,len_q1,len_q2,diff_len) %>%
        mutate(is_duplicate = as.numeric(is_duplicate),
               word_shares_ = word_shares(question1,question2),
               tfidf_word_match = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[1]))),
               tfidf_word_match_2root = sqrt(tfidf_word_match),
               tfidf_non_word_match = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[6]))),
               tfidf_non_word_match_2root = sqrt(tfidf_non_word_match),
               word_match = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[2]))),
               word_match_2root = sqrt(word_match),
               shared_count = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[3]))),
               stops1_ratio = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[4]))),
               stops2_ratio = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[5]))),
               diff_stops_r = stops1_ratio-stops2_ratio,
               words_hamming = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[7]))),
               shared_2grams = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[8]))),
               cosine = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[9]))),
               avg_word_length_q1_ = avg_word_length(question1),
               length_words_q1 = as.numeric(unlist(lapply(avg_word_length_q1_, function(x) unlist(stringr::str_split(x,pattern=":"))[1]))),
               count_words_q1 = as.numeric(unlist(lapply(avg_word_length_q1_, function(x) unlist(stringr::str_split(x,pattern=":"))[2]))),
               avg_word_length_q1 = as.numeric(unlist(lapply(avg_word_length_q1_, function(x) unlist(stringr::str_split(x,pattern=":"))[3]))),
               avg_word_length_q2_ = avg_word_length(question2),
               length_words_q2 = as.numeric(unlist(lapply(avg_word_length_q2_, function(x) unlist(stringr::str_split(x,pattern=":"))[1]))),
               count_words_q2 = as.numeric(unlist(lapply(avg_word_length_q2_, function(x) unlist(stringr::str_split(x,pattern=":"))[2]))),
               avg_word_length_q2 = as.numeric(unlist(lapply(avg_word_length_q2_, function(x) unlist(stringr::str_split(x,pattern=":"))[3]))),
               diff_length_words = length_words_q1-length_words_q2,
               diff_count_words = count_words_q1-count_words_q2,
               diff_avg_word_length = avg_word_length_q1-avg_word_length_q2,
               what_check_q1 = check_interrogative(question1,"what"),
               what_check_q2 = check_interrogative(question2,"what"),
               what_check_product = what_check_q1*what_check_q2,
               how_check_q1 = check_interrogative(question1,"how"),
               how_check_q2 = check_interrogative(question2,"how"),
               how_check_product = how_check_q1*how_check_q2,
               which_check_q1 = check_interrogative(question1,"which"),
               which_check_q2 = check_interrogative(question2,"which"),
               which_check_product = which_check_q1*which_check_q2,
               who_check_q1 = check_interrogative(question1,"who"),
               who_check_q2 = check_interrogative(question2,"who"),
               who_check_product = who_check_q1*who_check_q2,
               where_check_q1 = check_interrogative(question1,"where"),
               where_check_q2 = check_interrogative(question2,"where"),
               where_check_product = where_check_q1*where_check_q2,
               when_check_q1 = check_interrogative(question1,"when"),
               when_check_q2 = check_interrogative(question2,"when"),
               when_check_product = when_check_q1*when_check_q2,
               why_check_q1 = check_interrogative(question1,"why"),
               why_check_q2 = check_interrogative(question2,"why"),
               why_check_product = why_check_q1*why_check_q2) %>%
               select(-question1,-question2,-word_shares_,-avg_word_length_q1_,-avg_word_length_q2_)
        
# save train data
fwrite(train_c,"/Users/benwo/Dropbox/DataScience/Kaggle_QuoraQuestionPairs_local/data/train_c.csv")

## calculate vars for test data
test_c <- test %>%
        select(question1,question2,len_q1,len_q2,diff_len) %>%
        mutate(word_shares_ = word_shares(question1,question2),
               tfidf_word_match = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[1]))),
               tfidf_word_match_2root = sqrt(tfidf_word_match),
               tfidf_non_word_match = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[6]))),
               tfidf_non_word_match_2root = sqrt(tfidf_non_word_match),
               word_match = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[2]))),
               word_match_2root = sqrt(word_match),
               shared_count = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[3]))),
               stops1_ratio = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[4]))),
               stops2_ratio = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[5]))),
               diff_stops_r = stops1_ratio-stops2_ratio,
               words_hamming = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[7]))),
               shared_2grams = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[8]))),
               cosine = as.numeric(unlist(lapply(word_shares_, function(x) unlist(stringr::str_split(x,pattern=":"))[9]))),
               avg_word_length_q1_ = avg_word_length(question1),
               length_words_q1 = as.numeric(unlist(lapply(avg_word_length_q1_, function(x) unlist(stringr::str_split(x,pattern=":"))[1]))),
               count_words_q1 = as.numeric(unlist(lapply(avg_word_length_q1_, function(x) unlist(stringr::str_split(x,pattern=":"))[2]))),
               avg_word_length_q1 = as.numeric(unlist(lapply(avg_word_length_q1_, function(x) unlist(stringr::str_split(x,pattern=":"))[3]))),
               avg_word_length_q2_ = avg_word_length(question2),
               length_words_q2 = as.numeric(unlist(lapply(avg_word_length_q2_, function(x) unlist(stringr::str_split(x,pattern=":"))[1]))),
               count_words_q2 = as.numeric(unlist(lapply(avg_word_length_q2_, function(x) unlist(stringr::str_split(x,pattern=":"))[2]))),
               avg_word_length_q2 = as.numeric(unlist(lapply(avg_word_length_q2_, function(x) unlist(stringr::str_split(x,pattern=":"))[3]))),
               diff_length_words = length_words_q1-length_words_q2,
               diff_count_words = count_words_q1-count_words_q2,
               diff_avg_word_length = avg_word_length_q1-avg_word_length_q2,
               what_check_q1 = check_interrogative(question1,"what"),
               what_check_q2 = check_interrogative(question2,"what"),
               what_check_product = what_check_q1*what_check_q2,
               how_check_q1 = check_interrogative(question1,"how"),
               how_check_q2 = check_interrogative(question2,"how"),
               how_check_product = how_check_q1*how_check_q2,
               which_check_q1 = check_interrogative(question1,"which"),
               which_check_q2 = check_interrogative(question2,"which"),
               which_check_product = which_check_q1*which_check_q2,
               who_check_q1 = check_interrogative(question1,"who"),
               who_check_q2 = check_interrogative(question2,"who"),
               who_check_product = who_check_q1*who_check_q2,
               where_check_q1 = check_interrogative(question1,"where"),
               where_check_q2 = check_interrogative(question2,"where"),
               where_check_product = where_check_q1*where_check_q2,
               when_check_q1 = check_interrogative(question1,"when"),
               when_check_q2 = check_interrogative(question2,"when"),
               when_check_product = when_check_q1*when_check_q2,
               why_check_q1 = check_interrogative(question1,"why"),
               why_check_q2 = check_interrogative(question2,"why"),
               why_check_product = why_check_q1*why_check_q2) %>%
               select(-question1,-question2,-word_shares_,-avg_word_length_q1_,-avg_word_length_q2_)

# save train data
fwrite(test_c,"/Users/benwo/Dropbox/DataScience/Kaggle_QuoraQuestionPairs_local/data/test_c.csv")

toc()

########################################
####### Training the model  ############
########################################

## using XGBoost algorithm (according to https://cran.r-project.org/web/packages/xgboost/vignettes/xgboostPresentation.html)

# create data matrix for xgb model
train.model <- xgb.DMatrix(data = as.matrix(train_c %>% select(-is_duplicate)), label = train_c$is_duplicate)

#nthread = 8  #if not set, use of all threads
nfold = 5
nround = 500
max_depth = 7
eta = 0.5
seed = 12357

# set the model parameters
params <- list(objective = "binary:logistic",
               eval_metric = "logloss",
               max_depth = max_depth,
               eta = eta,
               seed = seed
)

# do a cross-validation run to find the optimal parameters
cv <- xgb.cv(train.model, params = params, nfold = nfold, nround = nround)

# train the final model
xgbModel <- xgboost(train.model, max.depth = max_depth, eta = eta, nround = nround,nfold = nfold, seed = seed, objective = "binary:logistic")

# importance of individual features
importance <- xgb.importance(feature_names = colnames(train.model), model = xgbModel)
importance

########################################
####### Predicting the test set ########
########################################

# prediction on test data
predictions <- predict(xgbModel, newdata = as.matrix(test_c))

# prepare submission
if(slice_flag) {
        submission <- mutate(sample_submission_[1:ex,],is_duplicate = predictions)
} else {
        submission <- mutate(sample_submission_,is_duplicate = predictions)
}

# save submission
fwrite(submission,"/Users/benwo/Dropbox/DataScience/Kaggle_QuoraQuestionPairs_local/data/submission.csv")
