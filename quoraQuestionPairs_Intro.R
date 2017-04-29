
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries & auxiliary functions -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rm(list=ls())

require("dplyr")
require("data.table")
require("xgboost")

require("tokenizers")
require("tm")
require("SnowballC")
require("wordcloud")
require("RColorBrewer")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Parameters -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

privateMac = FALSE

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Auxiliary functions -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## ascii handler function
get_ascii <- function(x, invert = FALSE) {
        i <- grep("\\+", iconv(x, "latin1", "ASCII", "+"),
                  invert = !invert)
        if (all(vapply(i, length, double(1)) == 0))
                return(NA_character_)
        x <- iconv(x, "latin1", "utf-8", " ")
        x[i]
}
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
## word counts
nword <- function(q) {
        vapply(q, length, double(1))
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
## count matches
nmatch <- function(q1, q2) {
        ## sum matches fun
        f1 <- function(q1, q2) {
                if (!any(q1 %in% q2)) return(0)
                sums <- c(sum(q1 %in% q2, na.rm = TRUE),
                          sum(q2 %in% q1, na.rm = TRUE))
                sums[which.max(sums)]
        }
        ## return # of matches
        x <- mapply(f1, q1, q2)
        ## return as numeric
        as.numeric(x)
}
## random sample
rsamp <- function(x, n = 100) x[sample(seq_len(nrow(x)), n), ]
## match big words
bigwordmatch <- function(q1, q2) {
        ## function to get biggest word
        bigword <- function(x) x[order(nchar(x))][1]
        ## function to collapse other big words
        pc <- function(x, n = 2) {
                if (length(x) < n) n <- length(x)
                x <- x[order(nchar(x))][seq_len(n)]
                paste(x, collapse = "")
        }
        ## function to match big words
        f1 <- function(q1, q2) {
                all(grepl(bigword(q1), pc(q2)),
                    grepl(bigword(q2), pc(q1)))
        }
        ## apply to each pair
        f2 <- function(q1, q2) mapply(f1, q1, q2)
        ## return # of big word matches
        as.numeric(f2(q1, q2))
}
## chunker
chunker <- function(q1, q2) {
        f0 <- function(x) strsplit(paste(x, collapse = ""), "")
        q1 <- lapply(q1, f0)
        q1 <- unlist(q1, recursive = FALSE)
        q2 <- lapply(q2, f0)
        q2 <- unlist(q2, recursive = FALSE)
        f1 <- function(n) {
                n <- n[n != " "]
                x <- lapply(seq(0, length(n), 3), function(x) c(1:3) + x)
                x <- lapply(x, function(i) paste(n[i], collapse = ""))
                grep("NA", x, invert = TRUE, value = TRUE)
        }
        ## apply function
        q1 <- lapply(q1, f1)
        q2 <- lapply(q2, f1)
        ## matching fun
        matching <- function(a, b) {
                sum(a %in% b, na.rm = TRUE)
        }
        x <- mapply(matching, q1, q2)
        as.numeric(x)
}
## who what when where why how
wwwwwh <- function(q1, q2) {
        types <- c("who", "what", "when", "where", "why", "how")
        w <- function(a, b, c) {
                mapply(function(a, b) grepl(c, paste(a, collapse = " ")) &
                               grepl(c, paste(b, collapse = " ")),
                       q1, q2, USE.NAMES = FALSE)
        }
        w1 <- w(q1, q2, "who")
        w2 <- w(q1, q2, "what")
        w3 <- w(q1, q2, "when")
        w4 <- w(q1, q2, "where")
        w5 <- w(q1, q2, "why")
        w6 <- w(q1, q2, "how")
        mapply(sum, w1, w2, w3, w4, w5, w6, na.rm = TRUE,
               USE.NAMES = FALSE)
}
## first word match
wfirst <- function(q1, q2) {
        mapply(function(a, b) a[1] == b[1], q1, q2, USE.NAMES = FALSE)
}
## last word match
wlast <- function(q1, q2) {
        mapply(function(a, b) a[length(a)] == b[length(b)],
               q1, q2, USE.NAMES = FALSE)
}
## non words
nonwords <- function(q1, q2) {
        foo <- function(q) vapply(q, function(.)
                grepl("[^[:lower:]]", .), logical(1),
                USE.NAMES = FALSE)
        q1 <- lapply(q1, function(q) sum(foo(q), na.rm = TRUE))
        q2 <- lapply(q2, function(q) sum(foo(q), na.rm = TRUE))
        mapply(function(a, b) min(c(a, b), na.rm = TRUE), q1, q2)
}
## rankcor
wordorder <- function(q1, q2) {
        foo <- function(a, b) {
                x <- list(a = unique(a[a %in% b]),
                          b = unique(b[b %in% a]))
                x <- list(factor(x[[1]], levels = x[[1]]),
                          factor(x[[2]], levels = x[[1]]))
                x <- lapply(x, as.integer)
                x <- cor(x[[1]], x[[2]], method = "kendall")
                x[is.na(x)] <- 0
                x
        }
        mapply(foo, q1, q2)
}
## samesies
samesies <- function(q1, q2) {
        foo <- function(q1, q2) {
                x <- all(q1 %in% q2)
                if (x) return(x)
                q1 <- fullstop(q1)
                q2 <- fullstop(q2)
                if (length(q1) > 0 & length(q2) > 0) return(all(q1 %in% q2))
                FALSE
        }
        mapply(foo, q1, q2)
}

## word intersects
calcIntersect <- function(q1, q2){
        intersect(q1,q2)
}

## logloss function
logloss <- function(actual, predicted, eps = 1e-15) {
        predicted = pmin(pmax(predicted, eps, na.rm = TRUE),
                         1 - eps, na.rm = TRUE)
        - (sum(actual * log(predicted) + (1 - actual) *
                       log(1 - predicted), na.rm = TRUE)) / length(actual)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if (privateMac){
        # Private MacBook
        train_ <- fread(file = "~/Projects/Git/Kaggle_QuoraQuestionPairs/data/train.csv")
        test_ <- fread(file = "~/Projects/Git/Kaggle_QuoraQuestionPairs/data/test.csv")
        sample_submission_ <- fread(file = "~/Projects/Git/Kaggle_QuoraQuestionPairs/data/sample_submission.csv")
        countries_ <- read.delim(file = "~/Projects/Git/Kaggle_QuoraQuestionPairs/countryNames.txt")
} else {
        # Work MacBook
        train_ <- fread(file = "~/Projects/Kaggle/Kaggle_QuoraQuestionPairs/data/train.csv")
        test_ <- fread(file = "~/Projects/Kaggle/Kaggle_QuoraQuestionPairs/data/test.csv")
        sample_submission_ <- fread(file = "~/Projects/Kaggle/Kaggle_QuoraQuestionPairs/data/sample_submission.csv")
        countries_ <- read.delim(file = "~/Projects/Kaggle/Kaggle_QuoraQuestionPairs/countryNames.txt")
}

countries_ <- lapply(countries_, tolower)$countryName
train <-  train_
test <- test_

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
# create wordcloud -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

train <- train %>%
        mutate(question1 = fullstop(wtoken(question1), TRUE)
               ,question2 = fullstop(wtoken(question2), TRUE)
        )

test <- test %>%
        mutate(question1 = fullstop(wtoken(question1), TRUE)
               ,question2 = fullstop(wtoken(question2), TRUE)
        )

allWordsTrain <-  unlist(c(train$question1, train$question2))
allWordsTest <- unlist(c(test$question1, test$question2))

corpusTrain <- Corpus(VectorSource(allWordsTrain))
corpusTest <- Corpus(VectorSource(allWordsTest))

pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]

wc1_Train <- wordcloud(corpus, max.words = 500, random.order = FALSE, color = pal, random.color = TRUE)
wc1_Test <- wordcloud(corpus, max.words = 500, random.order = FALSE, color = pal, random.color = TRUE)

## Include here manually words that appear often, which are later treated differently
specialWordsTrain <- c()
specialWordsTest <- c()

## Or alternatively compute tf-idf in order to classify words in questions that are not part of the intersect between two questions


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# create features -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

train <- train %>%
        mutate(question1 = fullstop(wtoken(question1), FALSE)
                ,question2 = fullstop(wtoken(question2), FALSE)
                ,nchars = nchar_diff(question1, question2)
                ,nmatchpct1 = nmatch1 / nword(question1)
                ,nmatchpct2 = nmatch1 / nword(question2)
                ,nwords = nword_diff(question1, question2)
                ,nmatch2 = nmatch(question1, question2)
                ,nmatch2 = match(question1, question2)
                ,samesies = sames(question1, question2)
                ,last = wlast(question1, question2)
                ,wordcor = wordorder(question1, question2))

train <- train %>%
        mutate(question1 = fullstop(wtoken(question1), FALSE)
               ,question2 = fullstop(wtoken(question2), FALSE)
               )



testing <- train[1:1000,.(question1 = fullstop(wtoken(question1), FALSE)
                  ,question2 = fullstop(wtoken(question2), FALSE)
                  ,intersect = calcIntersect(question1, question2)
                  )]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# train model -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## reduce and format data
train_backup <- train
train <- train %>%
        mutate(is_duplicate = as.numeric(is_duplicate)
                ,nonwords = as.numeric(nonwords)
                ,sames = as.numeric(sames)
                ,last = as.numeric(last)) %>%
                        .[, !names(.) %in% c("id", "qid1", "qid2", "nwords",
                             "question1", "question2")]

# create data matrix for xgb model
train.model <- xgb.DMatrix(data = as.matrix(train %>% select(-is_duplicate)), label = train$is_duplicate)

num_class = 2
nthread = 8
nfold = 5
nround = 20
max_depth = 3
eta = 1

params <- list(objective = "binary:logitraw",
               max_depth = max_depth,
               eta = eta
)

cv <- xgb.cv(train.model, params = params, nthread = nthread, nfold = nfold, nround = nround)

xgbModel <- xgboost(train.model, max.depth = max_depth, eta = eta, nthread = nthread, nround = nround, objective = "binary:logitraw")
importance <- xgb.importance(feature_names = colnames(train.model), model = xgbModel)


