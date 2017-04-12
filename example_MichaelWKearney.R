rm(list=ls())

########################################
####### load packages and data  ########
########################################

## load dplyr and gbm
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(gbm))

## load training data
e <- suppressMessages(readr::read_csv("/Users/bwolter/PhD/private/data/Kaggle_QuoraQuestionPairs/data/train.csv", n_max = 100000))

## read stop words
stopwords <- "a a's able about above according im accordingly across actually after afterwards again against ain't all allow allows almost alone along already also although always am among amongst an and another any anybody anyhow anyone anything anyway anyways anywhere amp apart appear appreciate appropriate are aren't around as aside ask asking associated at available away awfully b be became because become becomes becoming been before beforehand behind being believe below beside besides best better between beyond both brief but by c c'mon c's came can can't cannot cant cause causes certain certainly changes clearly co com come comes concerning consequently consider considering contain containing contains corresponding could couldn't course currently d definitely described despite did didn't different do does doesn't doing don't done down downwards during e each edu eg eight either else elsewhere enough entirely especially et etc even ever every everybody everyone everything everywhere ex exactly example except f far few fifth first five followed following follows for former formerly forth four from further furthermore g get gets getting given gives go goes going gone got gotten greetings h had hadn't happens hardly has hasn't have haven't having he he's hello help hence her here here's hereafter hereby herein hereupon hers herself hi him himself his hither hopefully how howbeit however i i'd i'll i'm i've ie if ignored immediate in inasmuch inc indeed indicate indicated indicates inner insofar instead into inward is isn't it it'd it'll it's its itself j just k keep keeps kept know knows known l last lately later latter latterly least less lest let let's like liked likely little look looking looks ltd m mainly many may maybe me mean meanwhile merely might more moreover most mostly much must my myself n name namely nd near nearly necessary need needs neither never nevertheless new next nine no nobody non none noone nor normally not nothing novel now nowhere o obviously of off often oh ok okay old on once one ones only onto or other others otherwise ought our ours ourselves out outside over overall own p particular particularly per perhaps placed please plus possible presumably probably provides q que quite qv r rather rd re they'd they'll they're they've try trying twice two u un under unfortunately unless unlikely until unto up upon us use used useful uses using usually uucp v value various very via viz vs w want wants was wasn't way we we'd we'll we're we've welcome well went were weren't what what's whatever when whence whenever where where's whereafter whereas whereby wherein whereupon wherever whether which while whither who who's whoever whole whom whose why will willing wish with within without won't wonder would would wouldn't x y yes yet you you'd you'll you're you've your yours yourself yourselves z zero really reasonably regarding regardless regards relatively respectively right s said same saw say saying says second secondly see seeing seem seemed seeming seems seen self selves sensible sent serious seriously seven several shall she should shouldn't since six so some somebody somehow someone something sometime sometimes somewhat somewhere soon sorry specified specify specifying still sub such sup sure t t's take taken tell tends th than thank thanks thanx that that's thats the their theirs them themselves then thence there there's thereafter thereby therefore therein theres thereupon these they think third this thorough thoroughly those though three through throughout thru thus to together too took toward towards tried tries truly"
## split string to create stop words vector
stopwords <- strsplit(stopwords, " ")[[1]]


########################################
####### User defined functions  ########
########################################

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
## logloss function
logloss <- function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps, na.rm = TRUE),
                   1 - eps, na.rm = TRUE)
  - (sum(actual * log(predicted) + (1 - actual) *
           log(1 - predicted), na.rm = TRUE)) / length(actual)
}

########################################
####### Parse text/create variables  ###
########################################

## calculate vars for training data
e <- e %>%
  mutate(nchars = nchar_diff(question1, question2),
         nmatch1 = nmatch(fullstop(wtoken(question1)),
                          fullstop(wtoken(question2))),
         question1 = fullstop(wtoken(question1), FALSE),
         question2 = fullstop(wtoken(question2), FALSE),
         nmatchpct1 = nmatch1 / nword(question1),
         nmatchpct2 = nmatch1 / nword(question2)) %>%
  mutate(nwords = nword_diff(question1, question2),
         nmatch2 = nmatch(question1, question2),
         bigword = bigwordmatch(question1, question2),
         chunks = chunker(question1, question2),
         sames = samesies(question1, question2),
         first = wfirst(question1, question2),
         last = wlast(question1, question2),
         wordcor = wordorder(question1, question2),
         nonwords = nonwords(question1, question2))

########################################
####### Training the model  ############
########################################

## reduce and format data
e <- e %>%
  mutate(sames = as.factor(sames),
         first = as.factor(first),
         last = as.factor(last)) %>%
  .[, !names(.) %in% c("id", "qid1", "qid2", "nwords",
                       "question1", "question2")]

## create test set #1
d1 <- e %>%
  rsamp(1000)

## create test set #2
d2 <- e %>%
  rsamp(1000)

## specify trees
ntrees <- 1000

## boosted gradiant model
mod <- gbm(is_duplicate ~ .,
           data = e,
           interaction.depth = 5,
           shrinkage = 0.1,
           n.minobsinnode = 10,
           bag.fraction = 0.5,
           train.fraction = 0.5,
           cv.folds = 5,
           distribution = "bernoulli",
           n.trees = ntrees)

## save model
#saveRDS(mod, "../input/mod.rds")

## view model results
summary(mod)

## best iteration
best.iter <- gbm.perf(mod, method = "cv")