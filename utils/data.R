library(stringr, quietly = TRUE )
library(stringi, quietly = TRUE )

BuildDataTarget <- function(raop.df, pos.words, neg.words){

    ## Defensive programming
    stopifnot( "requester_received_pizza" %in% names(raop.df) )

    raop.df <- BuildNewFeatures(raop.df, pos.words, neg.words)
    
    cat("Selecting target vars... \n")
    cols.target <- c("in_test_set","request_id","request_text",
                     "requester_account_age_in_days_at_request",
                     "requester_days_since_first_post_on_raop_at_request",
                     "requester_number_of_posts_at_request",
                     "requester_number_of_posts_on_raop_at_request",
                     "requester_received_pizza",
                     "requester_upvotes_minus_downvotes_at_request",
                     "requester_username","nword", "has.link",
                     "request.date", "first.half.of.month",
                     "posted.raop.before", "post.sent", "is.weekend",
                     "desire.score","family.score","job.score","money.score",
                     "student.score")
    
    raop.target <- raop.df[,cols.target]

    return(raop.target)
}

BuildNewFeatures <- function(raop.df, pos.words, neg.words){

    ## TODO Add stopifnot clause to check data interface
    ## (Defensice programming)
    cols.check <- c("in_test_set","request_id","request_text",
                    "requester_account_age_in_days_at_request",
                    "requester_days_since_first_post_on_raop_at_request",
                    "requester_number_of_posts_at_request",
                    "requester_number_of_posts_on_raop_at_request",
                    "requester_upvotes_minus_downvotes_at_request",
                    "requester_username")
    
    stopifnot( cols.check %in% names(raop.df) )
    
    ## TODO Refactor this code
    desire.words  <- readLines("./dict/desire.txt")
    family.words  <- readLines("./dict/family.txt")
    job.words     <- readLines("./dict/job.txt")
    money.words   <- readLines("./dict/money.txt")
    student.words <- readLines("./dict/student.txt")

    cat("Features engineering...\n")
    raop.df$nword          <- str_count(raop.df[,"request_text"], "\\S+")
    raop.df$has.link       <- str_detect( raop.df[,"request_text"], "https?://")

    raop.df$request.date   <- as.POSIXct(raop.df[,"unix_timestamp_of_request"],
                                         origin="1970-01-01", tz = "UTC")
    raop.df$first.half.of.month <- (lubridate::day(raop.df$request.date) < 16)
    raop.df$is.weekend          <- (lubridate::wday(raop.df$request.date) %in%  c(1,6,7))

    raop.df$posted.raop.before  <- raop.df[,"requester_number_of_posts_on_raop_at_request"] > 0

    cat("Text engineering...\n")
    raop.corpus <- GetCleanedCorpus(raop.df)
    raop.df$post.sent <- GetSentimentScoreFromCorpus(raop.corpus, pos.words, neg.words)

    raop.df$desire.score  <- GetNarrativesScoreFromCorpus(raop.corpus, desire.words)
    raop.df$family.score  <- GetNarrativesScoreFromCorpus(raop.corpus, family.words)
    raop.df$job.score     <- GetNarrativesScoreFromCorpus(raop.corpus, job.words)
    raop.df$money.score   <- GetNarrativesScoreFromCorpus(raop.corpus, money.words)
    raop.df$student.score <- GetNarrativesScoreFromCorpus(raop.corpus, student.words)

    return(raop.df)
}

DesignData <- function(raop.target){
    
    ## TODO Read from settings.json file
    cols.pred <- c("requester_received_pizza",
                   "requester_account_age_in_days_at_request",
                   "requester_number_of_posts_at_request",
                   "requester_upvotes_minus_downvotes_at_request",
                   "nword", "has.link",
                   "first.half.of.month",
                   "posted.raop.before", "post.sent","is.weekend",
                   "desire.score","family.score","money.score",
                   "job.score", "student.score")

    
    cat("Selecting model candidates vars ... \n")
    dev.data <- raop.target %>%
        dplyr::select( dplyr::one_of(cols.pred))

    dev.data <- BalanceDataClass(dev.data)

    ## XXX Improve this code. I need to pass the max n min used in
    ## train phase for new data. So I need to pass thereference !?
    ## This is a temp solution. I ma wasting memory and performance
    ## but reducing implementation cost
    dev.data <- TransformNumericalVars(dev.data,dev.data)
    split.list <- SplitData(dev.data) ## <= list(train.data, val.data)

    return(split.list)
}

BalanceDataClass <- function(raop.target){
    
    ## TODO Read from settings.json file
    karma.thr <- 2889
    nword.thr <- 200
    age.thr   <- 965
    npost_at_request <- 60
    
    cat("Balancing data and removing outliers ... \n")
    ## Balance class 0, 1
    dev.data <- raop.target %>%
        dplyr::filter( requester_received_pizza == TRUE) %>%
        dplyr::filter( requester_upvotes_minus_downvotes_at_request < karma.thr) %>%
        dplyr::filter( nword < nword.thr) %>%
        dplyr::filter( requester_account_age_in_days_at_request < age.thr) %>%
        dplyr::filter( requester_number_of_posts_at_request < npost_at_request)## %>%
        ##dplyr::select( dplyr::one_of(cols.pred))

    samp.data <- raop.target %>%
        dplyr::filter( requester_received_pizza == FALSE) %>%
        dplyr::filter( requester_upvotes_minus_downvotes_at_request < karma.thr) %>%
        dplyr::filter( nword < nword.thr) %>%
        dplyr::filter( requester_account_age_in_days_at_request < age.thr) %>%
        dplyr::filter( requester_number_of_posts_at_request < npost_at_request) %>%
        ##dplyr::select( dplyr::one_of(cols.pred)) %>%
        dplyr::sample_n(nrow(dev.data),replace=FALSE)

    dev.data <- rbind(dev.data,samp.data)
    
    return(dev.data)
}

TransformNumericalVars <- function(dev.data,train.data){

    ## Tranforms vars
    cat("Mapping numerical columns to 0 - 1 range ... \n")
    cols.to.transform <- c("requester_account_age_in_days_at_request",
                           "requester_number_of_posts_at_request",
                           "requester_upvotes_minus_downvotes_at_request",
                           "nword", 
                           "post.sent")
    
    for( cc in cols.to.transform){
        print(cc)
        x <- unlist((dev.data[, cc]))
        xref <- unlist((train.data[, cc]))
        dev.data[, cc] <- TransformVariable( x, max(xref), min(xref) )
    }
    
    return(dev.data)
}


SplitData <- function(dev.data, ratio_ = 0.70, seed_ = 13){

    cat("Spliting data in train n test ... \n")
    train.size <- round(nrow(dev.data)*ratio_)
    val.size   <- nrow(dev.data) - train.size
    
    set.seed(seed_)
    r <- sample(nrow(dev.data),train.size)

    train.data <- dev.data[r,]
    val.data   <- dev.data[-r,]

    return(list(train.data,val.data))
}
