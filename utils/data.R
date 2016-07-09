library(stringr, quietly = TRUE )
library(stringi, quietly = TRUE )
library(assertthat, quietly = TRUE )

BuildDataTarget <- function(raop.df, sent.dict, narrative.dict){

    ## Defensive programming
    stopifnot( "requester_received_pizza" %in% names(raop.df) )

    raop.df <- BuildNewFeatures(raop.df, sent.dict, narrative.dict)
    
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

BuildNewFeatures <- function(raop.df, sent.dict, narrative.dict){
    
    ## (Defensive programming)
    cols.check <- c("in_test_set","request_id","request_text",
                    "requester_account_age_in_days_at_request",
                    "requester_days_since_first_post_on_raop_at_request",
                    "requester_number_of_posts_at_request",
                    "requester_number_of_posts_on_raop_at_request",
                    "requester_upvotes_minus_downvotes_at_request",
                    "requester_username")
    
    stopifnot( cols.check %in% names(raop.df) )
    
    cat("Features engineering...\n")
    raop.df$nword          <- str_count(raop.df[,"request_text"], "\\S+")
    raop.df$has.link       <- str_detect( raop.df[,"request_text"], "https?://")

    raop.df$request.date   <- as.POSIXct(raop.df[,"unix_timestamp_of_request"],
                                         origin="1970-01-01", tz = "UTC")
    
    raop.df$first.half.of.month <- (lubridate::day(raop.df$request.date) < 16)
    raop.df$is.weekend          <- (lubridate::wday(raop.df$request.date) %in%  c(1,6,7))

    raop.df$posted.raop.before  <- raop.df[,"requester_number_of_posts_on_raop_at_request"] > 0

    cat("Text engineering...\n")
    raop.corpus       <- GetCleanedCorpus(raop.df)

    ## Sentiment score
    raop.df$post.sent <- GetSentimentScoreFromCorpus(raop.corpus,
                                                     sent.dict$pos,
                                                     sent.dict$neg)
    
    ## Narratives scores
    raop.df$desire.score  <- GetNarrativesScoreFromCorpus(raop.corpus,
                                                          narrative.dict$desire)

    raop.df$family.score  <- GetNarrativesScoreFromCorpus(raop.corpus,
                                                          narrative.dict$family)
    
    raop.df$job.score     <- GetNarrativesScoreFromCorpus(raop.corpus,
                                                          narrative.dict$job)

    raop.df$money.score   <- GetNarrativesScoreFromCorpus(raop.corpus,
                                                          narrative.dict$money)

    raop.df$student.score <- GetNarrativesScoreFromCorpus(raop.corpus,
                                                          narrative.dict$student)

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
    ## train phase for new data. So I need to pass a reference !?
    ## This is a temp solution. I am wasting memory and performance
    ## but reducing implementation cost
    dev.data   <- TransformNumericalVars(dev.data,dev.data)
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

    samp.data <- raop.target %>%
        dplyr::filter( requester_received_pizza == FALSE) %>%
        dplyr::filter( requester_upvotes_minus_downvotes_at_request < karma.thr) %>%
        dplyr::filter( nword < nword.thr) %>%
        dplyr::filter( requester_account_age_in_days_at_request < age.thr) %>%
        dplyr::filter( requester_number_of_posts_at_request < npost_at_request) %>%
        dplyr::sample_n(nrow(dev.data),replace=FALSE)

    dev.data <- rbind(dev.data,samp.data)
    
    return(dev.data)
}

TransformNumericalVars <- function(dev.data,train.data){

    ## TODO Pass cols to transform as args
    ## Tranforms vars
    cat("Mapping numerical columns to 0 - 1 range ... \n")
    cols.to.transform <- c("requester_account_age_in_days_at_request",
                           "requester_number_of_posts_at_request",
                           "requester_upvotes_minus_downvotes_at_request",
                           "nword", 
                           "post.sent")

    ## XXX If start to become slow, optimize it
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

LoadDcitionariesFromSetings <- function(settings){
    
    reqired.cols <- c("pos_dict_path","neg_dict_path",
                      "desire_dict_path",
                      "family_dict_path",
                      "job_dict_path",
                      "money_dict_path",
                      "student_dict_path")
    
    ##assert_that( is.flag (reqired.cols  %in% names(settings)))
    ## TODO use assert_that
    stopifnot( reqired.cols  %in% names(settings))
    
    pos <- readLines(settings$pos_dict_path)
    neg <- readLines(settings$neg_dict_path)
    sent.dict <- list(pos,neg)
    names(sent.dict) <- c("pos","neg")
    
    desire  <- readLines(settings$desire_dict_path)
    family  <- readLines(settings$family_dict_path)
    job     <- readLines(settings$job_dict_path)
    money   <- readLines(settings$money_dict_path)
    student <- readLines(settings$student_dict_path)

    narrative.dict <- list(desire,family,job,money,student)
    names(narrative.dict) <- c("desire","family","job","money",
                               "student")
    
    return(list(sent.dict, narrative.dict))
}
