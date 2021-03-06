library(stringr, quietly = TRUE )
library(stringi, quietly = TRUE )
library(assertthat, quietly = TRUE )

RAoPDataEngineer <- function(raop.df, sent.dict, narrative.dict,
                             raop.settings){
    
    raop.target <- RAoPDataEngineer_BuildDataTarget(raop.df, sent.dict,
                                                    narrative.dict,
                                                    raop.settings)
    
    list(
        GetDataTarget = function(){
            return(raop.target)
        },
        ## TODO Pass ratio_ n seed_ as args (Now is ignored)
        GetDesignData = function( balance_ = FALSE, seed_ = -1){

            data.split.list <- RAoPDataEngineer_DesignData( raop.target,
                                                           raop.settings,
                                                           balance_,
                                                           seed_)
            
            train.data      <- data.split.list[[1]]
            val.data        <- data.split.list[[2]]
            
            return(data.split.list)
        },
        PreProcessNewData = function(newdata.df){
            new.data <- RAoPDataEngineer_PreProcessNewData(newdata.df,
                                                           sent.dict,
                                                           narrative.dict,
                                                           raop.settings)
            return(new.data)
        }
    )
}

RAoPDataEngineer_BuildDataTarget <- function(raop.df, sent.dict, narrative.dict,
                                             raop.settings){
    
    ## Defensive programming
    stopifnot( "requester_received_pizza" %in% names(raop.df) )

    raop.df <- RAoPDataEngineer_BuildNewFeatures(raop.df, sent.dict, narrative.dict)

    ## Convert
    cat("Convert numerical vars to decile...\n")
    print(raop.settings$cols_to_transform)
    raop.df <- RAoPDataEngineer_ConvertToDecile(raop.df,raop.df,
                                                raop.settings$cols_to_transform)
    
    cat("Selecting target vars... \n")    
    raop.target <- raop.df %>%
        dplyr::select( dplyr::one_of( raop.settings$cols_target ))
    
    
    return(raop.target)
}

RAoPDataEngineer_BuildNewFeatures <- function(raop.df, sent.dict, narrative.dict){
    
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
    raop.df$nword <- str_count(raop.df[,"request_text"], "\\S+")

    raop.df$posted.raop.before <- raop.df[,"requester_number_of_posts_on_raop_at_request"] > 0

    raop.df <- RAoPDataEngineer_BuildTimeFeatures(raop.df)
    
    cat("Text engineering...\n")
    raop.df <- RAoPDataEngineer_BuildTextFeatures(raop.df, sent.dict, narrative.dict)

    return(raop.df)
}

RAoPDataEngineer_BuildTimeFeatures <- function(raop.df){

    raop.df$request.date   <- as.Date(as.POSIXct(raop.df[,"unix_timestamp_of_request"],
                                                 origin="1970-01-01", tz = "UTC"))
    
    raop.df$community_age         <- as.numeric(raop.df$request.date - as.Date('2010-12-08'))

    raop.df$first.half.of.month   <- (lubridate::day(raop.df$request.date) < 16)
    raop.df$is.weekend            <- (lubridate::wday(raop.df$request.date) %in%  c(1,6,7))

    
    return(raop.df)
}

RAoPDataEngineer_BuildTextFeatures <- function(raop.df, sent.dict, narrative.dict){

    raop.df$gratitude <- str_detect( raop.df[,"request_text"],
                                    "thank|appreciate|advance")


    raop.df$reciprocity <- str_detect( raop.df[,"request_text"],
                                      "pay.+forward|pay.+back|return.+favor|repay")
    
    
    raop.df$has.link   <- str_detect( raop.df[,"request_text"], "https?://")

    raop.corpus        <- GetCleanedCorpus(raop.df$request_text)

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

RAoPDataEngineer_ConvertToDecile <- function(dev.data,ref.data, cols.to.transform){

    cat('inside deciles\n' )
    cat(cols.to.transform)
    ## XXX If start to become slow, optimize it
    for( cc in cols.to.transform ){
        cat(cc,"...\n")
        x              <- unlist((dev.data[, cc]))
        ## Considering only values gretaher than zero
        xref           <- ref.data[ ref.data[, cc] > 0.0, cc]
        decile.x       <- quantile(xref, seq(0, .9, .1))
        dev.data[, cc] <- ConvertToDecile( x, decile.x )
    }
    
    return(dev.data)
}

RAoPDataEngineer_DesignData <- function(raop.target, raop.settings, balance_= FALSE,
                                        seed_= -1){
    
    dev.data <- raop.target
    if(balance_== TRUE){
        dev.data <- RAoPDataEngineer_BalanceDataClass(dev.data)
    }
        
    ## XXX Improve this code. I need to pass the max n min used in
    ## train phase for new data. So I need to pass a reference !?
    ## This is a temp solution. I am wasting memory and performance
    ## but reducing implementation cost
    dev.data   <- RAoPDataEngineer_TransformNumericalVars(dev.data,dev.data,
                                                          raop.settings$cols_to_transform)

    split.list <- RAoPDataEngineer_SplitData(dev.data, seed_ ) ## <= list(train.data, val.data)
    
    ## Select cols to models investigation
    dev.data <- dev.data %>%
        dplyr::select( dplyr::one_of( raop.settings$cols_model_investigation ))
    
    return(split.list)
}

RAoPDataEngineer_BalanceDataClass <- function(raop.target){
    
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

RAoPDataEngineer_TransformNumericalVars <- function(dev.data, train.data, cols_to_transform){

    cat("Mapping numerical columns to 0 - 1 range ... \n")

    ## XXX If start to become slow, optimize it
    for( cc in cols_to_transform ){        
        print(cc)
        x <- unlist((dev.data[, cc]))
        xref <- unlist((train.data[, cc]))
        dev.data[, cc] <- TransformVariable( x, max(xref), min(xref) )
    }
    
    return(dev.data)
}

RAoPDataEngineer_SplitData <- function(dev.data, seed_ = -1){

    ## XXX Not so good practice
    ratio_ = 0.70
    
    cat("Spliting data in train n test ... \n")
    if(seed_ > 0){
        ## random split

        train.size <- round(nrow(dev.data)*ratio_)
        val.size   <- nrow(dev.data) - train.size
    
        set.seed(seed_)
        r <- sample(nrow(dev.data),train.size)

        train.data <- dev.data[r,]
        val.data   <- dev.data[-r,]
        
    }else{
        ## use same test of the paper
        train.data <- dev.data %>% dplyr::filter(in_test_set ==  FALSE)
        val.data   <- dev.data %>% dplyr::filter(in_test_set ==  TRUE)
    }
    
    return(list(train.data,val.data))
}

RAoPDataEngineer_PreProcessNewData <- function(newdata.df,sent.dict,narrative.dict,
                                               raop.settings){
    
    ##newdata.df$requester_received_pizza <- as.numeric(newdata.df$requester_received_pizza)
    
    new.data <- RAoPDataEngineer_BuildNewFeatures(newdata.df, sent.dict,
                                                  narrative.dict)
    
    ## Select target cols
    new.data <- new.data %>%
        dplyr::select( dplyr::one_of( raop.settings$cols_target ))
    
    new.data <- RAoPDataEngineer_TransformNumericalVars(new.data, train.data,
                                                        raop.settings$cols_to_transform)
    
    return(new.data)
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
