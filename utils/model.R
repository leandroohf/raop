##* ****************************************************************
##  Programmer[s]: Leandro Fernandes
##  Company/Institution: Cargill
##  email: leandro_h_fernandes@cargill.com
##  Date: June 20, 2016
##  
##  The author believes that share code and knowledge is awesome.
##  Feel free to share and modify this piece of code. But don't be
##  impolite and remember to cite the author and give him his credits.
##* ****************************************************************

RAoPModel <- function(train.data, val.data, resp.var){

    stopifnot(resp.var %in% names(train.data))
    stopifnot(names(val.data) %in% names(train.data))
    
    predictors.name <- names(train.data)
    predictors.name <- predictors.name[predictors.name != resp.var]

    ## TODO Refator it: Pass formula as parameter
    m14 <- glm(requester_received_pizza ~
               requester_upvotes_minus_downvotes_at_request +
               nword +
               requester_account_age_in_days_at_request +
               money.score +
               post.sent +
               has.link +
               first.half.of.month+
               posted.raop.before,
           family=binomial(link='logit'),
           data=train.data)
    
    list(
        GetResponseVar = function(){
            return(resp.var)
        },
        GetData = function(){
            return(list(train.data,val.data))
        },
        GetGlmObject = function(){
            return(m14)
        },
        GetPrediction = function(new.data){
            stopifnot(predictors.name %in% names(new.data))
            pred <- predict(m14,new.data, type = 'response')
            return(pred)
        },
        GetPredictorsName = function(new.data){
            return(predictors.name)
        }
    )
}
