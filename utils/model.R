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


library(formula.tools, quietly = TRUE )

RAoPModel <- function(glm.formula, train.data, val.data){

    resp.var        <- as.character(formula.tools::lhs(glm.formula))
    predictors.name <- formula.tools::rhs.vars(glm.formula) 
    
    stopifnot(resp.var %in% names(train.data))
    stopifnot(names(val.data) %in% names(train.data))

    m14 <- glm( glm.formula, family=binomial(link='logit'),
               data=train.data)
    
    list(
        GetModelFormula = function(){
            return(glm.formula)
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
