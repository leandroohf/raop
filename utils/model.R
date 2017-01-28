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


CreateRAoPModelDefaultsParams <- function(params, train_data, model_comment){

    params$train_data_dim <- dim(train_data)
    params$features <- paste0(names(train_data), collapse = ", ")
    params$comment <- model_comment
    
    return(params)
}

SaveRAoPModel <- function(model_obj, model_description, model_params_list, settings){
    
    model_date <- lubridate::today()

    model_path <- file.path(settings$model_dir, paste0(model_date,
                                                       "-lhof-",
                                                       model_description,".rds"))
    
    model_json_path <- file.path(settings$model_dir, paste0(model_date,
                                                            "-lhof-",
                                                            model_description,".json"))
    
    saveRDS(model_obj, model_path)
    params_json <-  toJSON(x=model_params_list,pretty=TRUE)

    writeLines(params_json,model_json_path)
    
}

GetRAoPAUC <- function(raop_model,X,y){

    model_p   <- predict(raop_model, X, type = 'prob')
    raop <- roc(y, model_p$success)
    
    return(as.numeric(raop$auc))
}



