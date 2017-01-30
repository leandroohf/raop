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
library(jsonlite, quietly = TRUE )
library(lubridate, quietly = TRUE )

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



PlotRAoPImportance <- function(logit_m, rf_m, gbm_m, nnet_m){


    logit_imp = varImp(logit_m)
    gbm_imp = varImp(gbm_m)
    rf_imp = varImp(rf_m)
    nnet_imp = varImp(nnet_m)
    
    ## plot importance
    plot_imp <- function(model) {

        ## data hangler
        df = data.frame(model[[1]])
        names(df) = 'importance'
        df$variable = row.names(df)
        
        var_order = df$variable[order(df$importance)]
        df$variable = factor(df$variable, levels = var_order)

        p <- ggplot(df, aes(x = importance, y = variable)) +
            geom_segment(aes(yend = variable), xend = 0, colour = 'grey50') +
            geom_point(size = 3, colour = '#1d91c0') +
        ggtitle(model[[2]]) + theme_bw() + guides(fill = F)
        
        return(p)
    }
    
    p_logit_imp <- plot_imp(logit_imp)
    p_rf_imp    <-      plot_imp(rf_imp)
    p_gbm_imp   <- plot_imp(gbm_imp)
    p_nnet_imp  <- plot_imp(nnet_imp)
    ## p_svm_imp   <- plot_imp(svm_imp)

    grid.arrange(p_logit_imp, p_rf_imp, p_gbm_imp, p_nnet_imp, top= 'Variable importance')

}

GetRAoPModelsCorrelation <- function(logit_m, gbm_m, rf_m, nnet_m){

    pred_log  <- predict( object = logit_m, newdata = train_te, type = "prob")
    pred_gbm  <- predict( object = gbm_m, newdata = train_te[, ind_vars], type = "prob") 
    pred_rf   <- predict( object = rf_m, newdata = train_te, type = "prob")
    pred_nnet <- predict( object = nnet_m, newdata = train_te, type = "prob")
    

    pred_log  <- pred_log$success  
    pred_gbm  <- pred_gbm$success  
    pred_rf   <- pred_rf$success  
    pred_nnet <- pred_nnet$success  
    
    r_df <- cbind( pred_log, pred_gbm, pred_rf, pred_nnet)

    C <-  cor(r_df)
    
    return(C)
}


GetRAopConfusionMatrix <- function(model_m, X, y){

    pred_log  <- predict( object = model_m, newdata = X, type = "raw")

    C <- confusionMatrix(pred_log, y)
    
    return(C)

}

RAoPStackEnsembleDataPrepare <- function(train_data, ind_vars, logit_m, gbm_m, rf_m, nnet_m){

    ## X is predictors. Data whithout response variable
    
    train_data$logit_prob <-  predict(logit_m, train_data[,ind_vars], type = 'prob')$success
    train_data$rf_prob    <-  predict(rf_m, train_data[,ind_vars], type = 'prob')$success
    train_data$gbm_prob   <-  predict(gbm_m, train_data[,ind_vars],type ='prob')$success
    train_data$nnet_prob  <-  predict(nnet_m, train_data[,ind_vars], type = 'prob')$success
    ##train_te$svm_prob   <-  predict(svm_m, train_te[, ind_vars], type = 'prob')

    return(train_data)
}

RAoPPermutationTest <- function(X, y, nperm) {

    nrows <- nrow(X)

    ## ----------------------------------------------- [ GBM ]
    ctrl = trainControl(method = 'cv', summaryFunction = twoClassSummary, classProbs = T)

    gbm_tune <- expand.grid( interaction.depth = c(6,7,8),
                            n.trees = c(750, 800, 850),
                            shrinkage = c(.005),
                            n.minobsinnode = 9)
    
    aucs <- numeric(nperm)
    for(i in seq_len(nperm)) {
        cat("interation: ", i, "\n")
        ## random order of rows
        yl = sample(y, size=nrows, replace=FALSE)

        gbm_m <- train(x = X, y = yl,
                       method = 'gbm', tuneGrid = gbm_tune,
                       metric = 'ROC', verbose = F, trControl = ctrl)

        aucs[i] <- max(gbm_m$results$ROC)
    }
    return(aucs)
}

RAoPGetStackMedianPrediction <- function(X, ptype, logit_m, rf_m, gbm_m, nnet_m){

    logit_p <- predict(logit_m,X, type = ptype)
    rf_p    <- predict(rf_m,   X, type = ptype)
    gbm_p   <- predict(gbm_m,  X, type = ptype)
    nnet_p  <- predict(nnet_m, X, type = ptype)

    median_p <- apply(cbind(logit_p$success,rf_p$success,gbm_p$success,nnet_p$success),1,median)

    return(median_p)
}

RAoPGetStackMeanPrediction <- function(X, ptype, logit_m, rf_m, gbm_m, nnet_m){

    logit_p <- predict(logit_m,X, type = ptype)
    rf_p    <- predict(rf_m,   X, type = ptype)
    gbm_p   <- predict(gbm_m,  X, type = ptype)
    nnet_p  <- predict(nnet_m, X, type = ptype)

    mean_p <- (logit_p$success + rf_p$success + gbm_p$success + nnet_p$success) / 4

    return(mean_p)
}
