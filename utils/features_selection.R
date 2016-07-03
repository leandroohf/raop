##* ****************************************************************
##  Programer[s]: Leandro Fernandes
##  Company/Institution:  
##  email: leandroohf@gmail.com
##  Program: feature_selection_lib                                    
##  Commentary: Lib with function to help with features selections    
##  Date: November 25, 2015
##* ****************************************************************

library(formula.tools, quietly = TRUE )
library(ggplot2, quietly = TRUE )
library(ascii, quietly = TRUE )
library(leaps, quietly = TRUE )
library(corrplot, quietly = TRUE )
library(reshape2, quietly = TRUE )
##library(randomForest, quietly = TRUE )
library(xgboost, quietly = TRUE )
library(gridExtra, quietly = TRUE )


## ----------------------- [ Data Explorer ] ----------------------- ##

DataExplorer <- function(train.db, response.var){

    corr.matrix <- cor(train.db)    
    
    list(
        GetCorrDashBoard = function(){
            corrplot.mixed(corr.matrix, lower = "circle",
                           upper = "number")
        },
        GetLinePainelDashBoard = function(cols.painel = FALSE){
            DataExplorer_GetLinePainelDashBoard(train.db,response.var,
                                                   cols.painel=cols.painel)
        },

        GetScatterPainelDashBoard = function(cols.painel = FALSE){
            DataExplorer_GetScatterPainelDashBoard(train.db,response.var,
                                                   cols.painel=cols.painel)
        },
        GetHistogramDashBoard = function(cols.painel = FALSE,
                                         number.col = 3){
            DataExplorer_GetHistDashBoard(train.db,response.var,
                                          cols.painel=cols.painel,
                                          number.col = number.col)
        },
        GetAutocorrelationDashBoard = function(){
            y <- train.db[, response.var]
            a <- acf(train.db$y, plot = FALSE)
            plot(a, main = paste0("auto corr: ", response.var))
        }
    )
}

DataExplorer_GetLinePainelDashBoard <- function(train.db, response.var,
                                                   cols.painel = FALSE,
                                                   number.col = 3){

    if(cols.painel == FALSE){
        cols.painel <- names(train.db)
    }
    
    data.view <- train.db[,cols.painel]
    data.view$id <- seq(1:nrow(train.db))
    
    data.view <- melt(data.view, id.vars = "id",
                      measure.vars = cols.painel)
       
    p <- ggplot(data=data.view,aes(x=id,y=value))
    p <- p + geom_line() + facet_wrap( ~ variable ,
                                      scale = "free",
                                      ncol = number.col )
    print(p)
}

DataExplorer_GetScatterPainelDashBoard <- function(train.db, response.var,
                                            cols.painel = FALSE,
                                            number.col = 3){

    if(cols.painel == FALSE){
        cols.painel <- names(train.db)
        cols.painel <- cols.painel[cols.painel != response.var] 
    }

    ## Assure response var is not in cols.painel
    stopifnot( !(response.var %in% cols.painel) )

    data.view <- train.db[,c(response.var, cols.painel)]
    names(data.view)[1] <- "y"
    data.view <- melt(data.view, id.vars = "y",
                      measure.vars = cols.painel)

    p <- ggplot(data=data.view,aes(x=value,y=y))
    p <- p + geom_point() + facet_wrap( ~ variable ,
                                       scale = "free_x",
                                       ncol = number.col )
    print(p)
}

DataExplorer_GetHistDashBoard <- function(train.db, response.var,
                                          cols.painel = NULL,
                                          number.col = 3){

    if(cols.painel == FALSE){
        cols.painel <- names(train.db)        
    }

    data.view <- melt(train.db[,cols.painel], measure.vars = cols.painel)
    
    p <- ggplot(data=data.view,aes(x=value))
    p <- p + geom_histogram() + facet_wrap( ~ variable,
                                           scale = "free_x",
                                           ncol = number.col )
    print(p)
    
}

DataExplorer_GetBoxplotDashBoard <- function(train.db, response.var,
                                            cols.painel = FALSE){
    
    cat("NOT Implemented YET:\n")
    
}

## --------------------- [ RegSuset Explorer ] --------------------- ##
RegsubsetExplorer <- function(train.db,test.db,reg.formula,nvmax,
                              nbest=1,really.big=FALSE,force.in=NULL){

    reg.dev <- regsubsets(reg.formula,
                          data = train.db, nvmax = nvmax,
                          method="forward",
                          nbest=nbest,really.big=really.big,force.in=force.in)

    reg.summary <- summary(reg.dev)

    number.of.models <- length(reg.summary$adjr2)

    cat("number of models: ", number.of.models,"\n")
    
    train.size <- nrow(train.db)
    test.size  <- nrow(test.db)

    error.list <-
        RegsubsetExplorer_ComputeModelsError(reg.dev,reg.formula,
                                                        train.db, test.db)
    
    train.rmse <- error.list[[1]]
    test.rmse <- error.list[[2]]
    model.adjr2 <- reg.summary$adjr2

    list(
        GetRegsubsetDashBoard = function(bayes.error=-1){
            RegsubsetExplorer_BuildRegsubsetDashboard(model.adjr2,
                                                      train.rmse,
                                                      test.rmse,
                                                      bayes.error)

        },
        GetRegsubsetSummaryReport = function(){
            cat("Print Report: NOT Implemented YET\n")
            return(reg.summary)
        },
        GetModelRegSubset = function(model.index, verbose=FALSE){
            lm.model <- RegsubsetExplorer_GetModelRegSubset(model.index,
                                                            reg.dev,
                                                            train.db,
                                                            reg.formula,
                                                            nvmax,
                                                            nbest,
                                                            really.big,
                                                            force.in,
                                                            verbose=verbose)

            return(lm.model)
        },
        GetModelFormula = function(model.index){
            model.formula <- RegsubsetExplorer_GetModelFormula(reg.dev,reg.formula,model.index)
            return(model.formula)
        }
    )
}

RegsubsetExplorer_ComputeModelsError <- function(reg.dev, reg.formula,train.db, test.db){
    
    lhs.formula <- lhs(reg.formula)
    
    reg.summary <- summary(reg.dev)
    number.of.models <- length(reg.summary$adjr2)

    test.rmse = rep(NA, number.of.models)
    train.rmse = rep(NA, number.of.models)
    for (i in 1:number.of.models) {
        ## cat("model: ",i,"\n")
        mi <- RegsubsetExplorer_GetModelRegSubset(i,reg.dev,train.db,
                                                  reg.formula = reg.formula,
                                                  nvmax,nbest,
                                                  really.big,
                                                  force.in)

        train.rmse[i] <- sqrt(mean(mi$residuals^2))
        predi <- predict(mi, test.db)
        resi <- predi - test.db[,as.character(lhs.formula)]
        test.rmse[i] <- sqrt(mean(resi^2))
    }
    return(list(train.rmse,test.rmse))
}

RegsubsetExplorer_BuildRegsubsetDashboard <- function(model.adjr2,
                                                      train.rmse,
                                                      test.rmse,
                                                      bayes.error = -1){
    
    BuildCorrelDashoard(model.adjr2)
    BuildErrorsDashoard(train.rmse, test.rmse,bayes.error)
    
}

BuildCorrelDashoard <- function(model.corr){
    model.index <- seq(1:length(model.corr))
    data.view <- data.frame(model.index=model.index,
                            corr=model.corr)
    
    p1 <- ggplot(data=data.view,aes(x=model.index,y=corr,
                                    label=model.index))
    
    p1 <- p1 + geom_point(size=4) + labs(title="corr: Model vs Response Variable")
    p1 <- p1 + geom_text(hjust=-0.2, vjust=-0.2,angle = 15,size=6,
                         show_guide=FALSE)
    
    print(p1)
}

BuildErrorsDashoard <- function(train.rmse, test.rmse,bayes.error = -1){

    model.index <- seq(1:length(train.rmse))
    
    ymax <- max(append(train.rmse,test.rmse))*1.10
    ymin <- min(append(train.rmse,test.rmse))*0.90

    data.view2 <- data.frame(model.index=model.index,
                             train.rmse=train.rmse,
                             test.rmse=test.rmse)

    p4 <- ggplot(data=data.view2,aes(x=model.index,y=train.rmse,label=model.index))

    p4 <- p4 + geom_point(aes(color="Train"),size=3,show_guide=FALSE)
    p4 <- p4 + geom_line(size=1,linetype="dotdash",show_guide=FALSE) + ylim(ymin,ymax)
    p4 <- p4 + geom_text(hjust=-0.2, vjust=-0.2,angle = 15,size=6,
                         show_guide=FALSE)

    p4 <- p4 + geom_point(aes(x=model.index,y=test.rmse,color="Test"),size=3)
    p4 <- p4 + geom_line(aes(x=model.index,y=test.rmse,color="Test"),size=1,linetype="dotdash")
    p4 <- p4 + geom_text(aes(x=model.index,y=test.rmse,label=model.index),hjust=-0.2, vjust=-0.2,
                         angle = 15,size=6)

    p4 <- p4 + scale_colour_manual(name='',
                                   values=c('Train'='black', 'Test'='blue'))

    p4 <- p4 + labs(xlab="model.index",ylab="RMSE",title="RMSE: Train vs test")

    if( bayes.error > 0){
        print(bayes.error)
        p4 <- p4 + geom_abline(intercept=bayes.error,slope = 0.0,
                               colour="red", linetype = "dashed")
    }
    
    print(p4)
}

RegsubsetExplorer_GetModelFormula <- function(reg.dev,reg.formula,model.k){

    ck <- coef(reg.dev, model.k)
    ck.names <- names(ck)
    n <- length(ck.names)

    ## Building rhs formula
    lhs.formula <- lhs(reg.formula)
    rhs.formula <- paste(ck.names[2:n],collapse=" + ")

    model.formula <- paste(lhs.formula," ~ ",rhs.formula)

    return(model.formula)
}

RegsubsetExplorer_GetModelRegSubset <- function(model.k,reg.dev,
                                                train.db,reg.formula,
                                                nvmax, nbest,
                                                really.big=FALSE,
                                                force.in=NULL,
                                                verbose=FALSE){


    reg.summary <- summary(reg.dev)
    models.number <- length(reg.summary$adjr2)
    n=seq(1:models.number)

    ## Get coef of desired model
    ck <- coef(reg.dev, model.k)
    coef.k <- data.frame(names=names(ck),
                         coefs=as.numeric(coef(reg.dev, model.k)))


    ## Building rhs formula
    model.formula <- RegsubsetExplorer_GetModelFormula(reg.dev,reg.formula,model.k)

    names.list <- paste(names(coef(reg.dev,1)),collapse=" + ")
    for(k in 2:models.number){
        names.aux <- paste(names(coef(reg.dev,k)),collapse=" + ")
        names.list <- append(names.list,names.aux)
    }

    model.table <- data.frame(n=n,
                              adjr2=reg.summary$adjr2,
                              coef.names=names.list)

    if(verbose == TRUE){
        ## Print first 7 model in model tables
        cat("Print Model: ", model.k,"\n")
        print(ascii(coef.k,include.rownames=FALSE,digits=4),type = "org")

        ## Print first 7 model in model tables
        cat("Print Model: ", model.k," neighbors\n")
        start.model <- ifelse(model.k >1 ,
                              model.k - 1,
                              model.k)

        end.model <- ifelse(model.k == reg.dev$nvmax ,
                            model.k,
                            model.k + 1)


        print(ascii(model.table[ seq(model.k -1,model.k+1,by=1)
                               ,1:2],include.rownames=FALSE,digits=4),type = "org")

        cat("Printing model formula\n")
        print(model.formula)
    }

    lm.model <- lm(model.formula,data=train.db)
    
    return(lm.model)
}

## --------------------- [ XGBoost Explorer ] --------------------- ##

XGBoostExplorer <- function(train.db, test.db, response.var, number.of.models,param.list){

    stopifnot(number.of.models > 10)
    stopifnot(names(train.db) %in% names(test.db))
    stopifnot(ncol(train.db) %in% ncol(test.db))
    stopifnot(c("objective","eta","subsample","colsample_bytree") %in% names(
                                                                           param.list))
    
    min.nround <- 5
    max.nround <- 100
    
    train.label  <- as.matrix(train.db[,response.var])
    train.matrix <- as.matrix(train.db[,names(train.db) != response.var])
    xgb.train    <- xgb.DMatrix(data = train.matrix , label=train.label)
        
    test.label  <- as.matrix(test.db[,response.var])
    test.matrix <- as.matrix(test.db[, names(test.db) != response.var])
    xgb.test    <- xgb.DMatrix(data = test.matrix, label=test.label)
    
    cat("Train model nround: ", max.nround, "\n")
    xgb.model  <- XGBoostExplorer_TrainModel(xgb.train, xgb.test,
                                             param.list,
                                             max.nround)
    
    cat("\nComputing model selection \n")
    error.list <- XGBoostExplorer_ComputeModelsError(xgb.train,
                                                                train.label,
                                                                xgb.test,
                                                                test.label,
                                                                param.list,
                                                                min.nround,
                                                                max.nround,
                                                                number.of.models)
    
    train.rmse <- error.list[[1]]
    test.rmse  <- error.list[[2]]
    model.corr <- error.list[[3]]

    cat(" Grid Serachin: cols_samples n nrounds\n")
    test.error.grid <-
        XGBoostExplorer_GetModelsParameterSpaces(xgb.train,
                                                 train.label,
                                                 xgb.test, test.label,
                                                 param.list,
                                                 min.nround,
                                                 max.nround,
                                                 number.of.models)
    
    list(
        PlotRelativeImportance = function(){
            XGBoostExplorer_PlotRelativeImportance(train.db,
                                                   response.var,
                                                   xgb.model)
        },
        GetXGBoostDashBoard = function(bayes.error=-1){
            XGBoostExplorer_BuildXGBoostDashBoard(model.corr,
                                                  train.rmse,
                                                  test.rmse,
                                                  bayes.error)
        },
        GetTestErrorGridDashBoard = function(bayes.error=-1){
            XGBoostExplorer_BuildTestErrorGridDashBoard(test.error.grid,
                                                        bayes.error)
            
        }
    )
}

XGBoostExplorer_TrainModel <- function(xgb.train,xgb.test, param.list, nround){

    xgb.model <- xgboost::xgb.train(param=param.list,
                                    data = xgb.train, nthread = 3,
                                    nround = nround,
                                    verbose=0,
                                    maximize = FALSE)
    
    return(xgb.model)
}

XGBoostExplorer_ComputeModelsError <- function(xgb.train, train.label,
                                               xgb.test, test.label,
                                               param.list, min.nround,
                                               max.nround,
                                               number.of.models){

    stopifnot(min.nround < max.nround)
    
    test.rmse = rep(NA, number.of.models)
    train.rmse = rep(NA, number.of.models)
    model.corr = rep(NA, number.of.models)
    
    dround <- round(max.nround/number.of.models)
    nround <- min.nround
    for (k in seq(1,number.of.models,by=1)){

        xgb.model.k <- XGBoostExplorer_TrainModel(xgb.train,xgb.test,
                                                  param.list, nround)
        
        pred <- predict(xgb.model.k, xgb.train)
        train.rmse[k] <- sqrt( mean( (train.label - pred)^2 ) )
        model.corr[k] <- cor(pred,train.label)^2
        
        pred <- predict(xgb.model.k, xgb.test)
        test.rmse[k] <- sqrt( mean( (test.label - pred)^2 ) )
        
        ## Number of tree in the classifier
        nround <- nround + dround
    }
    return(list(train.rmse,test.rmse,model.corr))
}

XGBoostExplorer_GetModelsParameterSpaces <- function(xgb.train,
                                                          train.label,
                                                          xgb.test,
                                                          test.label,
                                                          param.list,
                                                          min.nround,
                                                          max.nround,
                                                          number.of.models){

    ## 2 parameters GridSearch
    stopifnot(min.nround < max.nround)
    
    dround <- round(max.nround/number.of.models)
    nround <- min.nround
    nrounds <- seq(1,number.of.models,by=1)*dround
    
    colsamples <- seq(0.30,1.0,by=0.30)
    ncolsample <- length(colsamples)

    test.rmse = matrix(NA, number.of.models,ncolsample)
    
    for (k in seq(1,number.of.models,by=1)){
        for(c in seq(1,ncolsample,by=1)){
            
            param.list$colsample_bytree <- colsamples[c]
            nround <- nrounds[k]
            xgb.model.k <- XGBoostExplorer_TrainModel(xgb.train,xgb.test,
                                                      param.list, nround)
                    
            pred <- predict(xgb.model.k, xgb.test)
            test.rmse[k,c] <- sqrt( mean( (test.label - pred)^2 ) )
            
        }
    }

    print(head(test.rmse))
    
    return(list(test.rmse,colsamples,nrounds))
}


XGBoostExplorer_PlotRelativeImportance <- function(train.db,
                                                   response.var,
                                                   xgb.model){

    train.matrix <- as.matrix(train.db[,names(train.db) != response.var])
    train.matrix.names = dimnames(train.matrix)[[2]]
    
    importance_matrix <- xgb.importance(train.matrix.names,
                                        model = xgb.model)
    
    p <- xgb.plot.importance(importance_matrix)
    print(p)
}

XGBoostExplorer_BuildXGBoostDashBoard <- function(model.corr,
                                                  train.rmse,
                                                  test.rmse,
                                                  bayes.error = -1){
    BuildCorrelDashoard(model.corr)
    BuildErrorsDashoard(train.rmse, test.rmse,bayes.error)
    
}

XGBoostExplorer_BuildTestErrorGridDashBoard <- function(test.error.grid,
                                                        bayes.error = -1){
    
    nu <- test.error.grid[[3]]
    co <- test.error.grid[[2]]
    d  <- test.error.grid[[1]]
    m  <- matrix(d,nrow = length(nu),ncol = length(co),byrow=FALSE,
                 dimnames = list(nu,co))
    
    long.data <- melt(m,id.vars = c("nu","co")) 
    long.data$Var2 <- factor(long.data$Var2)
    
    p <- ggplot(long.data,
                aes(x = Var1, y = value, colour = Var2)) + geom_line()
    
    if( bayes.error > 0){
        p <- p +
            geom_abline(intercept=bayes.error,slope = 0.0,
                        colour="red", linetype = "dashed")
    }
    print(p)
}
