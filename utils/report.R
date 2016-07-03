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

library(caret,quietly = TRUE )
library(ROCR,quietly = TRUE )

BuildModelReport <- function(model.log,response.var, train.data,val.dat){

    cat('  model summary\n')
    print(summary(model.log))

    y.train <- unlist(train.data[,response.var])
    pred.train    <- predict(model.log, type = 'response')

    y.val <- unlist(val.data[,response.var])
    pred.val    <- predict(model.log,val.data, type = 'response')

    ## score
    z <- log(pred.val/(1.0-pred.val))
    
    ##confusion matrix
    table(y.train, pred.train > 0.5)
    table(y.val, pred.val > 0.5)
    
    ## ROCR Curve
    ROCRpred <- prediction(pred.val, y.val)
    ROCRperf <- performance(ROCRpred, 'tpr','fpr')
    auc.perf <- performance(ROCRpred, measure = "auc")

    cat('=================================\n\n')
    cat('AUC: \n')
    print(as.numeric(auc.perf@y.values))

    plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
    abline(a = 0.0, b=1.0,col = "lightgray", lty = 2)
    
    cat('=================================\n\n')
    ##cat('Train confusion matrix:\n')
    ##print(confusionMatrix(pred.train > 0.5, y.train))
    cat('Val confusion matrix:\n')
    print(confusionMatrix(pred.val > 0.5 , y.val))
}
