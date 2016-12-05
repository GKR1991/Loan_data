# function for logistic regression
LogReg <- function(trainData,y,testData=data.frame(),cv=5,seed=123,metric="auc",importance=0)
{
  # defining evaluation metric
  eval_score <- function(x,z,metric)
  {
    switch(metric,
           accuracy = sum(abs(x-z)<=0.5)/length(x),
           auc = auc(x,z),
           logloss = -(sum(log(1-z[x==0])) + sum(log(z[x==1])))/length(x),
           mae = sum(abs(x-z))/length(x),
           precision = length(x[x==z])/length(x),
           rmse = sqrt(sum((x-z)^2)/length(x)),
           rmspe = sqrt(sum(((x-z)/x)^2)/length(x)))           
  }
  
  if (metric == "auc")
  {
    library(pROC)
  }
  
  cat("Preparing Data\n")
  trainData$order <- seq(1, nrow(trainData))
  trainData$result <- as.numeric(y)
  
  set.seed(seed)
  trainData$randomCV <- floor(runif(nrow(trainData), 1, (cv+1)))
  
  # cross validation
  cat(cv, "-fold Cross Validation\n", sep = "")
  for (i in 1:cv)
  {
    X_build <- subset(trainData, randomCV != i, select = -c(order, randomCV))
    X_val <- subset(trainData, randomCV == i) 
    
    # building model
    model_lr <- glm(result ~., data=X_build, family=binomial())
    
    # predicting on validation data
    pred_lr <- predict(model_lr, X_val, type="response")
    X_val <- cbind(X_val, pred_lr)
    
    # predicting on test data
    if (nrow(testData) > 0)
    {
      pred_lr <- predict(model_lr, testData, type="response")
    }
    
    cat("CV Fold-", i, " ", metric, ": ", eval_score(X_val$result, X_val$pred_lr, metric), "\n", sep = "")
    
    # initializing outputs
    if (i == 1)
    {
      output <- X_val
      if (nrow(testData) > 0)
      {
        testData <- cbind(testData, pred_lr)
      }      
    }
    
    # appending to outputs
    if (i > 1)
    {
      output <- rbind(output, X_val)
      if (nrow(testData) > 0)
      {
        testData$pred_lr <- (testData$pred_lr * (i-1) + pred_lr)/i
      }            
    }
    
    gc()
  } 
  
  # final evaluation score
  output <- output[order(output$order),]
  cat("\nLogisticRegression ", cv, "-Fold CV ", metric, ": ", eval_score(output$result, output$pred_lr, metric), "\n", sep = "")
  
  # returning CV predictions and test data with predictions
  return(list("train"=output, "test"=testData))  
}