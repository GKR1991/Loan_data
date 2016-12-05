train.y <- train$paid
train$paid <- as.integer(as.character(train$paid))
trainx <- sparse.model.matrix(paid ~ ., data = train)

dtrain <- xgb.DMatrix(data=trainx, label=train$paid)
watchlist <- list(trainx=dtrain)
# train$paid <- NULL

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.01,
                max_depth           = 5,
                subsample           = 0.68,
                colsample_bytree    = 0.7)

set.seed(1234)
xgb_cv <- xgb.cv(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 1000, 
                    verbose             = 2,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    nfold               = 10,
                    print.every.n       = 10,
                    stratified          = T
)
which.max(xgb_cv$test.auc.mean)
xgb_cv$test.auc.mean[which.max(xgb_cv$test.auc.mean)]

set.seed(1234)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = which.max(xgb_cv$test.auc.mean)+1, #809
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE, 
                    print.every.n       = 100
)
clf[1:10]
### generating predictions on test set
# test.y <- test$paid
test$paid <- -1
testx <- sparse.model.matrix(paid ~ ., data = test)
test$paid <- test.y

preds <- predict(clf, testx)
auc(test.y,preds) #Area under the curve: 0.7164

file <- xgb.model.dt.tree(feature_names = trainx@Dimnames[[2]], model = clf)

xgb_importance <- xgb.importance(feature_names = trainx@Dimnames[[2]], model = clf)
xgb.plot.importance(xgb_importance[1:10,])

xgb.plot.tree(feature_names = trainx@Dimnames[[2]], model = clf, n_first_tree = 5)
gc()
