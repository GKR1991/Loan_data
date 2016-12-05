## logistic regression
source("./logReg.R")

only_train <- train[,-17]
only_test <- test[,-17]

model_lr <- LogReg(only_train, as.integer(as.character(train$paid)), only_test, cv=5, seed=2016, metric="auc")
# View(model_lr$test)
auc(test$paid, model_lr$test$pred_lr) # Area under the curve: 0.7079

test.y <- test$paid
# simple logistic reg. on all vars
lrfit <- glm( paid ~ ., family=binomial(link='logit'),data=train)
summary(lrfit)

lrfit_pred <- predict(lrfit, test, type = "response")
range(lrfit_pred)

auc(test$paid,lrfit_pred) #Area under the curve: 0.7078

# stepwise logistic reg. 
nothing <- glm(paid ~ 1,family=binomial, data = train)
summary(nothing)

# # forwards = step(nothing, scope=list(lower=formula(nothing),upper=formula(lrfit)), direction="forward")
# summary(forwards)
# forwards_pred <- predict(forwards,test, type = "response")
# range(forwards_pred)
# auc(test$paid,forwards_pred) #Area under the curve: 0.7084 - 0.7077


# backwards = step(lrfit, direction="backward")
backwards <- glm(formula = paid ~ loan_amnt + term + int_rate + emp_length + 
      home_ownership + annual_inc + purpose + inq_last_6mths + 
      open_acc + pub_rec + revol_util + funded_amnt2, family = binomial(link = "logit"), 
    data = train)
summary(backwards)
backwards_pred <- predict(backwards,test, type = "response")
range(backwards_pred)
auc(test.y,backwards_pred) #Area under the curve: 0.7091


# bothways = step(nothing, scope=list(lower=formula(nothing),upper=formula(lrfit)), direction="both")
bothways <- glm(formula = paid ~ loan_amnt + term + int_rate + emp_length + 
                  home_ownership + annual_inc + purpose + inq_last_6mths + 
                  open_acc + pub_rec + revol_util + funded_amnt2, family = binomial(link = "logit"), 
                data = train)
summary(bothways)

wald.test(b = coef(bothways), Sigma = vcov(bothways), Terms = 10:22)
# the outcome states that the overall effect of "purpose" is statistically significant

VIF(bothways) # no vif more than 2, no multicollinearity

bothways_pred <- predict(bothways,test, type = "response")
range(bothways_pred)
auc(test$paid,bothways_pred) #Area under the curve: 0.7091



# calculating the values for ROC curve
library(ROCR)
pred <- prediction(backwards_pred, test.y)
perf <- performance(pred,"tpr","fpr")
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))

# ROC plot
plot(perf)
legend("bottomright",legend=c(paste(sep = "", "AUC = ",round(auc,2) )),bty = 'n')
# Add decorations to the plot. 
title(main="ROC Curve- Logistic Regression [Test Data]", 
      sub=paste("Nagesh Kommuri", format(Sys.time(), "%Y-%b-%d %H:%M:%S")))
grid()


# Cross Tab

thresh  <- .8
Predicted_lr <- cut(backwards_pred, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
Observed_lr <- test.y
cTab_lr <- table(Observed_lr,Predicted_lr)
cTab_lr

predict_lr <- as.table(addmargins(cTab_lr))
nrow(predict_lr)
accuracy_lr <- as.numeric(sum(diag(cTab_lr))/sum(cTab_lr))
accuracy_lr <- floor(accuracy_lr*100)
accuracy_lr
TN=predict_lr[1,1] 
FP=predict_lr[1,2] 
FN=predict_lr[2,1] 
TP=predict_lr[2,2] 
sensitivity_lr=TP/(TP+FN) 
sensitivity_lr=floor(sensitivity_lr*100) 
specificity_lr=TN/(TN+FP) 
specificity_lr=floor(specificity_lr*100) 
