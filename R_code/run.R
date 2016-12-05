###
# TITLE  : LendingClub Loan Data - 2007 - 2011
# Author : Nagesh Kommuri - https://in.linkedin.com/in/iamknagesh
# Date   : Sat Dec 03 21:49:34 2016
###

# importing all the required packages
library(vcd)        # for chi sqr and cramersV()
library(smbinning)  # find woe and iv 
library(caret)      # splitting the dataset using stratification
library(pROC)       # auc() is used to evaluate the test data
library(DescTools)  # for EDA and variable selection
library(mice)       # md.pattern to find the missing values 
library(Matrix)     # converting df to matrix before xgb
library(xgboost)    # for training xgb
library(aod)        # for wald.test() after logit to find significance of factor variables

set.seed(1234)
options(scipen = 999)

### User defined functions

### Function to remove constant variables
removeConst <- function(df){
  ##### Removing constant features
  cat("\n## Removing the constants features.\n")
  for (f in names(df)) {
    if (length(unique(df[[f]])) == 1) {
      cat(f, "is constant. We delete it.\n")
      df[[f]] <- NULL
    }
  }
  return(df)
}

source("code.R")
source("iv.R")
source("desc.R")
source("split.R")
source("lr_models.R")
source("xgb_model.R")

