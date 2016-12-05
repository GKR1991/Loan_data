# purpose_mean <- aggregate(accept$int_rate, by=list(paid), FUN=mean)
# 
# barplot(purpose_mean$x, names.arg=purpose_mean$Group.1)
# title("Mean Interest Rate per purpose")

# mths_since_last_record has 38647 missing values - imputing them and using would be daunting as well as misleading
# mths_since_last_delinq has 26752 missing values - imputing them and using would be daunting as well as misleading
accept$mths_since_last_record <- NULL
accept$mths_since_last_delinq <- NULL

# NONE in home_ownership to OTHER
levels(accept$home_ownership)[2] <- "OTHER"
# converting employment length into integer - replacing na values with 0
levels(accept$emp_length)<- c(0,1,10,2:9,0)
accept$emp_length <- as.integer(as.character(accept$emp_length))
barplot(table(accept$emp_length))

# Lets find out which variables are important using - WOE and IV
accept$paid <- NULL
accept$funded_amnt1 <- accept$loan_amnt - accept$funded_amnt
accept$funded_amnt2 <- accept$loan_amnt - accept$funded_amnt_inv
accept <- cbind(accept,paid)

# using error handling and loops we automate the retrieval of IV for each variable
# library(smbinning)
n <- NULL; iv <- NULL
for(i in 1:(length(accept)-1)){
  tryCatch({
    x <- smbinning(accept,colnames(accept)[i],y =  "paid")$iv
    # print(paste(colnames(accept)[i], x, sep = "-----"))
    n <- c(n, i); iv <- c(iv, x)
  }, error = function(e){})
}
cbind(n,colnames(accept)[n], iv)

# n                              iv      
# [1,] "1"  "loan_amnt"               "0.0213"
# [2,] "2"  "funded_amnt"             "0.0179"
# [3,] "3"  "funded_amnt_inv"         "0.0258"
# [4,] "5"  "int_rate"                "0.3529"
# [5,] "6"  "installment"             "0.0055"
# [6,] "11" "annual_inc"              "0.0358"
# [7,] "15" "dti"                     "0.0165"
# [8,] "16" "delinq_2yrs"             "0.0029"
# [9,] "17" "inq_last_6mths"          "0.0818"
# [10,] "20" "revol_bal"               "0.0039"
# [11,] "21" "revol_util"              "0.0768"
# [12,] "22" "total_acc"               "0.0076"
# [13,] "25" "total_pymnt"             "0.9342"
# [14,] "26" "total_pymnt_inv"         "0.877" 
# [15,] "27" "total_rec_prncp"         "1.9231"
# [16,] "28" "total_rec_int"           "0.0278"
# [17,] "29" "total_rec_late_fee"      "0.1892"
# [18,] "30" "recoveries"              "4.1751"
# [19,] "31" "collection_recovery_fee" "0.7348"
# [20,] "32" "last_pymnt_amnt"         "2.0372"
# [21,] "35" "funded_amnt2"            "0.0567"

# In the above IVs in variables from 25 to 32, are very abnormal - as their names and descriptions suggest 
# they are collected after giving the loan. Hence it is not a good idea to model them 

# The IVs from ("6"  "installment") ("20" "revol_bal") ("16" "delinq_2yrs") ("22" "total_acc") 
# seem useless for a4alysis

# 23 - out_prncp	      - Remaining outstanding principal for total amount funded
# 24 - out_prncp_inv	  - Remaining outstanding principal for portion of total amount funded by investors
# funded_amnt2 shall be used instead of 3 - funded_amnt_inv

# lets have a look at correlation of some of the variables 
cor(accept$loan_amnt, accept$funded_amnt)
cor(accept$loan_amnt, accept$funded_amnt_inv)
cor(accept$loan_amnt, accept$funded_amnt1)
cor(accept$loan_amnt, accept$funded_amnt2)
cor(accept$funded_amnt1, accept$funded_amnt2)

removeIV_vars <- c(2,3,6,16,20,22:34)
accept$paid <- NULL
length(removeIV_vars)
accept <- accept[,-removeIV_vars]; 
md.pattern(accept)
summary(accept)

# Importance of categorical variables
# chi square
factor_vars <- NULL

for(i in 1:length(accept)){
  if(is.factor(accept[,i])){
    factor_vars <- c(factor_vars, i)
    print(names(accept)[i])
    tab <- table(accept[,i], paid)
    # print(chisq.test(tab))
    # print("*********************** Cramer's V ***********************")# library(lsr)
    # print(cramersV(tab))
    print(assocstats(tab))
  }
}
names(accept)[factor_vars]
# The chi-square tests show that the outcome or DV is dependent on all the categorical variables
