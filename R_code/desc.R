# using desctools package to explore trends , patterns and do EDA
# Lets see what are the purposes of taking loans
Desc(accept$purpose, main = "Purposes of taking loans", plotit = TRUE)
Desc(accept$purpose[accept$paid == 0], main = "Purposes of taking loans which defaulted", plotit = TRUE)

# Lets see the distribution of loan amount
Desc(accept$loan_amnt, main = "Distribution of loan amounts", plotit = TRUE)
Desc(accept$loan_amnt[accept$paid == 0], main = "Distribution of loan amounts which defaulted", plotit = TRUE)
# Desc(accept$loan_amnt[accept$paid ==1], main = "Distribution of paid loan amounts", plotit = TRUE)
# Desc(accept$loan_amnt[accept$paid ==0], main = "Distribution of unpaid loan amounts", plotit = TRUE)
Desc(as.factor(paid), main = "Distribution of Dependent variable", plotit = TRUE)

# all.equal(accept$loan_amnt, accept$funded_amnt)

# loop for plotting desc of all the variables
# for(i in 1:length(accept)){ 
#   print(Desc(accept[,i], main = paste("Distribution of ",colnames(accept)[i]), plotit = T))
# }
