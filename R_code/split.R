# after iv.R

# splitting data into train and test for modelling - stratified sampling
# library(caret)
set.seed(1234)
split <- createDataPartition(paid,p = 0.7, list = F)

accept$paid <- as.factor(as.character(paid))
accept$addr_state <- NULL
# creating train and test based on the split
train <- accept[split,]; row.names(train) <- NULL
test <- accept[-split,]; row.names(test) <- NULL

