###### Analyzing Mushroom data


##############################  LIBRARIES  AND IMPORT DATA ##################################
library(tidyverse)
library(randomForest)
library(forecast)
library(corrplot)
library(RColorBrewer)
library(vcd)
library(rsample)      # data splitting 

cat("\014")
rm(list = ls())

source("ausiliaryFunctions.R")

pathinput<-"./mushrooms.csv"
dataset<-read.csv2(pathinput , sep =",",stringsAsFactors = FALSE)
set.seed(123)
mr_split <- initial_split(dataset, prop = .7)
mr_train <- training(mr_split)
mr_test  <- testing(mr_split)
##############################  GOING INTO INTO DATASET ##################################

head(mr_train)
str(mr_train)
mr_train <- mr_train %>% select(-"veil.type")
colSums(is.na(mr_train)) 
if (any(mr_train=='?')) print('At least one null value is in df_train')
df_train<- mr_train %>% select(-"stalk.root")

##############################  FEATURE ENGINEERING  ##################################


k<-ncol(df_train)+1
size<-ncol(df_train)
for(i in 2: size){
  if(i<size){
    for(j in (i+1):size){
      df_train[,k]<-combine_column(df_train[,i],df_train[,j])
      colnames(df_train)[k]<-paste(colnames(df_train)[i],colnames(df_train)[j])
      k<-k+1
    }
  }
}

df_train<-data.frame(apply(df_train,2,FUN = as.factor))

# Correlation matrix
cor_matrix <- matrix(ncol = ncol(df_train),
                  nrow = ncol(df_train),
                  dimnames = list(names(df_train), 
                                  names(df_train)))

cor_matrix <- calculate_cramer(cor_matrix ,df_train)
cor_matrix2<-cor_matrix[1:23,1:23]
corrplot(cor_matrix2, type="upper", order="original",tl.cex = 0.8,
         col=brewer.pal(n=8, name="RdYlBu"))

corClass<-cor_matrix[1,1:ncol(cor_matrix)]
threshold<-0.6
importantVariables<-colnames(df_train)[which(corClass>threshold)]
df_addFeatuers<-df_train %>% select(importantVariables)


n_folds <- 10 # k = 10
n_train<- nrow(df_addFeatuers)
folds_i <- sample(rep(1:n_folds, length.out = n_train))
mtry<-seq(10,500,20)
fitted_models <- apply(t(ntree), 2, function(par) randomForest(Survived~. ,data = X_train, ntree = par))

# we compute the train error using the confusion matrix
train_error <- sapply(fitted_models, function(obj) (obj$confusion[2]+obj$confusion[3])/nrow(X_train))
# let's compute the validation error
cv_tmp <- matrix(NA, nrow = n_folds, ncol = length(ntree))
for (k in 1:n_folds) {
  # here we select the validation set
  test_i <- which(folds_i == k) 
  train_xy <- X_train[-test_i, ]
  test_xy <- X_train[test_i, ]
  fitted_models <- apply(t(ntree), 2, function(par) randomForest(Survived~. ,data = train_xy, ntree = par))
  x <- test_xy %>% select(-Survived)
  y <- (test_xy %>% select(Survived))[[1]] %>% as.factor
  pred <- mapply(function(obj) predict(obj, data.frame(x)), fitted_models)
  
  cv_tmp[k, ] <- sapply(as.list(data.frame(pred)), function(y_hat) length(which(y_hat != y))/length(y))
  
}
cv <- colMeans(cv_tmp)
accuracyMin<-1-min(cv)
print(accuracyMin)
print(fitted_models[[which.min(cv)]])