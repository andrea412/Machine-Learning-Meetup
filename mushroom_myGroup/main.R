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
df_addFeatures<-df_train %>% select(importantVariables)
d<-rep(NA,1,ncol(df_addFeatures))
remove<-c()
for(i in 1:ncol(df_addFeatures)) {
  d[i]<-(length(levels(df_addFeatures[,i])))
  if(d[i]>20){
    remove<-c(remove,d[i])
  }
}
plot(d)
df_addFeatures<-df_addFeatures[,-remove]


n_folds <- 10 # k = 10
n_train<- nrow(df_addFeatures)
folds_i <- sample(rep(1:n_folds, length.out = n_train))
mtry<-seq(3,length(importantVariables)/2,5)
fitted_models <- apply(t(mtry), 2, function(par) randomForest(class~. ,data = df_addFeatures, ntree = 500,mtry = par))

# we compute the train error using the confusion matrix
train_error <- sapply(fitted_models, function(obj) (obj$confusion[2]+obj$confusion[3])/nrow(df_addFeatures))
# let's compute the validation error
cv_tmp <- matrix(NA, nrow = n_folds, ncol = length(mtry))
for (k in 1:n_folds) {
  # here we select the validation set
  test_i <- which(folds_i == k) 
  train_xy <- df_addFeatures[-test_i, ]
  test_xy <- df_addFeatures[test_i, ]
  fitted_models <- apply(t(mtry), 2, function(par) randomForest(class~. ,data = train_xy, ntree = 500,mtry = par))
  x <- test_xy %>% select(-class)
  y <- (test_xy %>% select(class))[[1]] %>% as.factor
  pred <- mapply(function(obj) predict(obj, data.frame(x)), fitted_models)
  
  cv_tmp[k, ] <- sapply(as.list(data.frame(pred)), function(y_hat) length(which(y_hat != y))/length(y))
  
}
cv <- colMeans(cv_tmp)
accuracyMin<-1-min(cv)
print(accuracyMin)
print(fitted_models[[which.min(cv)]])
print(fitted_models[[which.min(cv)]])
importance(fitted_models[[which.min(cv)]])
plot(mtry, train_error, type = "l", lwd = 2, col = gray(0.4), ylab = "Training error", 
     xlab = "Number of trees", main = paste0(n_folds,"-fold Cross-Validation"), ylim = c(0.1, 0.8))
lines(mtry, cv, lwd = 2, col = "steelblue2")
points(ntree, cv, col = "steelblue2", pch = 19)
legend(x = "topright", legend = c("Training error", "Cross-validation error"), 
       lty = c(1, 2, 1), lwd = rep(2, 3), col = c(gray(0.4), "darkred", "steelblue2"), 
       text.width = 0.2, cex = 0.85)