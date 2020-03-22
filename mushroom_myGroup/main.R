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
plot(corClass)
threshold<-0.6

importantVariables<-colnames(df_train)[which(corClass>threshold)] 
df_addFeatures<-df_train %>% select(importantVariables)
d<-rep(NA,1,ncol(df_addFeatures))
remove<-c()
for(i in 1:ncol(df_addFeatures)) {
  d[i]<-(length(levels(df_addFeatures[,i])))
  if(d[i]>20){
    remove<-c(remove,i)
  }
}
plot(d)
df_addFeatures<-df_addFeatures[,-remove]

rf<-tree(class~. ,data = df_addFeatures)

plot(rf)
text(rf,pretty = 0)
barplot(prop.table(table(df_addFeatures$odor.stalk.color.below.ring[df_addFeatures$class=="e"])))
barplot(prop.table(table(df_addFeatures$odor.stalk.color.below.ring[df_addFeatures$class=="p"])))
