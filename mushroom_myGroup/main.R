# Analyzing Mushroom data
library(tidyverse)
library(randomForest)
library(forecast)

pathinput<-"./mushrooms.csv"
dataset<-read.csv2(pathinput , sep =",")
head(dataset)
str(dataset)
dataset <- dataset %>% select(-"veil.type")
df<- dataset %>% select(c("class","cap.shape","cap.surface","cap.color","bruises","odor"))

df<-df %>% group_by(cap.shape,class) 
summarise(df, n())
colSums(is.na(dataset)) 
df<- dataset %>% select(-"stalk.root")
model<-randomForest(class ~. ,data = df, ntree= 500)
varImpPlot(model)
