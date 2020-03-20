# Analyzing Mushroom data
library(tidyverse)
library(randomForest)
library(forecast)
library(corrplot)
library(RColorBrewer)

cat("\014")
rm(list = ls())
pathinput<-"./mushrooms.csv"
dataset<-read.csv2(pathinput , sep =",",stringsAsFactors = FALSE)
head(dataset)
str(dataset)
dataset <- dataset %>% select(-"veil.type")
#df<- dataset %>% select(c("class","cap.shape","cap.surface","cap.color","bruises","odor"))

#df<-df %>% group_by(cap.shape,class) 
# summarise(df, n())
# colSums(is.na(dataset)) 
df<- dataset %>% select(-"stalk.root")
# model<-randomForest(class ~. ,data = df, ntree= 500)
# varImpPlot(model)

#Check other null values
if (any(df=='?'))
  print('At least one null value is in df')




# Combining function
combine_column<-function(col1,col2){
  return(paste(col1,col2,sep=""))    
}
k<-ncol(df)+1
size<-ncol(df)
for(i in 2: size){
  if(i<size){
    for(j in (i+1):size){
      df[,k]<-combine_column(df[,i],df[,j])
      print(k)
      k<-k+1
    }
  }
}
# From categorical to numerical
for (i in 1:ncol(df)){
  df[,i]<-df[,i] %>% as.factor
}
out_df<-df

# must_convert<-sapply(df,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
# 
# df2<-sapply(df[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
# out_df<-cbind(df[,!must_convert],df2)   # complete data.frame with all variables put together

# Correlation matrix
empty_m <- matrix(ncol = ncol(out_df),
                  nrow = ncol(out_df),
                  dimnames = list(names(out_df), 
                                  names(out_df)))
calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}
cor_matrix <- calculate_cramer(empty_m ,out_df)
corrplot(cor_matrix, type="upper", order="original",tl.cex = 0.8,
         col=brewer.pal(n=8, name="RdYlBu"))


#TO DO: apply to all columns

# Apply a selection function to select relevant features (Lasso,Ridge,Random Forest and so on)