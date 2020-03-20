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

#Check other null values
if (any(df=='?'))
  print('At least one null value is in df')

# From categorical to numerical
must_convert<-sapply(df,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
df2<-sapply(df[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
out_df<-cbind(df[,!must_convert],df2)   # complete data.frame with all variables put together

# Correlation matrix
library(corrplot)
library(RColorBrewer)
M <-cor(out_df)
corrplot(M, type="upper", order="original",tl.cex = 0.5,
         col=brewer.pal(n=8, name="RdYlBu"))

# Combining function
combine_column<-function(col1,col2){
return(paste(df$col1,df$col2,sep=""))    
}

#TO DO: apply to all columns

# Apply a selection function to select relevant features (Lasso,Ridge,Random Forest and so on)