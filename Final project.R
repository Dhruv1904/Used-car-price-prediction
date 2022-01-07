library(corrplot)
library(ggplot2)
library(tidyverse)
library(dplyr)
cor(house_data)
corrplot(cor(house_data), method="square",type = "lower", order="hclust", addCoef.col = "black",number.cex = 0.5)





library(readr)
df <- read_csv("usedcar_clean.csv")
head(df)


levels(as.factor(df$Seats))


x<-c(nrow(df[df$Owner_Type=="First",]),
     nrow(df[df$Owner_Type=="Second",]),
     nrow(df[df$Owner_Type=="Third",]),
     nrow(df[df$Owner_Type=="Fourth & Above",]))


as.data.frame(owners)



df$Poer_bhp <- as.numeric(df$Poer_bhp)

sum(is.na(df$Poer_bhp))

df<-na.omit(df)
df_numeric<-select(df,-c(Brand,Model,Location,Fuel_Type,
                         Transmission,Owner_Type))


corrplot(cor(df_numeric), method="square",type = "lower", order="hclust", addCoef.col = "black",number.cex = 0.5)


boxplot(df$Engine_CC)

boxplot(df$Poer_bhp)

df$performance <- NA
a<-0
for (a in 1:nrow(df)){
  if(df$Engine_CC[a]>2000){
    df$performance[a] <- 1}
  else if(df$Poer_bhp[a]>200)  {
    df$performance[a] <-1
  }
   else {
  df$performance[a]<-0
   }
}
sum(df$performance)

hist(df$Seats)

