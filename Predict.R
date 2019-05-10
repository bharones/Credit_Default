library(readr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(DataExplorer)

train <- read.csv("application_train.csv")
test <- read.csv("application_test.csv")
guide <- read.csv("HomeCredit_columns_description.csv") %>% View()

head(train)
head(test)
str(train)

##Data Exploratory Analysis##

#check missing values
a1 <- as.data.frame(colSums(is.na(train)))
b1 <- format(as.data.frame(colMeans(is.na(train))*100),digits=1)
trainmiss <- data.frame(cbind(setNames(a1,c("count")),
                              setNames(b1,c("percentage")))) %>% View()

a2 <- as.data.frame(colSums(is.na(test)))
b2 <- format(as.data.frame(colMeans(is.na(test))*100),digits=1)
testmiss <- data.frame(cbind(setNames(a2,c("count")),
                             setNames(b2,c("percentage")))) %>% View()

plot_missing(train, title="Train")
plot_missing(test, title="Test")


  #Check Distribution of Target
#Code: 1 = client had payment difficulties, 0 = loan was repaid on time
train$TARGET <- as.factor(train$TARGET)
summary(train$TARGET)

#imbalanced class problem

train <- train[,-2]

cor <- cor(train[,c("NAME_EDUCATION_TYPE.Higher.education","AMT_GOODS_PRICE",
                    "DAYS_BIRTH","EXT_SOURCE_1","EXT_SOURCE_2","EXT_SOURCE_3","TARGET")], use="complete", method="spearman")

melted_cormat <- melt(cor)
head(melted_cormat)
heatmap <- ggplot(data=melted_cormat,aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
  xlab("") + ylab("")

print(heatmap)
#Train
dmy1 <- dummyVars("~CODE_GENDER", train, fullRank = T)
trsf1 <- data.frame(predict(dmy1, newdata = train))

dmy2 <- dummyVars("~OCCUPATION_TYPE", train, fullRank = T)
trsf2 <- data.frame(predict(dmy2, newdata = train))

dmy3 <- dummyVars("~NAME_EDUCATION_TYPE", train, fullRank = T)
trsf3 <- data.frame(predict(dmy3, newdata = train))

dmy4 <- dummyVars("~NAME_FAMILY_STATUS", train, fullRank = T)
trsf4 <- data.frame(predict(dmy4, newdata = train))

dmy5 <- dummyVars("~NAME_CONTRACT_TYPE", train, fullRank = T)
trsf5 <- data.frame(predict(dmy5, newdata = train))

train <- cbind(train,trsf1,trsf2,trsf3,trsf4,trsf5,fill=T)

#Test
dmy1 <- dummyVars("~CODE_GENDER", test, fullRank = T)
trsf1 <- data.frame(predict(dmy1, newdata = test))

dmy2 <- dummyVars("~OCCUPATION_TYPE", test, fullRank = T)
trsf2 <- data.frame(predict(dmy2, newdata = test))

dmy3 <- dummyVars("~NAME_EDUCATION_TYPE", test, fullRank = T)
trsf3 <- data.frame(predict(dmy3, newdata = test))

dmy4 <- dummyVars("~NAME_FAMILY_STATUS", test, fullRank = T)
trsf4 <- data.frame(predict(dmy4, newdata = test))

dmy5 <- dummyVars("~NAME_CONTRACT_TYPE", test, fullRank = T)
trsf5 <- data.frame(predict(dmy5, newdata = test))

test <- cbind(test,trsf1,trsf2,trsf3,trsf4,trsf5,fill=T)

head(train)
head(test)
              