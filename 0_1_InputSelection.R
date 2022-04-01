
setwd("E:\\SFI project\\R\\codes\\Mohammad/Data/")
library("readxl")
Data11_LT1<-read_excel("QuAntile_Pixel_PRT_All_Basin_annually_LT1_with_null.xlsx", sheet = "Basin11_LT1")
ind_na<- which(is.na(Data11_LT1$Obs_Runoff)==TRUE)

Data11_LT1_withoutNA<- Data11_LT1[-ind_na,]


#Rank features by importance
library(caret)
control <- trainControl(method="LOOCV")

#method=gamboost   :  Boosted Generalized Additive Model
# method= lm  : linear regression
model <- train(Obs_Runoff~., data=Data11_LT1_withoutNA[,-c(1,2)], method="rf", preProcess="scale", trControl=control) #select the method from here: http://topepo.github.io/caret/train-models-by-tag.html

Var_Imp<- varImp(model, scale=FALSE)
plot(Var_Imp)


#-------------------------------------------------------------------
#Select features 
set.seed(7)
# load the library
library(mlbench)
library(caret)

#define random forest selection function
control <- rfeControl(functions=rfFuncs, method="LOOCV", number=10)

# run the RFE algorithm
results <- rfe(Data11_LT1_withoutNA[,3:38], Data11_LT1_withoutNA$Obs_Runoff, sizes=c(2:20), rfeControl=control)
# summarize the results
print(results)

# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o")) # seems that the first 6 predictors can be used for further steps
