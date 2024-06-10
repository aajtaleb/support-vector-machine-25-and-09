###################Support Vector Machine (SVM) model
# This is the Support Vector Machine (SVM) binary model (presence and absence of MPB) 2005 and 2009 
data= read.csv("C:/Users/aajta/OneDrive/Desktop/Data0509/Book2005.csv")
str(data)
rows <-nrow(data)
colums <-ncol(data)
str(rows)
str(colums)
head(data[,1:10])
pacman::p_load(raster, shapefiles, sp, rgdal, randomForest, caret, e1071, 
               MASS, ROCR, corrplot, rfUtilities, VSURF, rmarkdown)
library(plyr)
count(data ["PresenceAb"])
set.seed(123)
#create new data frame with only the raster
rastercolumns<- data[, 11:24]
print(rastercolumns)
# create a list of names
Predictors <- c(' band4' , 'NBR',  'TCG' ,'B54', 'DEM' ,'AnomB54 ' , 'DiffB54 ' )
##rastercolumns_cov2 <- rastercolumns[,Predictors]
rastercolumns1 <- data[,c(14,18,19,20,22,23,24)]
print(rastercolumns1)
# create a list of names
# define the response variable
response_var1<- as.factor(data$PresenceAb)
print (response_var1)
predictor_var1<- rastercolumns1
print(predictor_var1)
## combine the two to a single data frame
dataset05<-cbind(response_var1,predictor_var1)
print(dataset05)
### SVM model
library(e1071)


svm_model05= svm(x= predictor_var1, y= response_var1, data =dataset05,
                 kernel = 'linear',
                 gamma = 0.15,
                 cost = 1)
# print svmmodel1
svm_model05
print(svm_model05)
summary(svm_model05)
#plot the model
##plot(svm_model05, data = dataset05)
#the accuracy
predicted<- predict(svm_model05)
observed<- data$PresenceAb
accuracy(x=predicted, y=observed)
tab <- table(predicted, observed)
tab
# miss classification 
1- sum(diag(tab))/ sum(tab)
# data partition
set.seed(123)
ind <- sample(2, nrow(dataset05), replace = TRUE, prob = c(0.7, 0.3)) 
train <- dataset05[ind==1,]
test <- dataset05[ind==2,]
head(test)
head(train)
svm_model2 <- svm(response_var1~., data= train, kernel = 'radial', cost=32, epsilon=0, gamma=0.1)

print(svm_model2)
summary(svm_model2)
attributes(svm_model2)
#prediction p1 library caret
library(caret)
p1 <- predict(svm_model2, train)
confusionMatrix(p1, train$response_var1)
# prediction & confusion matrix with test data
p2 <- predict(svm_model2, test)
confusionMatrix(p2,test$response_var1)
accuracy(x=p2,y=test$response_var1)
# 
svm_predictions<-predict(svm_model2,test)
#plot(svm_model2)
# tune 
set.seed(123)
tmodel <- e1071::tune(svm, response_var1~., data = dataset05,
                      ranges = list(epsilon = seq(0,1,0.1), gamma = c(0.1:10),cost = 2^(2:7)))
tmodel
tmodel$performances
###
library(raster)
library(sp)
library(rgdal)
NBR <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/NBR05.tif")
print (NBr)
B54 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/B54clip.tif")
print (B54)
NDVI <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/NDVIclip.tif")
print(NDVI)
band1 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/band1.tif")
print (band1)
AnomNBR <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/AnomNBR.tif")
print(AnomNBR)
DEM = raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/DEMclip.tif")
print (DEM)
TCG <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/TCG.tif")
print (TCG)
## Making Results spatial binary model 2005
stack_5band7b<- stack(NBR,B54,AnomB54,band4,TCG,DiffB54,DEM)
names(stack_5band7b) =c('NBR' , 'B54' ,' AnomB54 ' ,' band4' , ' TCG' ,' DiffB54', 'DEM' )

predict(stack_5band7b ,svm_model2,filename="2ndSVMfinalBINARY2005.tif",
        fun=predict,
        # format="Gtiff",
        # datatype="FLT4S",
        overwrite=TRUE)
### convert to raster
library(rgdal)
library(raster)
binary <- raster("2ndSVMfinalBINARY2005.tif")
## Plot the results
par(mfrow=c(1,2), mar=c(3,3,3,5))
plot(binary, main= "2ndSVM Pre and Abs T M in 2005new", xaxt= 'n', yaxt='n')

######################### SVM 2009 binary and regression models 
data= read.csv("C:/Users/aajta/OneDrive/Desktop/Data0509/Book2009.csv")
str(data)
rows <-nrow(data)
colums <-ncol(data)
str(rows)
str(colums)
head(data[,1:10])
pacman::p_load(raster, shapefiles, sp, rgdal, randomForest, caret, e1071, 
               MASS, ROCR, corrplot, rfUtilities, VSURF, rmarkdown)
library(plyr)
count(data ["PresenceAb"])
count(data["Percent_Cov"])
set.seed(123)

# Regression model percent % tree mortality
#create new data frame with only the raster
rastercolumns<- data[, 11:24]
response_var <- data$Percent_Cov
rastercolumns1 <- data[,c(15,18,19,20,21,22)]
predictor_var <- rastercolumns1
##rastercolumns1 <- data[,c(15,19,23,31)]
print(predictor_var)
dataset09 <-cbind(response_var, predictor_var)
print (dataset09)
library(e1071)
svm_model09= svm(x= predictor_var, y= response_var, data =dataset09,
                 kernel = 'linear',
                 gamma = 0.15,
                 cost = 1)
# print svmmodel1
summary(svm_model09)
#plot the model
plot(svm_model09, data = dataset09)
#the accuracy
predicted<- predict(svm_model09)
observed<- data$Percent_Cov
sqrt(mean(predicted-observed)^2) # rmse -> root mean square error for regression

R2(predicted, observed) # r_squared
# 0.5916974
#the accuracy
predicted<- predict(svm_model09)
observed<- data$PresenceAb
accuracy(x=predicted, y=observed)
tab <- table(predicted, observed)
tab
# miss classification 
1- sum(diag(tab))/ sum(tab)
# data partition
# data partition
set.seed(123)
ind <- sample(2, nrow(dataset09), replace = TRUE, prob = c(0.7, 0.3)) 
train <- dataset09[ind==1,]
test <- dataset09[ind==2,]
head(train)
svm_model2 <- svm(response_var~., data= train, kernel = 'radial', cost=4, epsilon=0.1,gamma= 0.1)
summary(svm_model2)
print(svm_model2)
attributes(svm_model2)
#prediction p1 library caret
library(caret)
p1 <- predict(svm_model2, train)
R2(p1, train$response_var)
# prediction & confusion matrix with test data
p2 <- predict(svm_model2, test)
R2(p2, test$response_var)
##accuracy(x=p2,y= test$response_var)

# Error rate of Random forest 
#plot(svm_model2)
# tune 
set.seed(123)
tmodel <- e1071::tune(svm, response_var~., data = dataset09,
                      ranges = list(epsilon = seq(0,1,0.1), gamma = c(0.1:10), cost = 2^(2:7)))
tmodel
tmodel$performances
print(tmodel)

### SVM model Binary 2009
response_var1<- as.factor(data$PresenceAb)
print (response_var1)
#Predictors1<- c('' band4' , 'NBR',  'TCG' , 'DEM' ,'AnomB54 ' , 'DiffB54 ')
rastercolumns2<-  data[,c(14,18,19,20,22,23,24)]
#rastercolumns2 <- data[,c(14,18,19,20,22,23,24))]
predictor_var1<- rastercolumns2
print(predictor_var1)
## combine the two to a single data frame
dataset09b<-cbind(response_var1,predictor_var1)
print(dataset09b)

library(e1071)
svm_model09b1= svm(x= predictor_var1, y= response_var1, data =dataset09b,
                   kernel = 'linear',
                   gamma = 0.15,
                   cost = 1)
# print svmmodel1
summary(svm_model09b1)

#plot the model
##plot(svm_modeltest09b, data = dataset09b)

#the accuracy
predicted<- predict(svm_model09b1)
observed<- data$PresenceAb
accuracy(x=predicted, y=observed)
tab <- table(predicted, observed)
tab
# miss classification 
1- sum(diag(tab))/ sum(tab)
# data partition

set.seed(123)
ind <- sample(2, nrow(dataset09b), replace = TRUE, prob = c(0.7, 0.3)) 
train <- dataset09b[ind==1,]
test <- dataset09b[ind==2,]
head(train)


svm_model2b <- svm(response_var1~., data= train, kernel = 'radial', cost=8, epsilon=0,gamma=0.1)

print(svm_model2b)
attributes(svm_model2b)
#prediction p1 library caret
library(caret)
p1b <- predict(svm_model2b, train)
confusionMatrix(p1b, train$response_var1)
# prediction & confusion matrix with test data
p2b <- predict(svm_model2b, test)
confusionMatrix(p2b,test$response_var1)
# Accuracy
accuracy(x=p2b,y=test$response_var1)
# Error rate of Random forest 
#plot(svm_model2)
# tune 
set.seed(123)
tmodel <- e1071::tune(svm, response_var1~., data = dataset09b,
                      ranges = list(epsilon = seq(0,1,0.1), gamma = c(0.1:10), cost = 2^(2:7)))
tmodel
tmodel$performances
#####Making Results spatial 
library(raster)
library(sp)
library(rgdal)
NBR == raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/nbr09_Clip2.tif")
print(NBR)
B54 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/B54clip.tif")
print(B54)
TCG <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/TCGclip2.tif")
print(TCG)
AnomB54 <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/AnomB54clip.tif")
print(AnomB54)
DiffB54 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/DiffB54clip2.tif")
print(DiffB54)
band4 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/BAND409.tif")
print(band4)
DEM <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/DEMclip.tif")
print(DEM)
band5 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/BAND509.tif")
print(band5)
MeanB54 <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/MeanB54.tif")
print(MeanB54)

## Making Results spatial binary and regression 2009
stack_9band7<- stack(NBR,B54,AnomB54,band4,TCG,DiffB54,DEM)
names(stack_9band7) =c('NBR' , 'B54' ,' AnomB54 ' ,' band4' , ' TCG' ,' DiffB54', 'DEM' )
predict(stack_9band7 ,svm_model2b,filename="NEWBINARYSVM2009Final.tif",
        fun=predict,
        # format="Gtiff",
        # datatype="FLT4S",
        overwrite=TRUE)

## map regression based the model
##' NBR  ' , 'B54',  'TCG' , 'MeanB54' ,'band5 ' , 'DEM '
# stack the objects 
stack9_band6 <- stack(NBR,B54,TCG,MeanB54,band5,DEM)
# add the head
names(stack9_band6)= c('NBR','B54', 'TCG', 'MeanB54','band5', 'DEM')
predict(stack9_band6,svm_model2,filename="NEWPercentSVM2009Final.tif",
        fun=predict,
        #format="Gtiff",
        #datatype="FLT4S",
        overwrite=TRUE)
### convert to raster
library(rgdal)
library(raster)
regression <-raster("NEWPercentSVM2009Final.tif")
binary <- raster("NEWBINARYSVM2009Final.tif")
## Plot the results
par(mfrow=c(1,2), mar=c(3,3,3,5))
plot(regression, main="NEW Final Percent of tree mortality SVM2009", xaxt='n', yaxt='n')
plot(binary, main= "NEW Final Presence and Absence tree mortality SVM2009", xaxt= 'n', yaxt='n')
