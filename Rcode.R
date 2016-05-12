setwd('~/Desktop/case study/FarmersBusinessNetwork/reapplicationfordatascientistpositionatfarmersbusin/')

source('functions.R')

#merging the data based on mergeDataPoints function created in functions.R
finalData <- completeData(file1="harvest_sample_data", file2="planting_sample_data")

#complete data as a csv file
write.csv(finalData, file="completeData.csv")

str(finalData)
nrow(finalData) #same as harvest data
attach(finalData)

#check for missing data
colSums(is.na(finalData))

########################### EXPLORATORY DATA ANALYSIS AND SUMMARY STATISTICS ################
# summary by seed variety 
summary(finalData)
with(finalData, tapply(yield, variety, mean))
with(finalData, tapply(seeding_rate, variety, range))

#ggpairs to visualize all the variables in one plot
library(GGally)
ggpairs(finalData[,c(3:6)])


library(ggplot2)
ggplot(finalData, aes(x=seeding_rate, y=yield)) + geom_point(alpha=0.4) + facet_wrap(~variety) + theme_bw()
ggplot(finalData, aes(x=seed_spacing, y=yield)) + geom_point(alpha=0.4) + facet_wrap(~variety) + theme_bw()
ggplot(finalData, aes(x=yield)) + geom_histogram() +facet_grid(~ variety)
ggplot(finalData, aes(x=variety, y=seed_spacing)) + geom_boxplot()
cor(finalData[,c(3,5:6)])

#summary by factor variable - variety
library(dplyr)
finalData %>% select(yield:seed_spacing) %>% group_by(variety) %>% summarise_each(funs(mean))

#scale the variables
maxYield <- max(finalData$yield)
finalData <- finalData %>% mutate(yield=yield/max(yield), 
                                  seeding_rate=seeding_rate/max(seeding_rate), 
                                  seed_spacing=seed_spacing/max(seed_spacing))


#using decision tree to understand the relationship between variables
library(rpart)
cont.parms <- rpart.control(minsplit = 20,cp = 0.003)
model <- rpart("yield ~ variety + seeding_rate + seed_spacing", data = finalData, control=cont.parms)

#pruning the tree
min.xerror <- model$cptable[which.min(model$cptable[,"xerror"]),"CP"]
rt.pruned <- prune(model,cp = min.xerror) 

#plot the pruned tree
library(rpart.plot)
prp(rt.pruned) 

#random forest to get the variable importance plot
library(randomForest)
model.rf <- randomForest(yield ~ variety + seeding_rate +seed_spacing , data=finalData, importance=TRUE, ntree=2000)
varImpPlot(model.rf)

################################### PREDICTION ##################################
#predicting yield by divinding into train and test data 
train.ind <- sample(1:nrow(finalData), 0.70*nrow(finalData))
train <- finalData[train.ind,]
test <- finalData[-train.ind,]

#prediction accuracy on training data using randomForest
#randomforest_model is a function created in functions.r file
#rescaling the yield variable by multiplying with max yield value
train$predict <- randomforest_model(train, train)
rmse_train <- sqrt(mean(((train$yield * maxYield) - (train$predict * maxYield))^2))

#prediction accuracy on test data using randomForest
test$predict <- randomforest_model(train, test)
rmse_test <- sqrt(mean(((test$yield *maxYield)  - (test$predict* maxYield))^2))

#plot between predicted and original yield for train data 
png("yieldTrain.png")
plot(x=train$yield*maxYield, y=train$predict*maxYield, xlim=range(train$yield*maxYield), ylim=range(train$yield*maxYield), xlab="train original yield", ylab="train predicted yield")
abline(0,1)
dev.off()

#plot between predicted and original yield for test data 
png("yieldTest.png")
plot(x=test$yield*maxYield, y=test$predict*maxYield, xlim=range(test$yield*maxYield), ylim=range(test$yield*maxYield), xlab="test original yield", ylab="test predicted yield")
abline(0,1)
dev.off()


##10-fold cross validation to get a reliable estimate of rmse
nfolds <- 10
case.folds <- rep(1:nfolds,length.out=nrow(finalData))
# divide the cases as evenly as possible
case.folds <- sample(case.folds)

rmse_cv <- double(10)
for(fold in 1:nfolds)
{ 
  train <- finalData[case.folds!=fold,]
  test <- finalData[case.folds==fold, ]
  
  #using randomForest to get predicted value for yield
  test$predict <- randomforest_model(train, test)
  rmse_cv[fold] <- sqrt(mean((test$yield*maxYield - test$predict*maxYield)^2))
}

#final cross validation rmse
mean(rmse_cv)

#using gradient boosted method for predicting yield on training data
library(gbm)
fit <- gbm(yield ~ variety + seeding_rate +seed_spacing, data=train, distribution="gaussian")
predictions <- predict(fit, train)
rmse_gbm <- sqrt(mean((train$yield*maxYield - predictions*maxYield)^2))
plot(train$yield, predictions, xlim=range(train$yield), ylim=range(train$yield))

###############################################################################






