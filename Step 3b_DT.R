#--------------------------------Decision Tree--------------------------------------
# load the dataset
fulldata <- fulldata[,2:70]
useddata <- subset(fulldata, select = -c(Target1, Target3, Target4,Target5)) # Target 2 is selected, each target should be tried once

# Create the training and testing data sets
splitIndex <- createDataPartition(useddata$Target2, p = .9, list = FALSE, times = 1)
trainDF <- useddata[splitIndex,]
testDF <- useddata[-splitIndex,]

imp_vars <- c("close", "open", "high","low","P.E.ratio", "Wiki_5_day_Disparity","Wiki_Move","Wiki_MA3_Move", "Wiki_EMA5_Move", "Wiki_5_day_Disparity","Google_EMA5_Move", "Google_3dayDisparity_Move","Google_ROC_Move","Google_RSI_Move","Wiki_3_day_Disparity" ,"Stochastic.Oscillator" ,"RSI_Move", "Wiki_RSI_Move","Google_MA_6" ,"Google_Move","Target2")
imp_data_train <- trainDF[imp_vars]
imp_data_test <- testDF[imp_vars]

# Building the classifier

dc.model <- C5.0(imp_data_train[-21],imp_data_train$Target2)

summary(dc.model)

# Evaluating model performance

dc.predict <- predict(dc.model, imp_data_test)

CrossTable(imp_data_test$Target2, dc.predict, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actural','predicted'))


# Improving model performance

dc.model.10 <- C5.0(imp_data_train[-21],imp_data_train$Target2, trails =10)
dc.predict.10 <- predict(dc.model.10, imp_data_test)
summrary(dc.predict.10)
CrossTable(imp_data_test$Target2, dc.predict.10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actural','predicted'))


