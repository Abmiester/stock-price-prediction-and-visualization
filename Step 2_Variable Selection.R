#_________________________Feature Selection__________________________________________________
# load the dataset
fulldata <- fulldata[,2:70]
useddata <- subset(fulldata, select = -c(Target1, Target3, Target4,Target5))
# # load the library
library(mlbench)
library(caret)
# # define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# # run the RFE algorithm
results <- rfe(useddata[,1:64], useddata[,65], sizes=c(1:64), rfeControl=control)
# # summarize the results
print(results)
# # list the chosen features
predictors(results)
# # plot the results
plot(results, type=c("g", "o"))
