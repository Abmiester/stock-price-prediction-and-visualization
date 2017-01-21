#---------------------------------Neural Network-----------------------------------------
# Create the training and testing data sets
# load the dataset
fulldata <- fulldata[,2:70]
useddata <- subset(fulldata, select = -c(Target1, Target3, Target4,Target5))

# Checkout on datapoint is missing
library(MASS)
apply(useddata,2,function(x) sum(is.na(x)))

# Scale the data for num/int data structure

factor_cols <- sapply(useddata, is.factor)
factor_data <- as.data.frame(sapply(useddata[,factor_cols], as.factor))
factor_col_names <- names(factor_data)
non_factor_data <- useddata[, -which(names(useddata) %in% factor_col_names)]

maxs <- apply(non_factor_data, 2, max) 
mins <- apply(non_factor_data, 2, min)
scaled <- as.data.frame(scale(non_factor_data, center = mins, scale = maxs - mins))
useddata_scaled <- cbind(scaled,factor_data)

# Create the training and testing data sets
splitIndex <- createDataPartition(useddata_scaled$Target2, p = .9, list = FALSE, times = 1)
trainDF <- useddata_scaled[splitIndex,]
testDF <- useddata_scaled[-splitIndex,]

imp_vars <- c("close", "open", "high","low","P.E.ratio", "Wiki_5_day_Disparity",
              "Wiki_Move","Wiki_MA3_Move", "Wiki_EMA5_Move", "Wiki_5_day_Disparity",
              "Google_EMA5_Move", "Google_3dayDisparity_Move","Google_ROC_Move",
              "Google_RSI_Move","Wiki_3_day_Disparity" ,"Stochastic.Oscillator" ,
              "RSI_Move", "Wiki_RSI_Move","Google_MA_6" ,"Google_Move","Target2")
imp_data_train <- trainDF[imp_vars]
imp_data_test <- testDF[imp_vars]

# Check the trianing and testing data sets
dim(imp_data_train)
dim(imp_data_test)


#Build the model (Since neuralnet only deals with quantitative variables, 
#we can convert all the qualitative variables (factors) to binary ("dummy") variables, 
#with the model.matrix function)

n <- names(imp_data_train)

m_train <- model.matrix( 
  as.formula(paste("~",paste(n, collapse = " + "))), 
  data = imp_data_train
)

m_test <- model.matrix( 
  as.formula(paste("~",paste(n, collapse = " + "))), 
  data = imp_data_test
)


neural.model <- neuralnet(Target21 ~ close + open + high + low + 
                            P.E.ratio + Wiki_5_day_Disparity + 
                            Wiki_Move1 + Wiki_MA3_Move1 + Wiki_EMA5_Move1 
                          + Wiki_5_day_Disparity + Google_EMA5_Move1 + 
                            Google_3dayDisparity_Move1 + Google_ROC_Move1 + 
                            Google_RSI_Move1 + Wiki_3_day_Disparity + 
                            Stochastic.Oscillator + RSI_Move1 + Wiki_RSI_Move1 + 
                            Google_MA_6 + Google_Move1, data = m_train,
                          hidden=2, threshold = 0.1, stepmax = 1e+05)
# Plot the model

plot(neural.model)

# Evaluate the model

neural.model










