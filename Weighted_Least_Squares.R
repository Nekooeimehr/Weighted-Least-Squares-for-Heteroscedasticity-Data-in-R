######################## The function for Weighted Least Squares Model ######################
WLS_Predictor = function(TrainData, TestData, tol)
{
  j = 0
  eps = 1000
  OLSmodel = lm(y~x, data = TrainData)
  Coef = OLSmodel$coefficients
  b2 = Coef[2]
  TrainData$resid = abs(residuals(OLSmodel))
  while(eps > Tol & j < 2){
    j = j + 1
    X = TrainData$x
    X = cbind(1,X)
    b = b2
    Poly_model_Resid = lm(resid~poly(x,2), data = TrainData)
    Ws = (1/fitted(Poly_model_Resid)^2)
    Ws_Mtx = diag(Ws)
    NewCoef = solve(t(X) %*% Ws_Mtx %*% X) %*% t(X) %*% Ws_Mtx %*% TrainData$y
    b2 = NewCoef[2]
    TrainData$resid = abs(TrainData$y - (X %*% NewCoef))
    # WLSmodel = lm(y~x, data = TrainData, weight = Ws)
    # TrainData$resid = abs(residuals(WLSmodel))
    # Coef = WLSmodel$coefficients
    # b2 = coef[2]
    eps = abs(b2 - b)
  }
  Xtest = TestData$x
  Xtest = cbind(1,Xtest)
  Predictions = Xtest %*% NewCoef
  MSE = mean((Predictions - TestData$y)^2)
  RSqr = sum((Predictions - mean(TrainData$y))^2)/sum((TestData$y - mean(TrainData$y))^2)
  EVL = list(MSE, RSqr)
  return(EVL)
} 

######################### The Function for Ordianry Least Squares ############################
OLS_Predictor = function(TrainData, TestData){
  OLSmodel = lm(y~x, data = TrainData)
  Predictions = predict(OLSmodel, TestData)
  MSE = mean((Predictions - TestData$y)^2)
  RSqr = sum((Predictions - mean(TrainData$y))^2)/sum((TestData$y - mean(TrainData$y))^2)
  EVL = list(MSE, RSqr)
  return(EVL)
}
 
############# The Function for Ordianry Least Squares after Removing the Outliers##############
OLS_DelOut_Predictor = function(TrainData, TestData){
  OLSmodel = lm(y~x, data = TrainData)
  No_OutlierData = TrainData[residuals(OLSmodel) > quantile(residuals(OLSmodel), .25) - 
                              1.5*IQR(residuals(OLSmodel)) & 
                              residuals(OLSmodel) < quantile(residuals(OLSmodel), .75) + 
                              1.5*IQR(residuals(OLSmodel)),]
  OLSmodel_No_Outlier = lm(y~x, data = No_OutlierData)
  Predictions = predict(OLSmodel_No_Outlier, TestData)
  MSE = mean((Predictions - TestData$y)^2)
  RSqr = sum((Predictions - mean(TrainData$y))^2)/sum((TestData$y - mean(TrainData$y))^2)
  EVL = list(MSE, RSqr)
  return(EVL)
}

######################################## Main Script ##########################################
# Loading the necessary libraries
library(caret)
# Reading the data
#setwd("C:/Users/hp/Documents/R/Problem1")

fileLists = list.files(pattern="*.csv")
NDatasets = length(fileLists)

InputData = vector("list", NDatasets)
for (i in 1:NDatasets){
  InputData[[i]] = read.csv(fileLists[i]) 
}

Tol = 0.005
NumFolds = 5
MSE_OLS = matrix(data=NA, nrow = NDatasets, ncol = NumFolds)
RSqr_OLS = matrix(data=NA, nrow = NDatasets, ncol = NumFolds)

MSE_WLS = matrix(data=NA, nrow = NDatasets, ncol = NumFolds)
RSqr_WLS = matrix(data=NA, nrow = NDatasets, ncol = NumFolds)

MSE_OLS_No_Out = matrix(data=NA, nrow = NDatasets, ncol = NumFolds)
RSqr_OLS_No_Out = matrix(data=NA, nrow = NDatasets, ncol = NumFolds)

Final_Model = vector("list", NDatasets)
for (i in 1:NDatasets)
{
  Flds = createFolds(1:nrow(InputData[[i]]), k=NumFolds, list = TRUE, returnTrain = TRUE)
  for (k in 1:NumFolds)
  {
    TrainData = InputData[[i]][Flds[[k]],]
    TestData = InputData[[i]][-Flds[[k]],]
    EVL_WLS = WLS_Predictor(TrainData, TestData, tol)
    EVL_OLS = OLS_Predictor(TrainData, TestData)
    EVL_OLS_No_Out = OLS_DelOut_Predictor(TrainData, TestData)
    MSE_WLS[i,k] = EVL_WLS[[1]] 
    RSqr_WLS[i,k] = EVL_WLS[[2]]
    MSE_OLS[i,k] = EVL_OLS[[1]] 
    RSqr_OLS[i,k] = EVL_OLS[[2]]
    MSE_OLS_No_Out[i,k] = EVL_OLS_No_Out[[1]] 
    RSqr_OLS_No_Out[i,k] = EVL_OLS_No_Out[[2]]
  }
}
RSqr_WLS_All = rowSums(RSqr_WLS)/NumFolds
MSE_WLS_All = rowSums(MSE_WLS)/NumFolds
RSqr_OLS_All = rowSums(RSqr_OLS)/NumFolds
MSE_OLS_All = rowSums(MSE_OLS)/NumFolds
RSqr_OLS_No_Out_All = rowSums(RSqr_OLS_No_Out)/NumFolds
MSE_OLS_No_Out_All = rowSums(MSE_OLS_No_Out)/NumFolds

# Friedman's Test
friedman.test(matrix(c(as.vector(RSqr_WLS), as.vector(RSqr_OLS), as.vector(RSqr_OLS_No_Out)), nrow = 25))
friedman.test(matrix(c(as.vector(MSE_WLS), as.vector(MSE_OLS), as.vector(MSE_OLS_No_Out)), nrow = 25))
