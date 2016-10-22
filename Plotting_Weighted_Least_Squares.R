# Loading the necessary libraries
library(ggplot2)
library(gridExtra)

# Reading the data
setwd("C:/Users/hp/Documents/R/Problem1")

fileLists = list.files(pattern="*.csv")
NDatasets = length(fileLists)

InputData = vector("list", NDatasets)
for (i in 1:NDatasets){
  InputData[[i]] = read.csv(fileLists[i]) 
  InputData[[i]] = InputData[[i]][1:50,]
}

# Plotting the scatterplot for all the 5 datasets
plots = vector("list", NDatasets)
plot_resid = vector("list", NDatasets)
plots_Wresid = vector("list", NDatasets)
#plot_residSqr = vector("list", NDatasets)
plots_Models = vector("list", NDatasets)
for (i in 1:NDatasets){
  TempData = InputData[[i]]
  plots[[i]] = ggplot(InputData[[i]], aes(x,y)) + geom_point(color='darkblue') + 
    geom_smooth(aes(colour = "OLS Model", linetypes = "Dashed"), method='lm',formula=y~x, se=FALSE)
  OLSmodel = lm(y~x, data = InputData[[i]])
  TempData$resid = abs(residuals(OLSmodel))
  # TempData$residSqr = (residuals(OLSmodel)^2)
  plot_resid[[i]] = ggplot(TempData, aes(x, resid)) + geom_point(color='darkblue') + 
    geom_smooth(method='lm',formula = y~poly(x,2), se=FALSE) + 
    labs(x = "The Predictor (X)",y = "The Residuals") + geom_hline(yintercept = 0) 
  #  plot_residSqr[[i]] = ggplot(InputData[[i]], aes(x, residSqr)) + geom_point() + labs(x="X",y="Residuals") + 
  #   geom_hline(yintercept = 0)
  No_OutlierData = TempData[residuals(OLSmodel) > quantile(residuals(OLSmodel), .25) - 
                              1.5*IQR(residuals(OLSmodel)) & 
                              residuals(OLSmodel) < quantile(residuals(OLSmodel), .75) + 
                              1.5*IQR(residuals(OLSmodel)),]
  # Estimating the absolute residual function by fitting a polynomial model
  Poly_model_Resid = lm(resid~poly(x,2), data = TempData)
  Ws = (fitted(Poly_model_Resid)^2)
  # InputData[[i]]$Weightedy = InputData[[i]]$y*Ws
  plots_Models[[i]] = plots[[i]] + 
    geom_smooth(data = InputData[[i]], method='lm',aes(weight = Ws, colour = "WLS Model"), formula=y~x, se=FALSE) +
    geom_smooth(data = No_OutlierData, method='lm', aes(colour = "No Outliers"),formula = y~x, se=FALSE) +
    scale_colour_manual(name="Legend", values=c("green", "red", "lightblue"))

  WLSmodel = lm(y~x, data = InputData[[i]], weight = Ws)
  TempData$Wresid = abs(residuals(WLSmodel))
  plots_Wresid[[i]] = ggplot(TempData, aes(x, Wresid)) + geom_point(color='darkblue') + 
    geom_smooth(method='lm',formula = y~poly(x,2), se=FALSE) +
    labs(x="X",y="New Residuals") + geom_hline(yintercept = 0)
}

do.call("grid.arrange", c(plots, ncol = 3, top = "Scatter Plots (x vs y)"))
do.call("grid.arrange", c(plot_resid, ncol = 3, top = "Residuals vs the Predictor (x)"))
# do.call("grid.arrange", c(plot_residSqr, ncol = 3, top = "Squared Residuals Vs the Predictor (x)"))
do.call("grid.arrange", c(plots_Models, ncol = 3, top = "OLS Model vs WLS Model"))
do.call("grid.arrange", c(plots_Wresid, ncol = 3, top = "Residuals after Correction using WLS Model Vs the Predictosr (X)"))

