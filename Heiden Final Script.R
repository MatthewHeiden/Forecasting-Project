library(readxl)
library(YRmisc)
library(robust)
library(mgcv)

mainpath <- "~/Desktop/school/st johns/spring 2020/BUA 633 forecasting/"
filename <- "Heiden BUA633 Final.xlsx"
air_data <- read_xlsx(paste(mainpath,filename,sep = ""), sheet = "Data")

#-------changing air_data from a "table dataframe" to a "data frame"---------
data.class(air_data)
air_data <- as.data.frame(air_data)
data.class(air_data)

#------dim = "dimensions", and colnames = "names of the columns"
# Note that the data starts from Jan. 1, 2000 and goes until Feb. 1, 2020
dim(air_data)
colnames(air_data)

#----now let's see how our new data frame looks-------
air_data

#----creating air_data_is, and air_data_os-------------
#----where "-is" = "in sample", and "-os" = "out of sample"----

air_data_is<-air_data[air_data$obs<=241,]
dim(air_data_is) # should be 241 rows

air_data_os<-air_data[air_data$obs==242,] # should be 1 row (for Feb. 2020)

# ARPM ("air revenue passenger miles" is the dependent variable)
# independent variables are months, unemployment and RDPI (real disposable personal income)

# Figure 1) histograms of all variables
# par() means "partition your screen" where mfrow ("multi-figure by row") defines the vector you want,
par(mfrow=c(2,2))
	hist(air_data_is$ARPM, main = "Hist: Air Revenue Passenger Miles", xlab = "Miles (thousands)")
	hist(air_data_is$unrate, main = "Hist: Unemployment Rate", xlab = "Percentage")
	hist(air_data_is$RDPI, main = "Hist: Real Disposable Personal Income", xlab = "Billions Chained 2012 USD (seasonally adjusted)")
	
# Figure 2) time-series plots of all variables
# not sure if I should leave X-axis as "time" or change it to "Months since Jan 1, 2000" or something similar
par(mfrow=c(2,2))
  ts.plot(air_data_is$ARPM, main = "TS Plot: Air Revenue Passenger Miles", ylab = "Miles (thousands)")
  ts.plot(air_data_is$unrate, main = "TS Plot: Unemployment Rate", ylab = "Percentage")
  ts.plot(air_data_is$RDPI, main = "TS Plot: Real Disposable Personal Income", ylab = "Billions Chained 2012 USD (seasonally adjusted)")

# Figure 3) scatterplots of the dependent/target variable with each independent variable
#plot = "scatterplot", and always put in order of plot(x-variable, y-variable)
#scatter.smooth() function includes line
  
par(mfrow=c(1,2))
  scatter.smooth(air_data_is$unrate, air_data_is$ARPM, main = "Scatterplot: ARPM vs. Unemployment", sub = "Rho = -0.213", xlab = "Unemployment Rate (%)", ylab = "Miles (thousands)")
  scatter.smooth(air_data_is$RDPI, air_data_is$ARPM, main = "Scatterplot: ARPM vs. RDPI", sub = "Rho = 0.771", xlab = "RDPI", ylab = "Billions of 2012 USD")

# Table 1) descriptive statistics of each relevant variable
ds.summ(air_data_is[,3:5],2)

# Table 2) correlation
round(cor(air_data_is[,3:5]),3)

# Table 3) OLS linear regression
fit0<-lm(ARPM~obs+unrate+RDPI+Feb+March+April+May+June+July+August+Sept+Oct+Nov+Dec, data = air_data_is, na.action = na.omit)
fit0
summary(fit0)

#) Table 4) unweighted robust regression
fit1<-lmRob(ARPM~obs+unrate+RDPI+Feb+March+April+May+June+July+August+Sept+Oct+Nov+Dec, data = air_data_is, na.action = na.omit)
summary(fit1)

# Table 5) weighted robust regression
# (weights start from 1 on Jan 1, 2000, all the way to 2 on Feb 1, 2020)
my_weights <- seq(from = 1, to = 3, length.out = 241)
air_data_is <- cbind(air_data_is, my_weights)

fit2<-lmRob(ARPM~obs+unrate+RDPI+Feb+March+April+May+June+July+August+Sept+Oct+Nov+Dec, data = air_data_is, na.action = na.omit, weights = my_weights)
summary(fit2)

# Durbin-Watson could be a problem, but I'm not sure. Because I am taking seasonality into account
# with my month dummy variables
reg.dw(fit0)

names(fit0)
names(summary(fit0))

# Residual analysis
rdf<-data.frame(air_data_is[1:241,], pred=fit0$fitted.values, r=fit0$residuals)

# Figure 4) Histogram of the residuals
hist(rdf$r, main = "Hist: Residuals", xlab = "Residuals")

# Figure 5) Scatterplot of actual vs. predicted
scatter.smooth(rdf$pred,rdf$ARPM, main = "ARPM: Actual vs. Predicted Values", xlab = "Predicted Values", ylab = "Actual Values") 

#Figure 6) TS plot of actual vs. predicted
# function from YRMisc library
pl.2ts(rdf$ARPM,rdf$pred, "TS: Actual ARPM vs. Predicted (In Thousands of Miles)")

# Figure 7) Scatterplot of residuals with each independent variable
par(mfrow = c(1,2))
  scatter.smooth(rdf$unrate,rdf$r, main = "Residuals vs. Unemployment", xlab = "Unemployment Rate", ylab = "Residuals") 
  scatter.smooth(rdf$RDPI,rdf$r, main = "Residuals vs. RDPI", xlab = "RDPI", ylab = "Residuals") 

# Predict out of sample (ARPM in February 2020)
predict(fit0)
predict(fit0, air_data_os)

# Repeating analysis with GAM model
# Using mgcv library
# Table 6) GAM regression
fit3<-gam(ARPM~s(obs)+s(unrate)+RDPI+Feb+March+April+May+June+July+August+Sept+Oct+Nov+Dec,method = "REML",data=air_data, na.action=na.omit)
summary(fit3)
summary(fit3)$r.sq
reg.dw(fit3)

predict(fit3, air_data_os)

# Figure 8) Smoothing Terms

plot(fit3, air_data_is$obs, main = "Secular Increase in Air Travel in GAM Model", xlab = "Months (Beginning Jan. 2000)", ylab = "GAM Smooth Terms")
par(mfrow = c(1,2))
plot(fit3, air_data_is$unrate, main = "GAM Smooth Terms vs. Unemployment", xlab = "Unemployment Rate", ylab = "GAM Smooth Terms")

# GAM residual analysis
rdf3<-data.frame(air_data_is[1:241,], pred=fit3$fitted.values, r=fit3$residuals)

# Figure 9) GAM: histogram of the residuals
hist(rdf3$r, main = "Hist: Residuals (GAM model)", xlab = "Residuals")

# Figure 10) GAM: scatterplot of actual vs. predicted
scatter.smooth(rdf3$pred,rdf3$ARPM, main = "ARPM: Actual vs. Predicted Values (GAM model)", xlab = "Predicted Values", ylab = "Actual Values") 

# Figure 11) GAM: TS plot, real vs. predicted
pl.2ts(rdf3$ARPM,rdf3$pred, "TS: Actual ARPM vs. Predicted (GAM Model, In Thousands of Miles)")

# Figure 12) GAM: scatterplot of residuals with each independent variable
par(mfrow = c(1,2))
scatter.smooth(rdf3$unrate,rdf3$r, main = "Residuals (GAM Model) vs. Unemployment", xlab = "Unemployment Rate", ylab = "Residuals") 
scatter.smooth(rdf3$RDPI,rdf3$r, main = "Residuals (GAM Model) vs. RDPI", xlab = "RDPI", ylab = "Residuals") 

