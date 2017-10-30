library(ggplot2)
library(fpp)
# Open file for output
file_output <- "Gas.dat.txt"
sink (file_output, append=FALSE, split=TRUE)
# Read and print data
file_data <- "Gas.dat.txt"
Gas_data_table <- read.table(file_data, header = TRUE, sep = "") 
colnames(Gas_data_table) <- c("Time", "Gas")
cat ("\n", "First six rows of data are:", "\n", "\n") 
print(head(Gas_data_table))

#Gas consumption v time graph
figure = ggplot()
figure <- figure + geom_point(data=Gas_data_table, aes(Time, Gas))
figure <- figure + geom_line(data=Gas_data_table, aes(Time, Gas))
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Gas consumption vs. Time") + xlab("Time") + ylab("Gas") 
print (figure)

#transorm gas into log_gas
Gas_data_table$log_Gas<-log(Gas_data_table$Gas)

#put log gas into a time series object
log_Gas_time_series<-ts(Gas_data_table[,3], frequency=12)


# Use the R package stl to decompose log(Gas)
fit <- stl(log_Gas_time_series, s.window=7)
plot(fit)

#put seasonal indicies into table 
Gas_data_table$seasonal <- fit$time.series[,1] 
print(seasonal)

# Seasonally adjusted sales values
Gas_data_table$log_A_stl<-(Gas_data_table$log_Gas-Gas_data_table$seasonal)

#puts seasonally adjusted sales values back into original units 
Gas_data_table$A_stl <-exp(Gas_data_table$log_Gas-Gas_data_table$seasonal)

#plot of seasonally adj Gas
figure = ggplot()
figure <- figure + geom_point(data=Gas_data_table, aes(Time, log_A_stl))
figure <- figure + geom_line(data=Gas_data_table, aes(Time, log_A_stl))
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("log_A_stl vs. Time") + xlab("Time") + ylab("Log_A_stl") 
print (figure)


#ACF to check if we need a stationary stuff
acf_Y <- acf(Gas_data_table$log_A_stl)


#PACF to see number of lags needed
pacf_Y <- pacf(Gas_data_table$log_A_stl)

#differencing

diff_Orders_lnSeasAdj <- diff(Gas_data_table$log_A_stl, difference=1) 
Gas_data_table$diff_Orders_lnSeasAdj[1] <- NA 
Gas_data_table$diff_Orders_lnSeasAdj[2:193] <- diff_Orders_lnSeasAdj

cat ("\n", "First six rows of the data table are:", "\n", "\n") 
print (head(Gas_data_table))
cat ("\n", "Last six rows of the data table are:", "\n", "\n")
print (tail(Gas_data_table))

#plot of differences

mean_diff_Orders_SeasAdj <- mean(Gas_data_table$diff_Orders_SeasAdj[2:193])
cat ("\n", "Mean of first differences of seasonally adjusted Gas consumption is:", "\n")
print (mean_diff_Orders_SeasAdj)
figure = ggplot()
figure <- figure + geom_point(aes(Gas_data_table$Time, Gas_data_table$diff_Orders_SeasAdj))
figure <- figure + geom_line(aes(Gas_data_table$Time, Gas_data_table$diff_Orders_SeasAdj))
figure <- figure + geom_hline(aes(yintercept=mean_diff_Orders_SeasAdj))
figure <- figure + xlab("Time") + ylab("First Differences of Log of Seasonally Adjusted Gas Consumption") 
figure <- figure + ggtitle("First Differences of Log of Seasonally Adjusted Gas Consumption vs. Time")
figure <- figure + theme(plot.title = element_text(hjust = 0.5))
figure <- figure + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())
figure <- figure + theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))
print (figure)

#Auto correlation function
acf(Gas_data_table$diff_Orders_lnSeasAdj[2:193])  

#Compute partial autocorrelation function of diff_Orders_SeasAdj_lag1
pacf_diff_Orders_lnSeasAdj <- pacf(Gas_data_table$diff_Orders_lnSeasAdj[2:193]) 

#puts the seasonally adj log Gas into time series object
y_time_series <- ts(Gas_data_table[,5])


#ARIMA(2,1,0)
result_1<-Arima(y_time_series,order=c(2,1,0),include.constant=TRUE)
print(result_1)

#ACF of ARIMA (2,1,0) To show that more lags are needed 
acf_residuals_1<-acf(result_1$residuals)
print(acf_residuals_1)

#ARIMA(12,1,0)
result <- Arima(y_time_series, order=c(12, 1, 0), include.constant=TRUE)
cat ("\n", "ARIMA results are:", "\n", "\n")
print (result)  

#ACF of ARIMA (12,1,0)
acf_residuals<-acf(result$residuals)
print(acf_residuals)

#forecast of seasonally adj log Gas

forecast(result, h = 3)
# forecasting starts at Febuary, March, April... (real data ends in jan)
#182,183,184

# 194
print(exp(14.56753+0.21519454))
# 80%
print(exp(14.52644+0.21519454))
print(exp(14.60862+0.21519454))

#195
print(exp(14.64469+0.11890821))
#80%
print(exp(14.59421+0.11890821))
print(exp(14.69517+0.11890821))

print(tail(Gas_data_table_extend))

# creates new table w/ forecasted values, seasons 
Seasonal_extend<-c(Gas_data_table$seasonal,Gas_data_table$seasonal[182:184]) 
Time_extend<-c(Gas_data_table$Time,194,195,196)
log_A_stl_extend<-c(Gas_data_table$log_A_stl,NA,NA,NA)
Gas_data_table_extend <- data.frame(Time_extend, log_A_stl_extend, Seasonal_extend)

Gas_data_table_extend$Gas<-c(Gas_data_table$Gas[1:193],NA,NA,NA)
Gas_data_table_extend$Gas_extend<-exp(log_A_stl_extend+Seasonal_extend)
Gas_data_table$extend<-Gas_data_table$log_A_stl[1:196]

#Creating fitted values Residuals=actual-fitted -> fitted=actual-Residuals

Gas_data_table_extend$residuals<-c(result$residuals[1:193],NA,NA,NA)
Gas_data_table_extend$Fitted<-Gas_data_table_extend$log_A_stl_extend-Gas_data_table_extend$residuals
Gas_data_table_extend$Fitted[194:196]<-c(14.56753,14.64469,14.61919)

#Adding seasonality and exponential 
Gas_data_table_extend$fitted_after<-exp(Gas_data_table_extend$Fitted+Gas_data_table_extend$Seasonal_extend)

#In sample and out of sample forecast with seasonality
figure_forecast <- ggplot()
figure_forecast <- figure_forecast + geom_line(aes(x=Gas_data_table_extend$Time,y=Gas_data_table_extend$Gas),linetype=1) 
figure_forecast <- figure_forecast +geom_line(aes(x=Gas_data_table_extend$Time,y=Gas_data_table_extend$fitted_after),linetype=2, color="red") 
figure_forecast<- figure_forecast+geom_point(aes(Gas_data_table_extend$Time,Gas_data_table_extend$fitted_after,color="red"))
figure_forecast <- figure_forecast + ggtitle("Gas Consumption (black) and in-sample/out of sample(Red) Gas Consumption ") + xlab("Time") + ylab("Gas") 
print (figure_forecast)


#plot of forecast of log seasonally adj Gas
plot(forecast(result, h = 3))


#plot of the residuals 
mean_residuals <- mean(result$residual)
cat ("\n", "Mean of residuals is:", "\n")
print (mean_residuals)
figure = ggplot()
figure <- figure + geom_point(aes(Gas_data_table$Time, result$residuals))
figure <- figure + geom_line(aes(Gas_data_table$Time, result$residuals))
figure <- figure + geom_hline(aes(yintercept=mean_residuals))
figure <- figure + xlab("Time") + ylab("Residuals")
figure <- figure + ggtitle("Residuals vs. Time")
figure <- figure + scale_y_continuous()
figure <- figure + theme(plot.title = element_text(hjust = 0.5))
figure <- figure + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                         panel.background = element_blank())
figure <- figure + theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))
print (figure)


result <- holt(y_time_series, h=3)
print(result)
print (result$model)


