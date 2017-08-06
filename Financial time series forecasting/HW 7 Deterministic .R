library(ggplot2)
# Open file for output
file_output <- "Gas.dat.txt"
sink (file_output, append=FALSE, split=TRUE)
# Read and print data
file_data <- "Gas.dat.txt"
Sales_data_table <- read.table(file_data, header = TRUE, sep = "") 
colnames(natural_gas) <- c("Time", "Gas")
cat ("\n", "First six rows of data are:", "\n", "\n") 
print(head(natural_gas))

#Gas consumption v time graph
figure = ggplot()
figure <- figure + geom_point(data=natural_gas, aes(Time, Gas))
figure <- figure + geom_line(data=natural_gas, aes(Time, Gas))
figure <- figure + geom_hline(yintercept=0)
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Gas consumption vs. Time") + xlab("Time") + ylab("Gas") 
print (figure)

#transorm gas into log_gas
Sales_data_table$log_Gas<-log(Sales_data_table$Gas)

#put log gas into a time series object
log_Gas_time_series<-ts(Sales_data_table[,3], frequency=12)


# Use the R package stl to decompose log(Gas)
fit <- stl(log_Gas_time_series, s.window=7)
plot(fit)

#put seasonal indicies into table 
Sales_data_table$seasonal <- fit$time.series[,1] 
print(seasonal)

# Seasonally adjusted sales values
Sales_data_table$log_A_stl<-(Sales_data_table$log_Gas-seasonal)

#puts seasonally adjusted sales values back into original units 
Sales_data_table$A_stl <-exp(Sales_data_table$log_Gas-seasonal)

#plot of seasonally adj sales
figure = ggplot()
figure <- figure + geom_point(data=Sales_data_table, aes(Date, A_stl))
figure <- figure + geom_line(data=Sales_data_table, aes(Date, A_stl))
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("A_stl vs. Time") + xlab("Time") + ylab("A_stl") 
print (figure)

# Date^2 
Sales_data_table$Date_sq <- Sales_data_table$Date^2 

# Regress log_A against Time and Time^2
reg_output <- lm(Sales_data_table$log_A_stl ~ Sales_data_table$Date + Sales_data_table$Date_sq) 
cat ("\n", "Regression output for A = Alpha + Beta1*Date model + Beta2*Date^2 is:", "\n") 
print(summary(reg_output))

#residuals of deterministic trend
residuals <- reg_output$residuals
figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_data_table$Date,y=residuals))
figure <- figure + geom_line(aes(x=Sales_data_table$Date,y=residuals))
figure <- figure + ggtitle("Residuals (from lagged model) vs. Time") + xlab("Time") + ylab("Residuals") 
print (figure)


#ACF to check if we need a lag or not
acf_Y <- acf(Sales_data_table$A_stl)
print (acf_Y)

#PACF to see number of lags needed
pacf_Y <- pacf(Sales_data_table$A_stl)
print (pacf_Y)

#ACF of residuals 
acf_residuals <- acf(residuals)
print (acf_residuals)

