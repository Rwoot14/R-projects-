# Open file for output

file_output <- "STA372_Homework5_Question1_finished.txt" 
sink (file_output, append=FALSE, split=TRUE)

#Read Sales data from a file and compute log(Sales)

file <- "STA372_Homework5_Question1.dat.txt"
Sales_table <- read.table(file, header = FALSE, sep = "") 
colnames(Sales_table) <- c("Time","Quarter","Sales") 
Sales_table$LogSales <- log(Sales_table$Sales)
cat ("\n", "First six rows of the data set are:", "\n", "\n") 
print(head(Sales_table))

#(a) Use ggplot2 to plot Salest vs. Time. Is there a pattern in the data?
figure<-ggplot()
figure <- figure + geom_point(aes(x=Sales_table$Time, y=Sales_table$Sales), color="Black")
figure <- figure + geom_line(aes(x=Sales_table$Time, y=Sales_table$Sales), linetype=1, color="Black") 
figure <- figure + ggtitle("Sales Vs Time") + xlab("Time") + ylab("Sales")
print (figure)

# Save log(sales) data as a time series object - Note the "comma" before 3 in Sales_table[,3] 

Log_Sales_time_series <- ts(Sales_table[,4], frequency=4)

#Use the R package stl to decompose log(Sales)

fit <- stl(Log_Sales_time_series, s.window=7) 
plot(fit)
Sales_table$Log_seasonal <- fit$time.series[,1] 
Sales_table$seasonal<-exp(Sales_table$Log_seasonal)
print(seasonal)

# Seasonally adjusted sales values

Sales_table$A_stl <- exp(Sales_table$LogSales - seasonal)

# Plot seasonally adjusted values from stl

figure_2 <- ggplot()
figure_2 <- figure_2 + geom_point(aes(x=Sales_table$Time, y=A_stl), color="Black")
figure_2 <- figure_2 + geom_line(aes(x=Sales_table$Time, y=A_stl), linetype=1, color="Black") 
figure_2 <- figure_2 + scale_y_continuous()
figure_2 <- figure_2 + ggtitle("Seasonally adjusted sales using STL") + xlab("Time") + ylab("A")
print (figure_2)


#Compute log(A) and Time^2, and put them in the data table

Sales_table$log_A_stl <- log(Sales_table$A_stl)
Sales_table$Time_sq <- Sales_table$Time^2 
print (head(Sales_table))

# Regress log_A_stl against Time and Time^2

reg_output <- lm(log_A_stl ~ Time + Time_sq, Sales_table)
cat ("\n", "Regression output for log_A_stl = Alpha + Beta1*Time + Beta2*Time^2 is:", "\n")
print(summary(reg_output))

# Construct figure to plot Residuals vs. Time

residuals <- reg_output$residuals
figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_table$Time,y=residuals))
figure <- figure + geom_line(aes(x=Sales_table$Time,y=residuals))
figure <- figure + ggtitle("Residuals vs. Time") + xlab("Time") + ylab("Residuals") 
print (figure)

# Compute autocorrelation function of the residuals

acf_residuals <- acf(residuals) 
print (acf_residuals)

# Lag log_A by one period and then set first observation to NA since it is missing, i.e. Not Available

Sales_table$log_A_stl_lag[2:40] <- Sales_table$log_A_stl[1:39] 
is.na(Sales_table$log_A_stl_lag[1])
print (head(Sales_table))

# Regress log_A_stl against log_A_stl_lag, Time and Time^2

reg_output_lag <- lm(log_A_stl ~ log_A_stl_lag + Time + Time_sq, Sales_table)
cat ("\n", "Regression output for log_A_stl = Alpha + Beta1*log_A_stl(-1) + Beta2*Time + Beta3*Time^2 is:", "\n") 
print (summary(reg_output_lag))


# Construct figure to plot Residuals from the lagged model vs. Time
residuals_lag <- reg_output_lag$residuals
figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_table$Time[2:40],y=residuals_lag))
figure <- figure + geom_line(aes(x=Sales_table$Time[2:40],y=residuals_lag))
figure <- figure + ggtitle("Residuals (from lagged model) vs. Time") + xlab("Time") + ylab("Residuals") 
print (figure)

# Compute autocorrelation function of the residuals from the lagged regression
acf_residuals_lag <- acf(residuals_lag) 
print (acf_residuals_lag)



######## question g and on still in progress
Sales_table$fitted_values_log_A = reg_output$coef[1] + reg_output$coef[2]*Sales_table$Time + reg_output$coef[3]*Sales_table$Time_sq 
print(head(Sales_table$fitted_values_log_A))
print(tail(Sales_table$fitted_values_log_A))
Sales_table$fitted_values_A = exp(fitted_values_log_A)
print(head(Sales_table$fitted_values_A))
print(tail(Sales_table$fitted_values_A))



figure_3 <- ggplot()
figure_3 <- figure_3 + geom_point(aes(x=Sales_table$Time,y=A_stl))
figure_3 <- figure_3 + geom_line(aes(x=Sales_table$Time,y=fitted_values_A))
figure_3 <- figure_3 + ggtitle("At vs. Time") + xlab("Time") + ylab("Residuals") 
print (figure_3)

Sales_table$in_sample_forcast_Sales=Sales_table$fitted_values_A*Sales_table$seasonal
print(head(Sales_table$in_sample_forcast_Sales))
print(tail(Sales_table$in_sample_forcast_Sales))

figure_4<-ggplot()
figure_4<-figure_4+geom_point(aes(x=Sales_table$Time,y=Sales_table$in_sample_forcast_Sales))
figure_4<-figure_4+geom_line(aes(x=Sales_table$Time,y=Sales_table$in_sample_forcast_Sales))
figure_4<-figure_4+geom_line(aes(x=Sales_table$Time,y=Sales_table$Sales),linetype=2)
print(figure_4)

Time <- c(Sales_table$Time,41:42)
Time_sq <- Time^2
Seasonal <- c(Sales_table$seasonal,0.8570889, 0.8663745) 
Data_table_extended <- data.frame(Time, Time_sq, Seasonal)


Data_table_extended$fitted_values_log_A = reg_output$coef[1] + reg_output$coef[2]*Data_table_extended$Time + reg_output$coef[3]*Data_table_extended$Time_sq 
Data_table_extended$fitted_values_A = exp(Data_table_extended$fitted_values_log_A)
Data_table_extended$in_sample_forcast_Sales= Data_table_extended$fitted_values_A*Data_table_extended$Seasonal

figure_5<-ggplot()
figure_5<-figure_5+geom_point(aes(x=Data_table_extended$Time,y=Data_table_extended$in_sample_forcast_Sales))
figure_5<-figure_5+geom_line(aes(x=Data_table_extended$Time,y=Data_table_extended$in_sample_forcast_Sales))
figure_5<-figure_5+geom_line(aes(x=Sales_table$Time,y=Sales_table$Sales),linetype=2)
print(figure_5)

new.df <- data.frame(Total=c(41, 42))
predict_log<-predict(reg_output, new.df,interval="prediction")
predict_e<-exp(predict_log)
predict_sales<-predict_e*Data_table_extended$Seasonal
print(tail(predict_sales))









