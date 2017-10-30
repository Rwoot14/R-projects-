#LAGGED MODEL

residuals <- reg_output$residuals
figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_data_table$Time,y=residuals))
figure <- figure + geom_line(aes(x=Sales_data_table$Time,y=residuals))
figure <- figure + ggtitle("Residuals vs. Time") + xlab("Time") + ylab("Residuals") 
print (figure)

A_time_series <- ts(Sales_data_table[3]) 
colnames(Y_time_series) <- "A"
#Compute autocorrelation function
acf_coef <- acf(A_time_series) 
print(acf_coef)

acf_residuals <- acf(residuals)
print (acf_residuals)

Sales_data_table$A_lag_2[2:27] <- Sales_data_table$A[1:26]
is.na(Sales_data_table$A_lag_2[1])
print (head(Sales_data_table))
# Regress log_A against log_A_lag, Time and Time^2
reg_output_lag <- lm(A ~ A_lag_2 + Time + Time_sq, Sales_data_table)
cat ("\n", "Regression output for A = Alpha + Beta1*A(-1) + Beta2*Time + Beta3*Time^2 is:", "\n")
print (summary(reg_output_lag))
# Construct figure to plot Residuals from the lagged model vs. Time
residuals_lag <- reg_output_lag$residuals

figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_data_table$Time[2:27],y=residuals_lag))
figure <- figure + geom_line(aes(x=Sales_data_table$Time[2:27],y=residuals_lag))
figure <- figure + ggtitle("Residuals (from lagged model) vs. Time") + xlab("Time") + ylab("Residuals") 
print (figure)
# Compute autocorrelation function of the residuals from the lagged regression
acf_residuals_lag <- acf(residuals_lag)
print (acf_residuals_lag)

#SIMPLE EXP SMOOTHING

result <- ses(A_time_series, h=3) 
print(result)
print (result$model)
Sales_data_table$fitted <- result$fitted 
Sales_data_table$residuals <- result$residuals
cat ("\n", "First six rows of the data table are:", "\n", "\n")
print (head(Sales_data_table))
cat ("\n", "Last six rows of the data table are:", "\n", "\n") 
print (tail(Sales_data_table))

figure <- ggplot()
figure <- figure+geom_point(data=Sales_data_table, aes(time, A))
figure <- figure+geom_line(data=Sales_data_table, aes(time, A))
figure <- figure+geom_point(aes(Sales_data_table$time[2:nobs], Sales_data_table$fitted[2:nobs]), color="Red")
figure <- figure+geom_line(aes(Sales_data_table$time[2:nobs], Sales_data_table$fitted[2:nobs]), color="Red", lty=2)
figure <- figurexlab("Time") + ylab("Dow Jones") + ggtitle("Time series plot of Dow Jones (Solid black line) and in-sample forecasts")
print (figure)

figure <- ggplot()
figure <- figure + geom_point(aes(Sales_data_table$time[2:27], Sales_data_table$residuals[2:27]))
figure <- figure + geom_line(aes(Sales_data_table$time[2:27], Sales_data_table$residuals[2:27]))
figure <- figure + xlab("Time") + ylab("Residuals") + ggtitle("In-sample forecast errors")
print (figure)

acf_residuals <- acf(data_table$residuals)
print (acf_residuals)

#HOLTS MODEL
result <- holt(A_time_series, h=3)
print(result)
print (result$model)
Sales_data_table$forecasts <- result$fitted
Sales_data_table$residuals <- result$residuals
cat ("\n", "First six and last six rows of the data table are:", "\n", "\n") 
print (head(Sales_data_table))
print (tail(Sales_data_table))
cat ("\n", "First six and last six rows of L(t) and T(t) are:", "\n", "\n")
print (head(result$model$state[,1:2]))
print (tail(result$model$state[,1:2]))

figure = ggplot()
figure <- figure + geom_point(data=Sales_data_table, aes(Time, residuals))
figure <- figure + geom_line(data=Sales_data_table, aes(Time, residuals))
figure <- figure + geom_hline(yintercept=0)
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Residuals vs. Time") + xlab("Time") + ylab("Residuals") 
print (figure)
# Compute autocorrelation function of residuals
acf_residuals <- acf(Sales_data_table$residuals)
