library(ggplot2)
# Open file for output
file_output <- "HW_8"
sink (file_output, append=FALSE, split=TRUE)
# Read and print data
file_data <- "STA372_Homework8_Question1.dat.txt"
Sales_data_table <- read.table(file_data, header = FALSE, sep = "") 
colnames(Sales_data_table) <- c("Time", "Sales", "A", "seasonal_i") 
cat ("\n", "First six rows of data are:", "\n", "\n") 
print(head(Sales_data_table))

figure <- ggplot(Sales_data_table, aes(x=Time,y=Sales))
figure <- figure + geom_line()
figure <- figure + geom_point()
figure <- figure + ggtitle("Sales vs. Time") + xlab("Time") + ylab("Sales") 
print (figure)

figure <- ggplot(Sales_data_table, aes(x=Time,y=A))
figure <- figure + geom_line()
figure <- figure + geom_point()
figure <- figure + ggtitle("A vs. Time") + xlab("Time") + ylab("A") 
print (figure)

#Compute Time^2 for regression #
Sales_data_table$Time_sq <- Sales_data_table$Time^2
# Regress A against Time and Time^2
reg_output <- lm(Sales_data_table$A ~ Sales_data_table$Time + Sales_data_table$Time_sq)
cat ("\n", "Regression output for A = Alpha + Beta1*Time model + Beta2*Time^2 is:", "\n")
print(summary(reg_output))
# Compute and print in-sample fitted values for A
fitted_values_A = reg_output$coef[1] + reg_output$coef[2]*Sales_data_table$Time + reg_output$coef[3]*Sales_data_table$Time_sq
cat ("\n", "Last six observations of the in-sample fitted values are:", "\n", "\n") 
print (tail(fitted_values_A))

# Construct figure to plot A vs. Time and in-sample fitted values - Note idea of a "layered" plot in R
figure <- ggplot(Sales_data_table, aes(x=Time,y=A))
figure <- figure + geom_point(color="Black")
figure <- figure + ggtitle("A vs. Time") + xlab("Time") + ylab("A") 
figure <- figure + geom_line(aes(y=fitted_values_A))
print(figure)
# Constructed Time and Seasonal vectors with extra four rows for forecasting
Time <- c(Sales_data_table$Time,28:29)
Time_sq <- Time^2
Seasonal <- c(Sales_data_table$Seasonal,0.968967, 0.946485, 1.165604, 0.919514) 
Data_table_extended <- data.frame(Time, Time_sq, Seasonal)
# Compute in-sample and out-of-sample forecasts for A
Data_table_extended$forecasts_A = reg_output$coef[1] + reg_output$coef[2]*Data_table_extended$Time + reg_output$coef[3]*Data_table_extended$Time_sq
# Compute in-sample and out-of-sample forecasts for Sales
Data_table_extended$forecasts_Sales = Data_table_extended$forecasts_A * Data_table_extended$Seasonal
# Print in-sample and out-of-sample forecasts for A and Sales
cat ("\n", "Last two in-sample fitted values and forecasted values for the next four quarters are:", "\n", "\n") 
print (tail(Data_table_extended))
# Plot in-sample and out-of-sample forecasts for A

figure_forecast <- ggplot()
figure_forecast <- figure_forecast + geom_line(aes(x=Sales_data_table$Time,y=Sales_data_table$A),linetype=1) 
figure_forecast <- figure_forecast +  geom_line(aes(x=Data_table_extended$Time,y=Data_table_extended$forecasts_A),linetype=2) 
figure_forecast <- figure_forecast + ggtitle("Forecasts of seasonally adjusted sales") + xlab("Time") + ylab("A") 
print (figure_forecast)

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

