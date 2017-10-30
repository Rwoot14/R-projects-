# Open file for output

file_output <- "Home_4_Question_1_finished2.txt" 
sink (file_output, append=FALSE, split=TRUE)

# Read data

file_data <- "STA372_Homework4_Question1.dat.txt"
Sales_data_table <- read.table(file_data, header = TRUE, sep = "") 
colnames(Sales_data_table) <- c("Time", "Quarter", "Sales", "Seasonal","A")

#Plot A (b)

figure_1 <- ggplot(Sales_data_table, aes(x=Time,y=A))
figure_1 <- figure_1 + geom_point(color="Black")
figure_1 <- figure_1 + geom_line(aes(x=Time,y=A))
print(figure_1)

# Compute log(A) and Time^2

Sales_data_table$log_A <- log(Sales_data_table$A) 
Sales_data_table$Time_sq <- Sales_data_table$Time^2

# Regress log(A) against Time and compute fitted values for log(A) and A

reg_output <- lm(Sales_data_table$log_A ~ Sales_data_table$Time + Sales_data_table$Time_sq) 
cat ("\n", "Regression output for log_A = Alpha + Beta1*Time + Beta2*Time_sq is:", "\n") 
print(summary(reg_output))
fitted_values_log_A = reg_output$coef[1] + reg_output$coef[2]*Sales_data_table$Time + reg_output$coef[3]*Sales_data_table$Time_sq 
fitted_values_A = exp(fitted_values_log_A)

# Construct figure to plot A vs. Time and in-sample fitted values (d)

figure_2 <- ggplot(Sales_data_table, aes(x=Time,y=log_A))
figure_2 <- figure_2 + geom_point(color="Black")
figure_2 <- figure_2 + geom_line(aes(y=fitted_values_log_A))
figure_2 <- figure_2 + ggtitle("Log_A vs. Time") + xlab("Time") + ylab("A")
print (figure_2)

#Plot the in-sample forecasts and At (e)

figure_3 <- ggplot(Sales_data_table, aes(x=Time,y=A))
figure_3 <- figure_3 + geom_point(color="Black")
figure_3 <- figure_3 + geom_line(aes(y=fitted_values_A))
figure_3 <- figure_3 + ggtitle("A vs. Time") + xlab("Time") + ylab("A") 
print (figure_3)

# Constructed Time and Seasonal vectors with extra four rows for forecasting

Time <- c(Sales_data_table$Time,139:142)
Time_sq <- Time^2
Seasonal <- c(Sales_data_table$Seasonal,0.994791, 1.058880, 0.984625, 0.961701) 
Data_table_extended <- data.frame(Time, Time_sq, Seasonal)

# Compute in-sample and out-of-sample forecasts for A
#Out of sample
Data_table_extended$forecasts_A = exp(reg_output$coef[1] + reg_output$coef[2]*Data_table_extended$Time +  reg_output$coef[3]*Data_table_extended$Time_sq)
#in sample
Sales_data_table$in_sample_forcast_A= exp(reg_output$coef[1] + reg_output$coef[2]*Sales_data_table$Time +  reg_output$coef[3]*Sales_data_table$Time_sq)

#Compute in-sample and out-of-sample forecasts for Sales

#out of sample
Data_table_extended$forecasts_Sales = Data_table_extended$forecasts_A * Data_table_extended$Seasonal

#In sample
Sales_data_table$in_sample_forcast_Sales=Sales_data_table$in_sample_forcast_A*Sales_data_table$Seasonal

# Print in-sample and out-of-sample forecasts for A and Sales

cat ("\n", "Last two in-sample fitted values and forecasted values for the next four quarters are:", "\n", "\n") 
print (tail(Data_table_extended))

#Plot in-sample forcast for sales (f)

figure_forecast_0 <- ggplot()
figure_forecast_0 <- figure_forecast_0 + geom_line(aes(x=Sales_data_table$Time,y=Sales_data_table$Sales),linetype=1) 
figure_forecast_0 <- figure_forecast_0 + geom_line(aes(x=Sales_data_table$Time,y=Sales_data_table$in_sample_forcast_Sales),linetype=2) 
figure_forecast_0 <- figure_forecast_0 + ggtitle("Forecasts of In sample sales") + xlab("Time") + ylab("A") 
print (figure_forecast_0)


#end




# Plot in-sample and out-of-sample forecasts for A (not needed??)

figure_forecast_1 <- ggplot()
figure_forecast_1 <- figure_forecast_1 + geom_line(aes(x=Sales_data_table$Time,y=Sales_data_table$A),linetype=1) 
figure_forecast_1 <- figure_forecast_1 + geom_line(aes(x=Data_table_extended$Time,y=Data_table_extended$forecasts_A),linetype=2) 
figure_forecast_1 <- figure_forecast_1 + ggtitle("Forecasts of seasonally adjusted sales") + xlab("Time") + ylab("A") 
print (figure_forecast_1)

# Plot in-sample and out-of-sample forecasts for Sales (with seasonality incorporated) (not needed??)

figure_forecast_2 <- ggplot()
figure_forecast_2 <- figure_forecast_2 + geom_line(aes(x=Sales_data_table$Time,y=Sales_data_table$Sales),linetype=1) 
figure_forecast_2 <- figure_forecast_2 +geom_line(aes(x=Data_table_extended$Time,y=Data_table_extended$forecasts_Sales),linetype=2)
figure_forecast_2 <- figure_forecast_2 + ggtitle("Forecasts of Sales") + xlab("Time") + ylab("Sales") 
print (figure_forecast_2)

# Close file for output 

closeAllConnections()
