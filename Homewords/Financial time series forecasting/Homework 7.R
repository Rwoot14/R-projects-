library(ggplot2)
library(fpp)
# Open file for output
file_output <- "STA372_Homework7_Question2_FIN_2.txt"
sink (file_output, append=FALSE, split=TRUE)
# Read data and compute log(Sales)
file <- "STA372_Homework7_Question2.dat.txt"
data_table <- read.table(file, header = FALSE, sep = "") 
colnames(data_table) <- c("Time", "Quarter", "Sales","Seasonality")
data_table$log_Sales <- log(data_table$Sales)
cat ("\n", "First six rows of the data table are:", "\n", "\n")
print (head(data_table))
cat ("\n", "Last six rows of the data table are:", "\n", "\n")
print (tail(data_table))

# Plot Sales vs. Time
figure = ggplot()
figure <- figure + geom_point(data=data_table, aes(Time, Sales))
figure <- figure + geom_line(data=data_table, aes(Time, Sales))
figure <- figure + ggtitle("Sales vs. Time") + xlab("Time") + ylab("Sales") 
print (figure)

#Seasonalize
Y_time_series <- ts(data_table[4], frequency=4) 
colnames(Y_time_series) <- "Y"
A <- data_table$Sales / data_table$Seasonality
data_table$A <- A
colnames(A) <- "Seasonally adjusted Y"
cat ("\n", "Seasonally adjusted Y values are:", "\n", "\n")
print(A)

#plot of A Vs. Time
figure = ggplot()
figure <- figure + geom_point(data=data_table, aes(Time, A))
figure <- figure + geom_line(data=data_table, aes(Time, A))
figure <- figure + ggtitle("A vs. Time") + xlab("Time") + ylab("log(Sales)")
print (figure)


# Save A as a time series object
y_time_series <- ts(data_table[,6])

# Use Holt's method to obtained smoothed series 
result <- holt(y_time_series, h=4) 
print(result)
print (result$model)
data_table$forecasts <- result$fitted
data_table$residuals <- result$residuals
cat ("\n", "First six and last six rows of the data table are:", "\n", "\n")
print (head(data_table))
print (tail(data_table))

Seasonality <- c(data_table_extended$Seasonality,0.963, 0.937, 1.040, 1.058)
data_table_extended <- data.frame(result,Seasonality)

data_table_extended$forecasts_Sales = Data_table_extended$Point.Forecast * data_table_extended$Seasonality
data_table_extended$Quarter= c(2,3,4,1)
# Print in-sample and out-of-sample forecasts for A and Sales
cat ("\n", "Last two in-sample fitted values and forecasted values for the next four quarters are:", "\n", "\n")
print (tail(data_table_extended))

# Plot Residuals vs. Time
figure = ggplot()
figure <- figure + geom_point(data=data_table, aes(Time, residuals))
figure <- figure + geom_line(data=data_table, aes(Time, residuals))
figure <- figure + geom_hline(yintercept=0)
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Residuals vs. Time") + xlab("Time") + ylab("Residuals")
print (figure)
# Compute autocorrelation function of residuals
acf_residuals <- acf(data_table$residuals)

#print l and b estimates
cat ("\n", "First six and last six rows of L(t) and T(t) are:", "\n", "\n")
print (head(result$model$state[,1:2]))
print (tail(result$model$state[,1:2]))

print(head(data_table_extended))
# Close file for output
closeAllConnections()