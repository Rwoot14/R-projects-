#SIMPLE EXP SMOOTHING

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
