#HOLTS MODEL

library(ggplot2)
# Open file for output
file_output <- "HW_8_Holts"
sink (file_output, append=FALSE, split=TRUE)
# Read and print data
file_data <- "STA372_Homework8_Question1.dat.txt"
Sales_data_table <- read.table(file_data, header = FALSE, sep = "") 
colnames(Sales_data_table) <- c("Time", "Sales", "A", "seasonal_i") 
cat ("\n", "First six rows of data are:", "\n", "\n") 
print(head(Sales_data_table))

result <- holt(A_time_series, h=5)
print(result)
print (result$model)

print(247.5937*1.362)
print(225.9879*1.362)
print(269.1995*1.362)

print(249.9211*.955)
print(228.3153*.955)
print(271.5270*.955)

print(252.2486*.700)

print(254.5760*.979)

print(256.9034*1.367)

Sales_data_table$forecasts <- result$fitted
Sales_data_table$forecasts<-Sales_data_table$forecasts*Sales_data_table$seasonal_i
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



figure <- ggplot()
figure <- figure + geom_point(data=Sales_data_table, aes(x=Time, y=Sales))
figure <- figure + geom_line(data=Sales_data_table, aes(x=Time, y=Sales))
figure <- figure + geom_point(aes(Sales_data_table$Time[1:27], Sales_data_table$forecasts[1:27]), color="Red")
figure <- figure + geom_line(aes(Sales_data_table$Time[1:27], Sales_data_table$forecasts[1:27]), color="Red", lty=2)
figure <- figure + xlab("Time") + ylab("Sales") + ggtitle("Sales (black) and in-sample forecasts (Red)")
print (figure)