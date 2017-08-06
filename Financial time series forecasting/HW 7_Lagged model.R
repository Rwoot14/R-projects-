#LAGGED MODEL

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

residuals <- reg_output$residuals
figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_data_table$Time,y=residuals))
figure <- figure + geom_line(aes(x=Sales_data_table$Time,y=residuals))
figure <- figure + ggtitle("Residuals vs. Time") + xlab("Time") + ylab("Residuals") 
print (figure)

A_time_series <- ts(Sales_data_table[3]) 
colnames(A_time_series) <- "A"
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
