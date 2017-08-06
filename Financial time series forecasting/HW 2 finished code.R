file_output <- "Homework2.txt"
sink (file_output, append=FALSE, split=TRUE)
# Read and print data
file_data <- "STA372_Homework2.dat.txt" 
Sales_data_table<- read.table(file_data, header = FALSE, sep = "") 
colnames(Sales_data_table) <- c("Quarter", "Time","Sales")
cat ("\n", "Quarter, Time, and sales data are:", "\n", "\n","\n") 
print(head(Sales_data_table))
# Plot Sales against Time
plot (Sales_data_table$Time, Sales_data_table$Sales, pch=19, xlab="Time", ylab="Sales")

#Decompose Sales_time_series
#
# Save sales data as a time series object
#
Sales_time_series <- ts(Sales_data_table[3], frequency=4)
colnames(Sales_time_series) <- "Sales"
#
# Plot Sales vs. Time
#
plot (Sales_time_series, main="Sale values")
points(Sales_time_series, col="black", pch=19)
#
# Decompose Sales into trend, seasonal and irregular components using classical decomposition #
Sales_time_series_components <- decompose(Sales_time_series, type="multiplicative")
cat ("\n", "Sales_time_series_components are:", "\n", "\n")
print (Sales_time_series_components)
plot (Sales_time_series_components)
#
# Compute seasonally adjusted sales
#
A <- Sales_time_series_components$x / Sales_time_series_components$seasonal
colnames(A) <- "Seasonally adjusted Sales"
cat ("\n", "Seasonally adjusted Sales values are:", "\n", "\n")
print(head(A))
plot(A, main="Seasonally Adjusted Sales", ylab="Seasonally adjusted sales", xlab="Time")
points(A, col="black", pch=19)

Time_sq<-Sales_data_table$Time^2

# Regress Sales against Time
B <- lm(A ~ Sales_data_table$Time+Time_sq)
cat ("\n", "Regression output is:", "\n")
print(summary(B))

#Fitted values
fitted_values_B<-(fitted((B)))
print(head(fitted_values_B))


#plot the data
figure <- ggplot(Sales_data_table, aes(x=Time))
figure <- figure + scale_y_continuous()
figure <- figure + geom_point(aes(y=A))
figure <- figure + ggtitle("A vs. Time") + xlab("Time") + ylab("A")
figure <- figure + geom_line(aes(y=fitted_values_B))
print (figure)

closeAllConnections()