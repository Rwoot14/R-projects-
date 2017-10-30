# Open file for output
file_output <- "Homework3.txt" 
sink (file_output, append=FALSE, split=TRUE)
# Read data from a file
file <- "STA372_Homework3_Question1.dat.txt"
Data_table <- read.table(file, header = FALSE, sep = "") 
colnames(Data_table) <- c("Quarter", "Time","Sales")
n_obs = nrow(Data_table)
cat ("\n", "Number of observations is:", n_obs, "\n", "\n") 
print(head(Data_table))
# Save sales data as a time series object
Y_time_series <- ts(Data_table[3], frequency=4)
colnames(Y_time_series) <- "Sales"
# Plot Sales vs. Time
plot (Y_time_series, main="Sale values") 
points(Y_time_series, col="black", pch=19)
# Decompose Sales into trend, seasonal and irregular components using classical decomposition
Y_time_series_components <- decompose(Y_time_series, type="multiplicative") 
cat ("\n", "Y_time_series_components are:", "\n", "\n")
print (Y_time_series_components)
plot (Y_time_series_components)
# Compute seasonally adjusted sales
A <- Y_time_series_components$x / Y_time_series_components$seasonal 
colnames(A) <- "Seasonally adjusted Sales"
cat ("\n", "Seasonally adjusted Sales values are:", "\n", "\n")
print(A)
plot(A, main="Seasonally Adjusted Sales", ylab="Seasonally adjusted sales", xlab="Time") 
points(A, col="black", pch=19)
# Close file for output
closeAllConnections()