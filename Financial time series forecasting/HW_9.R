file <- "STA372_Homework9_Question2.dat.txt"
data_table <- read.table(file, header = FALSE, sep = "") 
colnames(data_table) <- c("Quarter", "Consumption")

file_output <- "Hw_9_output_FIN"
sink (file_output, append=FALSE, split=TRUE)

figure <- ggplot()
figure <- figure + geom_point(aes(x=data_table$Quarter, y=data_table$Consumption), color="Black")
figure <- figure + geom_line(aes(x=data_table$Quarter, y=data_table$Consumption), linetype=1, color="Black") 
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Seasonally adjusted sales using STL") + xlab("Time") + ylab("A")
print (figure)

data_table$ln_Consumption <- log(data_table$Consumption)

figure <- ggplot()
figure <- figure + geom_point(aes(x=data_table$Quarter, y=data_table$ln_Consumption), color="Black")
figure <- figure + geom_line(aes(x=data_table$Quarter, y=data_table$ln_Consumption), linetype=1, color="Black") 
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Seasonally adjusted sales using STL") + xlab("Time") + ylab("A")
print (figure)

diff_ln_Consumption <- diff(data_table$ln_Consumption, difference=1)
data_table$diff_ln_Consumption[1] <- NA 
data_table$diff_ln_Consumption[2:164] <- diff_ln_Consumption
cat ("\n", "First six rows of the data table are:", "\n", "\n") 
print (head(data_table))
cat ("\n", "Last six rows of the data table are:", "\n", "\n")
print (tail(data_table))

figure <- ggplot()
figure <- figure + geom_point(aes(x=data_table$Quarter, y=data_table$diff_ln_Consumption), color="Black")
figure <- figure + geom_line(aes(x=data_table$Quarter, y=data_table$diff_ln_Consumption), linetype=1, color="Black") 
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Seasonally adjusted sales using STL") + xlab("Time") + ylab("A")
print (figure)

data_table$PctChange[2:164] <- (data_table$Consumption[2:164] - data_table$Consumption[1:163]) / data_table$Consumption[1:163]
data_table$PctChange[1] <- NA
cat ("\n", "First six rows of the data table are:", "\n", "\n") 
print (head(data_table))

acf_diff_ln_Consumption <- acf(data_table$diff_ln_Consumption[2:164])
print (acf_diff_ln_Consumption)
# Compute partial autocorrelation function of diff_Orders_SeasAdj_lag1
pacf_diff_ln_Consumption <- pacf(data_table$diff_ln_Consumption[2:164])
print (pacf_diff_ln_Consumption)

y_time_series <- ts(data_table[,3])
result <- Arima(y_time_series, order=c(3, 1, 0), include.constant=TRUE)
cat ("\n", "ARIMA results are:", "\n", "\n")
print (result)
cat ("\n", "Three step ahead forecasts and standard errors are:", "\n", "\n") 
forecast(result, h = 3)
plot(forecast(result, h = 3))
acf_residuals <- acf(result$residuals)
print (acf_residuals)
cat ("\n", "21-step ahead forecasts and standard errors are:", "\n", "\n") 
forecast(result, h = 3)

print(exp(9.234957))
print(exp(9.226892))
print(exp(9.243021))

print(exp(9.242885))
print(exp(9.230138))
print(exp(9.255632))

mean_residuals <- mean(result$residual)
cat ("\n", "Mean of residuals is:", "\n")
print (mean_residuals)
figure = ggplot()
figure <- figure + geom_point(aes(data_table$Quarter, result$residuals))
figure <- figure + geom_line(aes(data_table$Quarter, result$residuals))
figure <- figure + geom_hline(aes(yintercept=mean_residuals))
figure <- figure + xlab("Quarter") + ylab("Residuals")
figure <- figure + ggtitle("Residuals vs. Quarter")
figure <- figure + scale_y_continuous()
figure <- figure + theme(plot.title = element_text(hjust = 0.5))
figure <- figure + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())
figure <- figure + theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))
print (figure)




