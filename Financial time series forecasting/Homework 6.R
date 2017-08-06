
#A

library(ggplot2)
library(fpp)
# Open file for output
file_output <- "STA372_Homework6_Question2_FIN.txt"
sink (file_output, append=FALSE, split=TRUE)
# Read data
file <- "STA372_Homework6_Question2.dat.txt"
data_table <- read.table(file, header = FALSE, sep = "") 
colnames(data_table) <- c("Week", "Y","FixedForecast")
nobs <- nrow(data_table)
cat ("\n", "Number of observations in the data table is:", "\n", "\n") 
print (nobs)
cat ("\n", "First six rows of the data table are:", "\n", "\n") 
print(head(data_table))

figure <- ggplot()
figure <- figure + geom_point(data=data_table, aes(Week, Y))
figure<- figure+ geom_line(data=data_table,aes(Week,Y))
print(figure)

#B

data_table$Error <- data_table$Y - data_table$FixedForecast 
RMSE_FixedForecast <- sqrt(sum(data_table$Error^2)/nrow(data_table))
print(RMSE_FixedForecast)                           

#C



#Save data as a time series object
y_time_series <- ts(data_table[,2])

# Use ses to obtained smoothed series using optimal value of alpha

result <- ses(y_time_series, h=3, alpha=.9999) 
print(result)
print (result$model)
data_table$fitted <- result$fitted 
data_table$residuals <- result$residuals
RMSE_model <- sqrt(sum(data_table$residuals^2)/nrow(data_table))
cat ("\n", "First six rows of the data table are:", "\n", "\n") 
print (head(data_table))
cat ("\n", "Last six rows of the data table are:", "\n", "\n") 
print (tail(data_table))
print(RMSE_model)
                           
#D

#Save data as a time series object
y_time_series<- ts(data_table[,2])
# Use ses to obtained smoothed series using optimal value of alpha

result <- ses(y_time_series, h=4, alpha=NULL) 
print(result)
print (result$model)
data_table$fitted <- result$fitted 
data_table$residuals <- result$residuals
cat ("\n", "First six rows of the data table are:", "\n", "\n") 
print (head(data_table))
cat ("\n", "Last six rows of the data table are:", "\n", "\n") 
print (tail(data_table))
print(832.5441*0.975)
print(726.1900*0.975)
print(938.8982*0.975)

figure <- ggplot()
figure <- figure + geom_point(data=data_table, aes(Week, Y))
figure<- figure+ geom_line(data=data_table,aes(Week,Y))
figure<-figure+ geom_point(aes(data_table$Week[2:nobs], data_table$fitted[2:nobs]), color="Red")
figure<-figure+ geom_line(aes(data_table$Week[2:nobs], data_table$fitted[2:nobs]), color="Red", lty=2)              
figure<-figure+ xlab("Week") + ylab("Y") + ggtitle("Week series plot of Y (Solid black line) and in-sample forecasts")
print(figure)

#E 

figure<-ggplot()
figure<-figure + geom_point(aes(data_table$Week[2:nobs], data_table$residuals[2:nobs]))
figure<-figure + geom_line(aes(data_table$Week[2:nobs], data_table$residuals[2:nobs]))
figure<-figure + xlab("Week") + ylab("Residuals") + ggtitle("In-sample forecast errors")
print (figure)

acf_residuals <- acf(data_table$residuals)
print (acf_residuals)
                        