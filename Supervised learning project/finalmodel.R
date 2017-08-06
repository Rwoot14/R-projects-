library(randomForest)
##RANDOM FOREST MODELS
Cars = read.csv("Cars.csv")
str(Cars)
Cars_X = read.csv("Cars_X_out.csv")
sapply(Cars, class)
sapply(Cars_X, class)
#bagging since we are using all the variables at every split
#cars.test = Cars_X["price"]

####making factor variables equal
levels(Cars_X$trim) <- levels(Cars$trim)
levels(Cars_X$condition) <- levels(Cars$condition)
levels(Cars_X$isOneOwner) <- levels(Cars$isOneOwner)
levels(Cars_X$color) <- levels(Cars$color)
levels(Cars_X$dispacement) <- levels(Cars$dispacement)
levels(Cars_X$fuel) <- levels(Cars$fuel)
levels(Cars_X$region) <- levels(Cars$region)
levels(Cars_X$soundSystem) <- levels(Cars$soundSystem)
levels(Cars_X$wheelType) <- levels(Cars$wheelType)
levels(Cars_X$wheelSize) <- levels(Cars$wheelSize)

set.seed(1)
finalmodel = randomForest(price~., data=Cars, ntree = 200,  mtry=6, nodesize=20, importance=TRUE)
finalmodel

yhat.finalmodel = predict(finalmodel, newdata = Cars_X )
write.csv(yhat.finalmodel, "Predictions.csv", row.names=FALSE)


plot(finalmodel, cars.test,  pch= 1, col='burlywood', xlab="Predicted", ylab="Actual", cex.lab = 1.25, main="Predicted vs. Actual")
abline(0,1)
mse = mean((finalmodel - cars.test)^2)
rmse = sqrt(mse)
rmse#6631.9
residuals = (finalmodel - cars.test)
plot(finalmodel, residuals, xlab="Predicted", ylab="Residuals", main = "Residuals vs. Fitted", cex.lab=1.25, col=ifelse(yhat.finalmodel>105000, "gold", "black"))
varImpPlot(finalmodel, col="gold")