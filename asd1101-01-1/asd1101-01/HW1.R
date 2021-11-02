# 1
oacs <- read.table("Statistical-Data-Analysis-for-Business-and-Management\\asd1101-01-1\\asd1101-01\\OACs.txt", header=TRUE)   # Read data for analysis
attach(oacs)
pairs(oacs)

# 1-a
fitoacs <- lm(GPA ~ Best6)
summary(fitoacs)

# Diagnostic Plots - Residual plot
plot(x=Best6, y=residuals(fitoacs), xlab="X", ylab="Residuals", main='Residual Plot(GPA ~ Best6)')
abline(h=0)

# Diagnostic Plots - Histogram on Residuals
hist(residuals(fitoacs))

# 1-b
fitbest4 <- lm(GPA ~ B4EC)

# Diagnostic Plots - Residual plot
plot(x=B4EC, y=residuals(fitbest4), xlab="X", ylab="Residuals", main='Residual Plot(GPA ~ B4EC)')
abline(h=0)

# Diagnostic Plots - Histogram on Residuals
hist(residuals(fitbest4))

summary(fitbest4)
detach()

# 2
# 2a
gas <- read.table("Statistical-Data-Analysis-for-Business-and-Management\\asd1101-01-1\\asd1101-01\\gas_consumption.txt", header=TRUE)
head(gas, 5)

fitgas <- lm(gas$Gas.CCF~gas$DegreesBelow65)

# Diagnostic Plots - Residual plot
plot(x=gas$DegreesBelow65, y=residuals(fitgas), xlab="X", ylab="Residuals", main='Residual Plot(fitgas)')
abline(h=0)

# Diagnostic Plots - Histogram on Residuals
hist(residuals(fitgas))

summary(fitgas)

plot(x=gas$DegreesBelow65, y=gas$Gas.CCF, ylab = "Gas.CCF", xlab = "DegreesBelow65")
abline(fitgas)

# 2b
## CI plot
xy <- data.frame(X=gas$DegreesBelow65)
yhat <- predict(fitgas, newdata=xy, interval="confidence")
ci <- data.frame(lower=yhat[,"lwr"], upper=yhat[,"upr"])

## Advanced Practice: CI & PI plot
yhat.pi <- predict(fitgas, newdata=xy, interval="prediction")
pi <- data.frame(lower=yhat.pi[,"lwr"], upper=yhat.pi[,"upr"])
plot(x=gas$DegreesBelow65, y=gas$Gas.CCF, main ="Confidence and Prediction Intervals",
     ylab = "Gas.CCF", 
     xlab = "DegreesBelow65")
abline(fitgas)
lines(xy$X, ci$lower, lty=2, col="red")
lines(xy$X, ci$upper, lty=2, col="red")
lines(xy$X, pi$lower, lty=2, col="blue")
lines(xy$X, pi$upper, lty=2, col="blue")

# 3
caracc <- read.table("Statistical-Data-Analysis-for-Business-and-Management\\asd1101-01-1\\asd1101-01\\car_accident.txt", header=TRUE)
pairs(caracc)

fitcar <- lm(caracc$Accidents ~ caracc$Cars + caracc$Speed)

# Diagnostic Plots - Residual plot
plot(x=caracc$Cars, y=residuals(fitcar), xlab="Cars", ylab="Residuals", main='Residual Plot(fitcar)')
abline(h=0)

# Diagnostic Plots - Residual plot
plot(x=caracc$Speed, y=residuals(fitcar), xlab="Speed", ylab="Residuals", main='Residual Plot(fitcar)')
abline(h=0)

# Diagnostic Plots - Histogram on Residuals
hist(residuals(fitcar))

summary(fitcar)

# 第二個模型只用car
fitcar1 <- lm(caracc$Accidents ~ caracc$Cars)
fitcarspd <- lm(caracc$Accidents ~ caracc$Speed)
anova(fitcar1, fitcar)
anova(fitcarspd, fitcar)
summary(fitcar1)
summary(fitcarspd)

