data <- read.csv('Statistical-Data-Analysis-for-Business-and-Management\\assignment2\\GoodBelly_data.csv')
data$Revenue = data$UnitsSold * data$AverageRetailPrice
attach(data)

# a
# easy summary
a <- subset(data, select = c('UnitsSold', 'AverageRetailPrice', 'Revenue'))
summary(a)
par(mfrow=c(1,1))
hist(UnitsSold)
hist(AverageRetailPrice, xlab='AverageRetailPrice(USD)')
hist(Revenue, xlab='Revenue(USD)')
hist(Natural, breaks=c(0, 1, 2, 3, 4, 5))
hist(Fitness, breaks=c(0, 1, 2, 3, 4, 5))

plot(x=Natural, y=Revenue, xlab="Natural", ylab="Revenue", main = 'Scatter')
plot(x=Fitness, y=Revenue, xlab="Fitness", ylab="Revenue", main = 'Scatter')
par(mfrow=c(1,2))
boxplot(UnitsSold~SalesRep, ylab="UnitsSold", xlab="SalesRep")
boxplot(UnitsSold~Endcap, ylab="UnitsSold", xlab="Endcap")
par(mfrow=c(1,3))
boxplot(UnitsSold~Demo, ylab="UnitsSold", xlab="Demo")
abline(h=400, col='red')
abline(h=200, col='red')
boxplot(UnitsSold~Demo1.3, ylab="UnitsSold", xlab="Demo1.3")
abline(h=400, col='red')
abline(h=200, col='red')
boxplot(UnitsSold~Demo4.5, ylab="UnitsSold", xlab="Demo4.5")
abline(h=400, col='red')
abline(h=200, col='red')
pairs(subset(data, select = c('UnitsSold', 'AverageRetailPrice', 'Revenue')), main = 'Pairplot')

par(mfrow=c(1,1))
boxplot(UnitsSold~Region, ylab="UnitsSold", xlab="Region")
# b
# SalesRep
plot(x = AverageRetailPrice, y = Revenue, type="n", xlab="AverageRetailPrice", ylab="Revenue",
     main ='SalesRep Scatterplot')
legend("topright", legend=c("1", "0"),
       col=c(4, 2), pch=1, cex=0.8)
for (i in 1:nrow(data) + 1) 
  points(AverageRetailPrice[i], Revenue[i], col=(SalesRep[i]+1)*2, pch=1)

# Endcap
plot(x = AverageRetailPrice, y = Revenue, type="n", xlab="AverageRetailPrice", ylab="Revenue",
     main ='Endcap Scatterplot')
legend("topright", legend=c("1", "0"),
       col=c(4, 2), pch=1, cex=0.8)
for (i in 1:nrow(data) + 1)
  points(AverageRetailPrice[i], Revenue[i], col=(Endcap[i]+1)*2, pch=1)

# Demo
plot(x = AverageRetailPrice, y = Revenue, type="n", xlab="AverageRetailPrice", ylab="Revenue",
     main ='Demo Scatterplot')
legend("topright", legend=c("1", "0"),
       col=c(4, 2), pch=1, cex=0.8)
for (i in 1:nrow(data) + 1)
  points(AverageRetailPrice[i], Revenue[i], col=(Demo[i]+1)*2, pch=1)

# Demo1.3
plot(x = AverageRetailPrice, y = Revenue, type="n", xlab="AverageRetailPrice", ylab="Revenue",
     main ='Demo1.3 Scatterplot')
legend("topright", legend=c("1", "0"),
       col=c(4, 2), pch=1, cex=0.8)
for (i in 1:nrow(data) + 1)
  points(AverageRetailPrice[i], Revenue[i], col=(Demo1.3[i]+1)*2, pch=1)

# Demo4.5
plot(x = AverageRetailPrice, y = Revenue, type="n", xlab="AverageRetailPrice", ylab="Revenue",
     main ='Demo4.5 Scatterplot')
legend("topright", legend=c("1", "0"),
       col=c(4, 2), pch=1, cex=0.8)
for (i in 1:nrow(data) + 1)
  points(AverageRetailPrice[i], Revenue[i], col=(Demo4.5[i]+1)*2, pch=1)

# c
mod1 <- lm(UnitsSold ~ (AverageRetailPrice + Natural + Fitness) * (SalesRep + Endcap + Demo + Demo1.3 + Demo4.5), data=data)
summary(mod1)

best <- step(mod1)
summary(mod2)
