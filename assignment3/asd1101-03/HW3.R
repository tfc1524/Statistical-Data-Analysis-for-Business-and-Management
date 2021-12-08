oatvar = read.table('Statistical-Data-Analysis-for-Business-and-Management\\assignment3\\asd1101-03\\oatvar.txt', header=TRUE)
attach(oatvar)
class(oatvar$block)
class(oatvar$variety)
oatvar$variety <- factor(oatvar$variety)
summary(oatvar)

# 1-ab
farmer <- lm(yield ~ block + variety, oatvar)
# Interaction plots
par(mfrow=c(1,2))
with(oatvar, interaction.plot(block,variety,yield,legend=F))
with(oatvar, interaction.plot(variety,block,yield,legend=F))

summary(lm(yield ~ block * variety, oatvar))
summary(farmer)
anova(farmer)
Smodel.matrix(farmer)

# 1-cd
par(mfrow=c(1,2))
plot(yield~factor(variety), ylab="yield", xlab="variety")
with(oatvar, stripchart(yield~factor(variety), vertical=TRUE, method="stack",
                        ylab="yield", xlab="variety"))

plot(yield~factor(block), ylab="yield", xlab="block")
with(oatvar, stripchart(yield~factor(block), vertical=TRUE, method="stack",
                        ylab="block", xlab="block"))

farmer2 <- lm(yield ~ factor(variety))
summary(farmer2)

# 1-e
## Multiple Comparison - Fisher's LSD ##
# Compare Group 1 and 3
summary(farmer2)$sigma	# sigma hat = residual standard error
(t13 <- 28.2/(47.05*sqrt(1/5+1/5)))
(tc.lsd <- qt(0.975, 35))	# critival value
(1-pt(t13, 35))*2

# tukey
ovhsd <- TukeyHSD(aov(yield~factor(block) + factor(variety), oatvar) )
par(mfrow=c(1,2))
plot(ovhsd,las=2)

# 2
detergent = read.table('Statistical-Data-Analysis-for-Business-and-Management\\assignment3\\asd1101-03\\detergent.txt', header=TRUE)
attach(detergent)
class(detergent$Detergent)
class(detergent$Temperature)
summary(detergent)

dt <- lm(Score ~ Detergent * Temperature, detergent)
summary(dt)
anova(dt)

# 3-ab
bmw <- read.table('Statistical-Data-Analysis-for-Business-and-Management\\assignment3\\asd1101-03\\used_bmw.txt', header=TRUE)
bmw$Year <- factor(bmw$Year)
pairs(bmw)
bmw$Age.o <- as.ordered(bmw$Age)
bmw$Year.o <- as.ordered(bmw$Year)

bmwmodel <- lm(ResidualPrice ~ Mileage * Age, bmw)
summary(bmwmodel)
plot(bmw$Mileage, residuals(bmwmodel))
abline(h=0, col='black')

# 3-c
confint(bmwmodel)
