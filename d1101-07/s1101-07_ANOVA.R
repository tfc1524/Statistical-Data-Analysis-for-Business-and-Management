#####-------------------------------------------------------
##### Business Analytics 
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 7, 09 Nov 2021
#####-------------------------------------------------------

#### ANOVA ####
coffee <- read.table("d1101-07_coffee.txt", header=TRUE)
attach(coffee)
table(OCCU)
par(mfrow=c(1,2))
plot(INTENST~factor(OCCU), ylab="Intensity", xlab="Occupation")
with(coffee, stripchart(INTENST~factor(OCCU), vertical=TRUE, method="stack",
     ylab="Intensity", xlab="Occupation"))

cfm1 <- lm(INTENST ~ factor(OCCU))
summary(cfm1)

anova(cfm1)


## Multiple Comparison - Fisher's LSD ##
# Compare Group 1 and 2
summary(cfm1)$sigma	# sigma hat = residual standard error
(t12 <- 25.101/(15.85486*sqrt(1/5+1/8)))
	(tc.lsd <- qt(0.975, 35))	# critival value
(1-pt(t12, 35))*2 		
# equal to p-value in summary(cfm1)

## Multiple Comparison - Bonferroni correction ##
# Compare Group 1 and 2
(t12 <- 25.101/(15.85486*sqrt(1/5+1/8)))
	(tc.bon <- qt(1-0.05/20, 35))	# critival value
(1-pt(t12, 35))*2*10	
# now Groups 1 & 2 are not significantly different

## Multiple Comparison - Tukey's HSD ##
# Compare Group 1 and 2
(t12 <- 25.101/(15.85486*sqrt(1/5+1/8)))
	(tc.hsd <- qtukey(0.95, 5, 35)/sqrt(2))	# critival value
1-ptukey(t12*sqrt(2), 5, 35)

cfhsd <- TukeyHSD(aov(INTENST~factor(OCCU), coffee))
plot(cfhsd)

# Compare Group 2 and 3
(49.970-25.101)/(15.85486*sqrt(1/13+1/5))
1-ptukey(2.980687*sqrt(2), 5, 35)

## Diagnostics ##
par(mfrow=c(1,2))
qqnorm(residuals(cfm1))
plot(fitted(cfm1), residuals(cfm1), xlab="Fitted", ylab="Residuals")

detach()



#### Credibility of Ads
credads <- read.table("d1101-07_credibility.txt", header=TRUE)
attach(credads)

plot(Credibility ~ Treatment)

credads.lm <- lm(Credibility ~ Treatment)
summary(credads.lm)

qqnorm(residuals(credads.lm), ylab="Residuals")
qqline(residuals(credads.lm))

Treatment <- relevel(Treatment, ref="Tame")
	credads.lm <- lm(Credibility ~ Treatment)
	summary(credads.lm)

credads.hsd <- TukeyHSD(aov(Credibility ~ Treatment))
credads.hsd
par(fig=c(0.2,1,0,1))
plot(credads.hsd, las=1)

detach()



#### Diet Restriction & Longevity: case0501

library(Sleuth2)

summary(case0501)
attach(case0501)

Dn <- summary(Diet)
Dm <- tapply(Lifetime, Diet, mean)
Ds <- tapply(Lifetime, Diet, sd)
Dx <- tapply(Lifetime, Diet, max)
Dy <- tapply(Lifetime, Diet, min)
	tapply(Lifetime, Diet, quantile)
Dsum <- cbind(Dn, Dm, Ds, Dx, Dy)
colnames(Dsum) <- c("n", "Mean", "SD", "Max", "Min")
Dsum

par(mfrow=c(1,2))
boxplot(Lifetime~Diet)
stripchart(Lifetime~Diet, vertical=T, method="stack")

c501.lm <- lm(Lifetime~Diet)
summary(c501.lm)
confint(c501.lm)	# (e)

Diet <- relevel(Diet, ref="N/N85")
c501.lm2 <- lm(Lifetime~Diet)
summary(c501.lm2)
confint(c501.lm2)	# (a)

Diet <- relevel(Diet, ref="N/R50")
c501.lm3 <- lm(Lifetime~Diet)
summary(c501.lm3)
confint(c501.lm3)	# (b), (c), (d), (a)

( c501hsd <- TukeyHSD(aov(Lifetime~Diet)) )
par(fig=c(0.1,1,0,1))
plot(c501hsd, las=1)


# Residual plot
plot(fitted(c501.lm),residuals(c501.lm))
abline(h=0, lty=2, col='grey')






####------------------------
####  Block Designs       
####------------------------ 

### Quantitative Factors and Orthogonal Polynomials
( composite <- read.table("d1101-07_composite.txt", header=T, sep="\t") )
class(composite$laser)

cm <- lm(strength ~ laser, composite)
anova(cm)
summary(cm)
# model.matrix(cm)

plot(composite$strength ~ composite$laser)

## make 'laser' ordered factors...
composite$laser.o <- as.ordered(composite$laser)
cm.o <- lm(strength ~ laser.o, composite)

	contr.poly(3)
	# model.matrix(cm.o)

anova(cm.o)
summary(cm.o)	#-> anova() unchanged, but summary() informative


## make 'laser' numerical predictors... 
composite$laser.n <- rep(c(40,50,60),3)
cm.n <- lm(strength ~ laser.n, composite)		# similar to SLM
	# summary(cm.n)
cm.m <- lm(strength ~ poly(laser.n, 2), composite)
	# summary(cm.m)

	# model.matrix(cm.m);  model.matrix(cm.o)

## 'cm.o' cannot be used for prediction:
predict(cm.n, data.frame(laser.n = 55))
predict(cm.m, data.frame(laser.n = 55))



# -----------------------------------------------------------------------
### Randomized Block Design
( girder <- read.table("d1101-07_girder.txt", header=T) )
attach(girder)
xtabs(ratio ~ method + gtype)		# table on p. 8-9

	plot(ratio ~ method)
	plot(ratio ~ gtype)

gd <- lm(ratio ~ method + gtype, girder)
anova(gd);   summary(gd)


## Multiple comparison  (p. 8-10)
(gdmean <- tapply(girder$ratio, girder$method, mean))

t.st <- rep(0,6)
names(t.st) <- c("A-C", "A-K", "A-L", "C-K", "C-L", "K-L")
se <- summary(gd)$sigma*sqrt(1/9+1/9)
t.st[1] <- as.numeric(gdmean[2]-gdmean[1]) / se
t.st[2] <- as.numeric(gdmean[3]-gdmean[1]) / se
t.st[3] <- as.numeric(gdmean[4]-gdmean[1]) / se
t.st[4] <- as.numeric(gdmean[3]-gdmean[2]) / se
t.st[5] <- as.numeric(gdmean[4]-gdmean[2]) / se
t.st[6] <- as.numeric(gdmean[4]-gdmean[3]) / se
t.st

# (p. 8-11)
(tc.lsd <- qt(0.975, 24))
(tc.bon <- qt(1-0.05/12, 24))
(tc.hsd <- qtukey(0.95, 4, 24)/sqrt(2))
   1-ptukey(t.st[1]*sqrt(2), 4, 24)

# (p. 8-12)
gdv <- aov(ratio ~ method + gtype, girder)
gdhsd <- TukeyHSD(gdv, "method")
plot(gdhsd)

	## Diagnostics...
	plot(fitted(gd), residuals(gd), xlab="Fitted", ylab="Residuals")
	abline(h=0, lty=3)
	qqnorm(residuals(gd));	qqline(residuals(gd))

   	gd0 <- lm(log(ratio)~method+gtype, girder)
   	anova(gd0);   summary(gd0)
   	qqnorm(residuals(gd0));	qqline(residuals(gd0))






####------------------------
####  Factorial Designs       
####------------------------ 

### Factorial Design - Two-way ANOVA
composite <- read.table("d1101-07_composite.txt", header=T, sep="\t")
class(composite$laser);   class(composite$tape)
summary(composite)

cm <- lm(strength ~ laser + tape, composite)

# Interaction plots
par(mfrow=c(1,2))
with(composite, interaction.plot(laser,tape,strength,legend=F))
with(composite, interaction.plot(tape,laser,strength,legend=F))

summary(lm(strength ~ laser * tape, composite))

summary(cm);   anova(cm)
model.matrix(cm)

## make 'laser' & 'tape' to be ordered factors...
composite$laser.o <- as.ordered(composite$laser)
composite$tape.o <- as.ordered(composite$tape)

cm.o <- lm(strength ~ laser.o + tape.o, composite)

contr.poly(3)
model.matrix(cm.o)
summary(cm.o);   anova(cm.o)

## make 'laser' & 'tape' to be numerical predictors...
composite$tape.n <- rep(c(6.42,13,27), each=3)
composite$laser.n <- rep(c(40,50,60), 3)
cm.n <- lm(strength ~ laser.n + poly(log(tape.n),2), composite)
summary(cm.n)
	summary(cm);	summary(cm.o)




# -----------------------------------------------------------------------
### Two-Way ANOVA - More than one observations per cell (1)
torque <- read.table("Statistical-Data-Analysis-for-Business-and-Management\\d1101-07\\d1101-07_torque.txt", header=T, sep="\t")
summary(torque)

tq <- lm(value ~ test * plating, torque)
summary(tq);   anova(tq)

# Interaction plots
par(mfrow=c(1,2))
with(torque, interaction.plot(test,plating,value))
with(torque, interaction.plot(plating,test,value))

plot.design(value~test+plating, data=torque)	# main-effect plot
	model.tables(aov(value~test*plating, torque))
	mean(torque$value[torque$test=="bolt"]);   mean(torque$value)

	model.matrix(tq)		# treatment contrast

# Diagnostics
torque$tt <- factor(substr(torque$test, 1,1))

# Box-Whisker Plots
plot(torque$tt:torque$plating, tq$residuals,
 xlab="Treatment Groups",ylab="Residuals",
 main="Box-Whisker Plots of Residuals, Bolt Experiment")

plot(fitted(tq), residuals(tq), xlab="Fitted", ylab="Residuals")
abline(h=0)
qqnorm(residuals(tq));  qqline(residuals(tq))

# Transformation on response
tq0 <- lm(log(value) ~ test * plating, torque)
summary(tq0);   anova(tq0)
   par(mfrow=c(1,2))
   plot(torque$tt:torque$plating, tq0$residuals)
   qqnorm(residuals(tq0))
   qqline(residuals(tq0))

# Multiple Comparison
tqhsd <- TukeyHSD(aov(value~tt*plating, torque) )
tq0hsd <- TukeyHSD(aov(log(value)~tt*plating, torque) )
   par(mfrow=c(2,3))
   plot(tqhsd,las=2);  plot(tq0hsd,las=2)




