# 1
y <- c(83,72,3,14)					# listed by row
smoker <- gl(2,2,4, c("Yes","No"))			# row names
patient <- gl(2,1,4, c("Cancer","Control"))	# col names
# case1803 <- data.frame(y, smoker, patient)
( case1803 <- xtabs(y ~ smoker + patient) )

fisher.test(case1803, alternative = 'greater')

# 2
library(Sleuth2)
case1902
attach(case1902)
c1902 <- xtabs(cbind(Death, Nodeath)~Victim)
c1902

# 2-a
case1902_new <- case1902
attach(case1902_new)
case1902_new$m = case1902_new$Death + case1902_new$Nodeath
case1902_new$logitval = log10((case1902_new$Death + 0.5) / (case1902_new$m - case1902_new$Death + 0.5))
case1902_new

# 2-a
plot(as.numeric(case1902_new$Aggravation), case1902_new$logitval, xlab="Aggravation", 
     ylab="logit", type="n")
points(case1902_new$Aggravation, case1902_new$logitval, pch=ifelse(Victim=="White", 1, 16))
lines(case1902_new$Aggravation[Victim=="White"], case1902_new$logitval[Victim=="White"], lty=2)
lines(case1902_new$Aggravation[Victim=="Black"], case1902_new$logitval[Victim=="Black"], lty=1)
legend("topleft", legend=c("White", "Black"), pch=c(1, 16), cex=0.8)

# 2-b
case1902_new$Vicim_label = ifelse(Victim=="White", '1', '0')
case1902_new$Aggravation <- as.numeric(case1902_new$Aggravation)
case1902_new$deathproportion <- case1902_new$Death / case1902_new$m
case1902_new
detach()
attach(case1902_new)
model <- glm(cbind(Death, Nodeath) ~ Victim + Aggravation, data=case1902_new, binomial)
summary(model)

# 2-c
deviance(model)
model$df.residual
1 - pchisq(deviance(model), model$df.residual)

# 2-d
confint(model)

# 2-e
model2 <- glm(cbind(Death, Nodeath) ~ Victim + as.factor(Aggravation), data=case1902_new, binomial)
summary(model2)
