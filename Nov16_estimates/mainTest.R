# run main
setwd("C:/Main/AQM/Lessons/Nov2")
dat <- read.csv(file="Advertising.csv", header=TRUE, sep=",")
dat <- as.data.frame(dat)

#mod <- lm(log(Sales) ~ (TV), dat, weights=sqrt(TV))
mod <- lm(log(Sales) ~ log(TV), dat)
res <- residuals(mod)
plot(res ~ predict(mod), main = "Residual Plot", xlab = "observation", ylab = "Residuals")

plot(log(dat$Sales) ~ sqrt(dat$TV))
## Nov 9 part2-----------------

x <- 1:1000

e<-matrix(0,1000,1)
e<- rnorm(1000,mean=0, sd=0.001*x)
y <- 0.25*x+e

plot(y,x)

## NOV 9 part3 (optimize least squares with matrix)--------
X <- cbind(1,dat$TV)
Y <- cbind(dat$Sales)

B <-solve(t(X)%*%X)%*%(t(X)%*%Y)
summary(lm(Sales ~ TV, dat))

ee <- t(Y-X%*%B)%*%(Y-X%*%B)

ee/198


######run analysis------------
library(plyr)

head(dat)

######run analysis------------
str(dat)

summary(dat)

ncol(dat)
nrow(dat)

#####numunique----

sapply(dat, function(x) length(unique(x)))

#####plot----
par(mfrow=c(2,2))
plot(dat$Radio, dat$Sales, main = "Plot of sales vs Radio")
abline(lm(dat$Sales~dat$Radio), col=c("red"))
plot(dat$TV, dat$Sales, main = "Plot of sales vs TV")
abline(lm(dat$Sales~dat$TV) , col=c("red"))
plot(dat$Newspaper, dat$Sales, main = "Plot of sales vs Newspaper")
abline(lm(dat$Sales~dat$Newspaper), col=c("red") )

####regression analysis------
reg1  <- lm(dat$Sales~dat$Newspaper)
summary(reg1)
reg2  <- lm(dat$Sales~dat$Radio)
summary(reg2)
reg3  <- lm(dat$Sales~dat$TV)
summary(reg3)

####regression analysis------
reg1  <- lm(dat$Sales~dat$Newspaper+dat$Radio+dat$TV)
summary(reg1)
#####linear regression----
fit <- lm(Sales ~ Radio, data=dat)
summary(fit) # show results
