rm(list = ls())
setwd("C:/Main/AQM/newRepo")

# write a Newton-Raphson optimization algorithm to estimate the coefficients of logistic regression on the BreastCancer.csv file,
# to find whether the tumor is benign or maliglent 

bc.dat <- read.csv(file="bcancer/BreastCancer.csv", header=TRUE, sep=",")

#get rid of unknowns
bc.dat <-  bc.dat[complete.cases(bc.dat),]

#don't need ID column
bc.dat <- bc.dat[,c(2:11)]

attach(bc.dat)
head(bc.dat)

# sum of squares
ssquares <- function(x) 
{
  n <- nrow(bc.dat)
  sum((bc.dat[,10] - as.matrix(cbind(1, bc.dat[,1:9])) %*% x)^2) / n
}

# define the derivatives
derivative <- function(x) 
{
  n <- nrow(bc.dat) # 200
  derva<- sum(-2*(bc.dat[,10] - as.matrix(cbind(1, bc.dat[,1:9])) %*% x))
  for (i in 1:10){
    derva <- cbind(derva, sum(-2*(bc.dat[,i])*(bc.dat[,10] - as.matrix(cbind(1, bc.dat[,1:9])) %*% x)))
  }
  derva <- derva / n
}

# definition of the gradient descent method in 2D
gradient_descent <- function(func, derv, start, step=0.00001, tol=1e-8) 
{
  pt1 <- start
  grdnt <- derv(pt1)
  pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
  while (abs(func(pt1)-func(pt2)) > tol) 
  {
    pt1 <- pt2
    grdnt <- derv(pt1)
    pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
    print(func(pt2)) # print progress
  }
  pt2 # return the last point
}

# locate the minimum of the function using the Gradient Descent method
result <- gradient_descent(
  ssquares, # the function to optimize
  derivative, # the gradient of the function
  c(0,0), # start point of theplot_loss(simple_ex)  search 
  0.00001, # step size (alpha)
  1e-8) # relative tolerance for one step

# display a summary of the results
print(result) # coordinate of fucntion minimum
print(ssquares(result)) # response of fucntion minimum











#define the sum of squared residuals----------------
data <- read.csv("Advertising.csv")[,-1]
attach(data)
head(data)

#data[,1] <- (data[,1] - mean(TV))/sd(TV)

ssquares <- function(x) 
{
  n <- nrow(data) # 200
  sum((data[,4] - cbind(1, data[,1]) %*% x)^2) / n
}

# define the derivatives
derivative <- function(x) 
{
  n <- nrow(data) # 200
  c(sum(-2*(data[,4] - cbind(1, data[,1]) %*% x)), sum(-2*(data[,1])*(data[,4] - cbind(1, data[,1]) %*% x))) / n
}


# definition of the gradient descent method in 2D
gradient_descent <- function(func, derv, start, step=0.00001, tol=1e-8) 
{
  pt1 <- start
  grdnt <- derv(pt1)
  pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
  while (abs(func(pt1)-func(pt2)) > tol) 
  {
    pt1 <- pt2
    grdnt <- derv(pt1)
    pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
    print(func(pt2)) # print progress
  }
  pt2 # return the last point
}

# locate the minimum of the function using the Gradient Descent method
result <- gradient_descent(
  ssquares, # the function to optimize
  derivative, # the gradient of the function
  c(0,0), # start point of theplot_loss(simple_ex)  search 
  0.00001, # step size (alpha)
  1e-8) # relative tolerance for one step

# display a summary of the results
print(result) # coordinate of fucntion minimum
print(ssquares(result)) # response of fucntion minimum









## Cross check coefficient results using build-in logistic regression-------------------------

bc.dat <- read.csv(file="bcancer/BreastCancer.csv", header=TRUE, sep=",")

bc.dat <-  bc.dat[!is.na(bc.dat),]

bc.dat <- bc.dat[,c(2:11)]
  
mylogit<- glm(as.factor(Class)~. , data = bc.dat, family = "binomial")
summary(mylogit)

# place probabilities to the closest binary response
predValue <- predict(mylogit, type = "response")
predValue[predValue > 0.5] <- 1
predValue[predValue <= 0.5] <- 0

predValue <- as.data.frame(predValue)
probDiff <- sum(abs(predValue - bc.dat$Class))/(nrow(bc.dat))

# using logistic regression, find out what percept is correct
1-probDiff




#Test--------------------------
dx2x <- deriv(~ (x2+x1)^2, "x1","x2")
dx2x
