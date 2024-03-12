###########################################################
#####     Simulating over- and underfitting models    #####
###########################################################


set.seed(4852)   #For reproducability
rm(list = ls())


##Data generating process (DGP)

#parameters
n <- 1000
n.test <- 200
b0 <- 10
b1 <- 2
b2 <- -1.8

x <- runif(n, min=1, max=5)
x.2 <- x^2
y <- b0 + b1*x + b2*x.2 + rnorm(n, mean = 0, sd = 3)

#true model
summary(lm(y ~ x + x.2))


##Test data (from same DGP)
x.test <- runif(n.test, min=1, max=5)
x.2.test <- x.test^2
y.test <- b0 + b1*x.test + b2*x.2.test + rnorm(n.test, mean = 0, sd = 3)


#underfitting
train.uf <- lm(y ~ x)
prediction.uf <- predict(train.uf, list(x = x.test))

plot(x.test, y.test, xlab = "X", ylab = "Y")
abline(lm(y.test ~ x.test), col = "red")

#fitting well (true DGP)
train.fw <- lm(y ~ x + x.2)
prediction.fw <- predict(train.fw, list(x = x.test, x.2 = x.2.test))

ix <- sort(x.test,index.return=T)$ix  #sort values of X for plotting curve
plot(x.test, y.test, xlab = "X", ylab = "Y")
lines(x.test[ix], prediction.fw[ix], type = "l", col = "blue")



#Overfitting
smoothspline = smooth.spline(x,y,df = 80) #fitting 
predict_spline = predict(smoothspline, x.test)$y #prediction on test data set
plot(x.test, y.test, xlab = "X", ylab = "Y")
lines(smoothspline, col = "green")