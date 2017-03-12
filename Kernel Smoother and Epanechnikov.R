# KNN running-mean function
Index.ClosestK <- function(x1, x2, K , ... ){
  L <- length(x1)
  temp <- rep(0 , L)
  KFirst <- rep(0 , K)
  for (i in 1:L){
    temp[i] <- dist(rbind(x1[i] , x2))
  }
  temp2 <- sort(temp , index.return = TRUE)
  for (i in 1:K){
    KFirst[i] <- temp2$ix[i]
  }
  return(KFirst)
}


# Kernel Smoother functions
KernelFunction <- function(x, xi, lambda , ... ){
  t <- (x - xi)/lambda
  return(3/4*(1 - t^2))
}




InWindowIndices <- function(x, xi , lambda){
  L <- length(xi)
  temp <- rep(0 , L)
  Indices <- 0
  for (i in 1:L){
    temp[i] <- dist(rbind(x , xi[i]))
    if (temp[i] <= lambda){
      Indices <- cbind(Indices , i)
    }
  }
  # In.Window <- temp[temp <= lambda]
  return(Indices[,-1])
}

# Data generation
x <- runif(1000, min = -5 , max = 5)
head(x)
LENGTH.X <- length(x)
LENGTH.X
noise <- rnorm(n = LENGTH.X , mean = 0 , sd = 0.1)
y <- 1/(1 + x^2) + noise


plot(x , y)

# Test data
LIMIT <- range(x)
x.test <- seq(from = LIMIT[1] , to = LIMIT[2] , by = 0.005)
y.test <- 1/(1 + x.test^2)
LENGTH.X.TEST <- length(x.test)


# EKS Fitting
y.predict.EKS <- rep(0 , LENGTH.X.TEST)

for (i in 1:LENGTH.X.TEST){
  NumErator <- sum((y*KernelFunction(x.test[i] , x , lambda = 0.5))[InWindowIndices(x.test[i] , x , lambda = 0.5)])
  DenumErator <- sum(KernelFunction(x.test[i] , x , lambda = 0.5)[InWindowIndices(x.test[i] , x , lambda = 0.5)])
  y.predict.EKS[i] <- NumErator/DenumErator
}



# KNN Fitting
y.predict.KNN <- rep(0 , LENGTH.X.TEST)

for (i in 1:LENGTH.X.TEST){
  y.predict.KNN[i] <- sum(y[Index.ClosestK(x , x.test[i] , K = 30)])/length(Index.ClosestK(x , x.test[i] , K = 30))
}



# Plot the KNN

plot(x , y , col = "green" , pch = 1 , title("30-Nearest Neighbor Kernel running-mean Smoother"))
points(x.test , y.test , type = "l" , col = "blue")
points(x.test , y.predict.KNN , col = "red" , type = "l")
legend("topright" , legend = c("True Function" , "Mean Smoother Estimator") , col = c("blue" , "red"),lty=1,lwd=2,cex=.8)
MSE.KNN <- sum((y.predict.KNN - y.test)^2)/length(y.predict.KNN)
MSE.KNN


# Plot the Epanechnikov kernel method

plot(x , y , col = "green" , pch = 1 , title("Epanechnikov Kernel smoother"))
points(x.test , y.test , type = "l" , col = "blue")
points(x.test , y.predict.EKS , col = "red" , type = "l")
legend("topright" , legend = c("True Function" , "Mean Smoother Estimator") , col = c("blue" , "red"),lty=1,lwd=2,cex=.8)
MSE.EKS <- sum((y.predict.EKS - y.test)^2)/length(y.predict.EKS)
MSE.EKS

