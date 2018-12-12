
library(e1071)
library(quadprog)



data <- read.table("C:/Users/anmol/Downloads/pb2.txt")
#Features

X = as.matrix(data[,2:5])
y = as.matrix(data[, 1])
n <- length(y)
for (i in 1:n){
  if (y[i] > 1){
    y[i]<--1
  }
}


gaussianKern <- function(x, y, sigma){
  exp(-(t(x-y)%*%(x-y))/(2*sigma^2))
}

gram_mat <- function(mydat, sigma){
  N <- dim(mydat)[1]
  if (!is.matrix(mydat)) mydat <- as.matrix(mydat) #change class of mydat to matrix
  gram_matrix <- matrix(0, N, N)
  for(i in 1:N){
    for(k in 1:N){
      gram_matrix[i,k] <- gaussianKern(mydat[i,], mydat[k,], sigma=sigma)
    }
  }
  print(gram_matrix)
}
kernelDistance <- function(point, data, alphas, gramMat, sigma){
  #calculate the distance for a single data point in the gaussian kernel space
  t1 <- gaussianKern(point, point, sigma)
  t2 <- -2*sum(sapply(1:length(alphas), function(m) alphas[m]*gaussianKern(point, data[m,], sigma)))
  t3 <- t(alphas) %*% gramMat %*% alphas
  sqrt(t1+t2+t3)
}

make.d.vec <- function(mydat, sigma){
  #creates the d vector for quadprog
  
  d <- sapply(1:dim(mydat)[1], function(m) gaussianKern(mydat[m,], mydat[m,], sigma=sigma))
  print(d)
}
dataTrain <- function(n, p, negativeProportion=0){
  numNegative <- round(negativeProportion*n)
  numPositive <- n-numNegative
  positiveMean <- rnorm(p, mean=4, sd=1)
  negativeMean <- rnorm(p, mean=-4, sd=1)
  Mat <- matrix(0, p, p)
  for(i in 1:p){
    for(j in 1:p){
      if(i==j){Mat[i, j] <- 2}
      else{
        Mat[i, j] <- 0.1 ^ abs(i-j)}
    }
  }
  sigma <- Mat
  positiveData <- mvrnorm(numPositive, positiveMean, sigma)
  if(numNegative > 0) {
    negativeData <- mvrnorm(numNegative, negativeMean, sigma)
    return(rbind(positiveData, negativeData))
  }
  else return(positiveData)
}

svddTrain <- function(X, Gram_Matrix, sigma, C1, C2=0, negativeProportion=0){
  if (!is.matrix(X)) X <- as.matrix(X)
  N <- dim(X)[1]
  numNegative <- round(negativeProportion*N) #number of negative rows in training data
  numPositive <- N-numNegative #number of positive rows in training data
  d <- make.d.vec(X, sigma)
  D <- gram_mat(X, sigma)
  D <- 2*D
  D <- D + diag(dim(D)[1])*1e-12
  
  # create b, the first and second row makes sure alphas sum 
  #   to 1, the others gaurantee they are greater than 0:
  bv <- c(1, 
          rep(0, N), 
          rep(-C1, numPositive), 
          rep(-C2, numNegative))
  
  #Initialize the designed A matrix to go along with bv:
  A  <- cbind(rep(1, N), diag(N), -diag(N))
  alpha <- solve.QP(D, d, A, bv, meq=1)$solution #the alphas
  non_zero_alphas <- alpha[round(alpha, digits=4) > 0]
  locations <- which(round(alpha, digits=4) > 0)
  support_vectors <- X[locations,]
  num_SVs <- length(locations)
  center <- t(alpha) %*% X
  return(list(num_SVs=num_SVs, 
              locations=locations, 
              alpha=alpha, 
              nza=non_zero_alphas, 
              sv=support_vectors, 
              ctr=center))
}

