featureNormalize <- function(X) {
  #FEATURENORMALIZE Normalizes the features in X
  #   FEATURENORMALIZE(X) returns a normalized version of X where
  #   the mean value of each feature is 0 and the standard deviation
  #   is 1. This is often a good preprocessing step to do when
  #   working with learning algorithms.
  
  
  # You need to set these values correctly
  X_norm <- X
  
  # dimnesion of X dim(X) = 47*3
  # rep(x,n) -> replicate x 'n' times, so rep(0,dim(X0[2]) = rep(0,3) =  0 0 0
  # mu will contain the mean of each feature and sigma will have standard deviation
  mu <- rep(0,dim(X)[2])
  sigma <- rep(0,dim(X)[2])
  
  # ---------------------- YOUR CODE HERE ----------------------
  # Instructions: First, for each feature dimension, compute the mean
  #               of the feature and subtract it from the dataset,
  #               storing the mean value in mu. Next, compute the
  #               standard deviation of each feature and divide
  #               each feature by it's standard deviation, storing
  #               the standard deviation in sigma.
  #
  #               Note that X is a matrix where each column is a
  #               feature and each row is an example. You need
  #               to perform the normalization separately for
  #               each feature.
  #
  # Hint: You might find the 'mean' and 'sd' functions useful.
  #
  
  # find the mean of each set of features and store it in
  # mu[1], mu[2], m[3] respectively, iterate from 1 to dim(X)[2]
  for(i in 1:dim(X)[2]){
    mu[i] <- mean(X[,i])
  }
  
  # similarly sigma
  for (p in 1:dim(X)[2]) {
    sigma[p] <- sd(X[,p])
  }
  
  # Normalization: ( x(i) - mean(i) ) / sigma(i)
  
  # For each row/feature if sigma for that feature !=0
  # then for each training value of that feature 
  # do X_norm[i, p] <- (X[i, p] - mu[p]) / sigma[p]
  for (p in 1:dim(X)[2]) {
    if (sigma[p] != 0)
      for (i in 1:dim(X)[1])
        X_norm[i, p] <- (X[i, p] - mu[p]) / sigma[p]
      else
        # sigma(p) == 0 <=> forall i, j,  X(i, p) == X(j, p) == mu(p)
        # In this case,  normalized values are all zero.
        # (mean is 0,  standard deviation is sigma(=0))
        X_norm[, p] <- t(rep(0,dim(X)[1]))
  }
  
  list(X_norm = X_norm, mu = mu, sigma = sigma)
  # ------------------------------------------------------------
  
}
