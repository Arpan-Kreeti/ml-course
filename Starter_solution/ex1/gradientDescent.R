gradientDescent <- function(X, y, theta, alpha, num_iters) {
  #GRADIENTDESCENT Performs gradient descent to learn theta
  #   theta <- GRADIENTDESENT(X, y, theta, alpha, num_iters) updates theta by
  #   taking num_iters gradient steps with learning rate alpha
  
  # Initialize some useful values
  m <- length(y) # number of training examples
  J_history = rep(0,num_iters + 1)
  theta_history = matrix(0,num_iters + 1,length(theta))
  
  theta_history[1,] = t(theta)
  J_history[1] = computeCost(X, y, theta)
  
  # We start from 2 since theta 0 or theta 1 here is 1 (assumed)
  for (iter in 2:(num_iters + 1)) {
    # ---------------------- YOUR CODE HERE ----------------------
    # Instructions: Perform a single gradient step on the parameter vector
    #               theta.
    #
    # Hint: While debugging, it can be useful to print out the values
    #       of the cost function (computeCost) and gradient here.
    #
    
    h = X %*% theta
    
    error = h-y
    
    gradient = alpha * (1/m) * (t(X) %*% error)
    
    theta = theta - gradient
    
    # Save the cost J in every iteration
    J_history[iter] = computeCost(X, y, theta)
    theta_history[iter,] = t(theta)
  }
  
  list(theta = theta, J_history = J_history, theta_history = theta_history)
  # ------------------------------------------------------------
}
