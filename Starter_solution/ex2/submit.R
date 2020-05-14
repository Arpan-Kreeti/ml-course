submit = function(){
  source("lib/submitWithConfiguration.R")

  conf <- list()
  conf$assignmentSlug = 'logistic-regression';
  conf$itemName = 'Logistic Regression';
  conf$partArrays =  c(
    '1', 
    'sigmoid.R' , 
    'Sigmoid Function', 
    '2', 
    'costFunction.R' , 
    'Logistic Regression Cost', 
    '3', 
    'costFunction.R' , 
    'Logistic Regression Gradient', 
    '4', 
    'predict.R' , 
    'Predict', 
    '5', 
    'costFunctionReg.R' , 
    'Regularized Logistic Regression Cost',      
    '6', 
    'costFunctionReg.R' , 
    'Regularized Logistic Regression Gradient')
  
  
  conf$output = output;

  submitWithConfiguration(conf)
}

output = function(partId, auxstring) {
  source("sigmoid.R")
  source("costFunction.R")
  source("predict.R")
  source("costFunctionReg.R")
  # Random Test Cases
  X = cbind(rep(1,20), exp(1) * sin(seq(1,20,1)), exp(0.5) * cos(seq(1,20,1))  )
  y = (sin(X[,1]+X[,2]) > 0)
  theta = c(.25,.5,-.5)
  if (partId == '1')
    out = paste0(sprintf('%0.5f ', sigmoid(X)), collapse = '')
  else if (partId == '2')
    out = sprintf('%0.5f ', costFunction( X, y)(theta))
  else if (partId == '3')
    out = paste0(sprintf('%0.5f ', grad( X, y)(theta)), collapse = '')
  else if (partId == '4')
    out = paste0(sprintf('%0.5f ', predict(theta, X) ), collapse = '')
  else if (partId == '5')
    out = paste0(sprintf('%0.5f ', costFunctionReg(X, y, 0.1)(theta)), collapse = '')
  else if (partId == '6')
    out = paste0(sprintf('%0.5f ', gradReg(X, y, 0.1)(theta)), collapse = '' )
}
