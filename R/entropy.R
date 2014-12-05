entropy <-
function(x, parameter=1){ # using basic Theil index as standard, 
  x <- as.numeric(x)
  n <- length(x)
  fx<- 1/n
  if (is.null(parameter)) 
    parameter <- 1
  if (parameter == 0) { # yields mean logarithmic 
    entropy <- -sum(log(x/mean(x))*fx) 
  }
  else if (parameter == 1) 
    entropy <- sum(x/mean(x)*log(x/mean(x))*fx)
  else {
    entropy <- 1/(parameter * (parameter - 1))*sum(((x/mean(x))^parameter-1)*fx)
  }
  return(entropy)
}
