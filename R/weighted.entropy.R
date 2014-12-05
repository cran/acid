weighted.entropy <-
function(x,w=NULL,parameter=1){
  if(is.null(w)) w<-rep(1/length(x),length(x))
  x <- as.numeric(x)
  n <- length(x)
  fx<- w/sum(w)
  if (is.null(parameter)) 
    parameter <- 1
  if (parameter == 0) { # yields mean logarithmic 
    entropy <- -sum(log(x/weighted.mean(x,w))*fx) 
  }
  else if (parameter == 1) 
    entropy <- sum(x/weighted.mean(x,w)*log(x/weighted.mean(x,w))*fx)
  else {
    entropy <- 1/(parameter * (parameter - 1))*t((x/weighted.mean(x,w))^parameter-1)%*%fx
  }
  return(entropy)
}
