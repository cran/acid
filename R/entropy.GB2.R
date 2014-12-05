entropy.GB2 <-
function(b,a,p,q,zcens=4800,alpha=NULL,ylim=c(0,1000000),zeroapprox=0.01){ ## - see Cowell (2000), pp.109-110 for details.
  if (is.null(alpha)) alpha <- 1
  y      <- seq(ylim[1],ylim[2],length.out=10001)+zcens+zeroapprox
  entropy<-rep(NA,length(b))
  for(j in 1:length(b)){
    den<- dGB2(y-zcens,b[j],a[j],p[j],q[j])
    meanGB2<- b[j]*beta(p[j]+1/a[j],q[j]-1/a[j])/beta(p[j],q[j])+zcens
    yi     <-y[-length(y)]+(max(y)-min(y))/(length(y)-1)/2
    deni   <- dGB2(yi-zcens,b[j],a[j],p[j],q[j])
    
    if (alpha == 0){
      entropyj <- -1*(t(log(yi/meanGB2))%*%deni*(max(y)-min(y))/(length(y)-1))
    }else if (alpha == 1){ 
      entropyj<- t(log(yi/meanGB2)*(yi/meanGB2))%*%deni*(max(y)-min(y))/(length(y)-1)
    }else{
      int_alpha<-  t(yi^alpha)%*%deni*(max(y)-min(y))/(length(y)-1)
      entropyj <- (int_alpha/meanGB2^alpha-1)/(alpha*(alpha-1))
    }
    entropy[j]<-entropyj
  }  
  return(entropy)
}
