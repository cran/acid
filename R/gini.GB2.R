gini.GB2 <-
function(b,a,p,q,zcens=4800,ylim=c(0,1000000),zeroapprox=0.01){ # for exact see Kleiber p.193
  x<-seq(ylim[1],ylim[2],length.out=10001)+zcens+zeroapprox # approx not needed, but kept analogously to entropy
  x<-x[-length(x)]+(max(x)-min(x))/(length(x)-1)/2 # centering x within on interval
  Gini<-rep(NA,length(b))
  for(j in 1:length(b)){
    w<-dGB2(x-zcens,b[j],a[j],p[j],q[j])
    w<-w/sum(w)
    x.sort<- sort(x)
    x.order<- order(x)
    x<-x.sort
    n<- length(x)
    w<-w[x.order]/sum(w)
    w.c <- cumsum(w)
    xw.c <- cumsum(w*x)
    xw.c <- xw.c/xw.c[n] #coercing such cumulative distr with max=1
    Ginij<-t(xw.c[-1])%*%w.c[-n] - t(xw.c[-n])%*%w.c[-1]
    Gini[j]<-Ginij
  }  
  return(Gini)
}
