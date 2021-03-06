\name{midks.plot}
\alias{midks.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot Comparing Parametric and Empirical Cumulative Density Functions
%%  ~~function to do ... ~~
}
\description{ This function plots a graph entailing the empirical cdf and the parametrically specified cdf composed of a mixture distribution either by cdf.mix.dag or cdf.mix.LN.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
midks.plot(x.seq, y, dist, w.emp = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.seq}{ the sequence on the x-axis for which the parametric distribution is plotted.
%%     ~~Describe \code{x.seq} here~~
}
  \item{y}{ a vector of observed incomes.
%%     ~~Describe \code{y} here~~
}
  \item{dist}{ a function specifying the parametric cdf.
%%     ~~Describe \code{dist} here~~
}
  \item{w.emp}{ the weights of the observations contained in y.
%%     ~~Describe \code{w.emp} here~~
}
  \item{\dots}{ arguments to be passed to dist.
%%     ~~Describe \code{\dots} here~~
}
}

\author{ Alexander sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{midks.test}},\code{\link{cdf.mix.dag}} ,\code{\link{cdf.mix.LN}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# parameter values
pi0.s<-0.2
pi1.s<-0.1
thres0.s<-0
thres1.s<-25000
mu.s<-20000
sigma.s<-5
nu.s<-0.5
tau.s<-1
x.seq<-seq(0,200000,by=1000)

# generate sample
n<-100
s<-as.data.frame(matrix(NA,n,3))
names(s)<-c("cat","y","w")
s[,1]<-sample(1:3,n,replace=TRUE,prob=c(pi0.s,pi1.s,1-pi0.s-pi1.s))
s[,3]<-rep(1,n)
for(i in 1:n){
  if(s$cat[i]==1){s$y[i]<-0
  }else if(s$cat[i]==2){s$y[i]<-runif(1,thres0.s,thres1.s)
  }else s$y[i]<-rGB2(1,mu=mu.s,sigma=sigma.s,nu=nu.s,tau=tau.s)+thres1.s
}
# display
midks.plot(x.seq,s$y,dist=cdf.mix.dag,pi0=pi0.s,thres0=thres0.s,pi1=pi1.s,
thres1=thres1.s,mu=mu.s,sigma=sigma.s,nu=nu.s,tau=tau.s)


}