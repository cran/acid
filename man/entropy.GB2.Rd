\name{entropy.GB2}
\alias{entropy.GB2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Entropy Measures for a Generalised Beta Distribution of Second Kind
%%  ~~function to do ... ~~
}

\description{ This function provides a numerical approximation to standard entropy measures for Generalised Beta Distribution of Second Kind.
 }
\usage{
entropy.GB2(b, a, p, q, zcens = 4800, alpha = NULL, ylim = c(0, 1e+06), zeroapprox = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{ the parameter b of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{b} here~~
}
  \item{a}{ the parameter a of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{a} here~~
}
  \item{p}{ the parameter p of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{p} here~~
}
  \item{q}{ the parameter q of the Dagum distribution as defined by Kleiber and Kotz (2003). 
%%     ~~Describe \code{q} here~~
}
  \item{zcens}{ a location shift parameter, indicating the lower bound of the distribution. 
%%     ~~Describe \code{zcens} here~~
}
  \item{alpha}{ measure for the entropy measure as denoted by Cowell (2000). The default is alpha=1, i.e. the Theil Index. 
%%     ~~Describe \code{alpha} here~~
}
  \item{ylim}{ limits of the interval of y considered needed for the approximation of the entropy measure. The default is [0,1e+06].
%%     ~~Describe \code{ylim} here~~
}
  \item{zeroapprox}{ an approximation for zero needed for the approximation of the entropy measure. The default is 0.01.
%%     ~~Describe \code{zeroapprox} here~~
}
}
\value{ returns the selected entropy measure.

}
\references{ Kleiber, C. and Kotz, S. (2003): Statistical Size Distributions in Economics and Actuarial Sciences, Wiley, Hoboken.

Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 1-86, Elsevier, Amsterdam.
}
\author{ Alexander Sohn
%%  ~~who you are~~
}

\examples{
a.test<- 4
b.test<- 20000
p.test<- 0.7
q.test<- 1
alpha.test<-1
GB2sample<-rGB2(10000,b.test,a.test,p.test,q.test)
entropy.GB2(b.test,a.test,p.test,q.test,zcens=0,alpha=alpha.test,ylim=c(0,1e+07))
entropy(GB2sample, parameter=alpha.test)
}

