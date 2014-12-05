\name{gini.GB2}
\alias{gini.GB2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Gini Coefficient for a Generalised Beta Distribution of Second Kind
%%  ~~function to do ... ~~
}
\description{ This function provides a numerical approximation to standard entropy measures for Generalised Beta Distribution of Second Kind.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gini.GB2(b, a, p, q, zcens = 4800, ylim = c(0, 1e+06), zeroapprox = 0.01)
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
  \item{ylim}{ limits of the interval of y considered needed for the approximation of the entropy measure. The default is [0,1e+06].
%%     ~~Describe \code{ylim} here~~
}
  \item{zeroapprox}{ an approximation for zero needed for the approximation of the entropy measure. The default is 0.01.
%%     ~~Describe \code{zeroapprox} here~~
}
}
\value{ returns the Gini coefficient.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 1-86, Elsevier, Amsterdam.

%% ~put references to the literature/web site here ~
}
\author{ Alexander Sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
a.test<- 4
b.test<- 20000
p.test<- 0.7
q.test<- 1
alpha.test<-1
GB2sample<-rGB2(10000,b.test,a.test,p.test,q.test)
gini.GB2(b.test,a.test,p.test,q.test,zcens=0,ylim=c(0,1e+07))
gini(GB2sample)
}