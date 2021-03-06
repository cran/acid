\name{ysample.md}
\alias{ysample.md}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Sampling Incomes from a Mixture of Income Distributions
%%  ~~function to do ... ~~
}
\description{ This function samples incomes from a mixture of two continuous income distributions and a point mass for zero-incomes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ysample.md(n, dist1, dist2, theta, p0, p1, p2, dist.para.table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ number of observations.
%%     ~~Describe \code{n} here~~
}
 \item{dist1}{character string with the name of the first continuous distribution used. Must be listed in dist.para.table. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
}
  \item{dist2}{character string with the name of the second continuous distribution used. Must be listed in dist.para.table. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
}
  \item{theta}{vector with the parameters of dist1 and dist2. Order must be the same as in the functions for the distributions.
}
  \item{p0}{scalar with probability mass for the point mass.
}
  \item{p1}{scalar with probability mass for dist1.
}
  \item{p2}{scalar with probability mass for dist2.
}
  \item{dist.para.table}{a table of the same form as \code{\link{dist.para.t}} with distribution name, function name and number of parameters.
}
}

\value{ returns the sample of observations.
}
\author{ Alexander Sohn
}
\seealso{  \code{\link{pval.md}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(dist.para.t)
ygrid<-seq(0,1e5,by=1000)
theta<-c(5,1,10,3)
p0<-0.2
p1<-0.3
p2<-0.5
n <-10
ysample.md(n, "LOGNO", "LOGNO", theta, p0, p1, p2, dist.para.t)

}