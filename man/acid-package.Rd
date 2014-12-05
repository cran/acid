\name{acid-package}
\alias{acid-package}
\alias{acid}
\docType{package}
\title{
Analysing Conditional Distributions of Income
}
\description{Functions for the analysis of income distributions for subgroups of the population as defined by a set of variables like age, gender, region, etc. 
This entails a Kolmogorov-Smirnov test for a mixture distribution as well as functions for moments, inequality measures and entropy measures of income distributions. 
This package thus aides the analysis of income inequality by offering tools for the exploratory analysis of income distributions at the disaggregated level. 
}
\details{
\tabular{ll}{
Package: \tab acid\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-11-18\cr
License: \tab GPL-3\cr
}

\code{\link{cdf.mix.dag}}, 
\code{\link{weighted.entropy}},\code{\link{weighted.gini}},
\code{\link{midks.test}},\code{\link{midks.plot}}  
}

\references{
Sohn, A., Klein, N. and Kneib. T. (2014): A New Semiparametric Approach to Analysing Conditional Income Distributions, in: SOEPpapers, No. 676.
%% ~put references to the literature/web site here ~
}

\author{
Alexander Sohn <asohn@uni-goettingen.de>
}

\keyword{ package }
\seealso{ \code{\link[gamlss]{gamlss}}, \code{\link[ineq]{ineq}}
}