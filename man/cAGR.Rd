\name{cAgr}
\alias{cAgr}
\title{CompGR: Complete Annual Growth Rate Generator}
\usage{
cAgr(time, obs, model = NULL)
}
\arguments{
\item{time}{A numeric vector containing sequence of time points}

\item{obs}{A numeric vector containing sequence of observations}

\item{model}{Three models. User can may select one of the three methods including Linear, Logarithmic and Compound growth}
}
\value{
CAGR
}
\description{
CompGR: Complete Annual Growth Rate Generator
}
\examples{
\donttest{
time<-c(1,2,3,4,5,6,7)
obs<-c(14,18,19,15,14,17,16)
CAGR_out<-cAgr(time=time,obs=obs,model="lin")
}
}
\references{
1. Sharma, M. K., Sisodia, B. V. S., & Lal, K. (2013). Growth and trends of pulse production in India. Journal of Food Legumes, 26(1and2), 86-92.
}

