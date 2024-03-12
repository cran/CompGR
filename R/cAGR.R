#' @title CompGR: Complete Annual Growth Rate Generator
#'
#' @param time A numeric vector containing sequence of time points
#' @param obs A numeric vector containing sequence of observations
#' @param model Three models. User can may select one of the three methods including Linear, Logarithmic and Compound growth
#' @return CAGR
#' @examples
#' \donttest{
#' time<-c(1,2,3,4,5,6,7)
#' obs<-c(14,18,19,15,14,17,16)
#' CAGR_out<-cAgr(time=time,obs=obs,model="lin")
#' }
#' @references
#' 1. Sharma, M. K., Sisodia, B. V. S., & Lal, K. (2013). Growth and trends of pulse production in India. Journal of Food Legumes, 26(1and2), 86-92.
#'@import stats

cAgr<-function(time,obs,model=NULL)
{
  n<-length(time)
  if (length(time)!=length(obs)){
    stop(gettext("length of time shoulb be same as length of obs"))
  }
  else
  {
    time<-time-median(time)
    data<-data.frame(time,obs)}

  if ("lin" %in% model)
  {
    gm<-lm(obs~time,data=data)
    CAGR<-gm$coefficients[2]/mean(obs)*100
  }

  if ("log" %in% model)
  {
    logobs<-log10(obs)
    data<-data.frame(time,logobs)
    gm<-lm(logobs~time,data=data)
    CAGR<-gm$coefficients[2]*100
  }
  if ("comp" %in% model)
  {
    logobs<-log10(obs)
    data<-data.frame(time,logobs)
    gm<-lm(logobs~time,data=data)
    CAGR<-((10^gm$coefficients[2])-1)*100
  }
  return(CAGR)
}
