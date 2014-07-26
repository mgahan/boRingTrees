#'Rolling Calculations Based Off of Dates (or numbers) With a By Statement
#' 
#'Compute rolling calcs such as length (counts), sum, min, max, or median
#'@param data A data.frame or data.table
#'@param bylist A reference list of variables to pivot off (e.g. c("Cust.ID","Prod.ID")).
#'@param dates A reference to the name of the dates column in data
#'@param target Reference to column in data you want to perform calculations on. If target=NULL, then only rolling count operations will be performed
#'@param lower The lower bound of the date range (e.g. lower=0)
#'@param upper The upper bound of the date range (e.g. upper=31)
#'@param incbounds TRUE means inclusive bounds i.e. [lower,upper]. FALSE means exclusive bounds i.e. (lower,upper).
#'@param stat Default is length (calculates rolling counts). Can also use mean, median, min, max
#'@param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#'@param cores If using a Mac or Linux, this forks out the process in the amount of cores you desire. This could be useful in speeding up the code. Defaults to 1. Windows users should use 1.

#'@examples 
#'set.seed(1)
#'Trans_Dates <- as.Date(c(31,33,65,96,150,187,210,212,240,273,293,320,
#'                         32,34,66,97,151,188,211,213,241,274,294,321,
#'                         33,35,67,98,152,189,212,214,242,275,295,322),origin="2010-01-01")
#'Cust_ID <- c(rep(1,12),rep(2,12),rep(3,12))
#'Target <- rpois(36,3)
#'require("data.table")
#'data <- data.table(Trans_Dates,Cust_ID,Target)

#'data[,Roll:=rollingByCalcs(data=data,bylist="Cust_ID",dates="Trans_Dates",
#'                           target="Target",lower=0,upper=31,incbounds=T,stat=sum,na.rm=T,cores=1)]
#'print(data)
#'@note A couple examples of how to use lower,upper, and incbounds :

#'If you want to get rolling sum of the past 31 days (including the current day)
#'lower=0, upper=31, incbounds=T.  

#'If you want to get rolling sum of the next 31 days (including the current day)
#'lower=-31, upper=0, incbounds=T.  

#'If you want to get rolling sum of the next 31 days (excluding the current day and the 31st day in future)
#'lower=-31, upper=0, incbounds=F.
#'
#'@return A vector of the desired rolling calculation
#'@export

rollingByCalcs <- function(data,bylist=NULL,dates,target=NULL,
                           lower,upper,incbounds=T,stat=length,na.rm=T,cores=1){
  tic <- Sys.time()
  
  require("data.table")
  require("parallel")
  data <- data.table(data)
  
  
  if (is.null(bylist)){
    data[, id.filler := .I]
    bylist <- "id.filler"
  }
  
  if (is.null(target)){
    data[,target:=1]
    target <- "target"
  }
  
  ##Create group by variable
  data[,Grp.Var:=.GRP,by=bylist]
  
  ##Assign variable names
  data[,target:=data[,eval(parse(text=target))]]
  data[,dates:=data[,eval(parse(text=dates))]]
  
  ##Create "list" of comparison dates
  Ref <- data[,list(Compare_Value=list(I(target)),Compare_Date=list(I(dates))), by=c("Grp.Var")]
  
  ##Compare two lists and see of the compare date is within N days
  data$Roll.Val <- mcmapply(FUN = function(RD, NUM) {
    d <- as.numeric(RD-Ref$Compare_Date[[NUM]])
    true.vals <- between(x=d,lower=lower,upper=upper,incbounds=incbounds)  
    out <- stat(Ref$Compare_Value[[NUM]][true.vals])
    return(out)
  }, RD = data$dates,NUM=data$Grp.Var,mc.cores=cores)
  
  print(Sys.time()-tic)
  return(data$Roll.Val)
}