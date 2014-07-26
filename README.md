boRingTrees
===========

R Package for UIUC Dalling Lab

# Install 

To install the development version from github, use the
**devtools** package,

```r
library("devtools")
install_github("boRingTrees","mgahan")
```

Windows users also must first install
[Rtools](http://cran.rstudio.com/bin/windows/Rtools/).

# Examples

Below is an example of how to use the `rollingByCalcs` function to find the rolling
sum by a Customer ID variable.

```r
library("boRingTrees")

set.seed(1)
Trans_Dates <- as.Date(c(31,33,65,96,150,187,210,212,240,273,293,320,
                         32,34,66,97,151,188,211,213,241,274,294,321,
                         33,35,67,98,152,189,212,214,242,275,295,322),origin="2010-01-01")
Cust_ID <- c(rep(1,12),rep(2,12),rep(3,12))
Target <- rpois(36,3)
require("data.table")
data <- data.table(Trans_Dates,Cust_ID,Target)

data[,Roll:=rollingByCalcs(data=data,bylist="Cust_ID",dates="Trans_Dates",
        target="Target",lower=0,upper=31,incbounds=T,stat=sum,na.rm=T,cores=1)]

```
