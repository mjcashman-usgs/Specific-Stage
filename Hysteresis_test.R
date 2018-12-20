if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(smwrData, DVstats)
data(ChoptankFlow)
# Process by calendar year as that is the retrieval range
ChopPart <- with(ChoptankFlow, hysep(Flow, datetime, da=113,STAID="01491000"))
ChopPart
lines(df[1:366,"Qw"],df[1:366,"Qs"],
        lwd=2        )
s <- seq(from=1, to=length(df[,"Qw"])-1, by=22)
arrows(df[,"Qw"][s], df[,"Qs"][s], df[,"Qw"][s+1], df[,"Qs"][s+1], 
        length=0.1,
        code=2,
        lwd=2
        )

himid <- function(x){

mid <- function(x) 0.5*(max(x, na.rm = T) – min(x, na.rm = T)) + min(x, na.rm = T)

target <- mid(x$q)
idx 0)

f <- function(i, target) approx(c(x$q[i], x$q[i+1]), c(x$ssc[i], x$ssc[i+1]), xout=target)$y
yp <- sapply(idx, f, target = target)

if (!length(yp) == 2) {
h <- NA
return(h)
} else {

if (yp[1] < yp[2]) {
h <- -1/(yp[1]/yp[2]) + 1
return(h)
} else {

h <- (yp[1]/yp[2]) – 1

return(h)
}
}
}
install.packages("devtools")
devtools::install_github("USGS-R/Rainmaker")
library(Rainmaker)


