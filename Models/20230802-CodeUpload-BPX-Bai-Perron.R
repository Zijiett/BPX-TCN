cat("\014")
setwd("F:\\Rfile\\Structural_Breaks_master\\HX_NEW")
Total_ELN_HRV_AR<-read.csv("17data.csv")

### (9) Structural changes ####

# Create a Time Series Towers, ELN and FARC HRV
library(xts)
library(tseries)
ELN_HRV <- ts(Total_ELN_HRV_AR$flow, start=c(1), end=c(365), frequency=1)
ELN_HRV
plot(ELN_HRV,xaxt="n" ,main="Streamflow in Huaxian station", xlab="time(year-month)", ylab="flow(m3/s)")

tt<-NULL
ty<-1:12
for (i in 2016){
  ls<-paste(i,"-",ty)
  gsub(" ","",ls)
  tt<-c(tt,ls)
}
tt
axis(side = 1 ,at=seq(1,365,30.4) , labels =tt)
# Run the structural change analysis
library(strucchange)
#bp_ts <- breakpoints(ELN_HRV ~ 1, h=0.25, breaks=2)

bp_ts <- breakpoints(ELN_HRV ~ 1)
summary(bp_ts)
bp_ts
#ci_ts<-confint(bp_ts, breaks = 1)
ci_ts<-confint(bp_ts)

lines(bp_ts)
lines(ci_ts)
legend("topright", c("Breakpoints","Confidence Interval"), # puts text in the legend
       lty=c(2,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("black","red")) # gives the legend lines the correct color and width
legend("topright", c("Breakpoints"), # puts text in the legend
       lty=c(2,1)) # gives the legend appropriate symbols (lines)
res<-residuals(bp_ts)
acf(res)
pacf(res)

fs.ELN_HRV <- Fstats(ELN_HRV ~ 1)
plot(fs.ELN_HRV)
sctest(fs.ELN_HRV)
lines(breakpoints(fs.ELN_HRV))
