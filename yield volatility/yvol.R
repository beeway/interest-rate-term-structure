
# https://rdrr.io/cran/roll/man/roll_sd.html
# https://rdrr.io/cran/roll/man/roll_cor.html

install.packages("roll")
library(roll)
library(readxl)

dm=read_excel("1920spreads.xlsx")

attach(dm)
ws=24; rvs=roll_sd(ys, width=ws); rvm=roll_sd(ym, width=ws); rvl=roll_sd(yl, width = ws)
date=seq(as.Date('1920-01-01'), to=as.Date('2020-03-01'), by="month")

# same line type
plot(date, rvs, type = "l", col=2, ylim=c(0,3), xaxt="n", ylab="", xlab="");
lines(date, rvm, type = "l", col=3)
lines(date, rvl, type = "l", col=4)
# different line types
plot(date, rvs, type="l", lty=1, col=2, ylim=c(0,3), xaxt="n", ylab="Treasury Yield Volatility (SD)", xlab="")
lines(date, rvm, lty=2, col=3)
lines(date, rvl, lty=3, col=4)

legend(x=15000, y=3, bty="n", pch="", pt.cex=1, cex = 0.7, lty=c(1:3), x.intersp=0.2, y.intersp=0.4,
       legend=c("V(Ys)","V(Ym)","V(Yl)"), col=c(2:4)) # inset=-0.05

labDates = seq(as.Date('1920-01-01'), to=as.Date('2020-03-01'), format="%Y%m", by = "24 months")
axis.Date(1, date, at=labDates, format="%Y", cex.axis=0.7, las=2) 
# las controls the direction, tcl controls the tick length

# add minor ticks with no labels, shorter tick length
mtick= seq(as.Date('1920-01-01'), to=as.Date('2020-03-01'), format="%Y%m", by = "12 months")
axis.Date(1, date, at=mtick, labels = F, cex.axis=0.7, tcl=-0.25)
# # https://stackoverflow.com/questions/15575625/how-to-replace-numbers-on-x-axis-by-dates-when-using-plot-in-r

# Data staring 1920 NBER recession dates do not change with new obervations
# shaded recessions for plot, matching factor date format
xl=as.Date(c( "1920-02-01", "1923-06-01", "1926-11-01", "1929-09-01", "1937-06-01", "1945-03-01", "1948-12-01", 
      "1953-08-01", "1957-09-01", "1960-05-01", "1970-01-01", "1973-12-01", "1980-02-01", "1981-08-01", 
      "1990-08-01", "2001-04-01", "2008-01-01")) # beginning

xr=as.Date(c("1921-07-01", "1924-07-01", "1927-11-01", "1933-03-01", "1938-06-01", "1945-10-01", "1949-10-01", 
           "1954-05-01", "1958-04-01", "1961-02-01", "1970-11-01", "1975-03-01", "1980-07-01", "1982-11-01", 
           "1991-03-01", "2001-11-01", "2009-06-01")) # ending

rect(xl, rep(-1, 17), xr, rep(4, 17), density=25, col="gray80", border="gray80")

