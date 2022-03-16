# Sample code for yield curve classification.R

getwd()
setwd("/Users/beeway/Desktop/Interest Rates/Mypapers/Markov")

## read data and form data frame
mc=read.csv("mc.csv")   # mc is the original data; mmc include level, slope, curvature; mms includes the chain seq
yt=read.csv('mmc.csv')  # ts tm tl yields and spreads for classification
yn=read.csv('yn.csv')  # yn is the daily date graph for bc with straight lines

library(psych);describe(yt)
describe(mc)
summary(mc)
str(mc)

# creating counting dummy for each type - exhausting 756
attach(yt)
u=ifelse((tm-ts>0.1 & tm<=tl)| (tl-tm>0.1 & tm>=ts), 1, 0)     # upward    546
h=ifelse((tm-ts>0.1 & tm>tl) | (tm-tl>0.1 & tm>ts), 1, 0)       # hump      78
f=ifelse(    abs(tm-ts)<=0.1 & abs(tl-tm)<=0.1, 1,0)            # flat      16  
b=ifelse((ts-tm>0.1 & tl>tm) | (tl-tm>0.1 & ts>tm), 1, 0)       # bowl      40  
d=ifelse((ts-tm>0.1 & tm>=tl)| (tm-tl>0.1 & ts>=tm), 1, 0)     # downward  76      


## calculating the number of ocurrence for each type and frequency
fc=cbind(u,h,f,b,d)
apply(fc,2,sum,na.rm=T)
ts=sum(colSums(fc));ts
fd=colSums(fc)/ts;fd

# initial distribution
u-0.7222; h-0.1032; f-0.0212 ; b-0.0529 ; d-0.1005

cs=colSums(fc,na.rm=TRUE)  # colSums();rowSums()
cs/756

# statistics for level, slope, curvature conditiional on each type
# use five category dummy
mc=data.frame(yt,u,h,f,d,b)
umc=subset(mc,u==1); describe(umc)  # 546
hmc=subset(mc,h==1); describe(hmc)  # 78
fmc=subset(mc,f==1); describe(fmc)  # 16
bmc=subset(mc,b==1); describe(bmc)  # 40
dmc=subset(mc,d==1); describe(dmc)  # 76
  
View(umc)  # 
View(hmc)  #
View(fmc)  #
View(bmc)  #
View(dmc)  #

write.csv(mc,file='mct.csv')
##############################################


# calculate the percentage of each type
length(u);table(u);table(h);table(f);table(b);table(d)
cb=cbind(date,u,h,f,b,d)

yt=data.frame(yt,fc)
write.csv(yt,file='yt.csv')


##################################

# level measure
l1=ys
l2=(ys+ym+yl)/3

# slope measure
s1=ym-ys
s2=yl-ym
s3=yl-ys

# curvature measure
c=ym-(ys+yl)/2

###################################
# Graphing the five types of yield curves based on data availability for 11 yields

u 1603  h 8207 f 0602 b 0608 d 8202  
u 1001  h 8207 f 8910 b 7209 d 8003
# plot type: p l b c o h s S

# method 1 specify the row number
y1=read.csv('mc.csv');y1=y1[,2:12]
yc1=as.numeric(y1[756,2:12]);
yc2=as.numeric(y1[352,2:12]);
yc3=as.numeric(y1[635,2:12]);
yc4=as.numeric(y1[641,2:12]);
yc5=as.numeric(y1[347,2:12]);
plot( yc1,lty=1,lwd=2,type='l',col=1,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(0,16))
lines(yc2,lty=2,lwd=2,type="l",col=2) # h
lines(yc3,lty=3,lwd=2,type="l",col=3) # f
lines(yc4,lty=4,lwd=2,type="l",col=4) # b
lines(yc5,lty=5,lwd=2,type="l",col=5) # d
legend("topleft",cex=0.8,c("2016-3 U","1982-7 H","2006-2 F","2006-8 B","1982-2 D"),col=c(1:5),lty=c(1:5))
legend("bottomright",cex=0.8,c("2016-3","1982-7","2006-2","2006-8","1982-2"),col=c(1:5),lty=c(1:5))

title("U.S. Treasury Monthly Yield Curves")
legend("bottomright",cex=0.8,c("1", "2","3"),col=c(1:3),lty=c(1:3))

# method 2 by date
yk1=subset(y1,date=="2016-03");yk1=as.numeric(yk1)
yk2=subset(y1,date=="1982-07");yk2=as.numeric(yk2)
yk3=subset(y1,date=="2006-02");yk3=as.numeric(yk3)
yk4=subset(y1,date=="2006-08");yk4=as.numeric(yk4)
yk5=subset(y1,date=="1982-02");yk5=as.numeric(yk5)
plot(yk1,type="o",lty=1,lwd=2,col=1,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(0,10))
lines(yk2,lty=2,lwd=2,type="o",col=2);lines(yk3,lty=2,lwd=2,type="o",col=3)
lines(yk4,lty=2,lwd=2,type="o",col=4);lines(yk5,lty=2,lwd=2,type="o",col=5)

yd1=subset(y1,date=="2000-08");yd1=as.numeric(yd1)
yd2=subset(y1,date=="2000-09");yd2=as.numeric(yd2)

## hightlight each type in different scales
plot( yc1,lty=1,lwd=2,type='o',col=1,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(0,5)) # U
plot( yc2,lty=1,lwd=2,type='o',col=2,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(10,15)) # H
plot( yc3,lty=1,lwd=2,type='o',col=3,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(0,10)) # F
plot( yc4,lty=1,lwd=2,type='o',col=4,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(4,7)) # B
plot( yc5,lty=1,lwd=2,type='o',col=5,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(10,16)) # D
plot( yd1,lty=1,lwd=2,type='o',col=5,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(5,7)) # D
plot( yd2,lty=1,lwd=2,type='o',col=5,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(5,7)) # D

### graphing all the classified the yield curves
mc=read.csv('mc.csv'); attach(mc); uyc=subset(mc, ms=='U'); 
hyc=subset(mc,ms=='H'); fyc=subset(mc,ms=='F'); byc=subset(mc, ms=='B'); dyc=subset(mc,ms=='D')

uy=matrix(NA,nrow(uyc), 11); for (j in 1: nrow(uyc)) {uy[j,]=as.numeric(uyc[j,2:12])}
plot(uy[1, ], type='l', lwd=2, col=1, ylab="Yield (%)",xlab="Maturity Index", xlim=c(1,12),ylim=c(0,15))
opt=c(2:nrow(uyc)); for(i in 2:nrow(uyc)){lines(uy[i,], lwd=2, lty=opt[i], col=opt[i]) }

fy=matrix(NA,nrow(fyc), 11); for (j in 1: nrow(fyc)) {fy[j,]=as.numeric(fyc[j,2:12])}
plot(fy[1, ], type='l', lwd=2, col=1, ylab="Yield (%)",xlab="Maturity Index", xlim=c(1,12),ylim=c(0,10))
opt=c(2:nrow(fyc)); for(i in 2:nrow(fyc)){lines(fy[i,], lwd=2, lty=opt[i], col=opt[i]) }

hy=matrix(NA,nrow(hyc), 11); for (j in 1: nrow(hyc)) {hy[j,]=as.numeric(hyc[j,2:12])}
plot(hy[1, ], type='l', lwd=2, col=1, ylab="Yield (%)",xlab="Maturity Index", xlim=c(1,12),ylim=c(0,18))
opt=c(2:nrow(hyc)); for(i in 2:nrow(hyc)){lines(hy[i,], lwd=2, lty=opt[i], col=opt[i]) }

by=matrix(NA,nrow(byc), 11); for (j in 1: nrow(byc)) {by[j,]=as.numeric(byc[j,2:12])}
plot(by[1, ], type='l', lwd=2, col=1, ylab="Yield (%)",xlab="Maturity Index", xlim=c(1,12),ylim=c(0,10))
opt=c(2:nrow(byc)); for(i in 2:nrow(byc)){lines(by[i,], lwd=2, lty=opt[i], col=opt[i]) }

dy=matrix(NA,nrow(dyc), 11); for (j in 1: nrow(dyc)) {dy[j,]=as.numeric(dyc[j,2:12])}
plot(dy[1, ], type='l', lwd=2, col=1, ylab="Yield (%)",xlab="Maturity Index", xlim=c(1,12),ylim=c(0,18))
opt=c(2:nrow(dyc)); for(i in 2:nrow(dyc)){lines(dy[i,], lwd=2, lty=opt[i], col=opt[i]) }


## graphing level, slope and curvature series, but the first line is not a line due to the "level" var-- date
ys=read.csv('mmc.csv')
str(ys$date)
class(ys);str(ys)
with (ys, {plot(date,ts,lwd=1,col=1,ylim=c(-3,20),ylab="Treasury Yields (%)",xlab="")
  lines(date,ts,lwd=1.5) # repeat ys again to make it a line!
  lines(date,tm,lty=2,lwd=2,col=2);lines(date,tl,lty=3,lwd=2,col=3);
  lines(date,tm.ts,lty=4,lwd=2,col=4);lines(date,tl.tm,lty=5,lwd=2,col=5);
  lines(date,tl.ts,lty=6,lwd=2,col=6);lines(date,tm..tl.ts..2,lty=7,lwd=2,col=7); })
abline(h=0, col=2, lwd=1, lty=2);
rect(xl, rep(-5,10), xr, rep(25,10),density=10,col='gray60',border="gray60")

legend(x=600,y=20,cex=0.7,c("Average short yield","Average median yield","Average long yield",
                            "Median-short spread","Long-median spread","Long-short spread",
                            "Curvature"),col=c(1:7),lty=c(1:7))
#"topright"

# four yields  colored
with (ys, {plot(date,ts,ylim=c(-3,20),ylab="Treasury Yields (%)",xlab="");
  lines(date,ts,lwd=1.5);lines(date,tm,lty=2,lwd=1.5,col=2);
  lines(date,tl,lty=3,lwd=1.5,col=3);lines(date,tl.ts,lty=6,lwd=2,col=4);})
abline(h=0, col=2, lwd=1, lty=2);
xl=c(4,53,85,201,248,322,340,448,576,657); xr=c(14,61,95,212,264,328,356,456,584,675)
rect(xl, rep(-5,10), xr, rep(25,10),density=10,col='gray60',border="gray60") 
legend(x=600,y=20,cex=0.7,c("Average short yield","Average median yield","Average long yield",
                            "Long-short spread"),col=c(1:4),lty=c(1:4))

# four yields black and white
with (ys, {plot(date,ts,ylim=c(-3,20),ylab="Treasury Yields (%)",xlab="");
  lines(date,ts,lwd=1.5);lines(date,tm,lty=2,lwd=1.5,col=1);
  lines(date,tl,lty=3,lwd=1.5,col=1);lines(date,tl.ts,lty=6,lwd=2,col=1);})
abline(h=0, lwd=1, lty=2);
xl=c(4,53,85,201,248,322,340,448,576,657); xr=c(14,61,95,212,264,328,356,456,584,675)
rect(xl, rep(-5,10), xr, rep(25,10),density=10,col='gray60',border="gray60") 
legend(x=600,y=20,cex=0.7,c("Average short yield","Average median yield","Average long yield",
                            "Long-short spread"),lty=c(1:4))

# dates from the NBER business cycle dating committee
xleft=c("1953-07", "1957-08", "1960-04", "1969-12", "1973-11", "1980-01", "1981-07", "1990-07", "2001-03", "2007-12") # beginning
xright=c("1954-05","1958-04", "1961-02", "1970-11", "1975-03", "1980-07", "1982-11", "1991-03", "2001-11", "2009-06") # end
xleft=as.factor(xleft); xright=as.factor(xright)

# yl=subset(ys, date==xleft)
attach(ys)
View(ys[(date=="1953-07") | (date=="1957-08")| (date=="1960-04")| (date=="1969-12")| (date=="1973-11")| (date=="1980-01")
        | (date=="1981-07")| (date=="1990-07")| (date=="2001-03")| (date=="2007-12"),])
View(ys[(date=="1954-05") | (date=="1958-04")| (date=="1961-02")| (date=="1970-11")| (date=="1975-03")| (date=="1980-07")
        | (date=="1982-11")| (date=="1991-03")| (date=="2001-11")| (date=="2009-06"),])
xl=c(4,53,85,201,248,322,340,448,576,657); xr=c(14,61,95,212,264,328,356,456,584,675)

abline(h=0, col=2, lwd=2, lty=2)
# rect(xl, rep(-5,10), xr, rep(25,10),density=10,col='gray50',border=NA)
rect(xl, rep(-5,10), xr, rep(25,10), density=10,col='gray60',border="gray60")
# xl, xr are the left and rigth border; 
# rep(-5,10); rep(25,10);   -5 and 25 is the range of the bottom to top, repeat for 10 times


#fitgdp is the lm() object for the cubic trend model
plot(residuals(fitgdp), ylim=c(-0.10,0.10), xlab="", ylab="") 

abline(h=0, col=2, lwd=2, lty=2)

rect(xleft, rep(-0.10,10), xright, rep(0.10,10), col="green", border=NA)



## rect
plot(c(100, 200), c(300, 450), type= "n", xlab = "", ylab = "")
rect(100, 300, 125, 350) # transparent
rect(100, 400, 125, 450, col = "green", border = "blue") # coloured
rect(115, 375, 150, 425, col = par("bg"), border = "transparent")
rect(150, 300, 175, 350, density = 10, border = "red")
rect(150, 400, 175, 450, density = 30, col = "blue", angle = -30, border = "transparent")

legend(180, 450, legend = 1:4, fill = c(NA, "green", par("fg"), "blue"),
       density = c(NA, NA, 10, 30), angle = c(NA, NA, 30, -30))

par(op)




