
## read data and form data frame
library("readxl"); 
dm=read_excel("1953.xlsx")

library(tidyverse); dm=read.csv("1953S8.csv")

library(psych);describe(dm)
summary(dm); str(dm)

attach(dm); 
uyc=subset(dm, U=='1'); 
u1yc=subset(dm, U1=='1'); u2yc=subset(dm, U2=='1'); u3yc=subset(dm, U3=='1'); u4yc=subset(dm, U4=='1'); 
hyc=subset(dm, H=='1'); fyc=subset(dm,F=='1'); byc=subset(dm, B=='1'); dyc=subset(dm,D=='1')

# plot type "o" or "b" for all minor yield curves

par(mfrow=c(2,2))
by=matrix(NA,nrow(byc), 11); for (j in 1: nrow(byc)) {by[j,]=as.numeric(byc[j,2:12])}
plot(by[1, ], type='o', lty=1, lwd=1, col=1, main="B-Shape Yield Curve",
     ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(byc)); for(i in 2:nrow(byc)){lines(by[i,], lwd=1, type="b",lty=opt[i], col=opt[i]) }

fy=matrix(NA,nrow(fyc), 11); for (j in 1: nrow(fyc)) {fy[j,]=as.numeric(fyc[j,2:12])}
plot(fy[1, ], type='o', lwd=1, col=1, main="F-Shape Yield Curve",
     ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(fyc)); for(i in 2:nrow(fyc)){lines(fy[i,], lwd=1, type="b", lty=opt[i], col=opt[i]) }

hy=matrix(NA,nrow(hyc), 11); for (j in 1: nrow(hyc)) {hy[j,]=as.numeric(hyc[j,2:12])}
plot(hy[1, ], type='o', lwd=1, lty=1, col=1, main="H-Shape Yield Curve", 
     ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(hyc)); for(i in 2:nrow(hyc)){lines(hy[i,], lwd=1, type="b", lty=opt[i], col=opt[i]) }

dy=matrix(NA,nrow(dyc), 11); for (j in 1: nrow(dyc)) {dy[j,]=as.numeric(dyc[j,2:12])}
plot(dy[1, ], type='o', lty=1, lwd=1, col=1, main="D-Shape Yield Curve",
     ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(dyc)); for(i in 2:nrow(dyc)){lines(dy[i,], lwd=1, type="o", lty=opt[i], col=opt[i]) }


uy=matrix(NA,nrow(uyc), 11); for (j in 1: nrow(uyc)) {uy[j,]=as.numeric(uyc[j,2:12])}
plot(uy[1, ], type='o', lty=1, lwd=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(uyc)); for(i in 2:nrow(uyc)){lines(uy[i,], lwd=1, type="o", lty=opt[i], col=opt[i]) }


# plot for all U yield curves
par(mfrow=c(2,2))
u1y=matrix(NA,nrow(u1yc), 11); for (j in 1: nrow(u1yc)) {u1y[j,]=as.numeric(u1yc[j,2:12])}
plot(u1y[1, ], type='o', lty=1, lwd=1, col=1, main=expression('U'[1]-'Shape Yield Curve'),
     ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(u1yc)); for(i in 2:nrow(u1yc)){lines(u1y[i,], lwd=1, type="o", lty=opt[i], col=opt[i]) }

u2y=matrix(NA,nrow(u2yc), 11); for (j in 1: nrow(u2yc)) {u2y[j,]=as.numeric(u2yc[j,2:12])}
plot(u2y[1, ], type='o', lty=1, lwd=1, col=1, main=expression('U'[2]-'Shape Yield Curve'),
     ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(u2yc)); for(i in 2:nrow(u2yc)){lines(u2y[i,], lwd=1, type="o", lty=opt[i], col=opt[i]) }

u3y=matrix(NA,nrow(u3yc), 11); for (j in 1: nrow(u3yc)) {u3y[j,]=as.numeric(u3yc[j,2:12])}
plot(u3y[1, ], type='o', lty=1, lwd=1, col=1, main=expression('U'[3]-'Shape Yield Curve'),
     ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(u3yc)); for(i in 2:nrow(u3yc)){lines(u3y[i,], lwd=1, type="o", lty=opt[i], col=opt[i]) }

u4y=matrix(NA,nrow(u4yc), 11); for (j in 1: nrow(u4yc)) {u4y[j,]=as.numeric(u4yc[j,2:12])}
plot(u4y[1, ], type='o', lty=1, lwd=1, col=1, main=expression('U'[4]-'Shape Yield Curve'),
     ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(u4yc)); for(i in 2:nrow(u4yc)){lines(u4y[i,], lwd=1, type="o", lty=opt[i], col=opt[i]) }



