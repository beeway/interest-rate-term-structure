getwd()
setwd("/Users/beeway/Desktop/Interest Rates/Mypapers/Markov")

## read data and form data frame
install.packages("readxl")
library("readxl"); dm=read_excel("mupdate.xlsx")

library(psych);describe(dm)
summary(dm); str(dm)

# creat counting dummy variables for each type - exhausting
attach(dm)
u=ifelse((ym-ys>0.1 & ym<=yl)| (yl-ym>0.1 & ym>=ys), 1, 0)     # upward    
h=ifelse((ym-ys>0.1 & ym>yl) | (ym-yl>0.1 & ym>ys), 1, 0)       # hump      
f=ifelse(    abs(ym-ys)<=0.1 & abs(yl-ym)<=0.1, 1,0)            # flat        
b=ifelse((ys-ym>0.1 & yl>ym) | (yl-ym>0.1 & ys>ym), 1, 0)       # bowl        
d=ifelse((ys-ym>0.1 & ym>=yl)| (ym-yl>0.1 & ys>=ym), 1, 0)     # downward  


## calculating the number of ocurrence for each type and frequency
fc=cbind(u,h,f,b,d)
apply(fc,2,sum,na.rm=T)
ts=sum(colSums(fc));ts
fd=colSums(fc)/ts;fd # initial distribution
cs=colSums(fc,na.rm=TRUE); cs/804


# statistics for level, slope, curvature conditional on each type
# use five category dummy
mc=data.frame(dm,u,h,f,d,b)
umc=subset(mc,u==1); describe(umc)  # 
hmc=subset(mc,h==1); describe(hmc)  # 
fmc=subset(mc,f==1); describe(fmc)  # 
bmc=subset(mc,b==1); describe(bmc)  # 
dmc=subset(mc,d==1); describe(dmc)  # 

# creat a shape categorical indicator ms
attach(mc)
mc$ms[u==1]="u"; mc$ms[h==1]="h"; mc$ms[f==1]="f"; mc$ms[b==1]="b"; mc$ms[d==1]="d"
table(mc$ms)
detach(mc)

write.csv(mc, file = "mc.csv")


# single out the most typial shapes  
View(umc)  # 
View(hmc)  #
View(fmc)  #
View(bmc)  #
View(dmc)  #

View(umc[which(umc$sls>4),])     # 2010-04  685
View(hmc[which(hmc$curv>0.5),])  # 1982-07  352
View(fmc)  # 2006-02  635
View(bmc)  # 2007-03  648
View(bmc)  # 2000-12  573

View(dmc[which(dmc$sls<(-3)),])  # 1980-03  324 but lack of short-end obs

###################################
# Graphing the five types of yield curves based on data availability for 11 yields

# method 1 specify the row number
y1=dm[,2:12]
yc1=as.numeric(y1[685,]);
yc2=as.numeric(y1[352,]);
yc3=as.numeric(y1[635,]);
yc4=as.numeric(y1[573,]);
yc5=as.numeric(y1[324,]);

# plot type: p l b c o h s S
 plot(yc1,lty=1,lwd=2,type='o',col=1,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(0,20))
lines(yc2,lty=2,lwd=2,type="o",col=2) # h
lines(yc3,lty=3,lwd=2,type="o",col=3) # f
lines(yc4,lty=4,lwd=2,type="o",col=4) # b
lines(yc5,lty=5,lwd=2,type="o",col=5) # d
legend("topleft",cex=0.55,c("2010-04 U","1982-07 H","2006-02 F","2000-12 B","1980-03 D"),col=c(1:5),lty=c(1:5))

#legend("bottomright",cex=0.8,c("2016-3","1982-7","2006-2","2006-8","1982-2"),col=c(1:5),lty=c(1:5))
#title("U.S. Treasury Monthly Yield Curves")
#legend("bottomright",cex=0.8,c("1", "2","3"),col=c(1:3),lty=c(1:3))

# maturity in years
xm=c(1/12,3/12,6/12,1,2,3,5,7,10,20,30)
 plot(xm, yc1,lty=2, lwd=2, type="o", xlim=c(0, 30), ylim=c(0,20), xlab="Maturity (Years)", ylab="Treasury Yields (%)")
lines(xm, yc2,lty=2,lwd=2,type="o",col=2) # h
lines(xm, yc3,lty=3,lwd=2,type="o",col=3) # f
lines(xm, yc4,lty=4,lwd=2,type="o",col=4) # b
lines(xm, yc5,lty=5,lwd=2,type="o",col=5) # d
legend("topright",cex=0.55,c("2010-04 U","1982-07 H","2006-02 F","2000-12 B","1980-03 D"),col=c(1:5),lty=c(1:5))


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

## hightlight each type in different yield scales
plot( yc1,lty=1,lwd=2,type='o',col=1,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(0,5)) # U
plot( yc2,lty=1,lwd=2,type='o',col=2,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(10,15)) # H
plot( yc3,lty=1,lwd=2,type='o',col=3,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(0,10)) # F
plot( yc4,lty=1,lwd=2,type='o',col=4,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(4,7)) # B
plot( yc5,lty=1,lwd=2,type='o',col=5,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(10,16)) # D
plot( yd1,lty=1,lwd=2,type='o',col=5,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(5,7)) # D
plot( yd2,lty=1,lwd=2,type='o',col=5,ylab="Treasury Yields (%)",xlab="Maturity Index",xlim=c(0,12),ylim=c(5,7)) # D


### graphing all the classified the yield curves - save image 800*600
mc=read.csv('mc.csv'); 

attach(mc); 
uyc=subset(mc, ms=='u'); hyc=subset(mc,ms=='h'); fyc=subset(mc,ms=='f'); byc=subset(mc, ms=='b'); dyc=subset(mc,ms=='d')

uy=matrix(NA,nrow(uyc), 11); for (j in 1: nrow(uyc)) {uy[j,]=as.numeric(uyc[j,2:12])}
plot(uy[1, ], type='l', lwd=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(uyc)); for(i in 2:nrow(uyc)){lines(uy[i,], lwd=1, lty=opt[i], col=opt[i]) }

fy=matrix(NA,nrow(fyc), 11); for (j in 1: nrow(fyc)) {fy[j,]=as.numeric(fyc[j,2:12])}
plot(fy[1, ], type='l', lwd=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(fyc)); for(i in 2:nrow(fyc)){lines(fy[i,], lwd=1, lty=opt[i], col=opt[i]) }

hy=matrix(NA,nrow(hyc), 11); for (j in 1: nrow(hyc)) {hy[j,]=as.numeric(hyc[j,2:12])}
plot(hy[1, ], type='l', lwd=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(hyc)); for(i in 2:nrow(hyc)){lines(hy[i,], lwd=1, lty=opt[i], col=opt[i]) }

by=matrix(NA,nrow(byc), 11); for (j in 1: nrow(byc)) {by[j,]=as.numeric(byc[j,2:12])}
plot(by[1, ], type='l', lwd=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(byc)); for(i in 2:nrow(byc)){lines(by[i,], lwd=1, lty=opt[i], col=opt[i]) }

dy=matrix(NA,nrow(dyc), 11); for (j in 1: nrow(dyc)) {dy[j,]=as.numeric(dyc[j,2:12])}
plot(dy[1, ], type='l', lwd=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(dyc)); for(i in 2:nrow(dyc)){lines(dy[i,], lwd=1, lty=opt[i], col=opt[i]) }


# plot type "o" or "b" for all yield curves
uy=matrix(NA,nrow(uyc), 11); for (j in 1: nrow(uyc)) {uy[j,]=as.numeric(uyc[j,2:12])}
plot(uy[1, ], type='o', lty=1, lwd=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(uyc)); for(i in 2:nrow(uyc)){lines(uy[i,], lwd=1, type="b", lty=opt[i], col=opt[i]) }

hy=matrix(NA,nrow(hyc), 11); for (j in 1: nrow(hyc)) {hy[j,]=as.numeric(hyc[j,2:12])}
plot(hy[1, ], type='o', lwd=1, lty=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(hyc)); for(i in 2:nrow(hyc)){lines(hy[i,], lwd=1, type="b", lty=opt[i], col=opt[i]) }

fy=matrix(NA,nrow(fyc), 11); for (j in 1: nrow(fyc)) {fy[j,]=as.numeric(fyc[j,2:12])}
plot(fy[1, ], type='o', lwd=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(fyc)); for(i in 2:nrow(fyc)){lines(fy[i,], lwd=1, type="b", lty=opt[i], col=opt[i]) }

by=matrix(NA,nrow(byc), 11); for (j in 1: nrow(byc)) {by[j,]=as.numeric(byc[j,2:12])}
plot(by[1, ], type='o', lty=1, lwd=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(byc)); for(i in 2:nrow(byc)){lines(by[i,], lwd=1, type="b",lty=opt[i], col=opt[i]) }

dy=matrix(NA,nrow(dyc), 11); for (j in 1: nrow(dyc)) {dy[j,]=as.numeric(dyc[j,2:12])}
plot(dy[1, ], type='o', lty=1, lwd=1, col=1, ylab="Treasury Yield (%)",xlab="Maturity Index", xlim=c(0,12),ylim=c(0,17))
opt=c(2:nrow(dyc)); for(i in 2:nrow(dyc)){lines(dy[i,], lwd=1, type="o", lty=opt[i], col=opt[i]) }




