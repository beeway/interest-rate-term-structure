getwd()
setwd("/Users/biweichen/Desktop/Interest Rates/Mypapers/Macrostates")

## read data and form data frame
yt=read.csv("mms1.csv")   

# mc is the original data; mmc include level, slope, curvature; mms includes the chain seq

library(psych)
describe(yt)
summary(mc)
str(yt$ms)

#### statistics conditional on yield curve type
# Statistics for macroeconomic state variables conditional on yield curve shapes
attach(yt)
yu=subset(yt,ms=='U'); describe(yu) # 546
yh=subset(yt,ms=='H'); describe(yh) # 78
yf=subset(yt,ms=='F'); describe(yf) # 16
yb=subset(yt,ms=='B'); describe(yb) # 40
yd=subset(yt,ms=='D'); describe(yd) # 76

u1=subset(yu,tl.ts<2);  describe(u1) # 301 normal
u2=subset(yu,tl.ts>=2); describe(u2) # 245 steep

tapply(yt$ts,yt$ms,mean) # conditional on each type reporting mean for ts
View(yh)

#####################################################################
# conditional on recession indicator using mms3 data Jul1953-Jun2010
# Yield statistics over business cycle stages (1953.7–2010.6)
yt=read.csv("mms3.csv")
library(psych);describe(yt)
attach(yt); 
br=subset(yt,recin==3); describe(br) ;  #155; 18m before rece, teasing out overlaps with recessions
ir=subset(yt,recin==1); describe(ir) ;  #121; in rece always counts 
ar=subset(yt,recin==2); describe(ar) ;  #101; 12m after rece, teasing out overlaps with pre-recession
or=subset(yt,recin==0); describe(or) ;  #307; otherwise
# overlap adjusted indicators not the same as NBER dating convention
# 1958-10 to 1960-03 treated as 18-month pre-recession, which covers 1958-06 12-month post-r
# between 1980-01 to 07 in-rece and 1981-07 to 1982-11 in-rece as pre-r covers post-r
# gap is 12 between Jan.1980 : Jul.1980 (6) and Jul.1981 : Nov.1982 (16) counts as br

######################################################################
# check macro variables over the BC, whether they synchronize, gdp, unem, ipr, cur.

View(br)
View(ir)
View(ar)
View(or)

table(br$ms)
table(ir$ms)
table(ar$ms)
table(or$ms)

# robust check for H type noise
orH=subset(or,ms=='H');View(orH)
arH=subset(ar,ms=='H');View(arH)
irH=subset(ir,ms=='H');View(irH)
brH=subset(br,ms=='H');View(brH)

# robust check for F type noise
orF=subset(or,ms=='F');View(orF)
arF=subset(ar,ms=='F');View(arF)
irF=subset(ir,ms=='F');View(irF)



#######################################################################
# creating counting dummy for each type - exhausting 756
attach(yt)
u=ifelse((tm-ts>0.1 & tm<=tl) | (tl-tm>0.1 & tm>=ts), 1, 0)     # upward    546
h=ifelse((tm-ts>0.1 & tm>tl) | (tm-tl>0.1 & tm>ts), 1, 0)       # hump      78
f=ifelse(    abs(tm-ts)<=0.1 & abs(tl-tm)<=0.1, 1,0)            # flat      16  
b=ifelse((ts-tm>0.1 & tl>tm) | (tl-tm>0.1 & ts>tm), 1, 0)       # bowl      40  
d=ifelse((ts-tm>0.1 & tm>=tl) | (tm-tl>0.1 & ts>=tm), 1, 0)     # downward  76      

# calculate the percentage of each type
length(u);table(u);table(h);table(f);table(b);table(d)
cb=cbind(date,u,h,f,b,d)

yt=data.frame(yt,fc)
write.csv(yt,file='yt.csv')

## add one more category in u if spread >200bps
u1=
u=ifelse((tm-ts>0.1 & tm<=tl) | (tl-tm>0.1 & tm>=ts), 1, 0)     # upward  
table(u) # 546 times
u1=ifelse((<0.1 tm-ts<1 & tm<=tl) | (2>tl-tm & tl-tm>0.1 & tm>=ts), 1, 0) 
table(u1)
u2=ifelse((tm-ts>=1 & tl-tm>=2), 1, 0)  # steep upward sloping
table(u2) # 51 times
colSums(u2)


# statistics conditional on each type
mc=data.frame(yt,u,h,f,d,b)
describe(yt)
umc=subset(mc,u==1); describe(umc)  # 546
hmc=subset(mc,h==1); describe(hmc)  # 78
fmc=subset(mc,f==1); describe(fmc)  # 16
bmc=subset(mc,b==1); describe(bmc)  # 40
dmc=subset(mc,d==1); describe(dmc)  # 76

View(umc)  # 
View(hmc)  #
View(fmc)
View(bmc)
View(dmc)

write.csv(mc,file='mct.csv')
##############################################
# frequncy counts of yield curve types over BC
a=c(34, 41, 9, 26, 52); a/sum(a)
b=c(67, 21, 0, 13, 13); b/sum(b)
c=c(103, 7, 0, 0, 10);  c/sum(c)

# All period  U .72 H.10 F .02 B .53  D .10

## graphing macro variables
attach(yt)
plot(date,gr,lwd=2,type="l",col=2,ylim=c(-5,10),ylab="Real Output Variables (%)",xlab="")
lines(date,ur,lty=1,lwd=2,type="l",col=3)


with (yt, {plot(date,gr,lty=1,lwd=2,type="l",col=1,ylim=c(-15,20),ylab="Real Output Variables (%)",xlab="")
  lines(date,ur,lty=2,lwd=2,type="l",col=2);
  lines(date,ip,lty=3,type="l",col=3); })
legend("topright",cex=0.65,c("RGDP: real GDP growth","UNEM: unemployment rate",
 "INPR: industrial production growth"), col=c(1:3),lty=c(1:3))
cor(gr,ur)

cmpi=data.frame(cpi1,cpi2,ppi1,ppi2)
cor(cmpi)
cmro=data.frame(gr,ip,ur)
cor(cmro)
