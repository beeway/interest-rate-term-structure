
setwd("/Users/beeway/Desktop/Interest Rates/Mypapers/Markov")
# setwd("/Users/beeway/Documents/Academic Software/R")

dct=read.csv('dc.csv') 
install.packages('psych')
library(psych);describe(dct)
dim(dct)
str(dct)
head(dct)
class(dct)

# handling the missing value with "for" loop for each column
dct=dct
for (i in 2:19) {dct[,i][dct[,i]==""]=NA
                  dct[,i]=as.numeric(as.character(dct[,i]))}
str(dct)
describe(dct)
# dct=na.omit(dct[,i]='ND')
write.csv(dct, file = "dct.csv")  # save file in csv

############################################
## tl as (10y+20y+30y)/3
############################################

dmc=read.csv('dct.csv')
require(psych);describe(dmc)

# creating counting dummy for each type - effective algorithm
attach(dmc)
u=ifelse((tm-ts>0.1 & tm<=tl) | (tl-tm>0.1 & tm>=ts), 1, 0)     # upward    9891
h=ifelse((tm-ts>0.1 & tm>tl) | (tm-tl>0.1 & tm>ts), 1, 0)       # hump      1208
f=ifelse(    abs(tm-ts)<=0.1 & abs(tl-tm)<=0.1, 1,0)            # flat      245
b=ifelse((ts-tm>0.1 & tl>tm) | (tl-tm>0.1 & ts>tm), 1, 0)       # bowl      855 
d=ifelse((ts-tm>0.1 & tm>=tl) | (tm-tl>0.1 & ts>=tm), 1, 0)     # downward  1378    # total=13577

## calculating the number of ocurrence for each type and frequency
fc=cbind(u,h,f,b,d); str(fc)
is.matrix(fc)
apply(fc,2,sum,na.rm=T)
colSums(fc,na.rm=T)
ts=sum(colSums(fc,na.rm=T));ts
fd=colSums(fc,na.rm=T)/ts;fd
9891+1208+245+855+1378=13577
of=c(9891/13577, 1208/13577, 245/13577, 855/13577, 1378/13577);of
14184-13577

## frequency distribution
u=0.69733503; h=0.08516638; f=0.01727298; b=0.06027919; d=0.09715172

## subset statistics
dc=data.frame(dmc,u,h,f,d,b)
uc=subset(dc,u==1); describe(uc)
hc=subset(dc,h==1); describe(hc)
fc=subset(dc,f==1); describe(fc)
bc=subset(dc,b==1); describe(bc)
dc=subset(dc,d==1); describe(dc)
View(uc)


## creat a series for markov chain estimation 756 obs
mc=read.csv('dct.csv')
mc$ds[(tm-ts>0.1 & tm<=tl) | (tl-tm>0.1 & tm>=ts)]="U"
mc$ds[(tm-ts>0.1 & tm>tl) | (tm-tl>0.1 & tm>ts)]="H"
mc$ds[abs(tm-ts)<=0.1 & abs(tl-tm)<=0.1]="F"
mc$ds[(ts-tm>0.1 & tl>tm) | (tl-tm>0.1 & ts>tm)]="B"
mc$ds[(ts-tm>0.1 & tm>=tl) | (tm-tl>0.1 & ts>=tm)]="D"
## verify whether ms is full vector in categories
str(mc$ds)
table(mc$ds)
write.csv(mc, file = "dmc.csv")

############################################
#### Markov chain fit

require(markovchain)
ms=read.csv("dmc.csv")
seq=ms$ds  ## 607missing obs 1962.1.2 to 2016.5.13
head(seq)
str(seq)
table(seq)
seq=na.omit(seq)
createSequenceMatrix(stringchar=seq)
# seqm=createSequenceMatrix(seq,sanitize=FALSE);View(seqm)

## MLE
mcmle=markovchainFit(data=seq,method="mle",name="mce");mcmle$estimate
mcmle$standardError
mcmle$logLikelihood
mcmle$confidenceInterval
tpm=mcmle$estimate;str(tpm)
rowSums(tpm)
print(mcmle) # all classes
show(mcmle)  # all classes
mcmle=as(tpm, "markovchain")
plot(mcmle)

## MLE Laplace
mcl=markovchainFit(seq, method="laplace",laplacian=1);mcl$estimate
# increasing alpha will downweight the likelihood of a high transistion probalitity, 
# upweight the likelihood of low transition probability
mcl$standardError
mcl$confidenceInterval
mcl=as(mcl$estimate, "markovchain")
plot(mcl)

## Bootstrap
mcbs=markovchainFit(seq,method="bootstrap",nboot=1000, name="Bootstrap Mc")
mcbs$estimate
mcbs$standardError
$confidenceInterval
mcbs=as(mcbs$estimate, "markovchain")
plot(mcbs)

## Bayesian 
mcba=markovchainFit(seq, method="map",confidencelevel=0.95) # hyperparam default unform piror
mcba$estimate
mcba$standardError
predictiveDistribution(seq[])

## Bayesian
createSequenceMatrix(stringchar=seq)
inferHyperparam(data=seq)
hm=inferHyperparam(data=seq)$dataInference
mcba1=markovchainFit(seq, method="map",hyperparam=hm,confidencelevel=0.95)
mcba1$estimate
mcba1$standardError

priorDistribution()

verifyMarkovProperty(seq)
assessOrder(seq)
assessStationarity(seq,1)
divergenceTest(seq,mcmle$estimate@mcl$estimate)


## set up the mc mannually
ycmc=new("markovchain",states=c("b","d","f","h","u"),
         transitionMatrix=matrix(c(30/40,2/40,1/40,0,7/40,  2/76,63/76,1/76,9/76,1/76,  3/16,1/16,8/16,1/16,3/16,  
                                   1/78,10/78,2/78,54/78,11/78,  4/545,0,4/545,14/545,523/545), byrow=T,nrow=5))
states(ycmc)
show(ycmc)
conditionalDistribution(ycmc,"u")
steadyStates(ycmc)  # very close to empirical frequency of the sample
absorbingStates(ycmc)
transientStates(ycmc)
canonicForm(ycmc)
communicatingClasses(ycmc)
recurrentClasses(ycmc)
period(ycmc)
is.irreducible(ycmc)
is.accessible(object=ycmc,from="h", to ="b") 
# u to all; f to all; b to all; d to all; h to all.
plot(ycmc)
plot(ycmc,package="diagram")


## Prediction
predict(object=mcmle$estimate,newdata=c("u","f"),n.ahead=1000)

t0=c(0.72,0.10,0.02,0.05,)

t0=c(1,0,0,0,0)
tpm=matrix(c(0.933333333, 0.01286550, 0.012865497, 0.000000000, 0.04093567,
             0.009433962, 0.96226415, 0.007256894, 0.021044993, 0.00000000,
             0.040816327, 0.03265306, 0.804081633, 0.069387755, 0.05306122,
             0.000000000, 0.02731788, 0.010761589, 0.937913907, 0.02400662,
             0.003437816, 0.00000000, 0.001415571, 0.002932255, 0.99221436),byrow=T,nrow=5); rowSums(tpm)
colSums(tpm)

tpm=mcmle$estimate;tpm
str(tpm)
tpm^5
ts=c(6.30, 10.15, 1.80, 8.90, 72.85)

## business day to month
## weekly
(B D F H U)
t0=c(0,0,0,1,0)
t1w=t0*tpm^5; round(t1w,4)            # 1 w = 5 business days
t2w=t0*tpm^10;round(t2w,4)            # 2 w = 10 bds
t1m=t0*tpm^20;round(t1m,4)            # 1 m = 20 bds
t2m=t0*tpm^40;round(t2m,4)            # 2 m = 40 bds
t6m=t0*tpm^120;round(t6m,4)           # 6 m = 120 bds
t1y=t0*tpm^240;round(t1y,4)           # 1yr=240 bds
t2y=t0*tpm^480;round(t2y,4)          # 2yr=480 bds
t2h=t0*tpm^600;t2h                    # 2.5y=600 bds






