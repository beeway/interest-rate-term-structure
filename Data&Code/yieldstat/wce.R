
setwd("/Users/beeway/Desktop/Interest Rates/Mypapers/Markov")
# setwd("/Users/beeway/Documents/Academic Software/R")

wct=read.csv('wct.csv') 
library(psych);describe(wct)
str(wct)
head(wct)
class(wct)

# handle ND value for t30y
wct$t30y[wct$t30y=="ND"]=NA;wct$t30y=as.numeric(as.character(wct$t30y))
str(wct)
describe(wct)
write.csv(wct,file="wcm.csv")

############################################
## tl as (10y+20y+30y)/3
############################################

wmc=read.csv('wcm.csv')
library(psych);describe(wmc)

# creating counting dummy for each type - exhausting
attach(wmc)
u=ifelse((tm-ts>0.1 & tm<=tl) | (tl-tm>0.1 & tm>=ts), 1, 0)     # upward    2069
h=ifelse((tm-ts>0.1 & tm>tl) | (tm-tl>0.1 & tm>ts), 1, 0)       # hump      254
f=ifelse(    abs(tm-ts)<=0.1 & abs(tl-tm)<=0.1, 1,0)            # flat      51 
b=ifelse((ts-tm>0.1 & tl>tm) | (tl-tm>0.1 & ts>tm), 1, 0)       # bowl      174 
d=ifelse((ts-tm>0.1 & tm>=tl) | (tm-tl>0.1 & ts>=tm), 1, 0)     # downward  289    # total=2837

## inital distribution
u=0.72929151; h=0.08953119; f=0.01797674; b=0.06133239; d=0.10186817;

## calculating the number of ocurrence for each type and frequency
fc=cbind(u,h,f,b,d)
apply(fc,2,sum,na.rm=T)
ts=sum(colSums(fc));ts
fd=colSums(fc)/ts;fd

## subset statistics
wc=data.frame(wmc,u,h,f,d,b)
uwc=subset(wc,u==1); describe(uwc)
hwc=subset(wc,h==1); describe(hwc)
fwc=subset(wc,f==1); describe(fwc)
bwc=subset(wc,b==1); describe(bwc)
dwc=subset(wc,d==1); describe(dwc)
View(umc)


## creat a series for markov chain estimation 756 obs
wmc=read.csv('wcm.csv')
wmc$ws[(tm-ts>0.1 & tm<=tl) | (tl-tm>0.1 & tm>=ts)]="U"
wmc$ws[(tm-ts>0.1 & tm>tl) | (tm-tl>0.1 & tm>ts)]="H"
wmc$ws[abs(tm-ts)<=0.1 & abs(tl-tm)<=0.1]="F"
wmc$ws[(ts-tm>0.1 & tl>tm) | (tl-tm>0.1 & ts>tm)]="B"
wmc$ws[(ts-tm>0.1 & tm>=tl) | (tm-tl>0.1 & ts>=tm)]="D"
## verify whether ms is full vector in categories
str(wmc$ws)
table(wmc$ws)
write.csv(wmc, file = "wmc.csv")

############################################
#### Markov chain fit

require(markovchain)
wms=read.csv("wmc.csv")
seq=wms$ws
head(seq)
str(seq)
table(seq)

createSequenceMatrix(stringchar=seq)
# seqm=createSequenceMatrix(seq,sanitize=FALSE);View(seqm)

## MLE
mcmle=markovchainFit(data=seq,method="mle",name="mce");mcmle$estimate
mcmle$standardError
mcmle$logLikelihood
mcmle$confidenceInterval
tpm=mcmle$estimate;str(tpm)
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

       B         D          F          H         U
B 0.06135402 0.1019041 0.01798307 0.08956276 0.7291961
tpm=mcmle$estimate; tpm^500

(B D F H U)
t0=c(0,1,0,0,0)
t1=t0*tpm^4; round(t1,4)          # 1 month
t2=t0*tpm^8; round(t2,4)          # 2 month
t3=t0*tpm^13;round(t3,4)           # 3 month
t6=t0*tpm^26;round(t6,4)           # 6 month
t12=t0*tpm^52;round(t12,4)         # 1yr
t24=t0*tpm^104;round(t24,4)        # 2yr
t36=t0*tpm^260;round(t36,4)        # 5yr

t0=c();steps=12;
ts=t0*tpm^steps;ts





