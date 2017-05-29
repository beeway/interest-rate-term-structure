
getwd()
setwd("/Users/biweichen/Desktop/Interest Rates/Mypapers/Markov")

mc=read.csv("mc.csv")  # original yield data
library(psych);describe(mc)
round(describe(mc),4)
str(mc)
attach(mc)
detach(mc)

########## Two sample t-test unknow population variance #######################################
http://www.r-bloggers.com/two-sample-students-t-test-1/
# Fisher’s F-test to verify the homoskedasticity (homogeneity of variances).
var.test(ts,tm)   # reject homo at a signifcance level of 0.01 
qf(0.95, 755,755) # reject homoske
t.test(ts,tm, var.equal=TRUE, paired=FALSE)  # homo; independence
t.test(ts,tm, var.equal=TRUE, paired=TRUE)   # homo; dependent sampling
t.test(ts,tm, var.equal=FALSE, paired=FALSE) # hetero; independent
t.test(ts,tm, var.equal=FALSE, paired=TRUE)  # hetero; dependent
qt(0.975,1499.637) # reject the null of equal population mean
cor(mc[,2:8])

### Nonparametric Tests of Group Differences
http://www.statmethods.net/stats/nonparametric.html
wilcox.test(ts,tm) # independent 2-group Mann-Whitney U Test
wilcox.test(ts,tm,paired=TRUE)  # dependent 2-group Wilcoxon Signed Rank Test 
##################################################################################################

# Fisher’s F-test to verify the homoskedasticity (homogeneity of variances).
var.test(tm,tl)   # reject homo at a signifcance level of 0.01 
qf(0.95, 755,755) # reject homoske
t.test(tm,tl, var.equal=TRUE, paired=FALSE) # homo; independence
t.test(tl,tm, var.equal=TRUE, paired=TRUE)  # homo; dependent sampling
t.test(tl,tm, var.equal=FALSE, paired=FALSE) # hetero; independent
t.test(tl,tm, var.equal=FALSE, paired=TRUE)  # hetero; dependent
qt(0.975,1499.637) # reject the null of equal population mean
cor(mc[,2:8])
### Nonparametric Tests of Group Differences
http://www.statmethods.net/stats/nonparametric.html
wilcox.test(tl,tm) # independent 2-group Mann-Whitney U Test
wilcox.test(tl,tm,paired=TRUE)  # dependent 2-group Wilcoxon Signed Rank Test 
##################################################################################################

# Fisher’s F-test to verify the homoskedasticity (homogeneity of variances).
var.test(ts,tl)   # reject homo at a signifcance level of 0.01 
qf(0.95, 755,755) # reject homoske
t.test(ts,tl, var.equal=TRUE, paired=FALSE)  # homo; independence
t.test(ts,tl, var.equal=TRUE, paired=TRUE)   # homo; dependent sampling
t.test(ts,tl, var.equal=FALSE, paired=FALSE) # hetero; independent
t.test(ts,tl, var.equal=FALSE, paired=TRUE)  # homo; dependent
qt(0.975,1499.637) # reject the null of equal population mean
cor(mc[,2:8])
### Nonparametric Tests of Group Differences
http://www.statmethods.net/stats/nonparametric.html
wilcox.test(ts,tl) # independent 2-group Mann-Whitney U Test
wilcox.test(ts,tl,paired=TRUE)  # dependent 2-group Wilcoxon Signed Rank Test 
##################################################################################################

## mutually exclusive and exhaustive 
## creat a series for markov chain estimation 756 obs
mc=read.csv("mmc.csv")
attach(mc)
mc$ms[(tm-ts>0.1 & tm<=tl) | (tl-tm>0.1 & tm>=ts)]="U"
mc$ms[(tm-ts>0.1 & tm>tl) | (tm-tl>0.1 & tm>ts)]="H"
mc$ms[abs(tm-ts)<=0.1 & abs(tl-tm)<=0.1]="F"
mc$ms[(ts-tm>0.1 & tl>tm) | (tl-tm>0.1 & ts>tm)]="B"
mc$ms[(ts-tm>0.1 & tm>=tl) | (tm-tl>0.1 & ts>=tm)]="D"

## verify whether ms is full vector in categories
str(mc$ms)
table(mc$ms)
sum(40+76+16+78+546)
write.csv(mc, file = "mcms.csv")


## if not case, fix it manually
sum(40+74+16+78+545)  # 753 < 756
View(date[ms=="NA"])  # need to assign manually
1973-08  8.8200	7.6100	7.610	NA  # d
1970-11  5.5100	6.5800	6.580	NA  # u
1970-02  7.5900	7.5900	6.670	NA  # d
mc$ms[date=="1973-08"]="d"
mc$ms[date=="1970-11"]="u"
mc$ms[date=="1970-02"]="d"
table(mc$ms)
sum(40+76+16+78+546)
View(date[ms=="NA"]) 


install.packages("markovchain")
require(markovchain)

mcms=read.csv("mms.csv")
seq=mcms$ms
head(seq)
str(seq)
table(seq)
  B   D   F   H   U 
 40  76  16  78 546

createSequenceMatrix(stringchar=seq)   # horizontal transistion 
seqm=createSequenceMatrix(seq,sanitize=FALSE);View(seqm)

## MLE
mcmle=markovchainFit(data=seq,method="mle",name="mce",byrow=T);mcmle$estimate
mcmle$standardError
mcmle$logLikelihood
mcmle$confidenceInterval
tpm=mcmle$estimate;
str(tpm)
print(mcmle) # all classes
show(mcmle)  # all classes
mcmle=as(tpm, "markovchain")
plot(mcmle)

## MLE Laplace
mcl=markovchainFit(seq, method="laplace",laplacian=1);mcl$estimate   # try 0.5; 1; 5
# increasing alpha will downweight the likelihood of a high transistion probalitity, 
# upweight the likelihood of low transition probability
mcl$standardError
mcl$confidenceInterval
mcl=as(mcl$estimate, "markovchain")
plot(mcl)

## Bootstrap
mcbs=markovchainFit(seq,method="bootstrap",nboot=1000); mcbs$estimate # try 10, 1000
mcbs$standardError
mcbs$confidenceInterval
mcbs=as(mcbs$estimate, "markovchain")
plot(mcbs)

## Bayesian 
mcba=markovchainFit(seq, method="map",confidencelevel=0.95) # hyperparam default unform piror
mcba$estimate
mcba$standardError  # larger than inferred from the data
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


#### higher order MC
install.packages("Rsolnp");require(Rsolnp)
homc=fitHigherOrder(seq, order = 3);homc # byrow=F
seq2freqProb(seq)

seq2matHigh(seq, order)

fitHighOrderMultivarMC(seq, order = 2, Norm = 2)


## set up the mc mannually
ycmc=new("markovchain",states=c("B","D","F","H","U"),
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


 tpm^150 # converge! regardless of initial distribution
t0=c(0,1,0,0,0)    # (B D F H U )
t1=t0*tpm; round(t1,4)
t2=t0*tpm^2; round(t2,4)
t3=t0*tpm^3; round(t3,4)
t6=t0*tpm^6; round(t6,4)
t12=t0*tpm^12; round(t12,4)
t24=t0*tpm^24; round(t24,4)
t60=t0*tpm^60; round(t60,4)
t72=t0*tpm^72; round(t72,4)

t0=c(0.0530,0.1007,0.0212,0.1033,0.7219);
class(t0)
t1=t0*tpm;t1
t3=t0*tpm^3;t3
steps=12;
ts=t0*tpm^steps;ts

## verification
steadyStates(ycmc)
tpm
tpm^120
steadyStates(ycmc)*tpm

# initial distribution monthly
u-0.7222; h-0.1032; f-0.0212 ; b-0.0529 ; d-0.1005


