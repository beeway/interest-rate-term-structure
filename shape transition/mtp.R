
getwd()
setwd("/Users/biweichen/Desktop/Interest Rates/Mypapers/Markov")

install.packages("markovchain")
require(markovchain); library(ggplot2); library(readxl)
dm=read_excel("1945.xlsx")

dm=read.csv("1945.csv")
View(dm[903,])

seq=dm$ms
head(seq)
str(seq)
table(seq)

createSequenceMatrix(stringchar=seq)   # horizontal transistion 
seqm=createSequenceMatrix(seq,sanitize=FALSE);View(seqm)

## MLE
mcmle=markovchainFit(data=seq,method="mle",name="mce",byrow=T);
mcmle$estimate
mcmle$standardError
mcmle$logLikelihood
mcmle$confidenceInterval
print(mcmle) # all classes
show(mcmle)  # all classes

tpm=mcmle$estimate # str(tpm)
mctp=as(tpm, "markovchain")
plot(mctp, order=1, vertex.size=30 ,edge.arrow.size=0.4, vertex.color="pink")
# "darkseagreen" "lightblue1" "deepskyblue1"
# plot parameters https://igraph.org/r/doc/plot.common.html
# https://www.rdocumentation.org/packages/clickstream/versions/1.3.1/topics/plot%2CMarkovChain-method

## Visualize next period state probabilities
# https://www.datacamp.com/community/tutorials/markov-chain-analysis-r
#
## Prediction
predict(object=mcmle$estimate,newdata=c("u","f"), n.ahead=1000)

table(dm$ms)/903
tpm
tpm^10
tpm^50
tpm^100
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
steadyStates(ycmc)*tpm

# Visualize state probability evolution
# https://www.datacamp.com/community/tutorials/markov-chain-analysis-r

t0=c(1,0,0,0,0); B=c(); D=c(); F=c();H=c(); U=c();

for(k in 1:50) { 
  nstep=t0*tpm^(k-1)
  B[k]=nstep[1,1]
  D[k]=nstep[1,2]
  F[k]=nstep[1,3]
  H[k]=nstep[1,4]
  U[k]=nstep[1,5]
}

x=c(1:50); # steps=data.frame(B,D,F,H,U)
ggplot(steps, aes(x))+geom_line(aes(y=B, col="B"))+geom_point(aes(y=B, col="B"),size=1)+
  geom_line(aes(y=D, col="D")) + geom_point(aes(y=D, col="D"), size=1) +
  geom_line(aes(y=F, col="F")) + geom_point(aes(y=F, col="F"), size=1) +
  geom_line(aes(y=H, col="H")) + geom_point(aes(y=H, col="H"), size=1) +
  geom_line(aes(y=U, col="U")) + geom_point(aes(y=U, col="U"), size=1) +
  labs(y="", x="") +theme_bw() + 
  scale_y_continuous(breaks=seq(0,1, by=0.2), sec.axis=dup_axis()) +
  theme(legend.position=c(0.85,0.5), legend.title=element_blank(), 
        legend.text = element_text(color="black", size =10))
  
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/


#### higher order MC
install.packages("Rsolnp");require(Rsolnp)
homc=fitHigherOrder(seq, order = 3); homc # byrow=F
# higher order chains nest lower order tp

tpo1=as(homc$Q[[1]], "markovchain")
tpo2=as(homc$Q[[2]], "markovchain")
tpo3=as(homc$Q[[3]], "markovchain")

par(mfrow=c(1,3), mar=c(1,4,1,4)) # mar=c(t, r, b, l)
plot(tpo1, vertex.size=20, edge.arrow.size=0.2, vertex.color="pink")
plot(tpo2, vertex.size=20, edge.arrow.size=0.2, vertex.color="pink")
plot(tpo3, vertex.size=20, edge.arrow.size=0.2, vertex.color="pink")

# # One figure in row 1 and two figures in row 2
# layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

eq2freqProb(seq)
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




