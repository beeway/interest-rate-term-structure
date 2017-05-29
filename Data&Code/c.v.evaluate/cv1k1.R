
setwd("/Users/biweichen/Desktop/Interest Rates/Mypapers/Markov/evaluate")

# forecast evaluation all sample k=0
# retrieve the transition matrix
my=read.csv("mms.csv")
sq=my$seq
head(seq)
str(seq)
table(seq)

# creating counting dummy for each type - exhausting 756
attach(my)
u=ifelse((tm-ts>0.1 & tm<=tl) | (tl-tm>0.1 & tm>=ts), 1, 0)     # upward    546
h=ifelse((tm-ts>0.1 & tm>tl) | (tm-tl>0.1 & tm>ts), 1, 0)       # hump      78
f=ifelse(    abs(tm-ts)<=0.1 & abs(tl-tm)<=0.1, 1,0)            # flat      16  
b=ifelse((ts-tm>0.1 & tl>tm) | (tl-tm>0.1 & ts>tm), 1, 0)       # bowl      40  
d=ifelse((ts-tm>0.1 & tm>=tl) | (tm-tl>0.1 & ts>=tm), 1, 0)     # downward  76      
 
# mms1=data.frame(seq,b,d,f,h,u)
# write.csv(mms1,file="mms1.csv")

# 
attach(my);od=as.matrix(data.frame(b,d,f,h,u))           # convert to matrix for calculation of prediction error


require(markovchain)
mce=markovchainFit(data=seq,method="mle",name="mce");mce$estimate
is.matrix(mce$transitionMatrix)

attributes(mce$estimate)
tpm=mce$estimate;str(tpm)
tpm
tpm^2
# predm=od%*%tpm  # not multiplicable due to tpm non-standard 
# tpm=as.matrix(tpm) # does not work

is.matrix(tpm)
od=as.matrix(od)
cols=colSums(od);cols

# input the estimated tpm mannually 
table(seq)
createSequenceMatrix(stringchar=seq)
seqm=createSequenceMatrix(seq,sanitize=FALSE);
View(seqm);
is.matrix(seqm)
tm=matrix(c(30/40,2/40,1/40,0,7/40,  2/76,63/76,1/76,9/76,1/76,  3/16,1/16,8/16,1/16,3/16,  
            1/78,10/78,2/78,54/78,11/78,  4/545,0,4/545,14/545,523/545), byrow=T,nrow=5)
predm=od%*%tm   # need od and tm both numeric matrix
table(predm)
dim(predm)
predm[1,5]

# another way of convert the TPM
seqm=as.matrix(seqm); rowSums(seqm);
x=c(40,76,16,78,545) # row sum of transition counts
rep.col=function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}
rs=rep.col(x,5) ;rs   # create the matrix of row sums
tm=seqm*(1/rs);tm   # create the transition matrix for prediction
predm1=od%*%tm   # need od and tm both numeric matrix to get the predicted states in test set


# for predm(i,j)>=0.5, predm(i,j)=1. otherwise=0. transform to the success or failure indicator
predma=matrix(0,755,5)
for (r in 1:755)  {
   for (c in 1:5) {
     if (predm1[r,c]==max(predm1[r,])) {predma[r,c]=1}
      else {predma[r,c]=0} } }
# since all one-step prediction for each state with different initial distribution greater than 0.5

# FOMC prediction error rate all sample mapping the prediction timing
# if transition probability fixed in each loocv, then they are equivalent
od1=od[2:756,]
predm1=predma[1:755,]
ce=od1-predma   # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce)
er=77/755;er    # prediction error rate 0.1019868
apply(ce,2,sum) 

