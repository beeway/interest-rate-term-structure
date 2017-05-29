

# cv for 1st MC

my=read.csv("mms.csv")
seq=my$seq

# training set and validation set each 378
td=seq[1:378]
vd=seq[379:756]

# fit MC with td
require(markovchain)
mce=markovchainFit(data=td,method="mle");mce$estimate
is.matrix(mce$estimate)

# transform the TPM to a matrix 
seqm=createSequenceMatrix(td,sanitize=FALSE);
View(seqm)
rowSums(seqm)
x=c(21,74,8,64,210) # row sum of transition counts
rep.col=function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}
rs=rep.col(x,5) ;rs   # create the matrix of row sums
tm=seqm*(1/rs);tm   # create the transition matrix for prediction
is.matrix(tm)

# vd matrix
attach(my)
vm=data.frame(b,d,f,h,u) # [379:756,]
vm=as.matrix(vm)     # convert to matrix for calculation of prediction error

# predicted matrix using pi(t+1)=pi(t)*TPM with test data set
pred=vm[378:755,]%*%tm    # predicts states for 379 to 756
View(pred[50:60,])        # 57, 58 have 0.375, maxprobrow=0.375

# convert it to 0/1 predictions, the success or failure indicator
# max prob (row) = 1, otherwise 0. for predm(i,j)>=maxprobrow, predm(i,j)=1; otherwise=0. 
preda=matrix(0,378,5)
for (r in 1:378) {
  for (c in 1:5) {
    if (pred[r,c]==max(pred[r,])) {preda[r,c]=1}
    else {preda[r,c]=0} } }

# compute the prediction error rate
ce=vm[379:756,]-preda  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce)    # 21 ones; 1 and -1 indicate an error
er=21/378;er # prediction error rate 0.055556  # apply(ce,2,sum)


###############################################################################

# training set and validation set each 378
td=seq[379:756]

# fit MC with td
require(markovchain)
mce=markovchainFit(data=td,method="mle");mce$estimate

# transform the TPM to a matrix 
seqm=createSequenceMatrix(td,sanitize=FALSE);
View(seqm)
rowSums(seqm)
x=c(19,2,8,13,335) # row sum of transition counts
rep.col=function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}
rs=rep.col(x,5) ;rs   # create the matrix of row sums
tm=seqm*(1/rs);tm   # create the transition matrix for prediction

# vd matrix
attach(my)
vm=data.frame(b,d,f,h,u) # [2:379,]
vm=as.matrix(vm)     # convert to matrix for calculation of prediction error

# predicted matrix using pi(t+1)=pi(t)*TPM with test data set
pred=vm[1:378,]%*%tm      # predicts states for 2 to 379
View(pred[50:60,])        # 53, 54 have 0.5, maxprobrow=0.5

# convert it to 0/1 predictions, the success or failure indicator
# max prob (row) = 1, otherwise 0. for predm(i,j)>=maxprobrow, predm(i,j)=1; otherwise=0. 
preda=matrix(0,378,5)  
for (r in 1:378) {
  for (c in 1:5) {
    if (pred[r,c]==max(pred[r,])) {preda[r,c]=1}
    else {preda[r,c]=0} } }
rs2=subset(preda, rowSums(preda)==2) # 74 obs

# compute the prediction error rate
ce=vm[2:379,]-preda  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce)    # 21 ones; 1 and -1 indicate an error
ce1=subset(ce, rowSums(ce)==0)   # 304
ce2=subset(ce, rowSums(ce)==-1)  # 74
ce3=subset(ce, rowSums(ce)==1)   # 0 
# 304+74=378
er=74/378;er # prediction error rate 0.1957  # apply(ce,2,sum)

########### average prediction error rate over two fold
(.0555556+.19577)/2    #0.12566228
