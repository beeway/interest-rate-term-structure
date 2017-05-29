
## k=10 fold cv with sequential folds (not random sampling)
## the MC sequence might not be suitable for random sampling


my=read.csv("mms.csv")
seq=my$ms
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

# 10 fold test dataset for prediction error calculation
attach(my); od=data.frame(b,d,f,h,u)   # orignial matrix benchmark
od1=od[1:75,];od2=od[76:150,]; od3=od[151:225,];
od4=od[226:300,];od5=od[301:375,];od6=od[376:450,];
od7=od[451:525,];od8=od[526:600,];od9=od[601:675,];od10=od[676:756,]
od=list(od1, od2, od3)
od

lapply(od, markovchainFit(od,method="mle",name="mce"))

# for loop solution
nrow(od);k=10;incr=floor(nrow(od)/k);incr
# odtest=matrix(0,nrow(od),ncol(od))
odt=list()
for (i in 0:9) {odt=od[i*incr+1:(i+1)*incr,] }


# Leave one fold out, estimate the transition matrix
my=read.csv("mms.csv")
seq=my$ms
seq1=seq[-1:-75] #leave the first fold out for estimation
seq2=seq[-76:-150];seq3=seq[-151:-225];seq4=seq[-226:-300];
seq5=seq[-301:-375];seq6=seq[-376:-450];seq7=seq[-451:-525];
seq8=seq[-526:-600];seq9=seq[-601:-675];seq10=seq[-676:-756]


######################
table(seq1)
require(markovchain)
mce=markovchainFit(data=seq1,method="mle",name="mce"); mce$estimate
class(mce)
tpm=mcmle$estimate;str(tpm);tpm

mce=list()
  for (i in 1:10){
    mce[[i]]=markovchainFit(data=seqi, method="mle")
    return()
  }
pring

######################


seqm1=createSequenceMatrix(seq1,sanitize=FALSE);View(seqm1);
seqm1=as.matrix(seqm1); 
rowSums(seqm1);colSums(seqm1)
x=c(39,74,14,59,494)
rep.col=function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}
rs=rep.col(x,5)    # create the matrix of row sums
tm1=seqm1*(1/rs)   # create the transition matrix for prediction
predm1=od1%*%tm1   # need od and tm both numeric matrix to get the predicted states in test set

predma1=matrix(0,75,5)
for (r in 1:75) {
  for (c in 1:5) {
    if (predm1[r,c]>=0.50) {predma1[r,c]=1}
    else {predma1[r,c]=0} } }

# prediction error rate all sample mapping the prediction timing
# if transition probability fixed in each loocv, then they are equivalent
od1=od1[2:75,]
predm1=predma1[1:74,]
ce1=od1-predm1  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce1)
er=12/74;er # prediction error rate 0.1622
apply(ce,2,sum)


## K fold cross-validation
# divide the sequence into k fold, nonoverlapping
# leave one fold as the validation set, the rest k-1 as training set fitting the tpm
# calculate the prediction error for each hold out set, then average them to get the average
