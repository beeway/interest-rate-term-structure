

# 5k cv for 1st MC

my=read.csv("mms.csv")
seq=my$seq

# training set; validation set each 151 leave out
td1=seq[-1:-151]; td2=seq[-152:-302]; td3=seq[-303:-453];td4=seq[-454:-604]; td5=seq[-605:-755]
length(td1) # 605

# fit MC with td
require(markovchain)
mc1=markovchainFit(data=td1,method="mle");mc1$estimate
mc2=markovchainFit(data=td2,method="mle");mc2$estimate
mc3=markovchainFit(data=td3,method="mle");mc3$estimate
mc4=markovchainFit(data=td4,method="mle");mc4$estimate
mc5=markovchainFit(data=td5,method="mle");mc5$estimate


# transform the TPM to a matrix 
seqm1=createSequenceMatrix(td1,sanitize=FALSE); rowSums(seqm1)
x1=c(39,68,11,53,433); sum(x1) # 755-151=604 row sum of transition counts
rep.col=function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}
rs1=rep.col(x1,5) ;rs1   # create the matrix of row sums
tm1=seqm1*(1/rs1);tm1   # create the transition matrix for prediction
B          D           F          H          U
B 0.769230769 0.05128205 0.025641026 0.00000000 0.15384615
D 0.029411765 0.83823529 0.014705882 0.10294118 0.01470588
F 0.272727273 0.09090909 0.454545455 0.00000000 0.18181818
H 0.000000000 0.15094340 0.018867925 0.67924528 0.15094340
U 0.009237875 0.00000000 0.004618938 0.02309469 0.96304850

seqm2=createSequenceMatrix(td2,sanitize=FALSE);rowSums(seqm2)
x2=c(22,41,14,48,479); sum(x2) # 755-151=604  row sum of transition counts
rs2=rep.col(x2,5) ;rs2   # create the matrix of row sums
tm2=seqm2*(1/rs2);tm2   # create the transition matrix for prediction
B          D           F          H          U
B 0.636363636 0.04545455 0.045454545 0.00000000 0.27272727
D 0.024390244 0.85365854 0.000000000 0.09756098 0.02439024
F 0.214285714 0.00000000 0.500000000 0.07142857 0.21428571
H 0.020833333 0.10416667 0.041666667 0.66666667 0.16666667
U 0.006263048 0.00000000 0.008350731 0.02296451 0.96242171

seqm3=createSequenceMatrix(td3,sanitize=FALSE);rowSums(seqm3)
x3=c(36,45,9,61,453) ; sum(x3) # 755-151=604 row sum of transition counts
rs3=rep.col(x3,5) ;rs3   # create the matrix of row sums
tm3=seqm3*(1/rs3);tm3   # create the transition matrix for prediction
B          D           F          H          U
B 0.805555556 0.02777778 0.000000000 0.00000000 0.16666667
D 0.044444444 0.77777778 0.022222222 0.15555556 0.00000000
F 0.000000000 0.11111111 0.444444444 0.11111111 0.33333333
H 0.016393443 0.13114754 0.016393443 0.73770492 0.09836066
U 0.008830022 0.00000000 0.006622517 0.01766004 0.96688742

seqm4=createSequenceMatrix(td4,sanitize=FALSE);rowSums(seqm4)
x4=c(35,74,16,72,407); sum(x4) # 755-151=604 row sum of transition counts
rs4=rep.col(x4,5) ;rs4   # create the matrix of row sums
tm4=seqm4*(1/rs4);tm4   # create the transition matrix for prediction
B          D          F          H          U
B 0.771428571 0.05714286 0.02857143 0.00000000 0.14285714
D 0.013513514 0.83783784 0.01351351 0.12162162 0.01351351
F 0.187500000 0.06250000 0.50000000 0.06250000 0.18750000
H 0.013888889 0.12500000 0.02777778 0.68055556 0.15277778
U 0.007371007 0.00000000 0.00982801 0.03194103 0.95085995

seqm5=createSequenceMatrix(td5,sanitize=FALSE);rowSums(seqm5)
x5=c(28,76,14,78,408); sum(x5) # 755-151=604 row sum of transition counts
rs5=rep.col(x5,5); rs5   # create the matrix of row sums
tm5=seqm5*(1/rs5);tm5   # create the transition matrix for prediction
B          D           F          H          U
B 0.714285714 0.07142857 0.035714286 0.00000000 0.17857143
D 0.026315789 0.82894737 0.013157895 0.11842105 0.01315789
F 0.214285714 0.07142857 0.500000000 0.07142857 0.14285714
H 0.012820513 0.12820513 0.025641026 0.69230769 0.14102564
U 0.004901961 0.00000000 0.007352941 0.03431373 0.95343137


## vd matrix
attach(my)
vm=data.frame(b,d,f,h,u) # 
vm=as.matrix(vm)     # convert to matrix for calculation of prediction error

# td1=seq[-1:-151]; td2=seq[-152:-302]; td3=seq[-303:-453];td4=seq[-454:-604]; td5=seq[-605:-755]
# predicted matrix using pi(t+1)=pi(t)*TPM with test data set
pred1=vm[1:151,]%*%tm1;      # t predicts states for t+1 2:152
View(pred1[40:50,])          # maxprobrow=0.45 in 46th row

pred2=vm[152:302,]%*%tm2    # predicts states for t+1
View(pred[90:100,])         # minmaxprobrow=0.50 in 16th row

pred3=vm[303:453,]%*%tm3    # predicts states for 304:454
View(pred[:,])              # minmaxprobrow=0.44 in 3r

pred4=vm[454:604,]%*%tm4    # predicts states for 455:605
View(pred[,])              # minmaxprobrow=0.68 in 110r

pred5=vm[605:755,]%*%tm5    # predicts states for 606:756
View(pred[:,])              # minmaxprobrow=0.50 in 30r


# convert it to 0/1 predictions, the success or failure indicator
# max prob (row) = 1, otherwise 0. for predm(i,j)>=maxprobrow, predm(i,j)=1; otherwise=0. 
preda1=matrix(0,151,5)  
for (r in 1:151) {
  for (c in 1:5) {
    if (pred1[r,c]>=0.45) {preda1[r,c]=1}
    else {preda1[r,c]=0} } }
# rs2=subset(preda, rowSums(preda)==2) 

preda2=matrix(0,151,5)  
for (r in 1:151) {
  for (c in 1:5) {
    if (pred2[r,c]>=0.50) {preda2[r,c]=1}
    else {preda2[r,c]=0} } }

preda3=matrix(0,151,5)  
for (r in 1:151) {
  for (c in 1:5) {
    if (pred3[r,c]>=0.44) {preda3[r,c]=1}
    else {preda3[r,c]=0} } }

preda4=matrix(0,151,5)  
for (r in 1:151) {
  for (c in 1:5) {
    if (pred4[r,c]>=0.68) {preda4[r,c]=1}
    else {preda4[r,c]=0} } }

preda5=matrix(0,151,5)  
for (r in 1:151) {
  for (c in 1:5) {
    if (pred5[r,c]>=0.50) {preda5[r,c]=1}
    else {preda5[r,c]=0} } }


# compute the prediction error rate
# td1=seq[-1:-151]; td2=seq[-152:-302]; td3=seq[-303:-453];td4=seq[-454:-604]; td5=seq[-605:-755]
ce1=vm[2:152,]-preda1  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce1)    #  ones; 1 and -1 indicate an error
er1=18/151;er1 # prediction error rate .119205  # apply(ce,2,sum)

ce2=vm[153:303,]-preda2  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce2)    #  ones; 1 and -1 indicate an error
er2=23/151;er2 # prediction error rate .1523179   # apply(ce,2,sum)

ce3=vm[304:454,]-preda3  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce3)     #  24 ones; 1 and -1 indicate an error
er3=24/151;er3 # prediction error rate   # apply(ce,2,sum)

ce4=vm[455:605,]-preda4  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce4)    #  6 ones; 1 and -1 indicate an error
er4=6/151;er4 # prediction error rate   # apply(ce,2,sum)

ce5=vm[606:756,]-preda5  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce5)    #  6 ones; 1 and -1 indicate an error
er5=6/151;er5 # prediction error rate .039735  # apply(ce,2,sum)

# calculate the average prediction error rate
aer=(er1+er2+er3+er4+er5)/5;aer   #  0.1019868



