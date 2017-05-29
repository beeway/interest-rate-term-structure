

## 2nd order MC cv k=0 all sample

my=read.csv("mms.csv");seq=my$seq

require(markovchain)
install.packages("Rsolnp")
require(Rsolnp)


# training set and validation set each 378
td1=seq[1:378]; td2=seq[379:756]
mc1=fitHigherOrder(td1, order = 2);mc1  # train the first fold

# convert to P matrix 
P1=matrix(c(
  0.8095238, 0.01351351, 0.125, 0.015625, 0.004761905,
  0.0952381, 0.83783784, 0.125, 0.140625, 0.000000000,
  0.0000000, 0.01351351, 0.375, 0.015625, 0.014285714,
  0.0000000, 0.12162162, 0.125, 0.687500, 0.052380952,
  0.0952381, 0.01351351, 0.250, 0.140625, 0.928571429), byrow=F, nrow=5); rowSums(P1)
P2=matrix(c(
  0.6666667, 0.02702703, 0.125, 0.015625, 0.014354067,
  0.1904762, 0.75675676, 0.250, 0.156250, 0.009569378,
  0.0000000, 0.01351351, 0.125, 0.031250, 0.019138756,
  0.0000000, 0.14864865, 0.250, 0.578125, 0.071770335,
  0.1428571, 0.05405405, 0.250, 0.218750, 0.885167464), byrow=F, nrow=5); rowSums(P2)

## validation matrix 
attach(my);vm=as.matrix(data.frame(b,d,f,h,u));   # validation matrix for 2nd MC prediction 

# test data set td2=seq[379:756] 378 obs;
# predicted matrix using pi(t+2)=lamda1*pi(t+1)*P1+lamda2*pi(t)*P2 with test data set
pred1=0.5*vm[378:755,]%*%P1+0.5*vm[377:754,]%*%P2    # 2nd MC predicts states 381:756 with 376 obs
rowSums(pred1[1:100,])      # check row sum is one

# convert it to 0/1 predictions, the success or failure indicator
# max prob (row) = 1, otherwise 0. for predm(i,j)>=maxprobrow, predm(i,j)=1; otherwise=0. 
preda1=matrix(0,378,5)
for (r in 1:378) {
  for (c in 1:5) {
    if (pred1[r,c]==max(pred1[r,])) {preda1[r,c]=1}
    else {preda1[r,c]=0} } }
rs=subset(preda1,rowSums(preda1)==2);rs

# compute the prediction error rate
ce1=vm[379:756,]-preda1  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce1)    #  30 ones and 25 negative -1 indicate errors
er1=30/378;er1 # prediction error rate .07978723  # apply(ce,2,sum)


###############################################################################################################
### train the second fold and predict the first fold vm[3:378]
td2=seq[379:756]; mc2=fitHigherOrder(td2, order = 2);mc2  # train the second fold

# convert to P matrix 
P3=matrix(c(
  0.68421053, 0.5, 0.250, 0.00000000, 0.008955224,
  0.00000000, 0.5, 0.000, 0.07692308, 0.000000000,
  0.05263158, 0.0, 0.625, 0.07692308, 0.002985075,
  0.00000000, 0.0, 0.000, 0.76923077, 0.008955224,
  0.26315789, 0.0, 0.125, 0.07692308, 0.979104478), byrow=F, nrow=5); rowSums(P3)
  
P4=matrix(c(0.63157895, 1, 0.250, 0.0000000, 0.008982036,
            0.00000000, 0, 0.000, 0.1538462, 0.000000000,
            0.05263158, 0, 0.375, 0.1538462, 0.005988024,
            0.00000000, 0, 0.000, 0.5384615, 0.017964072,
            0.31578947, 0, 0.375, 0.1538462, 0.967065868), byrow=F, nrow=5); rowSums(P4)

## validation matrix 
attach(my);vm=as.matrix(data.frame(b,d,f,h,u));   # for 2nd MC prediction 

# predicted matrix using pi(t+1)=pi(t)*TPM with test data set seq[3:380];
pred2=0.5*vm[2:379,]%*%P3+0.5*vm[1:378,]%*%P4    # predicts states 3:378 with 376 obs
rowSums(pred2[1:100,])      # check row sum is one

# convert it to 0/1 predictions, the success or failure indicator
# max prob (row) = 1, otherwise 0. for predm(i,j)>=maxprobrow, predm(i,j)=1; otherwise=0. 
preda2=matrix(0,378,5)
for (r in 1:378) {
  for (c in 1:5) {
    if (pred2[r,c]==max(pred2[r,])) {preda2[r,c]=1}
    else {preda2[r,c]=0} } }
rs2=subset(preda2,rowSums(preda2)==2);rs2

# compute the prediction error rate
ce2=vm[3:380,]-preda2  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce2)    # ones and  negative -1 indicate errors
er2=120/378;er2 # prediction error rate 0.1755319  # apply(ce,2,sum)


## average prediction error rate
(er1+er2)/2    # 0.1984127


  