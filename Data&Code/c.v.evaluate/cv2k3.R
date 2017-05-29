
## 3rd order MC cv k=2 

my=read.csv("mms.csv");seq=my$seq

require(markovchain)
install.packages("Rsolnp")
require(Rsolnp)

# training set and validation set each 378
td1=seq[1:378]; td2=seq[379:756]
mc1=fitHigherOrder(td1, order = 3);mc1  # train the first fold, test the second fold
mc2=fitHigherOrder(td2, order = 3);mc2

# retrieve the two matrces for mc1
$lambda 0.3333333 0.3333333 0.3333333
0.05555556 0.19576720 0.02116402 0.17195767 0.55555556 
                  B D F H U
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
 0.1428571, 0.05405405, 0.250, 0.218750, 0.885167464), byrow=F, nrow=5)

P3=matrix(c(
 0.6190476, 0.04054054, 0.000, 0.015625, 0.01923077,
 0.1904762, 0.68918919, 0.375, 0.203125, 0.01442308,
 0.0000000, 0.01351351, 0.000, 0.031250, 0.02403846,
 0.0000000, 0.16216216, 0.375, 0.500000, 0.08653846,
 0.1904762, 0.09459459, 0.250, 0.250000, 0.85576923), byrow=F, nrow=5)


# retrieve the two matrces for mc2
B           D           F           H           U 
0.050264550 0.005291005 0.021164021 0.034391534 0.888888889 
$lambda 0.3333333 0.3333333 0.3333333
       
                        B D F H U
P4=matrix(c(
 0.68421053, 0.5, 0.250, 0.00000000, 0.008955224,
 0.00000000, 0.5, 0.000, 0.07692308, 0.000000000,
 0.05263158, 0.0, 0.625, 0.07692308, 0.002985075,
 0.00000000, 0.0, 0.000, 0.76923077, 0.008955224,
 0.26315789, 0.0, 0.125, 0.07692308, 0.979104478), byrow=F, nrow=5)
P5=matrix(c(
 0.63157895, 1, 0.250, 0.0000000, 0.008982036,
 0.00000000, 0, 0.000, 0.1538462, 0.000000000,
 0.05263158, 0, 0.375, 0.1538462, 0.005988024,
 0.00000000, 0, 0.000, 0.5384615, 0.017964072,
 0.31578947, 0, 0.375, 0.1538462, 0.967065868), byrow=F, nrow=5)
P6=matrix(c(
 0.52631579, 1, 0.125, 0.1538462, 0.012012012,
 0.00000000, 0, 0.000, 0.1538462, 0.000000000,
 0.05263158, 0, 0.375, 0.1538462, 0.006006006,
 0.05263158, 0, 0.000, 0.3846154, 0.021021021,
 0.36842105, 0, 0.500, 0.1538462, 0.960960961), byrow=F, nrow=5)


## validation matrix 
attach(my);vm=as.matrix(data.frame(b,d,f,h,u));   # validation matrix for 2nd MC prediction 

# test data set td2=seq[379:756] with 378 obs
# predicted matrix using pi(t+3)=lam1*pi(t+2)*P1+lam2*pi(t+1)*P2+lam3*pi(t)*P3 
pred1=1/3*vm[378:755,]%*%P1+1/3*vm[377:754,]%*%P2+1/3*vm[376:753,]%*%P3; rowSums(pred1[1:20,]) 
# 3rd MC predicts states 381:756 with 376 obs

# test data set td1=seq[1:378]  predicts 4:381
pred2=1/3*vm[3:380,]%*%P4+1/3*vm[2:379,]%*%P5+1/3*vm[1:378,]%*%P6; rowSums(pred2[1:20,]) 

# convert it to 0/1 predictions, the success or failure indicator
# max prob (row) = 1, otherwise 0. for predm(i,j)>=maxprobrow, predm(i,j)=1; otherwise=0. 
preda1=matrix(0,378,5)
for (r in 1:378) {
  for (c in 1:5) {
    if (pred1[r,c]==max(pred1[r,])) {preda1[r,c]=1}
    else {preda1[r,c]=0} } }
rs=subset(preda1,rowSums(preda1)==2);rs

preda2=matrix(0,378,5)
for (r in 1:378) {
  for (c in 1:5) {
    if (pred2[r,c]==max(pred2[r,])) {preda2[r,c]=1}
    else {preda2[r,c]=0} } }
rs=subset(preda2,rowSums(preda2)==2);rs

# compute the prediction error rate
ce1=vm[379:756,]-preda1  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce1)    #  30 ones and 25 negative -1 indicate errors
er1=33/378;er1 # prediction error rate .008730159  # apply(ce,2,sum)

ce2=vm[4:381,]-preda2  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce2)      #  30 ones and 25 negative -1 indicate errors
er2=131/378;er2 # prediction error rate .3465608  # apply(ce,2,sum)

(er1+er2)/2   # 0.2169312


  