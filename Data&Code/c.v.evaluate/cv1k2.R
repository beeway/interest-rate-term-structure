
## 2nd order MC cv k=0 all sample

my=read.csv("mms.csv")
seq=my$seq

require(markovchain)
install.packages("Rsolnp")
require(Rsolnp)

mc2=fitHigherOrder(seq, order = 2);mc2
# $lambda [1] 0.5 0.5
P1=matrix(c(0.750, 0.02631579, 0.1875, 0.01282051, 0.00733945,
            0.050, 0.82894737, 0.0625, 0.12820513, 0.00000000,
            0.025, 0.01315789, 0.5000, 0.02564103, 0.00733945,
            0.000, 0.11842105, 0.0625, 0.69230769, 0.02568807,
            0.175, 0.01315789, 0.1875, 0.14102564, 0.95963303), byrow=F,nrow=5);rowSums(P1); 

P2=matrix(c(0.650, 0.05263158, 0.1875, 0.01282051, 0.011029412,
            0.100, 0.73684211, 0.1250, 0.15384615, 0.003676471,
            0.025, 0.01315789, 0.2500, 0.05128205, 0.011029412,
            0.000, 0.14473684, 0.1250, 0.56410256, 0.038602941,
            0.225, 0.05263158, 0.3125, 0.21794872, 0.935661765), byrow=F, nrow=5); rowSums(P2)

# 0.05291005 0.10052910 0.02116402 0.10317460 0.72222222 

## validation matrix 
attach(my)
vm=as.matrix(data.frame(b,d,f,h,u));  # [3:756,] for 2nd prediction 

# predicted matrix using pi(t+2)=lam1*pi(t+1)*P1+lam2*pi(t)*P2 with test data set
pred=0.5*vm[2:755,]%*%P1+0.5*vm[1:754,]%*%P2    # predicts states for 3 to 756
rowSums(pred[1:100,])      # check row sum is one

# convert it to 0/1 predictions, the success or failure indicator
# max prob (row) = 1, otherwise 0. for predm(i,j)>=maxprobrow, predm(i,j)=1; otherwise=0. 
preda=matrix(0,754,5)
for (r in 1:754) {
  for (c in 1:5) {
    if (pred[r,c]==max(pred[r,])) {preda[r,c]=1}
    else {preda[r,c]=0} } }
rs2=subset(preda,rowSums(preda)==2);rs2

# compute the prediction error rate
ce=vm[3:756,]-preda  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce)    # 21 ones; 1 and -1 indicate an error
er=87/754;er # prediction error rate 0.1153846  # apply(ce,2,sum)

  

