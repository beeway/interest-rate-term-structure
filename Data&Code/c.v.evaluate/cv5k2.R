
### 5 fold cv for 2nd MC
my=read.csv("mms.csv")
seq=my$seq

require(markovchain)
install.packages("Rsolnp")
require(Rsolnp)

# training set and validation set each 605
td1=seq[-1:-151]; td2=seq[-152:-302]; td3=seq[-303:-453];td4=seq[-454:-604]; td5=seq[-605:-755]
length(td1) # 605

mc1=fitHigherOrder(td1, order = 2);mc1  # train the first fold
mc2=fitHigherOrder(td2, order = 2);mc2  
mc3=fitHigherOrder(td3, order = 2);mc3
mc4=fitHigherOrder(td4, order = 2);mc4  
mc5=fitHigherOrder(td5, order = 2);mc5


# retrieve the two matrces for mc1
$lambda 6.442393e-07 9.999994e-01
   B          D          F          H          U 
0.06446281 0.11239669 0.01818182 0.08760331 0.71735537 

P11=matrix(c(0.76923077, 0.02941176, 0.27272727, 0.00000000, 0.009237875,
             0.05128205, 0.83823529, 0.09090909, 0.15094340, 0.000000000,
             0.02564103, 0.01470588, 0.45454545, 0.01886792, 0.004618938,
             0.00000000, 0.10294118, 0.00000000, 0.67924528, 0.023094688,
             0.15384615, 0.01470588, 0.18181818, 0.15094340, 0.963048499), byrow=F, nrow=5); rowSums(P11)
 
P12=matrix(c(0.66666667, 0.05882353, 0.27272727, 0.00000000, 0.013888889,
             0.10256410, 0.76470588, 0.09090909, 0.15094340, 0.004629630,
             0.02564103, 0.01470588, 0.27272727, 0.03773585, 0.006944444,
             0.00000000, 0.10294118, 0.00000000, 0.58490566, 0.034722222,
             0.20512821, 0.05882353, 0.36363636, 0.22641509, 0.939814815), byrow=F,nrow=5);
             
# retrieve the two matrces for mc2
$lambda 0.5 0.5
B          D          F          H          U 
0.03636364 0.06776860 0.02314050 0.07933884 0.79338843 

P21=matrix(c(
  0.63636364, 0.02439024, 0.21428571, 0.02083333, 0.006263048,
  0.04545455, 0.85365854, 0.00000000, 0.10416667, 0.000000000,
  0.04545455, 0.00000000, 0.50000000, 0.04166667, 0.008350731,
  0.00000000, 0.09756098, 0.07142857, 0.66666667, 0.022964509,
  0.27272727, 0.02439024, 0.21428571, 0.16666667, 0.962421712),byrow=F,nrow=5);

P22=matrix(c(
  0.54545455, 0.04878049, 0.2142857, 0.02083333, 0.008368201,
  0.09090909, 0.73170732, 0.0000000, 0.14583333, 0.004184100,
  0.04545455, 0.00000000, 0.2142857, 0.08333333, 0.012552301,
  0.00000000, 0.14634146, 0.1428571, 0.52083333, 0.031380753,
  0.31818182, 0.07317073, 0.4285714, 0.22916667, 0.943514644),byrow=F,nrow=5);


# retrieve the two matrces for mc3
$lambda 0.5 0.5
B          D          F          H          U 
0.05950413 0.07438017 0.01487603 0.10082645 0.75041322 

P31=matrix(c(
  0.80555556, 0.04444444, 0.0000000, 0.01639344, 0.008830022,
  0.02777778, 0.77777778, 0.1111111, 0.13114754, 0.000000000,
  0.00000000, 0.02222222, 0.4444444, 0.01639344, 0.006622517,
  0.00000000, 0.15555556, 0.1111111, 0.73770492, 0.017660044,
  0.16666667, 0.00000000, 0.3333333, 0.09836066, 0.966887417), byrow=F, nrow=5)

P32=matrix(c(
  0.72222222, 0.08888889, 0.0000000, 0.01639344, 0.01106195,
  0.05555556, 0.66666667, 0.2222222, 0.18032787, 0.00000000,
  0.00000000, 0.02222222, 0.1111111, 0.03278689, 0.01106195,
  0.00000000, 0.20000000, 0.2222222, 0.59016393, 0.03097345,
  0.22222222, 0.02222222, 0.4444444, 0.18032787, 0.94690265), byrow=F, nrow=5)


# retrieve the two matrces for mc4
$lambda 0.5 0.5
B          D          F          H          U 
0.05785124 0.12231405 0.02644628 0.11900826 0.67438017 

P41=matrix(c(
  0.77142857, 0.01351351, 0.1875, 0.01388889, 0.007371007,
  0.05714286, 0.83783784, 0.0625, 0.12500000, 0.000000000,
  0.02857143, 0.01351351, 0.5000, 0.02777778, 0.009828010,
  0.00000000, 0.12162162, 0.0625, 0.68055556, 0.031941032,
  0.14285714, 0.01351351, 0.1875, 0.15277778, 0.950859951), byrow=F, nrow=5)
P42=matrix(c(
 0.68571429, 0.02702703, 0.1875, 0.01388889, 0.012315271, 
 0.11428571, 0.75675676, 0.1250, 0.13888889, 0.004926108,
 0.02857143, 0.01351351, 0.2500, 0.05555556, 0.014778325,
 0.00000000, 0.14864865, 0.1250, 0.55555556, 0.046798030,
 0.17142857, 0.05405405, 0.3125, 0.23611111, 0.921182266), byrow=F, nrow=5)


# retrieve the two matrces for mc5
$lambda 0.5 0.5
0.04628099 0.12561983 0.02314050 0.12892562 0.67603306 

P51=matrix(c(
  0.71428571, 0.02631579, 0.21428571, 0.01282051, 0.004901961,
  0.07142857, 0.82894737, 0.07142857, 0.12820513, 0.000000000,
  0.03571429, 0.01315789, 0.50000000, 0.02564103, 0.007352941,
  0.00000000, 0.11842105, 0.07142857, 0.69230769, 0.034313725,
  0.17857143, 0.01315789, 0.14285714, 0.14102564, 0.953431373), byrow=F, nrow=5)
P52=matrix(c(
  0.57142857, 0.05263158, 0.2142857, 0.01282051, 0.009828010,
  0.14285714, 0.73684211, 0.1428571, 0.15384615, 0.004914005,
  0.03571429, 0.01315789, 0.2857143, 0.05128205, 0.009828010,
  0.00000000, 0.14473684, 0.1428571, 0.56410256, 0.051597052,
  0.25000000, 0.05263158, 0.2142857, 0.21794872, 0.923832924), byrow=F, nrow=5)


# calculate the prediction matrix for each validation set 
## vd matrix
attach(my)
vm=as.matrix(data.frame(b,d,f,h,u)); # 

# td1=seq[-1:-151]; td2=seq[-152:-302]; td3=seq[-303:-453];td4=seq[-454:-604]; td5=seq[-605:-755]
# predicted matrix using pi(t+2)=lamda1*pi(t+1)*P1+lamda2*pi(t)*P2 with test data set
pred1=6.442393e-07*vm[2:152,]%*%P11+9.999994e-01*vm[1:151,]%*%P12      # t predicts states for t+2 3:153
pred2=0.5*vm[153:303,]%*%P21+0.5*vm[152:302,]%*%P22    # predicts states for 154:304
pred3=0.5*vm[304:454,]%*%P31+0.5*vm[303:453,]%*%P32    # predicts states for 305:455
pred4=0.5*vm[455:605,]%*%P41+0.5*vm[454:604,]%*%P42    # predicts states for 456:606
pred5=0.5*vm[605:755,]%*%P51+0.5*vm[604:754,]%*%P52    # predicts states for 606:756

# convert it to 0/1 predictions, the success or failure indicator
# max prob (row) = 1, otherwise 0. for predm(i,j)>=maxprobrow, predm(i,j)=1; otherwise=0. 
preda1=matrix(0,151,5)  
for (r in 1:151) {
  for (c in 1:5) {
    if (pred1[r,c]==max(pred1[r,])) {preda1[r,c]=1}
    else {preda1[r,c]=0} } }
# rs2=subset(preda, rowSums(preda)==2) 

preda2=matrix(0,151,5)  
for (r in 1:151) {
  for (c in 1:5) {
    if (pred2[r,c]==max(pred2[r,])) {preda2[r,c]=1}
    else {preda2[r,c]=0} } }

preda3=matrix(0,151,5)  
for (r in 1:151) {
  for (c in 1:5) {
    if (pred3[r,c]==max(pred3[r,])) {preda3[r,c]=1}
    else {preda3[r,c]=0} } }

preda4=matrix(0,151,5)  
for (r in 1:151) {
  for (c in 1:5) {
    if (pred4[r,c]==max(pred4[r,])) {preda4[r,c]=1}
    else {preda4[r,c]=0} } }

preda5=matrix(0,151,5)  
for (r in 1:151) {
  for (c in 1:5) {
    if (pred5[r,c]==max(pred5[r,])) {preda5[r,c]=1}
    else {preda5[r,c]=0} } }


# compute the prediction error rate
# td1=seq[-1:-151]; td2=seq[-152:-302]; td3=seq[-303:-453];td4=seq[-454:-604]; td5=seq[-605:-755]
ce1=vm[3:153,]-preda1  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce1)    #  ones; 1 and -1 indicate an error
er1=30/151;er1 # prediction error rate .1986755  # apply(ce,2,sum)

ce2=vm[154:304,]-preda2  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce2)    #  ones; 1 and -1 indicate an error
er2=24/151;er2 # prediction error rate .1589404   # apply(ce,2,sum)

ce3=vm[305:455,]-preda3  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce3)     #  24 ones; 1 and -1 indicate an error
er3=28/151;er3 # prediction error rate .1854305  # apply(ce,2,sum)

ce4=vm[456:606,]-preda4  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce4)    #  6 ones; 1 and -1 indicate an error
er4=7/151;er4 # prediction error rate .04635762  # apply(ce,2,sum)

ce5=vm[606:756,]-preda5  # some rows have 1 and -1, indicating mistakes, need to count how many 1s
table(ce5)    #  6 ones; 1 and -1 indicate an error
er5=7/151;er5 # prediction error rate .04635762  # apply(ce,2,sum)

# calculate the average prediction error rate
aer=(er1+er2+er3+er4+er5)/5;aer   #  0.127153



