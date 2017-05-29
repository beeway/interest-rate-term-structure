

## split data into training (80%) and validation (20%)
ind=sample(2, nrow(data), replace=T, prob=c(.8,.2))
trd=data[ind==1,]
vad=data[ind==2,]



# get the data from somewhere and specify number of folds
data <- read.csv('my_data.csv')
nrFolds <- 10

# generate array containing fold-number for each sample (row)
data=data.frame(seq)
folds <- rep_len(1:nrFolds, nrow(data))

# actual cross validation
for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  data.train <- data[-fold,]
  data.test <- data[fold,]
  # train and test your model with data.train and data.test
}
data.test[]
View(fold)
View(data.test[1,])
# Note that the code above assumes that the data is already shuffled. 
# If this would not be the case, you could consider adding something like

folds <- sample(folds, nrow(data))


#################################
install.packages("cvTools")
library(cvTools) #run the above line if you don't have this library

k <- 10 #the number of folds
dataset=seq
table(seq)

folds <- cvFolds(NROW(dataset), K=k)
dataset$holdoutpred <- rep(0,nrow(dataset))

for(i in 1:k){
  train <- dataset[folds$subsets[folds$which != i], ] #Set the training set
  validation <- dataset[folds$subsets[folds$which == i], ] #Set the validation set
  
  newlm <- lm(y~x,data=train) #Get your new linear model (just fit on the train data)
  newpred <- predict(newlm,newdata=validation) 
  #Get the predicitons for the validation set (from the model just fit on the train data)
  
  dataset[folds$subsets[folds$which == i], ]$holdoutpred <- newpred 
  #Put the hold out prediction in the data set for later use
}

dataset$holdoutpred #do whatever you want with these predictions


############# Here is a simple way to perform 10-fold using no packages:
  
# Randomly shuffle the data
yourData<-yourData[sample(nrow(yourData)),]

# Create 10 equally size folds
folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)

# Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- yourData[testIndexes, ]
  trainData <- yourData[-testIndexes, ]
  #Use the test and train data partitions however you desire...
}

#######################
install.packages('caret')
library(caret)

