

da=vehicle
ind=sample(2, nrow(data), replace=T, prob=c(.8,.2))
trd=data[ind==1,]
vad=data[ind==2,]

head(trd)
head(vad)

# multiple linear regression model
results=lm(y~x+z, trd)
summary(results)

results$coefficients
coef(results)

# prediction
pred=predict(results, vad)
head(pred)
head(vad)