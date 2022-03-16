
library(ggplot2)
library(readxl)

dm=read_excel("1920spreads.xlsx")

attach(dm)
dys=diff(ys)
dym=diff(ym)
dyl=diff(yl)

head(ys); head(dys)

## area chart as.Date as x axis
date=seq(as.Date('1920-02-01'), to=as.Date('2020-03-01'), format="%Y%m", by="month")
ndm=data.frame(dys, dym, dyl); ndm$date=date[2:1203]

ggplot(ndm, aes(date, dys) ) + geom_area() + labs(x="", y="") + theme_bw() 
ggplot(ndm, aes(date, dym)) + geom_area()
ggplot(ndm, aes(date, dyl)) + geom_area()
