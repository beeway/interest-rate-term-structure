
setwd("/Users/beeway/Desktop/Interest Rates/Mypapers/DV/3D")
yc=read.csv('tyc.csv')
dim(yc)
str(yc)
class(yc)

install.packages('plotly')
library(plotly)

y1=yc[700:759,3:13]
yield=as.matrix(y1)
plot_ly(z=yield, type="surface",showscale=T)

y2=yc[2:759,3:13]
yield=as.matrix(y2)
plot_ly(z=yield, type="surface",showscale=F)




install.packages('plotly')
library(plotly)
z <- c( c(8.83,8.89,8.81,8.87,8.9,8.87), c(8.89,8.94,8.85,8.94,8.96,8.92),
  c(8.84,8.9,8.82,8.92,8.93,8.91),  c(8.79,8.85,8.79,8.9,8.94,8.92),
  c(8.79,8.88,8.81,8.9,8.95,8.92),  c(8.8,8.82,8.78,8.91,8.94,8.92),
  c(8.75,8.78,8.77,8.91,8.95,8.92),  c(8.8,8.8,8.77,8.91,8.95,8.94),
  c(8.74,8.81,8.76,8.93,8.98,8.99),  c(8.89,8.99,8.92,9.1,9.13,9.11),
  c(8.97,8.97,8.91,9.09,9.11,9.11),  c(9.04,9.08,9.05,9.25,9.28,9.27),
  c(9,9.01,9,9.2,9.23,9.2),  c(8.99,8.99,8.98,9.18,9.2,9.19),
  c(8.93,8.97,8.97,9.18,9.2,9.18))
dim(z) <- c(15,6)
str(z)
class(z)
plot_ly(z=z, type="surface",showscale=FALSE) 


