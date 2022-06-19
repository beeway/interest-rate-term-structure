
getwd()

## read data and form data frame
install.packages("readxl")
library("readxl"); dm=read_excel("1953.xlsx")

library(psych);describe(dm)
summary(dm); str(dm)
tail(dm)

# creat counting dummy variables for each type - exhausting
attach(dm)
U=ifelse((ym-ys>0.1 & ym<=yl)| (yl-ym>0.1 & ym>=ys), 1, 0)     # upward    
H=ifelse((ym-ys>0.1 & ym>yl) | (ym-yl>0.1 & ym>ys), 1, 0)       # hump      
F=ifelse(    abs(ym-ys)<=0.1 & abs(yl-ym)<=0.1, 1,0)            # flat        
B=ifelse((ys-ym>0.1 & yl>ym) | (yl-ym>0.1 & ys>ym), 1, 0)       # bowl        
D=ifelse((ys-ym>0.1 & ym>=yl)| (ym-yl>0.1 & ys>=ym), 1, 0)     # downward  

## calculating the number of ocurrence for each type and frequency
fc=cbind(U,H,F,B,D)
apply(fc,2,sum,na.rm=T)
ts=sum(colSums(fc));ts
fd=colSums(fc)/ts;fd # initial distribution
cs=colSums(fc,na.rm=TRUE); cs/804

# statistics for level, slope, curvature conditional on each type
# use five category dummy
mc=data.frame(date,U,H,F,D,B)

# creat a shape categorical indicator ms
attach(mc)
mc$Shape[U==1]="U"; mc$Shape[H==1]="H"; mc$Shape[F==1]="F"; mc$Shape[B==1]="B"; mc$Shape[D==1]="D"; table(mc$Shape)

# creat a shape numerical indicator ns
mc$ns[U==1]="1"; mc$ns[H==1]="2"; mc$ns[D==1]="3"; mc$ns[B==1]="4"; mc$ns[F==1]="5"; table(mc$ns)
detach(mc)

write.csv(mc, file = "mshapes53.csv")


# 
mc$ns[u==1]="1"; mc$ns[h==1]="2"; mc$ns[f==1]="5"; mc$ns[b==1]="4"; mc$ns[d==1]="3"
umc=subset(mc,U==1); describe(umc)  # 
hmc=subset(mc,h==1); describe(hmc)  # 
fmc=subset(mc,f==1); describe(fmc)  # 
bmc=subset(mc,b==1); describe(bmc)  # 
dmc=subset(mc,d==1); describe(dmc)  # 

mc$ms[U==1]="u"; mc$ms[H==1]="h"; mc$ms[F==1]="f"; mc$ms[B==1]="b"; mc$ms[D==1]="d"

# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

library(ggplot2)

theme_set(theme_bw())  
g=ggplot(mc,aes(date,ns))
g + geom_point(aes(col=ms), size=3, show.legend=T)+labs(y="Yield Curve Shape Dots", x="") 


# graph evolution as of 202012
md=read.csv("mshapes53.csv");
head(md); tail(md)

# create the year and month vectors for wide data ggplot visulization so that each year has a column
year=c(1953:2020); year=rep(year, each=12); year=year[4:816]
month=c(1:12) ; month=rep(month,68); month=month[4:816]
ms=md$Shape; ns=as.character(md$ns) 
wmc=data.frame(year, month, ms, ns)

g=ggplot(wmc,aes(year,ns))  # ggplot graph chr-type in y-axsis without margins but int-type with margins
g + geom_point(aes(col=ms),size=3) + labs(y="Yield Curve Shape Dots", x="") 

# apply jitter to simulate the monthly shapes in each column
g + geom_jitter(aes(col=ms), size=3) + theme_bw() +
  scale_x_continuous(breaks = seq(1953,2021, by=2)) +
  annotate("rect", xmin=1953.67, xmax=1954.42, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=1957.75, xmax=1958.33, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=1960.42, xmax=1961.17, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=1970.08, xmax=1970.92, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=1974.00, xmax=1975.25, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=1980.17, xmax=1980.58, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=1981.67, xmax=1982.92, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=1990.67, xmax=1991.25, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=2001.33, xmax=2001.92, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=2008.08, xmax=2009.50, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=2020.25, xmax=2021.00, ymin = 0, ymax = 6, alpha = .5) +
  labs(y="Yield Curve Shape Dots", x="") + scale_color_discrete("Shape")  +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

#  annotate("rect", xmin=1920.17, xmax=1921.58, ymin = 0, ymax = 6, alpha = .5) +
#  annotate("rect", xmin=1923.50, xmax=1924.70, ymin = 0, ymax = 6, alpha = .5) +
#  annotate("rect", xmin=1926.92, xmax=1927.92, ymin = 0, ymax = 6, alpha = .5) +
#  annotate("rect", xmin=1929.75, xmax=1933.25, ymin = 0, ymax = 6, alpha = .5) +
#  annotate("rect", xmin=1937.50, xmax=1938.50, ymin = 0, ymax = 6, alpha = .5) +
#  annotate("rect", xmin=1945.25, xmax=1945.83, ymin = 0, ymax = 6, alpha = .5) +
#  annotate("rect", xmin=1949.00, xmax=1949.83, ymin = 0, ymax = 6, alpha = .5) +

# theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank())
# annotate("rect", xmin=, xmax =  , ymin = 0, ymax = 6, alpha = .5) +
# c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12, 10/12, 11/12)
# c(0.08, 0.17, 0.25, 0.33, 0.42, 0.5,  0.58, 0.67, 0.75, 0.83, 0.92)

g + geom_point(aes(col=ms),size=2, show.legend=T) 
g + geom_point(aes(col=month, size=ms)) + labs(y="Yield Curve Shape Dots", x="") 
g + geom_point(aes(col=month), size=1, show.legend=T) + labs(y="Yield Curve Shape Dots", x="") 

