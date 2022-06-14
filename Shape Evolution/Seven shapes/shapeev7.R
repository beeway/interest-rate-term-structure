
getwd()

## read data and form data frame
install.packages("readxl")
library("readxl"); dm=read_excel("m1945.xlsx")

library(psych);describe(dm)
summary(dm); str(dm)

# creat counting dummy variables for each type - exhausting
attach(dm)
U=ifelse((ym-ys>0.1 & ym<=yl)| (yl-ym>0.1 & ym>=ys), 1, 0)     # upward    
H=ifelse((ym-ys>0.1 & ym>yl) | (ym-yl>0.1 & ym>ys), 1, 0)       # hump      
F=ifelse(    abs(ym-ys)<=0.1 & abs(yl-ym)<=0.1, 1,0)            # flat        
B=ifelse((ys-ym>0.1 & yl>ym) | (yl-ym>0.1 & ys>ym), 1, 0)       # bowl        
D=ifelse((ys-ym>0.1 & ym>=yl)| (ym-yl>0.1 & ys>=ym), 1, 0)     # downward  

tabulate(U) 

## further divide upward shape
Ufh=subset(mu, U==1 & sls<2 & curv>0)    # U1 flat and hump
Ufb=subset(mu, U==1 & sls<2 & curv<=0)   # U2 flat and bowl
Usb=subset(mu, U==1 & sls>=2 & curv<=0)  # U3 steep and bowl 
Ush=subset(mu, U==1 & sls>=2 & curv>0)   # U4 steep and hump
281+177+127+112

Ufh=ifelse( (U==1 & sls<2 & curv>0), 1, 0)    # flat and hump
Ufb=ifelse( (U==1 & sls<2 & curv<=0), 1, 0)   # flat and bowl
Usb=ifelse( (U==1 & sls>=2 & curv<=0), 1, 0)  # steep a d bowl 
Ush=ifelse( (U==1 & sls>=2 & curv>0), 1, 0)   # steep and hump
tabulate(Ufh); tabulate(Ufb); tabulate(Usb); tabulate(Ush)
s7=data.frame(date, Ufh, Ufb, Usb, Ush, H, D, B, F)

# creat a shape numerical indicator ns (7 shapes)
s7$ns[Ufh==1]=0.25;s7$ns[Ufb==1]=0.5; s7$ns[Usb==1]=0.75; s7$ns[Ush==1]=1
s7$ns[H==1]=2; s7$ns[D==1]=3; s7$ns[B==1]=4; s7$ns[F==1]=5; 
table(as.numeric(s7$ns))

# creat a shape indicator ms (7 shapes)
s7$ms[Ufh==1]="U1"; s7$ms[Ufb==1]="U2"; s7$ms[Usb==1]="U3"; s7$ms[Ush==1]="U4"; 
s7$ms[H==1]="H"; s7$ms[F==1]="F"; s7$ms[B==1]="B"; s7$ms[D==1]="D"
table(s7$ms)

write.csv(s7, file = "7shapes.csv")


library(ggplot2)   # http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

g=ggplot(s7,aes(date,ns))
g + geom_point(aes(col=ms), size=1, show.legend=T) + labs(y="Yield Curve Shape Dots", x="") 

# graph evolution as of 202012

s7=read.csv("7shapes.csv")

# create a year vector for wide data ggplot visulization so that each year has a column
year=c(1945:2020); year=rep(year, each=12)
month=c(1:12) ; month=rep(month,76)
ms=s7$ms; ns=as.numeric(s7$ns)
wmc=data.frame(year, month, ms, ns)

g=ggplot(wmc,aes(year,ns))  # ggplot graph chr-type in y-axsis without margins but int-type with margins
g + geom_point(aes(col=ms),size=3) + labs(y="Yield Curve Shape Dots", x="") 

g + geom_jitter(aes(col=ms), size=3) + theme_bw() +
  scale_x_continuous(breaks = seq(1945,2020, by = 3), expand = c(0.02,0.02)) +
  annotate("rect", xmin=1945.25, xmax=1945.83, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=1949.00, xmax=1949.83, ymin = 0, ymax = 6, alpha = .5) +
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
  annotate("rect", xmin=2020.25, xmax=2020.60, ymin = 0, ymax = 6, alpha = .5) +
  labs(y="Yield Curve Shape Dots", x="") + scale_color_discrete("Shape")  +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())


# without inner margins
g + geom_jitter(aes(col=ms), size=3) + theme_bw() +
  scale_x_continuous(breaks = seq(1945,2020, by = 3), expand = c(0.02,0.02)) +
  scale_y_discrete(breaks=seq(0,5,1), expand = c(0,0)) +
  annotate("rect", xmin=1945.25, xmax=1945.83, ymin = 0, ymax = 6, alpha = .5) +
  annotate("rect", xmin=1949.00, xmax=1949.83, ymin = 0, ymax = 6, alpha = .5) +
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
  annotate("rect", xmin=2020.25, xmax=2020.60, ymin = 0, ymax = 6, alpha = .5) +
  labs(y="Yield Curve Shape Dots", x="") + scale_color_discrete("Shape") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())


