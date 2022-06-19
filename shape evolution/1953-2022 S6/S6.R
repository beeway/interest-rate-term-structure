
install.packages("readxl")
library("readxl")

dm=read_excel("1953.xlsx")

library(psych);describe(dm)
summary(dm); str(dm)

#############################################################
# Five shapes: creat counting dummy variables for each type 
# Algorithm (mutually exclusive and exhaustive) by Chen (2021) 

attach(dm)
B=ifelse((ys-ym>0.1 & yl>ym) | (yl-ym>0.1 & ys>ym), 1, 0)       # bowl        
D=ifelse((ys-ym>0.1 & ym>=yl)| (ym-yl>0.1 & ys>=ym), 1, 0)      # downward  
F=ifelse(    abs(ym-ys)<=0.1 & abs(yl-ym)<=0.1, 1,0)            # flat        
H=ifelse((ym-ys>0.1 & ym>yl) | (ym-yl>0.1 & ym>ys), 1, 0)       # hump      
U=ifelse((ym-ys>0.1 & ym<=yl)| (yl-ym>0.1 & ym>=ys), 1, 0)      # upward   

# check occurrence and sum
tabulate(B); tabulate(D); tabulate(F); tabulate(H); tabulate(U) 
tabulate(B)+tabulate(D)+tabulate(F)+tabulate(H)+tabulate(U) 

## calculating the number of occurrence/frequency by type 
fc=cbind(B,D,F,H,U)
apply(fc,2,sum,na.rm=T)
ts=sum(colSums(fc));ts
fd=colSums(fc)/ts;fd # distribution
cs=colSums(fc,na.rm=TRUE); cs/nrow(fc)

# Sub-classes, upward-sloping
Ua=ifelse( (U==1 & sls<2), 1, 0)   # flat
Ub=ifelse( (U==1 & sls>=2), 1, 0)  # steep
tabulate(U); tabulate(Ua); tabulate(Ub) # verify sum
table(U)

# creat a shape categorical indicator ms
S6=data.frame(date,B,D,F,H,U,Ua,Ub)
S6$Shape[Ua==1]="Ua"; S6$Shape[Ub==1]="Ub"; # S6$Shape[U==1]="U"
S6$Shape[H==1]="H"; S6$Shape[F==1]="F"; S6$Shape[B==1]="B"; S6$Shape[D==1]="D"; 
table(S6$Shape)

# creat a shape numerical indicator in the order of rising yield levels (on average)
S6$S[Ua==1]="0.5"; S6$S[Ub==1]="1"; 
S6$S[B==1]="2"; S6$S[F==1]="3"; S6$S[H==1]="4"; S6$S[D==1]="5"; 
table(S6$S)

write.csv(S6, file = "S6.csv")

S6=read.csv("S6.csv")

#############################################################
## Graph shape evolution
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

library(ggplot2)   
ggplot(S6,aes(date,S)) + geom_point(aes(col=Shape), size=1, show.legend=T) + 
  labs(y="Yield Curve Shape Dots", x="") 

# create a year vector for WIDE data ggplot visulization so that each year has a column
year=c(1953:2022); year=rep(year, each=12); year=year[4:833] # factor id
month=c(1:12) ; month=rep(month,70); month=month[4:833] # factor id
ms=S6$Shape # factor id for colors
ns=as.numeric(S6$S) # numerical value of shapes on the vertical axis
gse=data.frame(year, month, ms, ns)

# ggplot graph shape dots on the y-axsis 
ggplot(gse,aes(year,ns)) + geom_point(aes(col=ms),size=3)

# adding legends and axis labels
ggplot(gse,aes(year,ns)) + geom_point(aes(col=ms),size=4.5) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  scale_x_continuous(breaks=seq(1945,2022, by=2), expand=c(0.02,0.02)) + scale_color_discrete("Shape") + 
  labs(y="Yield Curve Shape Dots", x="", caption="Source: Biwei Chen (2022). Data: FRB; FRED; NBER.") 


# jitter the dots for non-overlapping
# adding NBER recessions - shaded periods
# no margins on Y in the plot (expand=c(0,0))
ggplot(gse,aes(year,ns)) + geom_jitter(aes(col=ms), size=3) + theme_bw() +
  scale_x_continuous(breaks = seq(1953,2022, by=1), expand = c(0.02,0.02)) +
  scale_y_discrete(breaks=seq(0,5,1), expand = c(0,0)) + scale_color_discrete("Shape") +
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
  annotate("rect", xmin=2020.25, xmax=2020.33, ymin = 0, ymax = 6, alpha = .5) +
  labs(y="Yield Curve Shape Dots", x="", caption="Source: Biwei Chen (2022). Data: FRB; FRED; NBER.") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(axis.text.x=element_text(angle=90, vjust=0.5, size = 9))  
