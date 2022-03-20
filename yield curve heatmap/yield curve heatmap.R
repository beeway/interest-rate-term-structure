
http://compbio.ucsd.edu/making-heat-maps-r/
https://www.rdocumentation.org/packages/ComplexHeatmap/versions/1.10.2/topics/Heatmap
http://stackoverflow.com/questions/15505607/diagonal-labels-orientation-on-x-axis-in-heatmaps
getwd()
setwd("/Users/beeway/Desktop/Interest Rates/Mypapers/DV/heatmap")

d=read.csv('mys.csv')
str(d)
dim(d)

install.packages(c("pheatmap","RColorBrewer"))
library(RColorBrewer)
library(pheatmap)

# pheatmap() function requires that the data be in a matrix format
# heatmap levels: horizontal - columns; vertical - rows

is.matrix(d)
d=data.matrix(d) 
dt=t(d[635:700,12:2])
pheatmap(dt) # with clusters default
pheatmap(dt,cluster_row=F,cluster_col=F) # main ="Monthly Yield Curve Heat Map"

require(lattice)
levelplot(d, scale=list(x=list(rot=45)))

# plain heatmap ?heatmap
# heatmap(dt,Rowv=NA,Colv=NA,main="Monthly Yield Curve Heat Map",xlab=" ",ylab="")

# fix the column names
d=read.csv('mys.csv')
rownames(d)=d$date 
dt=t(d[635:700,12:2])
pheatmap(dt,cluster_row=F,cluster_col=F,display_numbers=F, filename = "hm0.png")
# color = colorRampPalette(c("navy", "white", "firebrick3"))(50)
# display_numbers = T, cellwidth = 15, cellheight = 12, fontsize = 8
# filename = "t.pdf"

# change the color
heatcolor = brewer.pal(7,"Greens")
pheatmap(dt,cluster_row=F,cluster_col=F,color = heatcolor)
pheatmap(dt,cluster_row=F,cluster_col=F,color = heatcolor,filename='hm1.png')
# display_numbers=T,fontsize_number=10


## subset 
d$date=as.Date(d$date,'%m/%d/%y')
str(d)
start=as.Date('2006-02')
end=as.Date('2016-05')
newd=d[which(d$date>=start & d$date<=end),]

## annual data
ad=read.csv('tya.csv')   # missing data
rownames(ad)=ad$date  
da=t(ad[2:54,2:12])
pheatmap(da,cluster_row=F,cluster_col=F,display_numbers=F,na.rm=T)

