
rcs file indicators
1- recessions
2- after recession 12 months
3- before recession 18 months
0- otherwise

########################################
# P(R_t+m | St) + P (B_t+m | St) + P(A_t+m | St) + P(O_t+m | St) = 1   ; Underway =1 
# R recession; B before rece; A after rece; O other
dat=read.csv("rcs.csv")   
attach(dat)     #write.csv(dat,file='dat.csv')

s='U'; m=15; # shape and forward time step (m-month)

b=rep(0, length(ms)); # before recession
for (t in 1:(length(ms)-m)) if ( ms[t]==s & (rec[t+m]==3)) {b[t]=1}

r=rep(0, length(ms)); # in recession 
for (t in 1:(length(ms)-m)) if ( ms[t]==s & (rec[t+m]==1)) {r[t]=1}

a=rep(0, length(ms)); # after recession
for (t in 1:(length(ms)-m)) if ( ms[t]==s & (rec[t+m]==2)) {a[t]=1}

o=rep(0, length(ms)); # other periods
for (t in 1:(length(ms)-m)) if ( ms[t]==s & (rec[t+m]==0)) {o[t]=1}

c(sum(b),sum(r),sum(a),sum(o));sum(sum(b),sum(r),sum(a),sum(o));
round( c(sum(b),sum(r),sum(a),sum(o))/sum(sum(b),sum(r),sum(a),sum(o)), digit=3)


########################################
# P(R_t+1|ft) + P (B_t+1 | ft) + P(A_t+1 | ft) + P(O_t+1 | ft) = 1
# R recession; B before rece; A after rece; O other
dat=read.csv("rcs.csv")   
attach(dat)
m=1
ub=rep(0, length(ms))
for (t in 1:(length(ms)-1)) if ( ms[t]=="U" & (rec[t+1]==3)) {ub[t]=1}

ur=rep(0, length(ms))
for (t in 1:(length(ms)-1)) if ( ms[t]=="U" & (rec[t+1]==1)) {ur[t]=1}

ua=rep(0, length(ms))
for (t in 1:(length(ms)-1)) if ( ms[t]=="U" & (rec[t+1]==2)) {ua[t]=1}

uo=rep(0, length(ms))
for (t in 1:(length(ms)-1)) if ( ms[t]=="U" & (rec[t+1]==0)) {uo[t]=1}

c(sum(ub),sum(ur),sum(ua),sum(uo))
round(c(sum(ub),sum(ur),sum(ua),sum(uo))/sum(sum(ua),sum(ur),sum(ub),sum(uo)), digit=3)

ub=rep(0, length(ms)); for (t in 1:(length(ms)-1)) if ( ms[t]=="B" & (rec[t+1]==3)) {ub[t]=1}
ur=rep(0, length(ms)); for (t in 1:(length(ms)-1)) if ( ms[t]=="B" & (rec[t+1]==1)) {ur[t]=1}
ua=rep(0, length(ms)); for (t in 1:(length(ms)-1)) if ( ms[t]=="B" & (rec[t+1]==2)) {ua[t]=1}
uo=rep(0, length(ms)); for (t in 1:(length(ms)-1)) if ( ms[t]=="B" & (rec[t+1]==0)) {uo[t]=1}

############# recession start and end date
dat=read.csv("rcs.csv")   
attach(dat)

# 9 cycles
rs=as.factor(c('1957-08','1960-04','1969-12','1973-11','1980-01','1981-07','1990-07','2001-03','2007-12'))
re=as.factor(c('1958-04','1961-02','1970-11','1975-03','1980-07','1982-11','1991-03','2001-11','2009-06'))

# 10 cycles
rs=as.factor(c('1953-07','1957-08','1960-04','1969-12','1973-11','1980-01','1981-07','1990-07','2001-03','2007-12'))
re=as.factor(c('1954-05','1958-04','1961-02','1970-11','1975-03','1980-07','1982-11','1991-03','2001-11','2009-06'))
length(rs)

# start and end date shapes
drs=subset(dat,date %in% rs)
dre=subset(dat,date %in% re)

attach(dat)  # seq(as.Date('2000-6-1'),to=as.Date('2000-8-1'),by='month')

# one month before recession starts
rsl1=as.factor(c('1957-07','1960-03','1969-11','1973-10','1979-12','1981-06','1990-06','2001-02','2007-11'))
drsl1=subset(dat,date %in% rsl1)
table(drsl1$ms)
round(table(drsl1$ms)/c(546,78,16,76,40), digit=3)

# two month before recession starts
rsl2=as.factor(c('1957-06','1960-02','1969-10','1973-09','1979-11','1981-05','1990-05','2001-01','2007-10'))
drsl2=subset(dat,date %in% rsl2)
table(drsl2$ms)

# three month before recession starts
rsl3=as.factor(c('1957-05','1960-01','1969-09','1973-08','1979-10','1981-04','1990-04','2000-12','2007-09'))
drsl3=subset(dat,date %in% rsl3)
table(drsl3$ms)

# four month before recession starts
rsl4=as.factor(c('1957-04','1959-12','1969-08','1973-07','1979-09','1981-03','1990-03','2001-11','2007-08'))
drsl4=subset(dat,date %in% rsl4)
table(drsl4$ms)

# five month before recession starts
rsl5=as.factor(c('1957-03','1959-11','1969-07','1973-06','1979-08','1981-02','1990-02','2001-10','2007-07'))
drsl5=subset(dat,date %in% rsl5)
table(drsl5$ms)

# six month before recession starts
rsl6=as.factor(c('1957-02','1959-10','1969-06','1973-05','1979-07','1981-01','1990-01','2001-09','2007-06'))
drsl6=subset(dat,date %in% rsl6)
table(drsl6$ms)

# seven month before recession starts
rsl7=as.factor(c('1957-01','1959-09','1969-05','1973-04','1979-06','1980-12','1989-12','2001-08','2007-05'))
drsl7=subset(dat,date %in% rsl7)
table(drsl7$ms)

# eight month before recession starts
rsl8=as.factor(c('1956-12','1959-08','1969-04','1973-03','1979-05','1980-11','1989-11','2001-07','2007-04'))
drsl8=subset(dat,date %in% rsl8)
table(drsl8$ms)

# nine month before recession starts
rsl9=as.factor(c('1956-11','1959-07','1969-03','1973-02','1979-04','1980-10','1989-10','2001-06','2007-03'))
drsl9=subset(dat,date %in% rsl9)
table(drsl9$ms)

# ten month before recession starts
rsl10=as.factor(c('1956-10','1959-06','1969-02','1973-01','1979-03','1980-09','1989-09','2001-05','2007-02'))
drsl10=subset(dat,date %in% rsl10)
table(drsl10$ms)

# eleven month before recession starts
rsl11=as.factor(c('1956-09','1959-05','1969-01','1972-12','1979-02','1980-08','1989-08','2001-04','2007-01'))
drsl11=subset(dat,date %in% rsl11)
table(drsl11$ms)

# twelve month before recession starts
rsl12=as.factor(c('1956-08','1959-04','1968-12','1972-11','1979-01','1980-07','1989-07','2001-03','2006-12'))
drsl12=subset(dat,date %in% rsl12)
table(drsl12$ms)

# 13 month before recession starts
rsl13=as.factor(c('1956-07','1959-03','1968-11','1972-10','1978-12','1980-06','1989-06','2001-02','2006-11'))
drsl13=subset(dat,date %in% rsl13)
table(drsl13$ms)

# 14 month before recession starts
rsl14=as.factor(c('1956-06','1959-02','1968-10','1972-09','1978-11','1980-05','1989-05','2001-01','2006-10'))
drsl14=subset(dat,date %in% rsl14)
table(drsl14$ms)

# 15 month before recession starts
rsl15=as.factor(c('1956-05','1959-01','1968-09','1972-08','1978-10','1980-04','1989-04','2000-12','2006-09'))
drsl15=subset(dat,date %in% rsl15)
table(drsl15$ms)

# 16 month before recession starts
rsl16=as.factor(c('1956-04','1958-12','1968-08','1972-07','1978-09','1980-03','1989-03','2000-11','2006-08'))
drsl16=subset(dat,date %in% rsl16)
table(drsl16$ms)

# 17 month before recession starts
rsl17=as.factor(c('1956-03','1958-11','1968-07','1972-06','1978-08','1980-02','1989-02','2000-10','2006-07'))
drsl17=subset(dat,date %in% rsl17)
table(drsl17$ms)

# 18 month before recession starts
rsl18=as.factor(c('1956-02','1958-10','1968-06','1972-05','1978-07','1980-01','1989-01','2000-09','2006-06'))
drsl18=subset(dat,date %in% rsl18)
table(drsl18$ms)

# 19 month before recession starts
rsl19=as.factor(c('1956-01','1958-09','1968-05','1972-04','1978-06','1979-12','1988-12','2000-08','2006-05'))
drsl19=subset(dat,date %in% rsl19)
table(drsl19$ms)



########################################## b  r  a  o 
# calculating state probability starts next month given a shape this month
# nine cycles calcuation 
rs=as.factor(c('1957-08','1960-04','1969-12','1973-11','1980-01','1981-07','1990-07','2001-03','2007-12'))
re=as.factor(c('1958-04','1961-02','1970-11','1975-03','1980-07','1982-11','1991-03','2001-11','2009-06'))

# one month before each state starts

# one month before recession starts
rsl1=as.factor(c('1957-07','1960-03','1969-11','1973-10','1979-12','1981-06','1990-06','2001-02','2007-11'))
drsl1=subset(dat,date %in% rsl1)

# one month before post-recession starts
asl1= as.factor(c('1958-04','1961-02','1970-11','1975-03','1980-07','1982-11','1991-03','2001-11','2009-06'))
dasl1=sl1=subset(dat,date %in% asl1)

# one month before boom starts
bsl1=as.factor(c('1956-01','1958-09','1968-05','1972-04','1978-06','1980-07','1988-12','1999-08','2006-05'))  
dbsl1=subset(dat,date %in% bsl1)

# one month before other times start
all U shape!  
  
  