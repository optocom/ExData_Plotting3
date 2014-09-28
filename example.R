if (F) {

setwd("~/R/working/Exploratory Data Analysis/Example")

# data link
EDA.url<-"http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html"
# PM2.5 FRM Mass (88101)
zip2013<-"http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/RD_501_88101_2013.zip"
zip2000<-"http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/Rd_501_88101_2000.Zip"

download.file(zip2013,destfile="zip2013.zip",mode="wb")
download.file(zip2000,destfile="zip2000.zip",mode="wb")
unzip("zip2013.zip")
unzip("zip2000.zip")

}

pm2013<-read.table("RD_501_88101_2013-0.txt",comment.char="#",header=F,sep="|",na.strings="")
pm2000<-read.table("RD_501_88101_2000-0.txt",comment.char="#",header=F,sep="|",na.strings="")

head(pm2013,3)
cNames <- readLines("RD_501_88101_2013-0.txt",n=1)
cNames <- strsplit(cNames, "|", fixed=TRUE) # TRUE for non-regExp
names(pm2013)<-cNames[[1]]
names(pm2013)
names(pm2013)<-make.names(cNames[[1]])
names(pm2013)
dim(pm2013) # rows

x2013<-pm2013$Sample.Value
class(x2013)
str(x2013)
summary(x2013) # check NA #, negtive values
mean(is.na(x2013)) # fraction of missing values (6.3% missing)

cNames <- readLines("RD_501_88101_2000-0.txt",n=1)
cNames <- strsplit(cNames, "|", fixed=TRUE) # TRUE for non-regExp
names(pm2000)<-make.names(cNames[[1]])
x2000<-pm2000$Sample.Value
str(x2000)
summary(x2000) # check NA #
mean(is.na(x2000)) # 8.2% missing
rm(cNames)



boxplot(x2000,x2013)
boxplot(log10(x2000),log10(x2013)) # warning: negative values
#rug(x2000,side=2)
#rug(x2013,side=4)
hist(x2013,breaks=400,xlim=c(0,100))
hist(x2000,breaks=100,xlim=c(0,100))



sum(x2013<0,na.rm=T) # how many negative values 
mean(x2013<0,na.rm=T) # fraction of negative values

head(pm2013$Date)
str(pm2013$Date) # int
dat2013<-as.Date(as.character(pm2013$Date),"%Y%m%d")
str(dat2013)
hist(dat2013,"month")
hist(dat2013[x2013<0],"month")

c1=rgb(0,0,0,alpha=1/100) # black with alpha
par(mar=c(4,3,2,1)) # set margins: bottom,left,top,right
#plot(dat2013,x2013,cex=0.5,pch=20,col=c1,ylim=c(0,300))
rm(c1,dat2013,x2000,x2013) # remove dat2013



site2013<-unique(subset(pm2013, 
                      subset=(State.Code==36), # New York state
                      select=c(County.Code,Site.ID)))
site2000<-unique(subset(pm2000, 
                        subset=(State.Code==36), 
                        select=c(County.Code,Site.ID)))
head(site2013)

both<-intersect(paste(site2013[,1],site2013[,2],sep="."),paste(site2000[,1],site2000[,2],sep="."))
pm2013$county.site<-with(pm2013,paste(County.Code,Site.ID,sep="."))
pm2000$county.site<-with(pm2000,paste(County.Code,Site.ID,sep="."))
c2013<-subset(pm2013, State.Code==36 & county.site %in% both)
c2000<-subset(pm2000, State.Code==36 & county.site %in% both)
rm(both)

sapply(split(c2013, c2013$county.site), nrow) # number of rows
sapply(split(c2000, c2000$county.site), nrow)

d2013<-subset(pm2013, State.Code==36 & County.Code==61 & Site.ID==79)
d2000<-subset(pm2000, State.Code==36 & County.Code==61 & Site.ID==79)
dim(d2013)
dim(d2000)
summary(d2013$Sample.Value)
summary(d2000$Sample.Value)

d2013$date<-strptime(d2013$Date,"%Y%m%d")
d2013$dateCt <- as.POSIXct(d2013$date)
d2000$date<-strptime(d2000$Date,"%Y%m%d")
d2000$dateCt <- as.POSIXct(d2000$date)


par(mfrow=c(1,2),mar=c(4,4,2,0.5))
with(d2013,plot(date,Sample.Value,
                ylim=c(0,65),main="2013",
                ylab="sample",pch=20,col="blue"))
abline(h=median(d2013$Sample.Value,na.rm=T),col="blue")
abline(lm(Sample.Value~dateCt,data=d2013,na.action=na.omit),
       lty=2,col="blue") # linear model
with(d2000,plot(date,Sample.Value,
                ylim=c(0,65),main="2000",
                ylab="sample",pch=2,cex=0.6,col="red"))
abline(h=median(d2000$Sample.Value,na.rm=T),col="red")
abline(lm(Sample.Value~dateCt,data=d2000,na.action=na.omit),
       lty=2,col="red") # linear model


dif<-difftime(as.POSIXct(strptime("2013-01-01","%Y-%m-%d")),
              as.POSIXct(strptime("2000-01-01","%Y-%m-%d")))
par(mfrow=c(1,1),mar=c(4,4,2,1))
with(d2000,plot(dateCt,Sample.Value,
                type="n",  # setup only, no plotting
                ylim=c(0,65),
                main="dm2.5",xlab="date",ylab="Sample"))
with(d2013,points(dateCt-dif,Sample.Value, # needs to minus dif
                pch=20,col="blue"))
abline(h=median(d2013$Sample.Value,na.rm=T),col="blue")
abline(lm(Sample.Value~dateCt,data=d2013,na.action=na.omit),
       lty=2,col="blue") # linear model
with(d2000,points(dateCt,Sample.Value,
                pch=2,cex=0.6,col="red"))
abline(h=median(d2000$Sample.Value,na.rm=T),col="red")
abline(lm(Sample.Value~dateCt,data=d2000,na.action=na.omit),
       lty=2,col="red") # linear model



pm2013$Sample.Value[pm2013$Sample.Value>700]<-NA
m2013<-with(pm2013,tapply(Sample.Value,County.Code,mean,na.rm=T))
m2000<-with(pm2000,tapply(Sample.Value,County.Code,mean,na.rm=T))
m<-intersect(names(m2013),names(m2000))
mn2013<-m2013[names(m2013) %in% m]
mn2000<-m2000[names(m2000) %in% m]
rm(m)
sum(mn2013-mn2000>0) # number of counties becomes worse from 2000 to 2013
mn2013[mn2013-mn2000>0] # find the county and the value at 2013
mn2000[mn2013-mn2000>0]
mn0<-data.frame(year=2013,county=names(m2013),mean=m2013)
mn1<-data.frame(year=2000,county=names(m2000),mean=m2000)
mn0<-rbind(mn0,mn1) # narrow form
rm(mn1)
mn0<-data.frame(year=2013,county=names(m2013),mean=m2013)
mn1<-data.frame(year=2000,county=names(m2000),mean=m2000)
mn1<-merge(mn0,mn1,by=c("county"),all=T) # wide form including no matching
rm(mn0)
mn0<-data.frame(year=2013,county=names(m2013),mean=m2013)
mn1<-data.frame(year=2000,county=names(m2000),mean=m2000)
mn1<-merge(mn0,mn1,by=c("county"),all=F) # wide form excluding no matching
rm(mn0)

par(mar=c(4,4,2,1))
with(mn1,plot(mean.x,mean.y,xlim=c(0,20),ylim=c(0,20),
              xlab=bquote(mean[2013]),ylab=bquote(mean[2000])))
abline(lm(mean.y~mean.x,data=mn1),col=2)
abline(a=0,b=1,lty=2,col=3)
mn1$diff<-mn1$mean.x-mn1$mean.y
summary(mn1$diff)
hist(mn1$diff,main="Differece for each county",xlab=bquote(mean[2013]-mean[2000]))
abline(v=mean(mn1$diff),col="red")
c1=rgb(0,0,0,alpha=1/5) # black with alpha
plot(1,xlim=c(1999,2014),ylim=c(0,20),
     main="Comparison of each county",
     xlab="year",ylab=bquote(PM[2.5]))
with(mn1,segments(year.y,mean.y,year.x,mean.x,col=c1))




