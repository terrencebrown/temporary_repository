################################
# Loading data
################################

setwd("dir/with/csv/files")

list.files(".")
ndatacc <- read.csv("ACC_18_norm.csv",header = F)
colnames(ndatacc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")

ndatgp<-read.csv("GP_18_norm.csv",header = F)
colnames(ndatgp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")

ndatacc <- read.csv("acc_data03.csv",header = F)
colnames(ndatacc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")

ndatgp<-read.csv("gps_data03.csv",header = F)
colnames(ndatgp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")

ndatacc_2 <- read.csv("acc_data24.csv",header = F)
colnames(ndatacc_2) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")

ndatgp_2<-read.csv("gps_data24.csv",header = F)
colnames(ndatgp_2) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")

bdatacc<-read.csv("CSV_data13.csv",header = F)
colnames(bdatacc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")

bdatgp<-read.csv("GPS_data13.csv",header = F)
colnames(bdatgp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")

bdatacc_2<-read.csv("CSV_data20.csv",header = F)
colnames(bdatacc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")

bdatgp_2<-read.csv("GPS_data20.csv",header = F)
colnames(bdatgp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")

class<-rep(1,dim(ndatacc)[1])
ndatacc<-cbind(ndatacc,as.factor(class))
class<-rep(1,dim(ndatacc_2)[1])
ndatacc_2<-cbind(ndatacc_2,as.factor(class))
ndatall<-rbind(ndatacc,ndatacc_2)
names(ndatacc)<-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX"
                  , "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string"
                  ,  "gps_flag", "internal_clock","class")
names(ndatacc_2)<-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX"
                  , "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string"
                  ,  "gps_flag", "internal_clock","class")

class<-rep(2,dim(bdatacc)[1])
bdatacc<-cbind(bdatacc,as.factor(class))
class<-rep(2,dim(bdatacc_2)[1])
bdatacc_2<-cbind(bdatacc_2,as.factor(class))
bdatall<-rbind(bdatacc,bdatacc_2)
names(bdatacc)<-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX"
                  , "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string"
                  ,  "gps_flag", "internal_clock","class")


setwd('/Users/Maxim/Dropbox/Private/Kandidatuddannelsen/Danmark/MSc Thesis - Speciale/Data/02_Case3x/NN300')
list.files(".")
list.files(".")[2]

ndatacc <- read.csv(list.files(".")[1],header = F)
colnames(ndatacc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")

ndatgp<-read.csv(list.files(".")[2],header = F)
colnames(ndatgp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")

setwd('/Users/Maxim/Dropbox/Private/Kandidatuddannelsen/Danmark/MSc Thesis - Speciale/Data/02_Case3x/RR299')
bdatacc<-read.csv(list.files(".")[1],header = F)
colnames(bdatacc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")

bdatgp<-read.csv(list.files(".")[2],header = F)
colnames(bdatgp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")

################################
# Data analysis plot script
# Assumes loaded data
###############################
# attach(ndatacc)
install.packages("vioplot")
library("vioplot")
boxplot(accX,accY,accZ,ylab="Acceleration m/s^2",names=c("X","Y","Z"),col=rainbow(floor(runif(1, 1,101))))

par(mfrow=c(1,1))
boxplot(ndatacc$accX,ndatacc$accY,ndatacc$accZ,names=c("X","Y","Z"), col="green",notch=T,varwidth=T
        ,range=c(sd(ndatacc$accX),ndatacc$accY,ndatacc$accZ))
# boxplot(ndatacc$accX,ndatacc$accY,ndatacc$accZ,names=c("X","Y","Z"), col="azure")
title(main = "Normal Acc. measurements", ylab = "Acceleration m/s^2")
boxplot(ndatacc$roll,ndatacc$pitch,ndatacc$yaw,names=c("Roll","Pitch","Yaw"), col="green")
title(main = "Normal Gyro measurements", ylab = "Angle")

# detach(ndatacc)
# attach(bdatacc)
par(mfrow=c(1,1))
boxplot(bdatacc$accX,bdatacc$accY,bdatacc$accZ,names=c("X","Y","Z"), col="red",h=0.1)
title(main = "Abormal Acc. measurements", ylab = "Acceleration m/s^2")
boxplot(bdatacc$roll,bdatacc$pitch,bdatacc$yaw,names=c("Roll","Pitch","Yaw"), col="red",h=0.1)
title(main = "Abormal Gyro measurements", ylab = "Angle")



plot((ndatgp$speedInMS*3.6),type="l",ylab="Speed KM/h",xlab="Time")
plot((bdatgp$speedInMS*3.6),type="l",ylab="Speed KM/h",xlab="Time")
vioplot(ndatgp$speedInMS*3.6,bdatgp$speedInMS*3.6,h=5,names=c("Normal","Abnormal"),col="cornflowerblue")
boxplot(ndatgp$speedInMS*3.6,bdatgp$speedInMS*3.6,names=c("Normal","Abnormal"),col="cornflowerblue")
?vioplot

plot(ndatgp$latitude)
plot(ndatgp$lontitude)
plot(ndatgp)

ndatall
bdatall
nbdatall<-rbind(ndatall,bdatall)

p <- ggplot(nbdatall, aes(factor(class), accX))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("X Acceleration s/m^2")

p <- ggplot(nbdatall, aes(factor(class), accY))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("Y Acceleration s/m^2")

p <- ggplot(nbdatall, aes(factor(class), accZ))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("Z Acceleration s/m^2")

p <- ggplot(nbdatall, aes(factor(class), roll))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("Roll Angle")
p <- ggplot(nbdatall, aes(factor(class), pitch))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("Pitch Angle")
p <- ggplot(nbdatall, aes(factor(class), yaw))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("Yaw Angle")

ggplot(ndatall, aes(x=nbdatall$accX, colour=class)) + geom_density()

ggplot(data=ndatall, aes(ndatall$accX)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count")


newSP<-rep(0,length(ndatgp$speedInMS)*200)
for (i in 1:length(ndatgp$speedInMS)){  
  oldSP<-approx(1:2,ndatgp$speedInMS[i:(i+1)],n = 200)
  print(i)
  if(i==1){
    newSP<-rbind(newSP,oldSP)
    newSP<-newSP[-1]
  }

  if(i!=1){
    newSP<-rbind(newSP,newSP)
  } 
}

test<-approx(1:length(ndatgp$speedInMS),ndatgp$speedInMS,n = 200)
test

ndatacc$internal_clock[1]
ndatgp$time_as_string[1]



names=c("X","Y","Z")
mylist<-list(ndatacc$accX,ndatacc$accY,ndatacc$accZ)
makeProfilePlot(mylist,names)

newNorm.03<-convSpeed(ndatacc,ndatgp,forces=6)
newNorm.24<-convSpeed(ndatacc,ndatgp,forces=6)
convSpeed(bdatacc,bdatgp,forces=6)


NN300.conv<-convSpeed(ndatacc,ndatgp,forces=6)
RR299.conv<-convSpeed(bdatacc,bdatgp,forces=6)
bdatgp$time_as_string[1]
bdatacc$time_as_string[1]


plot(case3x.classres.5.n$votes[,1],allData$yawSp50[as.integer(row.names(case3x.classres.5.n$votes))]*3.6
     ,type="p",pch)


matplot(cbind(case3x.classres.5.n$votes[,1]
              ,case3x.classres.5.n$votes[,2]
              ,case3x.classres.5.n$votes[,3]
              ,case3x.classres.5.n$votes[,4]
              ,case3x.classres.5.n$votes[,5]),
        case3x.5.n.clean$accXminSp[as.integer(row.names(case3x.classres.5.n$votes))]*3.6
        ,ylab = "Speed in KM/h",xlab="Votes",type="p",pch=paste(1:5),col=1:5)
legend("bottomright", inset=.05, legend=c("LF","LR","NN","RF","RR"),, horiz=TRUE,pch=paste(1:5),col=1:5)

p3 <- ggplot(votes, 
             aes(x=case3x.5.n.clean$accXminSp[as.integer(row.names(case3x.classres.5.n$votes))]*3.6, colour=1:5)) +
  geom_density() +
  ggtitle("Final weight, by diet")


p1 <- qplot(votes, case3x.5.n.clean$accXminSp[as.integer(row.names(case3x.classres.5.n$votes))]*3.6
            , data=case3x.5.n.clean, colour=1:5)

n<-length(case3x.classres.5.n$votes[,1])
maxVote<-numeric(n)
minVote<-numeric(n)
for (i in 1:n){
  maxVote[i]<-max(case3x.classres.5.n$votes[i,])
  minVote[i]<-min(case3x.classres.5.n$votes[i,])
}

plot(maxVote,case3x.5.n.clean$accXminSp[as.integer(row.names(case3x.classres.5.n$votes))]*3.6
     ,ylab="Speed in KM/h",xlab="Fraction of votes")
plot(minVote,case3x.5.n.clean$accXminSp[as.integer(row.names(case3x.classres.5.n$votes))]*3.6)

dat<-(case3x.5.n.clean$accXminSp[as.integer(row.names(case3x.classres.5.n$votes))]*3.6)
summary(lm(maxVote~log(dat)))

plot(maxVote,log(dat))

plot(case3x.classres.5.n$votes[,3]
     ,case3x.5.n.clean$accXminSp[as.integer(row.names(case3x.classres.5.n$votes))]*3.6)

plot(case3x.classres.5.n$votes[,2]
     ,case3x.5.n.clean$accXminSp[as.integer(row.names(case3x.classres.5.n$votes))]*3.6)

plot(case3x.5.n.clean$accXSp50*3.6)

hist(case3x.5.n.clean$accXSp50)

case3x.5.n.clean$accXSp75==case3x.5.n.clean$accXSp50

plot(case3x.5.n.clean$accXSp75*3.6)
names(case3x.5.n.clean)[25:30]
plot(case3x.5.n.clean[25:30])
plot(case3x.5.n.clean$accXminSp[])

plot(allData$yawminSp*3.6,main="min")
plot(allData$yawSp75*3.6,main="p75")
plot(allData$yawSp50*3.6,main="p50")
plot(allData$yawSp25*3.6,main="p25")

plot(case3x.5.n.clean$accXminSp*3.6)


plot(allData$yawSp50)

test.ml<-confusion(case3x.reRun.case3$class,prediction)


image(test$table)
heatmap(test$table,Rowv=NA,Colv=NA,col=heat.colors(256))


####################################################################################################
# CASE3x CASE3x CASE3x CASE3x CASE3x CASE3x CASE3x CASE3x CASE3x CASE3x CASE3x CASE3x CASE3x CASE3x 
####################################################################################################

## Importing data
RF290acc<-read.csv("/Users/Maxim/Dropbox/private/kandidatuddannelsen/danmark/MSc Thesis - Speciale/Data/02_Case3x_csv/290RFacc.csv",header = F)
RF290gp<-read.csv("/Users/Maxim/Dropbox/private/kandidatuddannelsen/danmark/MSc Thesis - Speciale/Data/02_Case3x_csv/290RFgp.csv",header = F)
colnames(RF290acc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")
colnames(RF290gp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")

LF291acc<-read.csv("/Users/Maxim/Dropbox/private/kandidatuddannelsen/danmark/MSc Thesis - Speciale/Data/02_Case3x_csv/291LFacc.csv",header = F)
LF291gp<-read.csv("/Users/Maxim/Dropbox/private/kandidatuddannelsen/danmark/MSc Thesis - Speciale/Data/02_Case3x_csv/291LFgp.csv",header = F)
colnames(LF291acc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")
colnames(LF291gp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")


LR298acc<-read.csv("/Users/Maxim/Dropbox/private/kandidatuddannelsen/danmark/MSc Thesis - Speciale/Data/02_Case3x_csv/298LRacc.csv",header = F)
LR298gp<-read.csv("/Users/Maxim/Dropbox/private/kandidatuddannelsen/danmark/MSc Thesis - Speciale/Data/02_Case3x_csv/298LRgp.csv",header = F)
colnames(LR298acc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")
colnames(LR298gp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")


RR299acc<-read.csv("/Users/Maxim/Dropbox/private/kandidatuddannelsen/danmark/MSc Thesis - Speciale/Data/02_Case3x_csv/299RRacc.csv",header = F)
RR299gp<-read.csv("/Users/Maxim/Dropbox/private/kandidatuddannelsen/danmark/MSc Thesis - Speciale/Data/02_Case3x_csv/299RRgp.csv",header = F)
colnames(RR299acc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")
colnames(RR299gp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")

NN300acc<-read.csv("/Users/Maxim/Dropbox/private/kandidatuddannelsen/danmark/MSc Thesis - Speciale/Data/02_Case3x_csv/300NNacc.csv",header = F)
NN300gp<-read.csv("/Users/Maxim/Dropbox/private/kandidatuddannelsen/danmark/MSc Thesis - Speciale/Data/02_Case3x_csv/300NNgp.csv",header = F)
colnames(NN300acc) <-c("type","version", "vehicle_id", "trip_id", "comport", "sensor_id", "accX", "accY", "accZ", "roll", "pitch", "yaw", "seconds","weeknumber","time_as_string",  "gps_flag", "internal_clock")
colnames(NN300gp) <-c("type", "version", "vehicle_id", "trip_id", "comport", "latitude", "lontitude" , "horizontalAcc" , "altitude" , "altitudeAcc", "speedInMS", "speedAcc", "heading", "headingAcc", "seconds" , "weeknumber" , "time_as_string","validData")

NN300<-cbind(NN300acc,as.factor(rep("NN",length(NN300acc$accX))))
names(NN300)[18]<-"class"
RR299<-cbind(RR299acc,as.factor(rep("RR",length(RR299acc$accX))))
names(RR299)[18]<-"class"
LR298<-cbind(LR298acc,as.factor(rep("LR",length(LR298acc$accX))))
names(LR298)[18]<-"class"
LF291<-cbind(LF291acc,as.factor(rep("LF",length(LF291acc$accX))))
names(LF291)[18]<-"class"
RF290<-cbind(RF290acc,as.factor(rep("RF",length(RF290acc$accX))))
names(RF290)[18]<-"class"

allC3datacc<-rbind(
  NN300,RR299,LR298,LF291,RF290
)

NN300gp<-cbind(NN300gp,as.factor(rep("NN",length(NN300gp$speedInMS))))
names(NN300gp)[19]<-"class"
RR299gp<-cbind(RR299gp,as.factor(rep("RR",length(RR299gp$speedInMS))))
names(RR299gp)[19]<-"class"
LR298gp<-cbind(LR298gp,as.factor(rep("LR",length(LR298gp$speedInMS))))
names(LR298gp)[19]<-"class"
LF291gp<-cbind(LF291gp,as.factor(rep("LF",length(LF291gp$speedInMS))))
names(LF291gp)[19]<-"class"
RF290gp<-cbind(RF290gp,as.factor(rep("RF",length(RF290gp$speedInMS))))
names(RF290gp)[19]<-"class"

allC3datgp<-rbind(
  NN300gp,RR299gp,LR298gp,LF291gp,RF290gp)
names(allC3datgp)
hexbin()
library(hexbin)
plot(hexbin(allC3datgp$speedInMS*3.6))
qplot(allC3datacc$accX, data=allC3datacc, geom="histogram")

qplot(factor(class), accX, data = allC3datacc, geom = "boxplot",)
p <- ggplot(allC3datacc, aes(factor(class), accX))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("X Acceleration s/m^2")

p <- ggplot(allC3datacc, aes(factor(class), accY))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("Y Acceleration s/m^2")

p <- ggplot(allC3datacc, aes(factor(class), accZ))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("Z Acceleration s/m^2")

p <- ggplot(allC3datacc, aes(factor(class), roll))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("Roll Angle")
p <- ggplot(allC3datacc, aes(factor(class), pitch))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("Pitch Angle")
p <- ggplot(allC3datacc, aes(factor(class), yaw))
p + geom_boxplot(aes(fill = class)) + xlab("class") + ylab("Yaw Angle")

dat <- data.frame(xx = c(runif(100,20,50),runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))

ggplot(allC3datacc,aes(x=pitch)) + 
  geom_histogram(data=subset(allC3datacc,class == 'NN'),fill = "red", alpha = 0.2,binwidth=0.01) +
  geom_histogram(data=subset(allC3datacc,class == 'RF'),fill = "blue", alpha = 0.2,binwidth=0.01) +
  geom_histogram(data=subset(allC3datacc,class == 'LF'),fill = "green", alpha = 0.2,binwidth=0.01) +
  geom_histogram(data=subset(allC3datacc,class == 'LR'),fill = "purple", alpha = 0.2,binwidth=0.01) +
  geom_histogram(data=subset(allC3datacc,class == 'RR'),fill = "red", alpha = 0.2,binwidth=0.01) 

test.arima<-fitArimaModels(ndatacc,ndatgp,order=3,forces = 1,windowSize=200)

## Graphs as of 16JUL2015
g.dates<-convDates(ndatacc,acc = T)

x.dacc<-cbind(g.dates,ndatacc$accX)
x.dacc<-as.data.frame(x.dacc) 
colnames(x.dacc)<-c("date","acc")
x.dacc[1:2,1:2]

ggplot(x.dacc[1:20000,], aes(date, acc)) + geom_line() +
  scale_x_date(format = "%YYYY-%mm-%dd %HH:%mm:%ss:%ms") + xlab("") + ylab("X Acceleration m/s^2")
ts.plot(x.dacc$acc[1:2000])
plot(x.dacc$acc[1:2000])

ts.plot(ndatacc$accX[1:2500])

abline(v=800,lty=2)
n<-1000
# x<-as.POSIXct(x)
x<-as.POSIXct(g.dates[1:n])
y<-ndatacc$accX[1:n]
plot(x,y,type="l",xaxt = "n",frame.plot="F",yaxt = "n",xlab="")
# lines(x=x,y=ndatacc$accY[1:n],pch = 2)
for(i in 1:(n/200)){
if(i!=(n/200)){
  abline(v=x[200*i],lty=2,lwd=3,col="blue")  
}
}







