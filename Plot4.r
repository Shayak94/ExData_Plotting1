check<-function(a){                                         ##checking for missing value,function to remove "?"
  if(a=="?"){
    a<-NA
  }
}
ds<-read.table("household_power_consumption.txt",sep=";")  ##Reading and subsetting
ds[,1]<-as.Date(ds[,1],"%d/%m/%Y")
subs<-subset(ds,ds[,1]=="2007-2-1" | ds[,1]=="2007-2-2")
for(i in 2:9){                                             ##Removing all "?" not checking date values as it cannot be missing
  for(j in 1:2880){
    check(subs[j,i])
  }
}
GAP<-as.numeric(as.character(subs[,3]))                    ##Converting factors to numerics
dnt<-paste(subs[,1],subs[,2],sep=" ")                      ##combining date and time values
DnT<-as.POSIXlt(dnt)                                       ##converting to standard format

subs[,7]<-as.numeric(as.character(subs[,7]))               ##Converting submetering1 to numeric (line 18 to 22)
subs[,8]<-as.numeric(as.character(subs[,8]))               ## submetering2
subs[,9]<-as.numeric(as.character(subs[,9]))               ## submetering3
subs[,5]<-as.numeric(as.character(subs[,5]))               ## voltage
subs[,4]<-as.numeric(as.character(subs[,4]))               ##Reactive power
png(file="Plot4.png",width=480,height=480)
par(mfrow=c(2,2))
plot(DnT,GAP,ylab="Global Active Power",xlab="",las=1,type="l") ##Part1
plot(DnT,subs[,5],ylab="Voltage",xlab="datetime",las=1,type="l")     ##Part2
plot(DnT,subs[,7],col="black",type="l",xlab="",ylab="Energy sub metering")  ##Part3
points(DnT,subs[,8],col="Red",type="l")
points(DnT,subs[,9],col="Blue",type="l")
legend("topright",legend=c("sub_metering_1","sub_metering_2","sub_metering_3"),col=c("Black","Red","Blue"),lwd=2)
plot(DnT,subs[,4],ylab="Global_reactive_power",xlab="datetime",type="l")   ##Part4
dev.off()