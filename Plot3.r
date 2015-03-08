?parcheck<-function(a){                                         ##checking for missing value,function to remove "?"
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

subs[,7]<-as.numeric(as.character(subs[,7]))
subs[,8]<-as.numeric(as.character(subs[,8]))
subs[,9]<-as.numeric(as.character(subs[,9]))
png(file="Plot3.png",width=480,height=480)
plot(DnT,subs[,7],col="black",type="l",xlab="",ylab="Energy sub metering")
points(DnT,subs[,8],col="Red",type="l")
points(DnT,subs[,9],col="Blue",type="l")
legend("topright",legend=c("sub_metering_1","sub_metering_2","sub_metering_3"),col=c("Black","Red","Blue"),lwd=2)
dev.off()