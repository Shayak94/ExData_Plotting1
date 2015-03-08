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
png(file="Plot1.png",width=480,height=480)
hist(GAP,col="Red",main="Global Active Power",xlab="Global Active Power (kilowatts)") ##plotting according to specifications
dev.off()

