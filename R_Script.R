getwd()
setwd("D:\\Shiny\\Coursera")


if(!file.exists("4. EDA")){
  dir.create("4. EDA")
  
}

setwd("D:\\Shiny\\Coursera\\4. EDA")

if(!file.exists("data")){
  dir.create("data")
  
}

dir()


### Download Data

download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile = "./data/HH_power_consumption.zip")


df<-read.table(unz("./data/HH_power_consumption.zip","household_power_consumption.txt"),header = T,sep=";",na.strings = "?")

dim(df)
head(df$Date)


### Store Raw data for time and date conversion
df1<-df


df1$datetime<-paste0(df1$Date,df1$Time,sep=" ")

df1$datetime<-strptime(df1$datetime,"%d/%m/%Y %H:%M:%S")

summary(df1$datetime)

### Final Data to work on

df2<-df1[which(df1$datetime>='2007-02-01' & df1$datetime<'2007-02-03'),]

dim(df2)


png(filename = "plot1.png",width=480,height = 480)

hist(x = df2$Global_active_power,col= 'red',main="Global Active Power", xlab = "Global Active Power (kilowatts")

dev.off()

png(filename = "plot2.png",width=480,height = 480)

plot(x = df2$datetime,y=df2$Global_active_power,type = "l")

dev.off()


png(filename = "plot3.png",width=480,height = 480)

with(df2,{
  plot(x = df2$datetime,y=df2$Sub_metering_1,type = "l")
  lines(x = df2$datetime,y=df2$Sub_metering_2,type = "l",col='red')
  lines(x = df2$datetime,y=df2$Sub_metering_3,type = "l",col='blue')
  legend(x="topright", 0.9, legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),
         col=c("black","red","blue"),lty = 1)
}
)

dev.off()


png(filename = "plot4.png",width=480,height = 480)


par(mfrow=c(2,2))

with(df2,{
  plot(x = df2$datetime,y=df2$Global_active_power,type = "l")
  plot(x = df2$datetime,y=df2$Voltage,type = "l")
  plot(x = df2$datetime,y=df2$Sub_metering_1,type = "l")
  lines(x = df2$datetime,y=df2$Sub_metering_2,type = "l",col='red')
  lines(x = df2$datetime,y=df2$Sub_metering_3,type = "l",col='blue')
  legend(x="topright", 0.9, legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),
         col=c("black","red","blue"),lty = 1)
  plot(x = df2$datetime,y=df2$Global_reactive_power,type = "l")
}
)

dev.off()





