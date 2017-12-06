
### Download file

if(!file.exists("data")){
  dir.create("data")
  
}



fileUrl<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

download.file(url=fileUrl ,destfile ="./data/AirQuality.zip")

unzip(zipfile = "./data/AirQuality.zip")



### Load required libraries

library(dplyr)
library(ggplot2)

### Read Data from RDS

summarySCC_PM25<-readRDS(file = "summarySCC_PM25.rds")
head(summarySCC_PM25)


Source_Classification_Code<-readRDS("Source_Classification_Code.rds")
head(Source_Classification_Code)



### Aggregate Data

# Getting SCCs with vehicles by matching string 'vehicles' in SCC.Level.Two

motor_vehicle_sources<-Source_Classification_Code%>%
  filter(grepl(tolower(Source_Classification_Code$SCC.Level.Two),pattern="vehicles")==TRUE )%>%
  select(SCC)

emission_motor_vehicles_baltimore_laCounty<-summarySCC_PM25%>%
  filter(fips %in% c("24510","06037"))%>%  # Filter Baltimore and Los Angeles County
  filter(SCC %in% motor_vehicle_sources$SCC)%>%  # Filter only vehicle sources
  group_by(year,fips)%>%
  summarise(
    TotalPM2.5Emission_Tons = round(sum(Emissions),1)
  )%>%
  mutate(
    city=ifelse(fips=='24510','Baltimore','Los Angeles County')
  )%>%
  filter(year %in% c(1999,2008))

### Plot

png("plot6.png")

ggplot(emission_motor_vehicles_baltimore_laCounty,aes(x = city,y=TotalPM2.5Emission_Tons,fill=as.factor(year)))+
  geom_bar(position = "dodge",stat = "identity")+
  geom_text(aes(y=200+TotalPM2.5Emission_Tons,label=TotalPM2.5Emission_Tons),position = position_dodge(width=1))+
  labs(
    fill="Year",
    x="City",
    y="Total PM2.5 emission (Tons)",
    title="Total PM2.5 emission from vehicle sources in Baltimore and Los Angeles"   ,
    subtitle="Ans: Baltimore has seen greater changes over time in terms of emission by vehicles"
  )+
  theme(
    plot.subtitle = element_text(color="red")
  )+
  annotate("segment",x=c(0.7,0.7,1.2),xend = c(0.7,1.2,1.2),y = c(800,1000,1000),yend = c(1000,1000,400))+     #Line to show change
  annotate("segment",x=c(1.7,1.7,2.2),xend = c(1.7,2.2,2.2),y = c(4300,4800,4800),yend = c(4800,4800,4400))+
  annotate("text",x = 1,y=1200,label="% Change= -74.5%",size=3)+  # % Change to show above annotation line
  annotate("text",x = 2,y=5000,label="% Change= + 4.3%",size=3)

dev.off()

