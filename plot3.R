
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

emission_baltimore_yearly_type<-summarySCC_PM25%>%
  filter(fips=="24510")%>% # Filtering Baltimore
  group_by(year,type)%>%
  summarise(
    TotalPM2.5Emission_Thousand_Tons = round(sum(Emissions)/1000,2)
  )%>%
  filter(year %in% c(1999,2008)) # Filtering only 1999 and 2008


### Plot

png("plot3.png")

ggplot(emission_baltimore_yearly_type,aes(x = type,y=TotalPM2.5Emission_Thousand_Tons,fill=as.factor(year)))+
  geom_bar(position = "dodge",stat = "identity")+
  geom_text(aes(y=0.1+TotalPM2.5Emission_Thousand_Tons,label=TotalPM2.5Emission_Thousand_Tons),position = position_dodge(width=1))+
  labs(
    fill="Year",
    x="Types of source",
    y="Total PM2.5 emission (Thousand Tons)",
    title="Total PM2.5 emission by source over the years in Baltimore"   ,
    subtitle="Ans: Non-Road,NonPoint and On-Road saw decrease\nPoint sources saw increase"
  )+
  theme(
    plot.subtitle = element_text(color="red")
  )


dev.off()

