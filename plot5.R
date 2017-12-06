
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

# Getting vehicle sources i.e SSC.Level.Two matching the string 'vehicles'
# Motorcycles also included 

motor_vehicle_sources<-Source_Classification_Code%>%
  filter(grepl(tolower(Source_Classification_Code$SCC.Level.Two),pattern="vehicles")==TRUE )%>%
  select(SCC)





emission_motor_vehicles_baltimore<-summarySCC_PM25%>%
  filter(fips=="24510")%>%  # Filtering Baltimore
  filter(SCC %in% motor_vehicle_sources$SCC)%>% # Filtering only vehicle sources
  group_by(year)%>%
  summarise(
    TotalPM2.5Emission_Tons = round(sum(Emissions),1)
  )


### Plot

png("plot5.png")

bp_5<-barplot(height = emission_motor_vehicles_baltimore$TotalPM2.5Emission_Tons,
              names=emission_motor_vehicles_baltimore$year,
              col="lightgreen",
              main="Total PM2.5 emission from motor vehicles in Baltimore \n(Includes all vehicles including motorcycles)"   ,
              sub="Ans: Emission decreased by around 75% from 1999 to 2008 and has decreased in every 3 years",
              xlab = "Year",
              ylab="Total PM2.5 emission (Tons)",
              ylim=c(0,400),
              cex.sub=0.8,
              col.sub='red'
)

text(x=bp_5,y=emission_motor_vehicles_baltimore$TotalPM2.5Emission_Tons,
     labels = emission_motor_vehicles_baltimore$TotalPM2.5Emission_Tons,
     pos = 3, cex = 0.8, col = "blue"
)

dev.off()


