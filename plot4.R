
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

# Getting Coal combustion sources i.e. SCC.Level.One matching the string 'combustion'
# and SCC.Level.Three or .Four matching the string 'coal'

coal_combustion_sources<-Source_Classification_Code%>%
  filter(grepl(tolower(SCC.Level.One),pattern="combustion")==TRUE & ((grepl(tolower(SCC.Level.Three),pattern="coal")==TRUE) | grepl(tolower(SCC.Level.Four),pattern="coal")==TRUE))%>%
  select(SCC)



emission_coal_combustion_yearly<-summarySCC_PM25%>%
  filter(SCC %in% coal_combustion_sources$SCC)%>% # Filtering coal combustion sources
  group_by(year)%>%
  summarise(
    TotalPM2.5Emission_Thousand_Tons = round(sum(Emissions)/1000,1)
  )


### Plot

png("plot4.png")

bp_4<-barplot(height = emission_coal_combustion_yearly$TotalPM2.5Emission_Thousand_Tons,
              names=emission_coal_combustion_yearly$year,
              col="lightgreen",
              main="Total PM2.5 emission from coal-combustion sources"   ,
              sub="Ans: Emission decreased by around 40% from 1999 to 2008",
              xlab = "Year",
              ylab="Total PM2.5 emission (Thousand Tons)",
              ylim=c(0,650),
              cex.sub=0.8,
              col.sub='red'
)

text(x=bp_4,y=emission_coal_combustion_yearly$TotalPM2.5Emission_Thousand_Tons,
     labels = emission_coal_combustion_yearly$TotalPM2.5Emission_Thousand_Tons,
     pos = 3, cex = 0.8, col = "blue"
)

dev.off()


