
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

emission_baltimore_maryland_yearly<-summarySCC_PM25%>%
  filter(fips=="24510")%>%  # Filtering Baltimore
  group_by(year)%>%
  summarise(
    TotalPM2.5Emission_Thousand_Tons = round(sum(Emissions)/1000,1)
  )%>%
  filter(year %in% c(1999,2008)) # Filter only 1999 and 2008

### Plot

png("plot2.png")

bp_2<-barplot(height = emission_baltimore_maryland_yearly$TotalPM2.5Emission_Thousand_Tons,
              names=emission_baltimore_maryland_yearly$year,
              col="lightgreen",
              main="Total PM2.5 emission over the year in Baltimore,Maryland"   ,
              sub="Answer: Yes. Total emission decreased by around 42%",
              xlab = "Year",
              ylab="Total PM2.5 emission (Thousand Tons)",
              ylim=c(0,4),
              cex.sub=0.8,
              col.sub='red'
)

text(x=bp_2,y=emission_baltimore_maryland_yearly$TotalPM2.5Emission_Thousand_Tons,
     labels = emission_baltimore_maryland_yearly$TotalPM2.5Emission_Thousand_Tons,
     pos = 3, cex = 0.8, col = "blue"
)



dev.off()



