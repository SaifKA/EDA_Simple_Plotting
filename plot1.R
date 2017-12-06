
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

emission_yearly<-summarySCC_PM25%>%
    group_by(year)%>%
    summarise(
        TotalPM2.5Emission_Mn_Tons = round(sum(Emissions)/10^6,1)
    )

### Plot

png("plot1.png")

bp<-barplot(height = emission_yearly$TotalPM2.5Emission_Mn_Tons,
            names=emission_yearly$year,
            col="lightgreen",
            main="Total PM2.5 emission over the year"   ,
            sub="Answer: Yes. Total emission decreased by around 52%",
            xlab = "Year",
            ylab="Total PM2.5 emission (Mn Tons)",
            ylim=c(0,8),
            cex.sub=0.8,
            col.sub='red'
)

text(x=bp,y=emission_yearly$TotalPM2.5Emission_Mn_Tons,
     labels = emission_yearly$TotalPM2.5Emission_Mn_Tons,
     pos = 3, cex = 0.8, col = "blue"
)

dev.off()


