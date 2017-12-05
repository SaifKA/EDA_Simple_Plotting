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

###Week-2

library(swirl)
swirl()


### Week-4

fileUrl<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

download.file(url=fileUrl ,destfile ="./data/AirQuality.zip")

unzip(zipfile = "./data/AirQuality.zip")


dir()

# Load required libraries

library(dplyr)
library(ggplot2)

# Read RDS

summarySCC_PM25<-readRDS(file = "summarySCC_PM25.rds")

head(summarySCC_PM25)
str(summarySCC_PM25)



colSums(is.na(summarySCC_PM25))

Source_Classification_Code<-readRDS("Source_Classification_Code.rds")
head(Source_Classification_Code)
str(Source_Classification_Code)



# Q1

emission_yearly<-summarySCC_PM25%>%
    group_by(year)%>%
    summarise(
        TotalPM2.5Emission_Mn_Tons = round(sum(Emissions)/10^6,1)
    )


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


# Q2

emission_baltimore_maryland_yearly<-summarySCC_PM25%>%
    filter(fips=="24510")%>%
    group_by(year)%>%
    summarise(
        TotalPM2.5Emission_Thousand_Tons = round(sum(Emissions)/1000,1)
    )%>%
    filter(year %in% c(1999,2008))


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



#Q3


emission_baltimore_yearly_type<-summarySCC_PM25%>%
    filter(fips=="24510")%>%
    group_by(year,type)%>%
    summarise(
        TotalPM2.5Emission_Thousand_Tons = round(sum(Emissions)/1000,2)
    )%>%
    filter(year %in% c(1999,2008))


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

#Q4



coal_combustion_sources<-Source_Classification_Code%>%
    filter(grepl(tolower(SCC.Level.One),pattern="combustion")==TRUE & ((grepl(tolower(SCC.Level.Three),pattern="coal")==TRUE) | grepl(tolower(SCC.Level.Four),pattern="coal")==TRUE))%>%
    select(SCC)


emission_coal_combustion_yearly<-summarySCC_PM25%>%
    filter(SCC %in% coal_combustion_sources$SCC)%>%
    group_by(year)%>%
    summarise(
        TotalPM2.5Emission_Thousand_Tons = round(sum(Emissions)/1000,1)
    )


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


#Q5

motor_vehicle_sources<-Source_Classification_Code%>%
    filter(grepl(tolower(Source_Classification_Code$SCC.Level.Two),pattern="vehicles")==TRUE )%>%
    select(SCC)





emission_motor_vehicles_baltimore<-summarySCC_PM25%>%
    filter(fips=="24510")%>%
    filter(SCC %in% motor_vehicle_sources$SCC)%>%
    group_by(year)%>%
    summarise(
        TotalPM2.5Emission_Tons = round(sum(Emissions),1)
    )


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


#Q6


emission_motor_vehicles_baltimore_laCounty<-summarySCC_PM25%>%
    filter(fips %in% c("24510","06037"))%>%
    filter(SCC %in% motor_vehicle_sources$SCC)%>%
    group_by(year,fips)%>%
    summarise(
        TotalPM2.5Emission_Tons = round(sum(Emissions),1)
    )%>%
    mutate(
        city=ifelse(fips=='24510','Baltimore','Los Angeles County')
    )%>%
    filter(year %in% c(1999,2008))



png("plot6.png")

ggplot(emission_motor_vehicles_baltimore_laCounty,aes(x = city,y=TotalPM2.5Emission_Tons,fill=as.factor(year)))+
    geom_bar(position = "dodge",stat = "identity")+
    geom_text(aes(y=200+TotalPM2.5Emission_Tons,label=TotalPM2.5Emission_Tons),position = position_dodge(width=1))+
    labs(
        fill="Year",
        x="City",
        y="Total PM2.5 emission (Tons)",
        title="Total PM2.5 emission by source over the years in Baltimore"   ,
        subtitle="Ans: Baltimore has seen greater changes over time in terms of emission by vehicles"
    )+
    theme(
        plot.subtitle = element_text(color="red")
    )+
    annotate("segment",x=c(0.7,0.7,1.2),xend = c(0.7,1.2,1.2),y = c(800,1000,1000),yend = c(1000,1000,400))+
    annotate("segment",x=c(1.7,1.7,2.2),xend = c(1.7,2.2,2.2),y = c(4300,4800,4800),yend = c(4800,4800,4400))+
    annotate("text",x = 1,y=1200,label="% Change= -74.5%",size=3)+
    annotate("text",x = 2,y=5000,label="% Change= + 4.3%",size=3)

dev.off()





