#Second peer asessment
#Plot3.R
setwd("C:\\Users\\ssopic\\Desktop\\Coursera\\4. Exploratory data analysis\\Second assignment")
#Getting the data
zipurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(zipurl, destfile ="C:\\Users\\ssopic\\Desktop\\Coursera\\4. Exploratory data analysis\\Second assignment\\data.zip")
unzip("data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
intersect(names(NEI), names(SCC))
#These datasets connect on only one variable so we merge them with the SCC
total <- merge(NEI, SCC, by="SCC")
#Removing the old datasets
rm(NEI)
rm(SCC)

#Observing the data and its variables
###################################################################
#SCC
#Was used in merging the tables
str(total$SCC)
#fips, a string indicating the county
str(total$fips)
#Emission variable
str(total$Emissions)
#Type variable
str(total$type) #Shows the type of the observations
total$type <- as.factor(total$type) #Changing the variable into a factor variable
#looking at the year variable and changing it into a date variable
names(total)
total$year <- as.Date(as.character(total$year), "%Y")

#Removed data(more info on removed data available in document called extra)
total$Data.Category <- NULL
total$Short.Name <- NULL
total$EI.Sector <- NULL
total$Option.Group <- NULL
total$Option.Set <- NULL
total$Pollutant <- NULL
total$Map.To <- NULL
total$Last.Inventory.Year <- NULL
total$Created_Date <- NULL
total$Revised_Date <- NULL
total$Usage.Notes <- NULL

#Question 3
###################################################################
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
library(ggplot2)
#creating the dataframe
names(totalBaltimore)
totalBaltimore <- aggregate(Emissions~year+type,data=subset(total, fips=="24510"), sum)
g <- (ggplot(totalBaltimore,aes(year,Emissions)))
g+geom_point(color="steelblue", size=5) + facet_grid(.~type) + labs(title="Baltimore city, emissions by source type", x="Source type, divided according to years", y=expression( "Emissions of "*PM[2.5]))
ggsave("C:\\Users\\ssopic\\Desktop\\Coursera\\4. Exploratory data analysis\\Second assignment\\plot3.png")
