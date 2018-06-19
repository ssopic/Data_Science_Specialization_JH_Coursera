#Second peer asessment
#Plot2.R
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

#Question 2
###################################################################
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips=="24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
aggBaltimore <- aggregate(Emissions~year,subset(total, fips=="24510"), sum)
with(aggBaltimore,(plot(Emissions/10^6,
                        pch=15,
                           col=Emissions,
                        xlab="",
                           ylab="PM2.5 Emissions",
                           main="Total PM2.5 Emission in Baltimore per year", type="b")))
legend("topright", pch=15, col=c("red", "steelblue2", "green", "purple"), legend=c("1999","2002","2005","2008"))
#Save graph
dev.copy(png, file="plot2.png") #copy my plot to a png file
dev.off() #Don’t forget to close the png file device
