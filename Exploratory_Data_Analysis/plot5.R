#Second peer asessment
#Plot5.R
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

#Question 5
##################################################################
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

#Finding the locations of motor in the SCC levels
#motor1 and motor 2 returned all false while motor3 and motor 4 returned 237763 and 147817 trues respectively.
motor1 <- grepl("motor", total$SCC.Level.One, ignore.case = TRUE)
table(motor1)
rm(motor1)
motor2 <- grepl("motor", total$SCC.Level.Two, ignore.case = TRUE)
table(motor2)
rm(motor2)
motor3 <- grepl("motor", total$SCC.Level.Three, ignore.case = TRUE)
table(motor3)
motor4 <- grepl("motor", total$SCC.Level.Four, ignore.case=TRUE)
table(motor4)

#Finding the locations of vehicle in the SCC levels
#level one contains all falses, removing the variable
#level two contains 4930195 trues, keeping the variable
#level three contains 1880053 trues, keeping the variable
#level four contains 45708 trues, keeping the variable
vehicle1 <- grepl("vehicle", total$SCC.Level.One, ignore.case=TRUE)
table(vehicle1)
rm(vehicle1)
vehicle2 <- grepl("vehicle", total$SCC.Level.Two, ignore.case=TRUE)
table(vehicle2)
vehicle3 <- grepl("vehicle", total$SCC.Level.Three, ignore.case=TRUE)
table(vehicle3)
vehicle4 <- grepl("vehicle", total$SCC.Level.Four, ignore.case=TRUE)
table(vehicle4)

#Creating the vector
#MotoVehicle contains 252445 trues
#removing the unneeded variables(motor3 etc.)
MotoVehicle <- (c(motor3 | motor4) & c(vehicle2 | vehicle3 | vehicle4))
table(MotoVehicle)
rm(motor3)
rm(motor4)
rm(vehicle2)
rm(vehicle3)
rm(vehicle4)

#Creating the variable
totalMoto <- total[MotoVehicle,]
str(totalMoto)
agg.moto <- aggregate(Emissions~year,subset(totalMoto, fips=="24510"),sum)

#plotting the variable
g <- ggplot(totalMoto, aes(year, Emissions/10^6))
g+geom_bar(stat="identity") + labs(title="Motor vehicle emissions in Baltimore", x="Years", y=expression("Aggregated " *PM[2.5]))
ggsave("C:\\Users\\ssopic\\Desktop\\Coursera\\4. Exploratory data analysis\\Second assignment\\plot5.png")
