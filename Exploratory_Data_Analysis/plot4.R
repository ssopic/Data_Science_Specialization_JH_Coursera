#Second peer asessment
#Plot4.R
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

#Question 4
##################################################################
#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

#We need to subset data according to the SSC variables
levels(total$SCC.Level.One)
levels(total$SCC.Level.Two)
levels(total$SCC.Level.Three)
levels(total$SCC.Level.Four)


#First we have to find coal related variables and then the combustion variables

##Coal
#all false, removing variable
coal1 <- grepl("coal", total$SCC.Level.One, ignore.case = TRUE)
table(coal1)
rm(coal1)

#all false, removing variable
coal2 <- grepl("coal", total$SCC.Level.Two, ignore.case = TRUE)
table(coal2)
rm(coal2)

#contains 51079 trues, keeping the variable
coal3 <- grepl("coal", total$SCC.Level.Three, ignore.case = TRUE)
table(coal3)

#contains 10935 trues, keeping the variable
coal4 <- grepl("coal", total$SCC.Level.Four, ignore.case= TRUE)
table(coal4)

##Combust
#contains 384671 trues, keeping the variable
combust1 <- grepl("combust", total$SCC.Level.One, ignore.case = TRUE)
table(combust1)

#contains 27794 true, keeping the variable
combust2 <- grepl("Combust", total$SCC.Level.Two, ignore.case = TRUE)
table(combust2)

#Contains no trues, removing the variable
combust3 <- grepl("Combust", total$SCC.Level.Three, ignore.case = TRUE)
table(combust3)
rm(combust3)

#Contains 60383 true, keeping the variable
combust4 <- grepl("combust", total$SCC.Level.Four, ignore.case = TRUE)
table(combust4)

#Now we need to combine all of these logical vectors into one to be able to apply to the full dataset
#I decided to concatinate the coal ones and the combust ones(with an or logical symbol) and then use the "and" logic between them. This returned with 40707 trues
coalcombust <- (c(coal3 | coal4) & c(combust1 | combust2 | combust4))
table(coalcombust)

#removing unneeded variables
rm(coal3)
rm(coal4)
rm(combust1)
rm(combust2)
rm(combust4)

#Creating the data frame
totalCoal <- total[coalcombust,]
str(totalCoal)
agg.coal <- aggregate(Emissions~year,totalCoal,sum)

#Creating the plot
gg <- ggplot(agg.coal,aes(year, Emissions/10^6))
gg+geom_bar(stat="identity")+labs(title="Coal combustion related sources through the years", x="Years", y=expression("Aggregated " *PM[2.5]))
ggsave("C:\\Users\\ssopic\\Desktop\\Coursera\\4. Exploratory data analysis\\Second assignment\\plot4.png")
