require(RCurl)
library(dplyr)
library(ggplot2)

#import and read data from Google Spreadsheet
myCsv <- getURL("https://docs.google.com/spreadsheets/d/1PUZlBLgRGI-6hnDF_hLcn8hmxksIBIp6VZEncgt52wk/pub?output=csv")
RoadRatings <- read.csv(textConnection(myCsv), stringsAsFactors = FALSE)

#Ratings in the City should only be from 1-10, anything else is a type or inaccurate, so filter out
RoadRatings <- filter(RoadRatings, Overall <=10 & Overall >= 1)

#Count how many road blocks there are per road rating category, then create a data frame
RoadsPerRating <- data.frame(table(RoadRatings$Overall))
names(RoadsPerRating) <- c("Rating", "NumRoads") #rename columns so they are easier to understand
RoadsPerRating$Rating <- as.numeric(RoadsPerRating$Rating) #convert rating to numeric data type

#This step is in preparation for the data visualization. Categorize the road ratings into good, fair, and poor
RoadsPerRating$Status <- "Fair"
RoadsPerRating$Status[RoadsPerRating$Rating > 7] <- "Good"
RoadsPerRating$Status[RoadsPerRating$Rating < 6] <- "Poor"
RoadsPerRating$Status <- factor(RoadsPerRating$Status, levels = c("Poor", "Fair", "Good"))

#visualize the data using ggplot
ggplot(RoadsPerRating, aes(Rating, NumRoads, fill = Status)) + #calls out data and aesthetics that should be used (in aes(x-axis, y-axis, extra information))
  geom_bar(stat="identity") + #indicates we will create a bar chart. "identity" makes the heights of the bars equal the value you are using on the y-axis
  scale_fill_manual(values=c("red", "yellow", "green")) + #determine color of each bar (this is optional but makes sense for good, fair, poor)
  labs(title="Distribution of Roads by Rating", #name the title and axes
       x = "Rating",
       y = "Number of Road Blocks") +
  scale_x_continuous(breaks = c(0:10)) + # make a tick mark at every Rating value
  theme_bw() #take out the background color
