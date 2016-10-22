# Scatterplot matrix of DMC,DC,wind,rain,temp

png(file="E:/R/saving_plot1.png",
    width=600, height=350)
pairs(~DMC+DC+wind+rain+temp,data=df, 
      main="Simple Scatterplot Matrix")
dev.off()



# 3D Scatterplot of wind,rain,area

attach(df)
library(scatterplot3d)

png(file="E:/R/3Dscatter plot.png",
    width=600, height=350)
scatterplot3d(wind,rain,area, main="3D Scatterplot")
dev.off()

# Interactive 3D Scatterplot of wind,rain,area

library(rgl)
png(file="E:/R/Interactive 3Dscatter plot.png",
    width=600, height=350)
plot3d(wind, rain, area, col="red", size=3)
dev.off()

# Boxplot of X and Y

png(file="E:/R/Box plot.png",
    width=600, height=350)
boxplot(X~Y,data=df, main="Boxplot", 
        xlab="X", ylab="Y")
dev.off()

# Simple bar plot of temp, wind, rain [horizontal and vertical]

png(file="E:/R/Simple bar plot_temp.png",
    width=600, height=350)
counts <- table(df$temp)
barplot(counts, main="Temperature Distribution", 
        xlab="Temp")
dev.off()


png(file="E:/R/Simple bar plot_wind.png",
    width=600, height=350)
counts <- table(df$wind)
barplot(counts, main="Temperature Distribution", 
        xlab="Wind")
dev.off()

png(file="E:/R/Simple bar plot_rain.png",
    width=600, height=350)
counts <- table(df$rain)
barplot(counts, main="Temperature Distribution", 
        xlab="Rain")
dev.off()

# Grouped bar plot of X and Y

png(file="E:/R/Grouped bar plot_Xandy.png",
    width=600, height=350)
counts <- table(df$X, df$Y)
barplot(counts, main="Distribution by X and Y",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
dev.off()

# Histogram of probability distribution of X, Y, wind, temp, area along with line density

png(file="E:/R/Histogram_probability_distribution.png",
    width=600, height=350)
hist(df$wind, 
     main="Histogram for Air Passengers", 
     xlab="Passengers", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(df$wind))
dev.off()

# Histogram of frequency distribution of X, Y, wind, temp, area

png(file="E:/R/Histogram_frequency_distribution.png",
    width=600, height=350)
hist(df$wind, 
     main="Histogram for Air Passengers", 
     xlab="Passengers", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = FALSE)
lines(density(df$wind))
dev.off()

# Pie Chart of area, wind, rain, temp by month

library(dplyr)

png(file="E:/R/Pie_Chart_wind_bymonth.png",
    width=600, height=350)
df_pivot <- summarize(group_by(df,month),wind=sum(wind))
slices <- df_pivot[["wind"]] #c(10, 12,4, 16, 8)
pie(slices, labels=df[["month"]], main="Pie Chart of Wind")
dev.off()

png(file="E:/R/Pie_Chart_area_bymonth.png",
    width=600, height=350)
df_pivot <- summarize(group_by(df,month),area=sum(area))
slices <- df_pivot[["area"]] #c(10, 12,4, 16, 8)
pie(slices, labels=df[["month"]], main="Pie Chart of Area")
dev.off()

png(file="E:/R/Pie_Chart_rain_bymonth.png",
    width=600, height=350)
df_pivot <- summarize(group_by(df,month),rain=sum(rain))
slices <- df_pivot[["rain"]] #c(10, 12,4, 16, 8)
pie(slices, labels=df[["month"]], main="Pie Chart of Rain")
dev.off()

df_pivot <- summarize(group_by(df,month),temp=sum(temp))
slices <- df_pivot[["temp"]] #c(10, 12,4, 16, 8)
pie(slices, labels=df[["month"]], main="Pie Chart of temp")

# Pie Chart of area, wind, rain, temp by day

df_pivot <- summarize(group_by(df,day),wind=sum(wind))
slices <- df_pivot[["wind"]] #c(10, 12,4, 16, 8)
pie(slices, labels=df[["day"]], main="Pie Chart of Wind")

df_pivot <- summarize(group_by(df,day),area=sum(area))
slices <- df_pivot[["area"]] #c(10, 12,4, 16, 8)
pie(slices, labels=df[["day"]], main="Pie Chart of Area")

df_pivot <- summarize(group_by(df,day),rain=sum(rain))
slices <- df_pivot[["rain"]] #c(10, 12,4, 16, 8)
pie(slices, labels=df[["day"]], main="Pie Chart of Rain")

df_pivot <- summarize(group_by(df,day),temp=sum(temp))
slices <- df_pivot[["temp"]] #c(10, 12,4, 16, 8)
pie(slices, labels=df[["day"]], main="Pie Chart of temp")

# Map Plot of sourceAirportID
airports <- read.csv("E:/R/class examples/airports.dat")
colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")

routes <- read.csv("E:/R/class examples/routes.dat")
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")

library(plyr)
departures <- ddply(routes, .(sourceAirportID), "nrow")
names(departures)[2] <- "flights"
arrivals <- ddply(routes, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flights"

airportA <- merge(airports, departures, by.x = "ID", by.y = "sourceAirportID")

