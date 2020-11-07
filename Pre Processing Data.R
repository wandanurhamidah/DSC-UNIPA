energy_data <- read.csv("~/DSC/Pertemuan 3/energydata_complete.csv")
head(energy_data)
dim(energy_data)
View(energy_data)

#View(energy_data)
energy_data$date <- strptime(as.character(energy_data$date),format="%Y-%m-%d %H:%M:%S")
energy_data$date <- as.POSIXct(energy_data$date,tz = "UTC")
class(energy_data$date)
str(energy_data)

names(energy_data)

second_day <- function(x) {
  # x is an object in posixct format
  s <- hour(x)*3600+minute(x)*60+second(x)
  
}

weekend_weekday <- function(x) {
  val <- weekdays(x)
  if (val == "Saturday" | val == "Sunday") {
    val2 = "Weekend"
  }
  else {
    val2= "Weekday"
  }
  return(val2)
}
energy_data$WeekStatus <- unlist(lapply(energy_data$date,weekend_weekday))
energy_data$Day_of_week <-weekdays(energy_data$date)
unique(energy_data$WeekStatus)
unique(energy_data$Day_of_week)
class(energy_data$Day_of_week)
energy_data$Day_of_week <-as.factor(energy_data$Day_of_week)
energy_data$WeekStatus <- as.factor(energy_data$WeekStatus)
str(energy_data)
dim(energy_data)
names(energy_data)

# HEAT MAP visualization
# Visualization of the Energy use per week with heat map
library(lubridate)
energy_data$my <- floor_date(energy_data$date,"month")
energy_data$mhr <- floor_date(energy_data$date,"hour")

library(plyr)
energy_data_Total_per_hour <-  ddply(energy_data, "mhr", summarise,
                                     Appliances=sum(Appliances))
energy_data_Total_per_hour
energy_data_Total_per_hour$Day_week <- wday(energy_data_Total_per_hour$mhr,label=TRUE)
head(energy_data_Total_per_hour)
class(energy_data_Total_per_hour)
summary(energy_data_Total_per_hour)
energy_data_Total_per_hour_na_removed <- na.omit(energy_data_Total_per_hour)

dim(energy_data_Total_per_hour)
names(energy_data_Total_per_hour)
dim(energy_data_Total_per_hour_na_removed)
names(energy_data_Total_per_hour_na_removed)
summary(energy_data_Total_per_hour_na_removed)

#Stepwise selection
View(energy_data_Total_per_hour_na_removed)
library(MASS)
library(leaps)
library(caret)
# Fit the full model 
full.model <- lm(Appliances ~., data = energy_data_Total_per_hour_na_removed)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

# getting now the week of the year
energy_data_Total_per_hour_na_removed$week_year <- week(energy_data_Total_per_hour_na_removed$mhr)
head(energy_data_Total_per_hour_na_removed)

unique(energy_data_Total_per_hour_na_removed$week_year)
# [1]  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22
# first week only

energy_data_Total_per_hour_na_removed_w1 <- energy_data_Total_per_hour_na_removed[energy_data_Total_per_hour_na_removed$week_year ==3,]

energy_data_Total_per_hour_na_removed_w1

energy_data_Total_per_hour_na_removed_w1$Hour <- hour(energy_data_Total_per_hour_na_removed_w1$mhr)


names(energy_data_Total_per_hour_na_removed_w1)

library(ggplot2)
library(lubridate)
# "mhr"        "Appliances" "Day_week"   "week_year" "Hour" 
gg1 <-ggplot(energy_data_Total_per_hour_na_removed_w1,aes(x=Day_week,y=Hour,
                                                          fill=Appliances)) 

gg1

min(energy_data_Total_per_hour_na_removed_w1$Appliances)
# 190
max(energy_data_Total_per_hour_na_removed_w1$Appliances)

gg1 <- gg1 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(150,3800))
gg1

library(viridis)
library(ggthemes)
#gg1 <- gg1 +scale_fill_viridis(name="Appliances energy",option="A")
#gg <- gg +scale_y_continuous(breaks=seq(0,23,1),trans="reverse")
gg1 <- gg1 +scale_y_continuous(breaks=seq(0,23,1)) 

gg1


gg1 <- gg1 + coord_equal()
gg1
gg1 <- gg1 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg1 <- gg1 + theme_tufte(base_family="Helvetica")
gg1 <- gg1 + theme(plot.title=element_text(hjust=0))
gg1 <- gg1 + theme(axis.ticks=element_blank())
gg1 <- gg1 + theme(axis.text=element_text(size=9))
gg1 <- gg1 + theme(legend.title=element_text(size=9))
gg1 <- gg1 + theme(legend.text=element_text(size=9))
gg1


# Reading the training data

energy_data$my <- NULL
energy_data$mhr <- NULL
names(energy_data)

library(caret)
set.seed(1)
train_index <- createDataPartition(energy_data$Appliances,p=0.75,list=FALSE)

train_data <- energy_data[train_index,]
dim(train_data)
names(train_data)
