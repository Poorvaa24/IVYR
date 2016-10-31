#Predicting the number of customers in test data because the sales depend uopn the no of customers.

train <- read.csv("D:/COURSE/XXXX/R/class examples/input/train.csv", header = T)
test <-read.csv("D:/COURSE/XXXX/R/class examples/input/test.csv", header = T)
store <-read.csv("D:/COURSE/XXXX/R/class examples/input/store.csv")

train <- merge(train,store)
test <- merge(test,store)

munge_data <- function(dt){
  
  # replacing NA's by the mean value  
  dt$CompetitionDistance[is.na(dt$CompetitionDistance)] = round(mean(dt$CompetitionDistance, na.rm = T))
  dt$CompetitionOpenSinceMonth[is.na(dt$CompetitionOpenSinceMonth)] = round(mean(dt$CompetitionOpenSinceMonth, na.rm = T))
  dt$CompetitionOpenSinceYear[is.na(dt$CompetitionOpenSinceYear)] = round(mean(dt$CompetitionOpenSinceYear, na.rm = T))
  dt$Promo2SinceWeek[is.na(dt$Promo2SinceWeek)] = round(mean(dt$Promo2SinceWeek, na.rm = T))
  dt$Promo2SinceYear[is.na(dt$Promo2SinceYear)] = round(mean(dt$Promo2SinceYear, na.rm = T))

  # converting to numeric
  dt$StateHoliday = as.numeric(dt$StateHoliday)
  dt$StoreType = as.numeric(dt$StoreType)
  dt$Assortment = as.numeric(dt$Assortment)
  dt$PromoInterval = as.numeric(dt$PromoInterval)
  
  # seperating out the elements of the date column for the dt set # feature engineering
  dt$Date = as.Date(dt$Date, format = "%Y-%m-%d")
  dt$month <- as.integer(format(dt$Date, "%m"))
  dt$year <- as.integer(format(dt$Date, "%y"))
  dt$day <- as.integer(format(dt$Date, "%d"))
  
  # removing the date columns (Date not used) (Customers affect simetry) (CompetitionOpenSinceYear not relevant/correlated)
  dt$Date = NULL
  dt$Promo2SinceYear = NULL
  dt$year = NULL
  dt$CompetitionOpenSinceYear = NULL
  dt$Sales = NULL
  
  return(dt)
}

train = munge_data(train)
test = munge_data(test)

cor(train)

#training the model

mod = lm(Customers ~ ., data = train) 

summary(mod)

# predicting dataset
y = predict(mod, newdata = test)

#preparing dataset fro submission

submission <- data.frame(Id=test$Id, Customers=y)

write.csv(submission, "CustomersPrediction.csv")


# predicting sales in test data

train <- read.csv("D:/COURSE/XXXX/R/class examples/input/train.csv", header = T)
test <-read.csv("D:/COURSE/XXXX/R/class examples/input/test.csv", header = T)
store <-read.csv("D:/COURSE/XXXX/R/class examples/input/store.csv")

train <- merge(train,store)
test <- merge(test,store)

munge_data <- function(dt){
  
  # replacing NA's by the mean value  
  dt$CompetitionDistance[is.na(dt$CompetitionDistance)] = round(mean(dt$CompetitionDistance, na.rm = T))
  dt$CompetitionOpenSinceMonth[is.na(dt$CompetitionOpenSinceMonth)] = round(mean(dt$CompetitionOpenSinceMonth, na.rm = T))
  dt$CompetitionOpenSinceYear[is.na(dt$CompetitionOpenSinceYear)] = round(mean(dt$CompetitionOpenSinceYear, na.rm = T))
  dt$Promo2SinceWeek[is.na(dt$Promo2SinceWeek)] = round(mean(dt$Promo2SinceWeek, na.rm = T))
  dt$Promo2SinceYear[is.na(dt$Promo2SinceYear)] = round(mean(dt$Promo2SinceYear, na.rm = T))
  
  # converting to numeric
  dt$StateHoliday = as.numeric(dt$StateHoliday)
  dt$StoreType = as.numeric(dt$StoreType)
  dt$Assortment = as.numeric(dt$Assortment)
  dt$PromoInterval = as.numeric(dt$PromoInterval)
  
  # seperating out the elements of the date column for the dt set # feature engineering
  dt$Date = as.Date(dt$Date, format = "%Y-%m-%d")
  dt$month <- as.integer(format(dt$Date, "%m"))
  dt$year <- as.integer(format(dt$Date, "%y"))
  dt$day <- as.integer(format(dt$Date, "%d"))
  
  # removing the date columns (Date not used) (Customers affect simetry) (CompetitionOpenSinceYear not relevant/correlated)
  dt$Date = NULL
  dt$Promo2SinceYear = NULL
  dt$year = NULL
  dt$CompetitionOpenSinceYear = NULL
 
    return(dt)
}

train = munge_data(train)
test = munge_data(test)

cor(train)

#training the model
mod = lm(Sales ~ ., data = train) 

summary(mod)

# predicting dataset
y = predict(mod, newdata = test)

#preparing dataset fro submission
submission <- data.frame(Id=test$Id, Sales=y)

write.csv(submission, "SalesPrediction.csv")


