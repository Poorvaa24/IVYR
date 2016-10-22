# Load forestfires.csv
read.csv
df <- read.csv("E:/R/forestfires.csv")
df


# Q1. 1.	Compute the square of each data point in the X column and store the result in a new column called "X_square"
df1 <- df["X"]
df2 <- df1^2
colnames(df2) <- c("X_square")
print(df2)

install.packages("dplyr")
library(dplyr)
data_tdf <- tbl_df(df)

# Q2 2.	Compute the sum, mean, median, standard deviation of the following columns - 
#a.	FFMC
#b.	DMC
#c.	DC

df11 <- df["FFMC"]
df_sum1 <- sum(df["FFMC"])
df_sum2 <- sum(df["DMC"])
df_sum3 <- sum(df["DC"])

result.mean1 <- mean(df[,5])
result.mean2 <- mean(df[,6])
result.mean3 <- mean(df[,7])

result.median1 <- median(df[,5])
result.median2 <- median(df[,6])
result.median3 <- median(df[,7])

#Q3 Create another column called "Month", which has full values of month, i.e "aug" becomes "August", "sep" becomes "September" and so on

df$MONTH <- sapply(df$month,function(x)
  {
  if(x=="jan"){
    x <- as.factor("January")
  }
  if(x=="feb"){
    x <- as.factor("February")
  }
  if(x=="mar"){
    x <- as.factor("march")
  }
  if(x=="apr"){
    x <- as.factor("april")
  }
  if(x=="may"){
    x <- as.factor("May")
  }
  if(x=="jun"){
    x <- as.factor("June")
  }
    if(x=="jul"){
      x <- as.factor("July")
    }
    if(x=="aug"){
      x <- as.factor("August")
    }
    if(x=="sep"){
      x <- as.factor("September")
    }
    if(x=="oct"){
      x <- as.factor("October")
    }
    if(x=="nov"){
      x <- as.factor("November")
    }
    if(x=="dec"){
      x <- as.factor("December")
    }
  return (x)
}
)
df
# Q4 Create another Column Day_Num where day will be from 1 to 7 - 1 being Sunday, 2being Monday, 3 being Tuesday and so on

df$Day_Num <- sapply(df$day,function(x)
{
  if(x=="sun"){
    x <- as.factor("1")
  }
  if(x=="mon"){
    x <- as.factor("2")
  }
  if(x=="tue"){
    x <- as.factor("3")
  }
  if(x=="wed"){
    x <- as.factor("4")
  }
  if(x=="thu"){
    x <- as.factor("5")
  }
  if(x=="fri"){
    x <- as.factor("6")
  }
  if(x=="sat"){
    x <- as.factor("7")
  }
  
  return (x)
}
)
df

# Q6find total rain,wind for each month

summarise(group_by(df, month), sum_rain=sum(rain),sum_wind=sum(wind))

#Q7 Find the mean rain,wind for each month
summarise(group_by(df, month), mean_rain=mean(rain),mean_wind=mean(wind))

#Q8Find the number of records present for each month

summarise(group_by(df, month),count=n())

#Q9Find the number of records for each month-day combo

summarise(group_by(df, month,day),count=n())



