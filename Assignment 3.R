library(tidyverse)

ggplot(iris, aes(x = 1:150, y = Sepal.Length, color=Species)) + geom_point(size=3) + ggtitle("Sepal Length")
ggplot(iris, aes(x = 1:150, y = Petal.Length, color=Species)) + geom_point(size=3) + ggtitle("Petal Length")

# We conclude that the sepal length by species is directly correlated to petal length

texas <- txhousing
cities <- c(levels(as.factor(texas$city)))
ggplot(texas, aes(date, sales)) + 
  geom_line(aes(group = city), alpha = 0.6)

# There are a lot of ups and downs in the chart, suggesting some times every year 
# when sales are high and when sales are low
# Maybe we could calculate the variance across a year as a separate statistic,
# and plot that for every city

texas = cbind(texas, rep(0, times = 8602))
names(texas)[10] = "Variance"
dates <- texas$date
lastCity = "Abeline"
vec = texas$sales[1]
for (i in 2:8602){
  City = texas$city[i]
  if(City != lastCity){
    print(var(vec))
    texas$Variance[i - 1] = var(vec)
    vec = texas$sales[i]
    lastCity = City
  }
  else{
    vec = cbind(vec, texas$sales[i])
  }
}

# I wasn't able to finish writing this up, sorry

titanic <- read.csv("https://raw.githubusercontent.com/DVnyT/-R--Assignment-3/main/titanic.csv?token=GHSAT0AAAAAACC5BKJZWJOTS2ZF4DKUGEDSZE3DAZA")

for (i in 1:712){
  if(titanic$Survived[i] == 1){
    titanic$Survived[i] = "Survived"
  }
  else {
    titanic$Survived[i] = "Died"
  }
}
# Since you said you wanted the plot exactly, I have included the spelling error in Fare on the x axis label
final_Plot <- ggplot(titanic, aes(x = Fare, y = Survived, color = Sex)) + geom_boxplot(outlier.color = levels(titanic$Sex)) + labs(title = "Fare vs Survival", subtitle = "Irrespective of Sex, richer people survived", y = "", x = "Fair")
final_Plot
