library(ggplot2)

library(readxl)
Covid_Steam <- read_excel("Senior Project/Covid-Steam.xlsx")
View(Covid_Steam)

xValue<-Covid_Steam$`Change in Users`
yValue<-Covid_Steam$`Change in New Cases`
data <- data.frame(xValue,yValue)

ggplot(data, aes(x=xValue, y=yValue)) + 
  geom_point()