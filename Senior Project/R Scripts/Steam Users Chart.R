library(ggplot2)
library(hrbrthemes)

library(readxl)
Daily_Steam_Data <- read_excel("Senior Project/Daily Steam Data.xlsx")

xValue<-Daily_Steam_Data$Date
yValue<-Daily_Steam_Data$Users
data <- data.frame(xValue,yValue)


ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line( color="green", size=1, alpha=0.9) +
  theme_ipsum() +
  ggtitle("Steam Players") +
  xlab("Date") +
  ylab("Users")
