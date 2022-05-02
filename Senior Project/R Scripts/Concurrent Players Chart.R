library(ggplot2)
library(hrbrthemes)

library(readxl)
Concurrent_Steam_Users <- read_excel("Senior Project/Concurrent Steam Users.xlsx")


xValue<-Concurrent_Steam_Users$Date
yValue<-Concurrent_Steam_Users$`In-Game`
data <- data.frame(xValue,yValue)


ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line( color="green", size=.7, alpha=0.9) +
  theme_ipsum() +
  ggtitle("Concurrent Steam Players") +
  xlab("Date") +
  ylab("Users")
