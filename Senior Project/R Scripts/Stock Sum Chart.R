library(ggplot2)
library(hrbrthemes)

library(readxl)
Stock_Sum <- read_excel("Senior Project/Stock Sum.xlsx")
View(Stock_Sum)

xValue<-Stock_Sum$Date
yValue<-Stock_Sum$Sum
data <- data.frame(xValue,yValue)


ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line( color="red", size=1, alpha=0.9) +
  theme_ipsum() +
  ggtitle("Stock Sum") +
  xlab("Date") +
  ylab("Price")
