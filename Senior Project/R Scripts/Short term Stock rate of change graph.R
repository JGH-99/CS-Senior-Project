library(ggplot2)
library(hrbrthemes)

library(readxl)
Short_term_Stock_Rate_of_Change <- read_excel("Senior Project/Short term Stock Rate of Change.xlsx")
xValue<-Short_term_Stock_Rate_of_Change$Date
yValue<-Short_term_Stock_Rate_of_Change$`Rate of Change`
data <- data.frame(xValue,yValue)


ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line( color="blue", size=.5, alpha=0.9) +
  theme_ipsum() +
  ggtitle("Stock Sum") +
  xlab("Date") +
  ylab("Price")
