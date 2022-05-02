library(readxl)
Covid_Steam <- read_excel("Senior Project/Covid-Steam.xlsx")
View(Covid_Steam)

x<-Covid_Steam$`Change in Users`
y<-Covid_Steam$`Change in New Cases`
cor.test(x,y)