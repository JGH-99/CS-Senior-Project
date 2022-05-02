library(readxl)
Stock_Sum <- read_excel("Stock Sum.xlsx")
View(Stock_Sum)

x<-Stock_Sum$Before
y<-Stock_Sum$After

var.test(x,y, alternative = "less")

t.test(x,y, alternative= "greater")


