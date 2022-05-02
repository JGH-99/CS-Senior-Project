# importing the required library
# library(glmnet)
library(tidyverse)
library(readxl)
library(rio)
library(dygraphs)
library(xts)
library(GGally)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(car)



excel_data = read_excel("Covid-Steam.xlsx")

dfdata = data.frame(excel_data)

summary(dfdata)
str(dfdata)


######plot #####################
######user and new cases######
dfdata$Users.During.Covid = dfdata$Users.During.Covid/10^2

data_graph = subset(dfdata, select = c(Date, Users.During.Covid, New.Cases))
time_graph=xts( x = data_graph[,-1], order.by=data_graph$Date)
dygraph(time_graph)

######change in user and new cases######
data_graph = subset(dfdata, select = c(Date, Change.in.Users, Change.in.New.Cases))
time_graph=xts( x = data_graph[,-1], order.by=data_graph$Date)
dygraph(time_graph)

########box-plot######
excel_data = read_excel("Steam BeforeAfter.xlsx")

dfdata = data.frame(excel_data)

summary(dfdata)
str(dfdata)

boxplot(dfdata[,c(5, 6)])

###better box plot with violin plots
excel_data = read_excel("Stock Rate of Change.xlsx")

dfdata = data.frame(excel_data)

summary(dfdata)
str(dfdata)

# sample size
sample_size = dfdata %>% group_by(Period) %>% summarize(num=n())

dfdata %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Period, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=Rate.of.Change, fill=Period)) +
  geom_violin(width=1.0) +
  geom_boxplot(width=0.2, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Box and Violin plot of change rate of Stock Variance") +
  xlab("")

########hypothesis test#####

######Hypothesis Test 1: The number of people who play video games has in-
################creased since the pandemic began
excel_data = read_excel("Steam BeforeAfter.xlsx")

dfdata = data.frame(excel_data)

summary(dfdata)
str(dfdata)

t.test(dfdata$Before, dfdata$After, alternative = "less", var.equal = FALSE)

###date --- after covid, 3/1/2020

###########Hypothesis Test 2: The growth rate of stocks has increased since Covid-19 began
excel_data = read_excel("Stock Sum.xlsx")

dfdata = data.frame(excel_data)

summary(dfdata)
str(dfdata)

t.test(dfdata$Before, dfdata$After, alternative = "less", var.equal = FALSE)

###########Hypothesis Test : The stocks has increased since Covid-19 began

data_graph = subset(dfdata, select = c(Date, Sum, Rate.of.Change))
time_graph=xts( x = data_graph[,-1], order.by=data_graph$Date)
dygraph(time_graph)


maker.date = 373  ###2/27/2020  
end.date = 753  ###8/30/2021
stk.data.before = dfdata$Sum[1:maker.date]
stk.data.after = dfdata$Sum[(maker.date + 1):end.date]

t.test(stk.data.before, stk.data.after, alternative = "less", var.equal = FALSE)


############Hypothesis Test 3: The growth rate of stocks varies more after Covid-19
###Preleminary test
###########F-test is very sensitive to departure from the normal assumption. 
#########You need to check whether the data is normally distributed before using the F-test.
excel_data = read_excel("Stock Sum.xlsx")

dfdata = data.frame(excel_data)

summary(dfdata)
str(dfdata)

# Shapiro-Wilk normality test
########Null hypothesis: the data are normally distributed
#########Alternative hypothesis: the data are not normally distributed

##if the data are not normally distributed, it’s recommended to use the non-parametric correlation, including Spearman and Kendall rank-based correlation tests.
shapiro.test(dfdata$Before) 

shapiro.test(dfdata$After)

#####q-q plots
ggqqplot(dfdata$Before, ylab = "Change Rate of Stocks Before COVID-19")

ggqqplot(dfdata$After, ylab = "Change Rate of Stocks During COVID-19")

####F test
var.test(dfdata$Before, dfdata$After, alternative = "less")

######If there is doubt about normality, the better choice is to use Levene’s test or 
#######Fligner-Killeen test, which are less sensitive to departure from normal assumption.
excel_data = read_excel("Stock Sum_2.xlsx")

dfdata = data.frame(excel_data)

summary(dfdata)
str(dfdata)

leveneTest(Rate.of.Change ~ Period, data = dfdata)
fligner.test(Rate.of.Change ~ Period, data = dfdata)

#############30 days before vs after covid#########
excel_data = read_excel("Short term Stock Rate of Change.xlsx")

dfdata = data.frame(excel_data)

summary(dfdata)
str(dfdata)

# Shapiro-Wilk normality test
########Null hypothesis: the data are normally distributed
#########Alternative hypothesis: the data are not normally distributed

##if the data are not normally distributed, it’s recommended to use the non-parametric correlation, including Spearman and Kendall rank-based correlation tests.
shapiro.test(dfdata$Before) 

shapiro.test(dfdata$After)

#####q-q plots
ggqqplot(dfdata$Before, ylab = "Short Term Change Rate of Stocks Before COVID-19")

ggqqplot(dfdata$After, ylab = "Short Term Change Rate of Stocks During COVID-19")

####F test
var.test(dfdata$Before, dfdata$After, alternative = "less")

######If there is doubt about normality, the better choice is to use Levene’s test or 
#######Fligner-Killeen test, which are less sensitive to departure from normal assumption.
excel_data = read_excel("Stock Sum_2.xlsx")

dfdata = data.frame(excel_data)

summary(dfdata)
str(dfdata)

leveneTest(Rate.of.Change ~ Period, data = dfdata)
fligner.test(Rate.of.Change ~ Period, data = dfdata)





######Hypothesis Test 4: There is a positive correlation between daily changes
#########in Covid-19 cases and daily changes in the number of players
excel_data = read_excel("Covid-Steam.xlsx")

dfdata = data.frame(excel_data)

summary(dfdata)
str(dfdata)

marker.date.vac = 310 ###2021-1-4
end.date = 548

data.cs= subset(dfdata, select = c(Change.in.Users, Change.in.New.Cases))

#########before and after vaccine########
data.cs.bv = data.cs[1:(marker.date.vac-1),]
data.cs.av = data.cs[marker.date.vac:end.date,]


#####plot#######
ggpairs(data.cs)
ggpairs(data.cs.bv)
ggpairs(data.cs.av)


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data.cs.bv, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(data.cs.bv,columns = 1:2, lower = list(continuous = my_fn))
g


####correlation  test####
#####Preleminary test, normality test
# Shapiro-Wilk normality test
########Null hypothesis: the data are normally distributed
#########Alternative hypothesis: the data are not normally distributed

##if the data are not normally distributed, it’s recommended to use the non-parametric correlation, including Spearman and Kendall rank-based correlation tests.
shapiro.test(data.cs$Change.in.Users) 

shapiro.test(data.cs$Change.in.New.Cases)

#####q-q plots
ggqqplot(data.cs$Change.in.Users, ylab = "Change in Users")

ggqqplot(data.cs$Change.in.New.Cases, ylab = "Chang in New COVID Cases")

##cor(x, y, method = c("pearson", "kendall", "spearman"))

##If your data contain missing values, the following R code handle missing values by case-wise deletion
# cor(data.cs$Change.in.Users, data.cs$Change.in.New.Cases, method = c("pearson"), use = "complete.obs")
# 
# cor(data.cs.bv$Change.in.Users, data.cs.bv$Change.in.New.Cases, method = c("pearson"), use = "complete.obs")
# 
# cor(data.cs.av$Change.in.Users, data.cs.av$Change.in.New.Cases, method = c("pearson"), use = "complete.obs")


cor.test(data.cs$Change.in.Users, data.cs$Change.in.New.Cases, method = "pearson")
cor.test(data.cs.bv$Change.in.Users, data.cs.bv$Change.in.New.Cases, method = "pearson")
cor.test(data.cs.av$Change.in.Users, data.cs.av$Change.in.New.Cases, method = "pearson")

####Kendall rank correlation test, rank-based measure
cor.test(data.cs$Change.in.Users, data.cs$Change.in.New.Cases, method = "kendall")
cor.test(data.cs.bv$Change.in.Users, data.cs.bv$Change.in.New.Cases, method = "kendall")
cor.test(data.cs.av$Change.in.Users, data.cs.av$Change.in.New.Cases, method = "kendall")

# ######Spearman rank correlation coefficient
# #####Spearman’s rho statistic is also used to estimate a rank-based measure of association. This test may be used if the data do not come from a bivariate normal distribution.
# 
# cor.test(data.cs$Change.in.Users, data.cs$Change.in.New.Cases, method = "spearman")
# cor.test(data.cs.bv$Change.in.Users, data.cs.bv$Change.in.New.Cases, method = "spearman")
# cor.test(data.cs.av$Change.in.Users, data.cs.av$Change.in.New.Cases, method = "spearman")

