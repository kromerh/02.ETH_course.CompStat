setwd("/Users/hkromer/02_PhD/08.ETH_courses/01.CompStat/02.ETH_course.CompStat/")
getwd()

############################################################################################################
# Problem 1
############################################################################################################
# 1. In this exercise, we analyze a dataset about fruitflies, see 1 and 2. 
# This dataset contains observations on five groups of male fruitflies – 25 fruitflies in each group – 
# from an experiment designed to test if increased reproduction reduces longevity for male fruitflies. 
# (Such a cost has already been established for females.) The five groups are: males forced to live alone, 
# males assigned to live with one or eight interested females, and males assigned to live with one or eight 
# non-receptive females. The observations on each fly were longevity, thorax length, and the percentage of 
# each day spent sleeping. Note that the fruitflies were assigned randomly to the five groups.


## a) Read in the dataset and remove the variables id and sleep. Then create a pairs plot and comment on it.
url <- "https://ww2.amstat.org/publications/jse/datasets/fruitfly.dat.txt" 
data <- read.table(url)
data <- data[,c(-1,-6)] # remove id and sleep
names(data) <- c("partners","type","longevity","thorax")
summary(data)

pairs(data)




## b) Make a scatterplot of longevity versus thorax, using colors for the number of females and different plotting symbols for the different types of females. 
# Comment on the plot.
num_females <-  unique(data$partners)
type_females <- unique(data$type)

my_colors <- c("red", "blue", "green")
data$my_color[data$partners==num_females[1]]="red"
data$my_color[data$partners==num_females[2]]="blue"
data$my_color[data$partners==num_females[3]]="green"

my_markers <- c(15,16,17)
data$my_symbol[data$type==type_females[1]]=15
data$my_symbol[data$type==type_females[2]]=16
data$my_symbol[data$type==type_females[3]]=17

plot(data$thorax, data$longevity, col=data$my_color, pch=data$my_symbol, 
     ylim = c(0,110),
     xlab="thorax", ylab="longevity")
legend("bottomright",
       title="number of females", 
       legend=c(num_females[1], num_females[2], num_females[3]), 
       col=c(my_colors[1], my_colors[2], my_colors[3]), 
       pch=20)
legend("topleft",
       title="type of female", 
       legend=c(type_females[1], type_females[2], type_females[3]), 
       pch=c(my_markers[1], my_markers[2], my_markers[3]))

