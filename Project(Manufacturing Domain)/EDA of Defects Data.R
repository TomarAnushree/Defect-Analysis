setwd(choose.dir())
library(readxl)

#Preprocessing of data
data<-read_excel('Defect analysis sample Sheet - Copy.xlsx')
data<-data[-c(1,2),]
colnames(data)<-as.character(data[1,])
data<-data[-1,]
data<-data[,-c(12:17)]

#changing the data type of variable 
data$Shift<-as.factor(data$Shift)
data$`Line no.`<-as.factor(data$`Line no.`)
data$Product<-as.factor(data$Product)
data$`Defect Name`<-as.factor(data$`Defect Name`)
data$`Section No.`<-as.factor(data$`Section No.`)
data$`Primary Cause`<-as.factor(data$`Primary Cause`)
data$`Root Cause Category`<-as.factor(data$`Root Cause Category`)
data$`Operator Name`<-as.factor(data$`Operator Name`)
data$`Hour no.`<-as.numeric(data$`Hour no.`)
data$`No. of Occurance`<-as.numeric(data$`No. of Occurance`)
data$Date<-as.Date(data$Date,format = "%d.%m.%Y")

anyNA(data)
#custom Function
mode<-function(x){uniqv<-unique(x)
uniqv[which.max(tabulate(match(x,uniqv)))]}

#EDA

table(data$`Root Cause Category`)
unique(data$Product)

library(stringr)
aggregateDefect_mode<-aggregate.data.frame(data$`Defect Name`,by=list(data$`Operator Name`),FUN=mode)
colnames("aggregateDefect_mode")<-c("Operator name", "Defect Name")

aggregateDefect_count<-aggregate.data.frame(data$`No. of Occurance`,by=list(data$`Defect Name`),FUN=sum)
colnames(aggregateDefect_count)<-c("Defects","No. of occurance")


library(zoo)
library(tidyr)

#Month wise Occurence of Defects
#data["Q"]<-as.yearqtr(data$Date,"%Y%m")
data["M"]<-as.yearmon(data$Date)
dataM<-aggregate(data$`No. of Occurance`,by=list(data$M,data$`Defect Name`),FUN=sum)
colnames(dataM)<-c("M","Defects","no of occurance")
new<-spread(dataM,1,3,fill = NA,convert=FALSE,drop = TRUE,sep = NULL)

#shift wise Occurence of Defects
datashift<-aggregate(data$`No. of Occurance`,by=list(data$Shift,data$`Defect Name`),FUN=sum)
colnames(datashift)<-c("shifts","Defects","no of occurance")
new<-spread(datashift,1,3,fill = NA,convert=FALSE,drop = TRUE,sep = NULL)

#occurence of Defects,Hour No. 
datahours<-aggregate(data$`No. of Occurance`,by=list(data$`Hour no.`,data$`Defect Name`),FUN=sum)
colnames(datahours)<-c("Hours","Defects","no of occurance")
new<-spread(datahours,1,3,fill = NA,convert=FALSE,drop = TRUE,sep = NULL)


#cumulative defect 
aggregateDefect_count<-aggregateDefect_count[with(aggregateDefect_count,order(aggregateDefect_count$`No. of occurance`)),]

aggregateDefect_count["cumsum"]<-cumsum(aggregateDefect_count$`No. of occurance`)

aggregateDefect_count["freq"] = aggregateDefect_count$`No. of occurance`/701 
aggregateDefect_count["cumfreq"] = cumsum(aggregateDefect_count$freq) 

d<-aggregateDefect_count

# Pareto Chart

## Saving Parameters 
def_par <- par() 

## New margins
par(mar=c(5,5,4,5)) 

## bar plot, pc will hold x values for bars
pc = barplot(d$`No. of occurance`,  
             width = 1, space = 0.2, border = NA, axes = F,
             ylim = c(0, 1.05 * max(d$cumsum, na.rm = T)), 
             xlab = "Defects",
             ylab = "Cummulative Counts" , cex.names = 0.7, 
             names.arg = d$Defects,
             main = "Pareto Chart")

## Cumulative counts line 
lines(pc, d$cumsum, type = "b", cex = 0.7, pch = 19, col="cyan4")

## Framing plot
box(col = "grey62")

## adding axes
axis(side = 2, at = c(0, d$cumsum), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)
axis(side = 4, at = c(0, d$cumsum), labels = paste(c(0, round(d$cumfreq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "cyan4", col = "cyan4", cex.axis = 0.8)

## restoring default paramenter
par(def_par) 






