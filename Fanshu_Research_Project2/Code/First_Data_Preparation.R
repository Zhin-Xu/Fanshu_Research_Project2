library(MASS)
library(zoo)
library(dplyr)
library(purrr)
library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)


MyData<- read.csv('example_data_pops_lambda.csv', header = TRUE)


MyData<-MyData[,-1]

year_columns <- as.numeric(gsub("X(\\d+)", "\\1", colnames(MyData)[3:(ncol(MyData))]))
colnames(MyData)[3:(ncol(MyData))]<-year_columns
MyData$Binomial<- gsub("_"," ",MyData$Binomial)


for(specie in unique(MyData$Binomial)){
   if(nrow(MyData[MyData$Binomial==specie,])!=1){
      MyData<-MyData[MyData$Binomial!=specie,]
   }
}

MyDF<-MyData[3:(ncol(MyData))]
MyDF[!is.na(MyDF)]<-1
MyDF[is.na(MyDF)]<-0


value_count2<-rep(0,ncol(MyDF))
value_proportion2<-rep(0,ncol(MyDF))

for(i in 1:ncol(MyDF)){
   value_count2[[i]]<-sum(MyDF[,i])
   value_proportion2[[i]]<-value_count2[[i]]/nrow(MyDF)
}

value_count2<-data.frame(value_count2)
value_proportion2<-data.frame(value_proportion2)



value_count2<-cbind(value_count2,year_columns)
value_proportion2<-cbind(value_proportion2,year_columns)

colnames(value_count2)<-c("Value","Year")
colnames(value_proportion2)<-c("Value","Year")



fit3 <- fitdistr(value_count2$Value, "normal")

# 计算均值和标准差
mean_val3 <- fit3$estimate["mean"]
sd_val3 <- fit3$estimate["sd"]
conf_int3 <- qnorm(c(0.16, 0.84), mean = mean_val3, sd = sd_val3)


fit4 <- fitdistr(value_proportion2$Value, "normal")

# 计算均值和标准差
mean_val4 <- fit4$estimate["mean"]
sd_val4 <- fit4$estimate["sd"]
conf_int4 <- qnorm(c(0.16, 0.84), mean = mean_val4, sd = sd_val4)

filtered_data3 <- value_count2[value_count2$Value >= mean_val3, ]
filtered_data3<-as.data.frame(filtered_data3)



filtered_data4 <- value_proportion2[value_proportion2$Year>=filtered_data3$Year[2]&value_proportion2$Year<=filtered_data3$Year[length(filtered_data3$Year)],]

filtered_data4<-as.data.frame(filtered_data4)


png('../Results/species.count2.png')

plot(value_count2$Year,value_count2$Value)
lines(value_count2$Year,value_count2$Value)
abline(h = mean_val3, lty = 2, col = "red")
dev.off()

png('../Results/species.proportion2.png')

plot(value_proportion2$Year,value_proportion2$Value)
lines(value_proportion2$Year,value_proportion2$Value)
abline(h = mean_val4, lty = 2, col = "red")
dev.off()




MyDF<-MyDF[,which(colnames(MyDF)==filtered_data3$Year[2]):which(colnames(MyDF)==filtered_data3$Year[nrow(filtered_data3)])]


value_count<-rep(0,nrow(MyDF))
value_proportion<-rep(0,nrow(MyDF))

for(i in 1:nrow(MyDF)){
   value_count[[i]]<-sum(MyDF[i,])
   value_proportion[[i]]<-value_count[[i]]/ncol(MyDF)
}

value_count<-data.frame(value_count)
value_proportion<-data.frame(value_proportion)



value_count<-cbind(value_count,MyData$Binomial)
value_proportion<-cbind(value_proportion,MyData$Binomial)

colnames(value_count)<-c("Value","Binomial")
colnames(value_proportion)<-c("Value","Binomial")



fit1 <- fitdistr(value_count$Value, "normal")

# 计算均值和标准差
mean_val1 <- fit1$estimate["mean"]
sd_val1 <- fit1$estimate["sd"]
conf_int1 <- qnorm(c(0.05, 0.95), mean = mean_val1, sd = sd_val1)


fit2 <- fitdistr(value_proportion$Value, "normal")

# 计算均值和标准差
mean_val2 <- fit2$estimate["mean"]
sd_val2 <- fit2$estimate["sd"]
conf_int2 <- qnorm(c(0.05, 0.95), mean = mean_val2, sd = sd_val2)

filtered_data1 <- value_count[value_count$Value >= mean_val1, ]

filtered_data1<-as.data.frame(filtered_data1)


filtered_data2 <- value_proportion[value_proportion$Value >= mean_val2 , ]

filtered_data2<-as.data.frame(filtered_data2)

png('../Results/NA.count.png')

hist(value_count$Value,col="blue",main="Number of values for species",xlab="Number",ylab="Number of Species")
dev.off()

png('../Results/NA.proportion.png')

hist(value_proportion$Value,col="red",main="Proportion of values for species",xlab="Proportion",ylab="Number of Species")
dev.off()

    


    MyData<-MyData[MyData$Binomial%in%filtered_data1$Binomial,]
       

    MyData<-MyData[,c(1,21:45)]



 write.csv(MyData,'../Data/filled_lpd.csv',row.names=F)





