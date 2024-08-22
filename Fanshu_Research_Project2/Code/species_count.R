MYDF<-read.csv('../Data/mean_dt_LPI_U.csv',header=T)

MYDF[,-c(1,2,73)][!is.na(MYDF[,-c(1,2,73)])]<-1
MYDF[,-c(1,2,73)][is.na(MYDF[,-c(1,2,73)])]<-0

MYDF<-MYDF[,-c(1,2,73)]
value_count<-rep(0,length(colnames(MYDF)))
value_proportion<-rep(0,length(colnames(MYDF)))

for(i in 1:length(colnames(MYDF))){
   value_count[[i]]<-sum(MYDF[,i])
   value_proportion[[i]]<-value_count[[i]]/nrow(MYDF)
}

value_count<-data.frame(value_count)
value_proportion<-data.frame(value_proportion)
colnames(value_count)<-"Value"
colnames(value_proportion)<-"Value"

year_columns <- data.frame(as.numeric(gsub("GrowthRate_(\\d+)", "\\1", colnames(MYDF))))
colnames(year_columns)<- "Year"
value_count<-cbind(value_count,year_columns)
value_proportion<-cbind(value_proportion,year_columns)


png('../Results/species.count.png')

plot(value_count$Year,value_count$Value)
lines(value_count$Year,value_count$Value)
dev.off()

png('../Results/species.proportion.png')

plot(value_proportion$Year,value_proportion$Value)
lines(value_proportion$Year,value_proportion$Value)
abline(h = 0.5, lty = 2, col = "red")
dev.off()