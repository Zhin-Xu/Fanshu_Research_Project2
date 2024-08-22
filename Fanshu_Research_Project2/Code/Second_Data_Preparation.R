library(MASS)
library(zoo)
library(dplyr)
library(purrr)
library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)
library(patchwork)

MyData<-read.csv('../Data/filled_lpd.csv',header=T)

global_abundance_data<-read_excel('../Data/pnas.2023170118.sd01.xlsx')
rewritten_data1<- read.csv("../Data/searched_birds.csv",header=T)
rewritten_data2<- read.csv("../Data/found_birds.csv",header=T)



for(k in 1:nrow(MyData)){
   if(MyData$Binomial[k]%in%rewritten_data1$unique_name){
      MyData$Binomial[k]<-rewritten_data1[rewritten_data1$unique_name==MyData$Binomial[k],"search_string"]
   }
}


for(m in 1:nrow(global_abundance_data)){
   if(global_abundance_data$'Scientific name'[m]%in%rewritten_data2$unique_name){
      global_abundance_data$'Scientific name'[m]<-rewritten_data2[rewritten_data2$unique_name==global_abundance_data$'Scientific name'[m],"search_string"]
   }
}

matched_species <- intersect(MyData$Binomial, global_abundance_data$'Scientific name')

matched_DF<-data.frame()


for(specie in matched_species){

  global_abundance_value <- global_abundance_data[global_abundance_data$'Scientific name' == specie, "Abundance estimate"]
  specie_data<-data.frame(MyData[MyData$Binomial==specie,],'global_abundance'=as.numeric(global_abundance_value))
  
  matched_DF<-rbind(matched_DF,specie_data)

}

colnames(matched_DF)[2:(ncol(matched_DF)-1)] <- year_columns[which(year_columns==filtered_data3$Year[2]):which(year_columns==filtered_data3$Year[nrow(filtered_data3)])]






matched_DF<-data.frame(matched_DF,"max_growth_rate"=rep(NA,nrow(matched_DF)))

for(t in 1:nrow(matched_DF)){
   matched_DF$max_growth_rate[[t]]<-max(as.numeric(matched_DF[t,2:26][,!is.na(matched_DF[t,2:26])]))
}



matched_DF$max_growth_rate<-unlist(matched_DF$max_growth_rate)
matched_DF$global_abundance<-unlist(matched_DF$global_abundance)
matched_DF$Latitude<-unlist(matched_DF$Latitude)
matched_DF$Longitude<-unlist(matched_DF$Longitude)



bm_data<-read.csv("../Data/biomass.csv",header=T)


for(specie in unique(bm_data$Binomial)){
   if(nrow(bm_data[bm_data$Binomial==specie,])!=1){
      bm_data<-bm_data[bm_data$Binomial!=specie,]
   }
}

matched_species3<- intersect(matched_DF$Binomial,bm_data$Binomial)
matched_DF<-matched_DF[matched_DF$Binomial%in%matched_species3,]
bm_data<-bm_data[bm_data$Binomial%in%matched_species3,]

bm_data<-bm_data[,c(4,5)]

matched_DF$Binomial <- as.character(matched_DF$Binomial)
bm_data$Binomial <- as.character(bm_data$Binomial)

# 使用merge函数将两个数据框合并
merged_data <- merge(matched_DF, bm_data, by = "Binomial", all.x = TRUE)

# 如果需要，将新合并的数据框中的"Body.Mass.gram."列转换为数值型
merged_data$Body.Mass.gram. <- as.numeric(merged_data$Body.Mass.gram.)


year_columns <- as.numeric(gsub("X(\\d+)", "\\1", colnames(merged_data)[2:26]))
colnames(merged_data)[2:26]<-year_columns

write.csv(merged_data,'../Data/seems_useful_result.csv',row.names=F )



lpi_lambda<-read.csv('../Data/filled_lpd.csv',header=T)
matched_data<-read.csv('../Data/seems_useful_result.csv',header=T)
unmatched_data<-lpi_lambda[!(lpi_lambda$Binomial%in%matched_data$Binomial),
]
unmatched_data<-data.frame(unmatched_data,"Class"=rep(0,nrow(unmatched_data)))




original_lpi<-read.csv('../Data/LPD2022_public.csv',header=T)
original_lpi$Binomial<- gsub("_"," ",original_lpi$Binomial)

for(name in unique(unmatched_data$Binomial)){
  unmatched_data$Class[unmatched_data$Binomial == name] <- unique(original_lpi[original_lpi$Binomial == name, "Class"])
}

unmatched_data<-unmatched_data[unmatched_data$Class=="Aves",]
write.csv(unmatched_data,'../Data/unmatched_birds.csv',row.names=F)