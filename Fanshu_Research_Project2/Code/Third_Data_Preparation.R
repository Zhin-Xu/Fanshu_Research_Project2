library(MASS)
library(zoo)
library(dplyr)
library(purrr)
library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)
library(patchwork)

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
MyData<-MyData[,c(1,21:45)]



global_abundance_data<-read_excel('../Data/pnas.2023170118.sd01.xlsx')






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

write.csv(merged_data,'../Data/unselected_result.csv',row.names=F )


