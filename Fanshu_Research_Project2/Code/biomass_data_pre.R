library(readxl)

bm_data<-read_excel("../Data/bij1917_sm_figs_tables.xlsx")

bm_data<-as.data.frame(bm_data)

bm_data<-bm_data[,1:4]

colnames(bm_data)<-c("Family","Genus","Species","Body Mass(gram)")

Binomial<-data.frame("Binomial"=paste(bm_data$Genus,bm_data$Species))

bm_data<-cbind(bm_data,Binomial)


write.csv(bm_data,"../Data/biomass.csv",row.names=F)