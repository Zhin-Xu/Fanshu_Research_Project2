library(rotl)

unmatched_data<-read.csv("../Data/filled_lpd.csv",header=T)
my_taxa<-unmatched_data$Binomial
resolved_names <- tnrs_match_names(names = my_taxa)


text<-resolved_names$search_string
resolved_names$search_string<- paste(toupper(substring(text, 1, 1)), substring(text, 2, nchar(text)), sep = "")


resolved_names<-resolved_names[resolved_names$search_string!=resolved_names$unique_name,]
resolved_names<-na.omit(resolved_names)
 write.csv(resolved_names,"../Data/searched_birds.csv",row.names=F)





global_abundance_data<-read_excel('../Data/pnas.2023170118.sd01.xlsx')
my_taxa2<-global_abundance_data$'Scientific name'
resolved_names2 <- tnrs_match_names(names = my_taxa2)


text2<-resolved_names2$search_string
resolved_names2$search_string<- paste(toupper(substring(text2, 1, 1)), substring(text2, 2, nchar(text2)), sep = "")


resolved_names2 <- resolved_names2[resolved_names2$search_string!=resolved_names2$unique_name,]
resolved_names2<-na.omit(resolved_names2)
 write.csv(resolved_names2,"../Data/found_birds.csv",row.names=F)