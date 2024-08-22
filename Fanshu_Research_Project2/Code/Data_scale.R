library(zoo)
library(dplyr)
library(purrr)
library(tidyverse)
library(data.table)
library(readxl)
library(vegan)
library(MASS)
library(patchwork)
library(hillR)
library(RColorBrewer)
library(hilldiv)
library(randomcoloR)
library(cowplot)



selected_data<-read.csv('../Data/seems_useful_result.csv',header=T)
selected_data2<-read.csv('../Data/more_useful_result.csv',header=T)
unselected_data<-read.csv('../Data/unselected_result.csv',header=T)
selected_data3<-read.csv('../Data/up_more_useful_result.csv',header=T)
selected_data4<-read.csv('../Data/low_more_useful_result.csv',header=T)

selected_data5<-read.csv('../Data/not_so_up_more_useful_result.csv',header=T)
selected_data6<-read.csv('../Data/not_so_low_more_useful_result.csv',header=T)

year_columns <- c(gsub("X(\\d+)", "\\1", colnames(selected_data2)[c(2:26,length( colnames(selected_data2)))]))
colnames(selected_data2)[c(2:26,length( colnames(selected_data2)))] <- year_columns



selected_data2<-selected_data2[selected_data2$max_growth_rate>0,]

fit_normal <- fitdistr(log(selected_data2$max_growth_rate), "normal")

# 计算均值和标准差
mean_val <- fit_normal$estimate["mean"]
sd_val <- fit_normal$estimate["sd"]
conf_int <- qnorm(c(0.05, 0.95), mean = mean_val, sd = sd_val)





selected_data2<-selected_data2[log(selected_data2$max_growth_rate)>conf_int[1]&log(selected_data2$max_growth_rate)<conf_int[2],]

selected_data2_ <- selected_data2




#未筛选的数据

year_columns <- c(gsub("X(\\d+)", "\\1", colnames(selected_data)[c(2:26,length( colnames(selected_data)))]))
colnames(selected_data)[c(2:26,length( colnames(selected_data)))] <- year_columns



selected_data<-selected_data[selected_data$max_growth_rate>0,]

fit_normal_ <- fitdistr(log(selected_data$max_growth_rate), "normal")

# 计算均值和标准差
mean_val_ <- fit_normal_$estimate["mean"]
sd_val_ <- fit_normal_$estimate["sd"]
conf_int_ <- qnorm(c(0.05, 0.95), mean = mean_val_, sd = sd_val_)





selected_data<-selected_data[log(selected_data$max_growth_rate)>conf_int_[1]&log(selected_data$max_growth_rate)<conf_int_[2],]

selected_data_ <- selected_data




year_columns <- c(gsub("X(\\d+)", "\\1", colnames(unselected_data)[c(2:26,length( colnames(unselected_data)))]))
colnames(unselected_data)[c(2:26,length( colnames(unselected_data)))] <- year_columns



unselected_data<-unselected_data[unselected_data$max_growth_rate>0,]

fit_normal__ <- fitdistr(log(unselected_data$max_growth_rate), "normal")

# 计算均值和标准差
mean_val__ <- fit_normal__$estimate["mean"]
sd_val__ <- fit_normal__$estimate["sd"]
conf_int__ <- qnorm(c(0.05, 0.95), mean = mean_val__, sd = sd_val__)





unselected_data<-unselected_data[log(unselected_data$max_growth_rate)>conf_int__[1]&log(unselected_data$max_growth_rate)<conf_int__[2],]

unselected_data_ <- unselected_data




year_columns <- c(gsub("X(\\d+)", "\\1", colnames(selected_data3)[c(2:26,length( colnames(selected_data3)))]))
colnames(selected_data3)[c(2:26,length( colnames(selected_data3)))] <- year_columns



selected_data3<-selected_data3[selected_data3$max_growth_rate>0,]

fit_normal3 <- fitdistr(log(selected_data3$max_growth_rate), "normal")

# 计算均值和标准差
mean_val3 <- fit_normal3$estimate["mean"]
sd_val3 <- fit_normal3$estimate["sd"]
conf_int3 <- qnorm(c(0.05, 0.95), mean = mean_val3, sd = sd_val3)





selected_data3<-selected_data3[log(selected_data3$max_growth_rate)>conf_int3[1]&log(selected_data3$max_growth_rate)<conf_int3[2],]

selected_data3_<-selected_data3




year_columns <- c(gsub("X(\\d+)", "\\1", colnames(selected_data4)[c(2:26,length( colnames(selected_data4)))]))
colnames(selected_data4)[c(2:26,length( colnames(selected_data4)))] <- year_columns



selected_data4<-selected_data4[selected_data4$max_growth_rate>0,]

fit_normal4 <- fitdistr(log(selected_data4$max_growth_rate), "normal")

# 计算均值和标准差
mean_val4 <- fit_normal4$estimate["mean"]
sd_val4 <- fit_normal4$estimate["sd"]
conf_int4 <- qnorm(c(0.05, 0.95), mean = mean_val4, sd = sd_val4)





selected_data4<-selected_data4[log(selected_data4$max_growth_rate)>conf_int4[1]&log(selected_data4$max_growth_rate)<conf_int4[2],]

selected_data4_<- selected_data4







year_columns <- c(gsub("X(\\d+)", "\\1", colnames(selected_data5)[c(2:26,length( colnames(selected_data5)))]))
colnames(selected_data5)[c(2:26,length( colnames(selected_data5)))] <- year_columns



selected_data5<-selected_data5[selected_data5$max_growth_rate>0,]

fit_normal5 <- fitdistr(log(selected_data5$max_growth_rate), "normal")

# 计算均值和标准差
mean_val5 <- fit_normal5$estimate["mean"]
sd_val5 <- fit_normal5$estimate["sd"]
conf_int5 <- qnorm(c(0.05, 0.95), mean = mean_val5, sd = sd_val5)





selected_data5<-selected_data5[log(selected_data5$max_growth_rate)>conf_int5[1]&log(selected_data5$max_growth_rate)<conf_int5[2],]

selected_data5_<-selected_data5




year_columns <- c(gsub("X(\\d+)", "\\1", colnames(selected_data6)[c(2:26,length( colnames(selected_data6)))]))
colnames(selected_data6)[c(2:26,length( colnames(selected_data6)))] <- year_columns



selected_data6<-selected_data6[selected_data6$max_growth_rate>0,]

fit_normal6 <- fitdistr(log(selected_data6$max_growth_rate), "normal")

# 计算均值和标准差
mean_val6 <- fit_normal6$estimate["mean"]
sd_val6 <- fit_normal6$estimate["sd"]
conf_int6 <- qnorm(c(0.05, 0.95), mean = mean_val6, sd = sd_val6)





selected_data6<-selected_data6[log(selected_data6$max_growth_rate)>conf_int6[1]&log(selected_data6$max_growth_rate)<conf_int6[2],]

selected_data6_ <- selected_data6






ordered_columns <- c("Binomial","1987",colnames(selected_data2)[2:ncol(selected_data2)])


selected_data2$"1987"<-rep(NA,nrow(selected_data2))
for(k in 1:nrow(selected_data2)){
  if(!is.na(selected_data2[k,"1988"])){
     selected_data2$"1987"[k]<-1
  }
}

order<-rep(0,length(ordered_columns))

for(i in 1:length(ordered_columns)){
  order[[i]]<-which(colnames(selected_data2)==ordered_columns[[i]])
}

selected_data2<-selected_data2[,order]




consecutive_nonzero_count <- 2 



find_first_non_na_index <- function(row) {
  non_na_indices <- which(!is.na(row))
  
  if (length(non_na_indices) == 0) {
    return(NULL)
  }
  
  # 找到连续的非缺失值序列及其长度
  consecutive_sequence <- split(non_na_indices, cumsum(c(0, diff(non_na_indices) != 1)))
  max_sequence_index <- which.max(lengths(consecutive_sequence))
  first_index <- min(unlist(consecutive_sequence[which.max(lengths(consecutive_sequence))]))
  sequence_length <- max(lengths(consecutive_sequence)[max_sequence_index])
  
  return(list(first_index = first_index, sequence_length = sequence_length))
}


# 遍历每一行
for (row_index in 1:nrow(selected_data2)) {
  
  # 找到第一个连续的数值序列的起始索引
   indices_info <- find_first_non_na_index(selected_data2[row_index, 2:27])

# 如果找到了连续数值序列
if (!is.null(indices_info)) {
    start_index <- indices_info$first_index
    sequence_length <- indices_info$sequence_length
    end_index <- start_index + sequence_length - 1


    if(start_index==1){
      start_index<-start_index+1
    }
  
  selected_data2[,2:27][row_index, start_index - 1] <- 1
  
  # 计算It值
  for (i in start_index:end_index) {
    selected_data2[,2:27][row_index, i] <- selected_data2[,2:27][row_index, i - 1]*10^selected_data2[,2:27][row_index, i]
    }
  }
}


selected_data2$avg_I_value<-rep(NA,nrow(selected_data2))

for(t in 1:nrow(selected_data2)){
   selected_data2$avg_I_value[t]<-exp(sum(log(selected_data2[t,2:27][,!is.na(selected_data2[t,2:27])]))/length(selected_data2[t,2:27][,!is.na(selected_data2[t,2:27])]))
   selected_data2[t,2:27] <- selected_data2[t,2:27] * selected_data2$global_abundance[[t]]/selected_data2$avg_I_value[[t]]
}

selected_data2 <- selected_data2 [!is.nan(selected_data2$avg_I_value),]



shannon_DF<-selected_data2[,2:27]

shannon_DF_ <- shannon_DF




octaves <- function(abundance_vector) {
  if (length(abundance_vector) == 0) {
    return(numeric(0))
  }


  abundance_vector <- ceiling(log2(abundance_vector))

  return(abundance_vector)
}


octaves2 <- function(abundance_vector) {
  if (length(abundance_vector) == 0) {
    return(numeric(0))
  }

  max_abundance <- max(shannon_DF_, na.rm = TRUE)

  if (!is.finite(max_abundance)) {
    stop("Invalid input: abundances vector contains non-numeric or NA/NaN values.")
  }

  num_octaves <- ceiling(log2(max_abundance))

  octave_counts <- numeric(num_octaves)

  for (i in 1:num_octaves) {
    lower_bound <- 2^(i - 1)
    upper_bound <- 2^i - 1
  octave_counts[i] <- sum(abundance_vector >= lower_bound & abundance_vector <= upper_bound,na.rm = TRUE)
  }

  return(octave_counts)
}
octaves_DF <- data.frame(data.frame(matrix(0, nrow = length(numeric((ceiling(log2(max(shannon_DF_, na.rm = TRUE)))))), ncol = 26)))
colnames(octaves_DF) <- colnames(shannon_DF_)


for(l in 1:ncol(shannon_DF_)){

    octaves_DF[,l] <- octaves2(shannon_DF_[,l])

}



octaves_DF <- octaves_DF[1:(nrow(octaves_DF)-(nrow(octaves_DF)%%4)),]
octaves_DF$octaves <- 1:nrow(octaves_DF)

for(l in 1: nrow(octaves_DF)){

  if(as.numeric(octaves_DF[l,]$octaves) %% 4 != 0){
    
    octaves_DF[l,]$octaves <- paste("Octave",paste(as.character(floor(as.numeric(octaves_DF[l,]$octaves)/4)*4+1),as.character((floor(as.numeric(octaves_DF[l,]$octaves)/4)+1)*4),sep ="-"), sep=" ")

  }else{

     octaves_DF[l,]$octaves <- paste("Octave",paste(as.character((floor(as.numeric(octaves_DF[l,]$octaves)/4)-1)*4+1),as.character(floor(as.numeric(octaves_DF[l,]$octaves)/4)*4),sep ="-"), sep=" ")


  }

}



octaves_DF_long <- reshape2::melt(octaves_DF,"octaves",variable.name = "Year")

octaves_DF <- octaves_DF %>%
  group_by(octaves) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

p100_ <- ggplot() +
  labs(title = "All Species Group",
       x = "Year",
       y = "Frequency") +
  theme_minimal() +
  theme_bw() +
  guides(color = guide_legend(title = "Octaves")) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 23, face = "bold"),
    axis.text = element_text(size = 23, face = "bold"),
    legend.title = element_text(size = 23.5, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22.5, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(1.5, "lines")
  )

# 假设数据集中年份为 1987 到 2001 年
years <- 1987:2012

# 合并所有行数据
all_data <- data.frame()

for(q in 1:nrow(octaves_DF)) {
  # 将行数据转换为数据框
  row_data <- data.frame(
    Year = years,
    Value = as.numeric(octaves_DF[q, 2:ncol(octaves_DF)]),
    Octave = factor(rep(octaves_DF[q,]$octaves, length(years)))  # 转换 Octave 为因子
  )
  
  # 合并所有数据
  all_data <- rbind(all_data, row_data)
}

# 绘制所有数据
p100_ <- p100_ + 
  geom_point(data = all_data, aes(x = Year, y = Value, color = Octave),size=4) +
  geom_line(data = all_data, aes(x = Year, y = Value, color = Octave),size=3)


octaves_DF <- data.frame(octaves_DF) 
octaves_DF <- octaves_DF[,-1]

for(n in 1:nrow(octaves_DF)) {
  if(octaves_DF[n,1]== max(octaves_DF[,1])){
common_octave <- as.numeric(octaves_DF[n,])

  }

}



for(h in 1:nrow(shannon_DF_)){

  shannon_DF_[h,] <- octaves(shannon_DF_[h,])

}






shannon_DF_long <- melt(shannon_DF_)

shannon_DF_long_ <- shannon_DF_long


shannon_DF_long_ <- shannon_DF_long_ %>%
  group_by(variable) %>%
  summarize(std_dev = sd(value,na.rm = T))


df_wide <- shannon_DF_long_ %>%
  pivot_wider(names_from = variable, values_from = std_dev)

df_wide <- data.frame(df_wide)
evenness_std <- as.numeric(df_wide[1,])
mean_std <- mean(evenness_std)




n_colors <- length(unique(shannon_DF_long$variable))
colors <- colorRampPalette(c("red", "blue"))(n_colors)




  p10 <- ggplot(shannon_DF_long, aes(x = value, fill = variable)) +
  geom_histogram(position = "dodge",  binwidth = 1.5) + # 使用 dodge 以便不同年份的直方图并排显示
  labs(title = "All Species Group",
       x = "Abundance Octave",
       y = "Frequency") +
  theme_minimal() +
  theme_bw() +
  guides(fill = guide_legend(title = "Year")) + 
  scale_fill_manual(values = colors) + 
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 20,face = "bold"),
    legend.title = element_text(size = 23, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22,face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(2, "lines"),
    legend.position = "none"
  )







for(r in 1:nrow(selected_data2)){
  for (c in c(2:27)){
    shannon_DF[r,c-1]<-selected_data2[r,c]/sum(na.omit(selected_data2[,c]))
    
  }
}


hill_DF <- shannon_DF
simpson_DF <- shannon_DF

   



    
for(r in 1:nrow(selected_data2)){
  for (c in c(2:27)){
    shannon_DF[r,c-1]<-shannon_DF[r,c-1]*log(shannon_DF[r,c-1])
hill_DF[r,c-1]<-(hill_DF[r,c-1])^1.5
simpson_DF[r,c-1]<-(simpson_DF[r,c-1])^2

  }
}


shannon_index<-rep(0,26)
hill_index<-rep(0,26)
simpson_index<-rep(0,26)
t_shannon_index<-rep(0,26)
N0_ <- rep(nrow(shannon_DF),26)


for(m in 1:26){
shannon_index[[m]] <- exp(-sum(na.omit(shannon_DF[,m])))
hill_index[[m]] <- 1/(sum(na.omit(hill_DF[,m])))^2
simpson_index[[m]] <- 1/(sum(na.omit(simpson_DF[,m])))
t_shannon_index[[m]] <- -sum(na.omit(shannon_DF[,m]))

}


Ip1_ <- 0.5* ((N0_/simpson_index)-1)
Ip0.5_ <- 1/(0.5* 1.5) * ((N0_/hill_index) ^ 0.5 - 1)
pielou_evenness <- t_shannon_index / log(nrow(selected_data2))




ordered_columns <- c("Binomial","1987",colnames(selected_data3)[2:ncol(selected_data3)])


selected_data3$"1987"<-rep(NA,nrow(selected_data3))
for(k in 1:nrow(selected_data3)){
  if(!is.na(selected_data3[k,"1988"])){
     selected_data3$"1987"[k]<-1
  }
}

order<-rep(0,length(ordered_columns))

for(i in 1:length(ordered_columns)){
  order[[i]]<-which(colnames(selected_data3)==ordered_columns[[i]])
}

selected_data3<-selected_data3[,order]




consecutive_nonzero_count <- 2 



find_first_non_na_index <- function(row) {
  non_na_indices <- which(!is.na(row))
  
  if (length(non_na_indices) == 0) {
    return(NULL)
  }
  
  # 找到连续的非缺失值序列及其长度
  consecutive_sequence <- split(non_na_indices, cumsum(c(0, diff(non_na_indices) != 1)))
  max_sequence_index <- which.max(lengths(consecutive_sequence))
  first_index <- min(unlist(consecutive_sequence[which.max(lengths(consecutive_sequence))]))
  sequence_length <- max(lengths(consecutive_sequence)[max_sequence_index])
  
  return(list(first_index = first_index, sequence_length = sequence_length))
}


# 遍历每一行
for (row_index in 1:nrow(selected_data3)) {
  
  # 找到第一个连续的数值序列的起始索引
   indices_info <- find_first_non_na_index(selected_data3[row_index, 2:27])

# 如果找到了连续数值序列
if (!is.null(indices_info)) {
    start_index <- indices_info$first_index
    sequence_length <- indices_info$sequence_length
    end_index <- start_index + sequence_length - 1


    if(start_index==1){
      start_index<-start_index+1
    }
  
  selected_data3[,2:27][row_index, start_index - 1] <- 1
  
  # 计算It值
  for (i in start_index:end_index) {
    selected_data3[,2:27][row_index, i] <- selected_data3[,2:27][row_index, i - 1]*10^selected_data3[,2:27][row_index, i]
    }
  }
}


selected_data3$avg_I_value<-rep(NA,nrow(selected_data3))

for(t in 1:nrow(selected_data3)){
   selected_data3$avg_I_value[t]<-exp(sum(log(selected_data3[t,2:27][,!is.na(selected_data3[t,2:27])]))/length(selected_data3[t,2:27][,!is.na(selected_data3[t,2:27])]))
   selected_data3[t,2:27] <- selected_data3[t,2:27] * selected_data3$global_abundance[[t]]/selected_data3$avg_I_value[[t]]
}

selected_data3 <- selected_data3 [!is.nan(selected_data3$avg_I_value),]



shannon_DF2<-selected_data3[,2:27]

shannon_DF2_ <- shannon_DF2


octaves_DF2 <- data.frame(data.frame(matrix(0, nrow = length(numeric((ceiling(log2(max(shannon_DF2_, na.rm = TRUE)))))), ncol = 26)))
colnames(octaves_DF2) <- colnames(shannon_DF2_)


octaves20 <- function(abundance_vector) {
  if (length(abundance_vector) == 0) {
    return(numeric(0))
  }

  max_abundance <- max(shannon_DF2_, na.rm = TRUE)

  if (!is.finite(max_abundance)) {
    stop("Invalid input: abundances vector contains non-numeric or NA/NaN values.")
  }

  num_octaves <- ceiling(log2(max_abundance))

  octave_counts <- numeric(num_octaves)

  for (i in 1:num_octaves) {
    lower_bound <- 2^(i - 1)
    upper_bound <- 2^i - 1
  octave_counts[i] <- sum(abundance_vector >= lower_bound & abundance_vector <= upper_bound,na.rm = TRUE)
  }

  return(octave_counts)
}

for(l in 1:ncol(shannon_DF2_)){

    octaves_DF2[,l] <- octaves20(shannon_DF2_[,l])

}




octaves_DF2 <- octaves_DF2[1:(nrow(octaves_DF2)-(nrow(octaves_DF2)%%4)),]
octaves_DF2$octaves <- 1:nrow(octaves_DF2)

for(l in 1: nrow(octaves_DF2)){

  if(as.numeric(octaves_DF2[l,]$octaves) %% 4 != 0){
    
    octaves_DF2[l,]$octaves <- paste("Octave",paste(as.character(floor(as.numeric(octaves_DF2[l,]$octaves)/4)*4+1),as.character((floor(as.numeric(octaves_DF2[l,]$octaves)/4)+1)*4),sep ="-"), sep=" ")

  }else{

     octaves_DF2[l,]$octaves <- paste("Octave",paste(as.character((floor(as.numeric(octaves_DF2[l,]$octaves)/4)-1)*4+1),as.character(floor(as.numeric(octaves_DF2[l,]$octaves)/4)*4),sep ="-"), sep=" ")


  }

}


octaves_DF2_long <- reshape2::melt(octaves_DF2,"octaves",variable.name = "Year")

octaves_DF2 <- octaves_DF2 %>%
  group_by(octaves) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

p200_ <- ggplot() +
  labs(title = "Dominant Species Group",
       x = "Year",
       y = "Frequency") +
  theme_minimal() +
  theme_bw() +
  guides(color = guide_legend(title = "Octaves")) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 23, face = "bold"),
    axis.text = element_text(size = 23, face = "bold"),
    legend.title = element_text(size = 23.5, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22.5, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(1.5, "lines")
  )

# 假设数据集中年份为 1987 到 2001 年
years <- 1987:2012

# 合并所有行数据
all_data <- data.frame()

for(q in 1:nrow(octaves_DF2)) {
  # 将行数据转换为数据框
  row_data <- data.frame(
    Year = years,
    Value = as.numeric(octaves_DF2[q,2:ncol(octaves_DF2)]),
    Octave = factor(rep(octaves_DF2[q,]$octaves, length(years)))  # 转换 Octave 为因子
  )
  
  # 合并所有数据
  all_data <- rbind(all_data, row_data)
}

# 绘制所有数据
p200_ <- p200_ + 
  geom_point(data = all_data, aes(x = Year, y = Value, color = Octave),size=4) +
  geom_line(data = all_data, aes(x = Year, y = Value, color = Octave),size=3)


octaves_DF2 <- data.frame(octaves_DF2) 
octaves_DF2 <- octaves_DF2[,-1]

for(n in 1:nrow(octaves_DF2)) {
  if(octaves_DF2[n,1]== max(octaves_DF2[,1])){
common_octave2 <- as.numeric(octaves_DF2[n,])

  }

}


for(h in 1:nrow(shannon_DF2_)){

  shannon_DF2_[h,] <- octaves(shannon_DF2_[h,])

}

shannon_DF2_long <- melt(shannon_DF2_)

shannon_DF2_long_ <- shannon_DF2_long

shannon_DF2_long_ <- shannon_DF2_long_ %>%
  group_by(variable) %>%
  summarize(std_dev = sd(value,na.rm = T))


df_wide2 <- shannon_DF2_long_ %>%
  pivot_wider(names_from = variable, values_from = std_dev)

df_wide2 <- data.frame(df_wide2)
evenness_std2 <- as.numeric(df_wide2[1,])
mean_std2 <- mean(evenness_std2)





p20 <- ggplot(shannon_DF2_long, aes(x = value, fill = variable)) +
  geom_histogram(position = "dodge",  binwidth = 1.5) +
  labs(title = "Dominant Species Group",
       x = "Abundance Octave",
       y = "Frequency") +
  theme_minimal()+
  theme_bw()+
    guides(fill = guide_legend(title = "Year")) + 
  scale_fill_manual(values = colors) + 
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 20,face = "bold"),
    legend.title = element_text(size = 23, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22,face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(2, "lines"),
    legend.position = "none"
  )







for(r in 1:nrow(selected_data3)){
  for (c in c(2:27)){
    shannon_DF2[r,c-1]<-selected_data3[r,c]/sum(na.omit(selected_data3[,c]))
    
  }
}


hill_DF2 <- shannon_DF2
simpson_DF2 <- shannon_DF2

    




for(r in 1:nrow(selected_data3)){
  for (c in c(2:27)){
    shannon_DF2[r,c-1]<-shannon_DF2[r,c-1]*log(shannon_DF2[r,c-1])
hill_DF2[r,c-1]<-(hill_DF2[r,c-1])^1.5
simpson_DF2[r,c-1]<-(simpson_DF2[r,c-1])^2

  }
}


shannon_index2<-rep(0,26)
hill_index2<-rep(0,26)
simpson_index2<-rep(0,26)
t_shannon_index2<-rep(0,26)
N0_2 <- rep(nrow(shannon_DF2),26)


for(m in 1:26){
shannon_index2[[m]] <- exp(-sum(na.omit(shannon_DF2[,m])))
hill_index2[[m]] <- 1/(sum(na.omit(hill_DF2[,m])))^2
simpson_index2[[m]] <- 1/(sum(na.omit(simpson_DF2[,m])))
t_shannon_index2[[m]] <- -sum(na.omit(shannon_DF2[,m]))

}

Ip1_2 <- 0.5* ((N0_2/simpson_index2)-1)
Ip0.5_2 <- 1/(0.5* 1.5) * ((N0_2/hill_index2) ^ 0.5 - 1)
pielou_evenness2 <- t_shannon_index2 / log(nrow(selected_data3))


ordered_columns <- c("Binomial","1987",colnames(selected_data4)[2:ncol(selected_data4)])


selected_data4$"1987"<-rep(NA,nrow(selected_data4))
for(k in 1:nrow(selected_data4)){
  if(!is.na(selected_data4[k,"1988"])){
     selected_data4$"1987"[k]<-1
  }
}

order<-rep(0,length(ordered_columns))

for(i in 1:length(ordered_columns)){
  order[[i]]<-which(colnames(selected_data4)==ordered_columns[[i]])
}

selected_data4<-selected_data4[,order]




consecutive_nonzero_count <- 2 



find_first_non_na_index <- function(row) {
  non_na_indices <- which(!is.na(row))
  
  if (length(non_na_indices) == 0) {
    return(NULL)
  }
  
  # 找到连续的非缺失值序列及其长度
  consecutive_sequence <- split(non_na_indices, cumsum(c(0, diff(non_na_indices) != 1)))
  max_sequence_index <- which.max(lengths(consecutive_sequence))
  first_index <- min(unlist(consecutive_sequence[which.max(lengths(consecutive_sequence))]))
  sequence_length <- max(lengths(consecutive_sequence)[max_sequence_index])
  
  return(list(first_index = first_index, sequence_length = sequence_length))
}


# 遍历每一行
for (row_index in 1:nrow(selected_data4)) {
  
  # 找到第一个连续的数值序列的起始索引
   indices_info <- find_first_non_na_index(selected_data4[row_index, 2:27])

# 如果找到了连续数值序列
if (!is.null(indices_info)) {
    start_index <- indices_info$first_index
    sequence_length <- indices_info$sequence_length
    end_index <- start_index + sequence_length - 1


    if(start_index==1){
      start_index<-start_index+1
    }
  
  selected_data4[,2:27][row_index, start_index - 1] <- 1
  
  # 计算It值
  for (i in start_index:end_index) {
    selected_data4[,2:27][row_index, i] <- selected_data4[,2:27][row_index, i - 1]*10^selected_data4[,2:27][row_index, i]
    }
  }
}


selected_data4$avg_I_value<-rep(NA,nrow(selected_data4))

for(t in 1:nrow(selected_data4)){
   selected_data4$avg_I_value[t]<-exp(sum(log(selected_data4[t,2:27][,!is.na(selected_data4[t,2:27])]))/length(selected_data4[t,2:27][,!is.na(selected_data4[t,2:27])]))
   selected_data4[t,2:27] <- selected_data4[t,2:27] * selected_data4$global_abundance[[t]]/selected_data4$avg_I_value[[t]]
}

selected_data4 <- selected_data4 [!is.nan(selected_data4$avg_I_value),]



shannon_DF3<-selected_data4[,2:27]
shannon_DF3_ <- shannon_DF3



octaves_DF3 <- data.frame(data.frame(matrix(0, nrow = length(numeric((ceiling(log2(max(shannon_DF3_, na.rm = TRUE)))))), ncol = 26)))
colnames(octaves_DF3) <- colnames(shannon_DF3_)


octaves200 <- function(abundance_vector) {
  if (length(abundance_vector) == 0) {
    return(numeric(0))
  }

  max_abundance <- max(shannon_DF3_, na.rm = TRUE)

  if (!is.finite(max_abundance)) {
    stop("Invalid input: abundances vector contains non-numeric or NA/NaN values.")
  }

  num_octaves <- ceiling(log2(max_abundance))

  octave_counts <- numeric(num_octaves)

  for (i in 1:num_octaves) {
    lower_bound <- 2^(i - 1)
    upper_bound <- 2^i - 1
  octave_counts[i] <- sum(abundance_vector >= lower_bound & abundance_vector <= upper_bound,na.rm = TRUE)
  }

  return(octave_counts)
}

for(l in 1:ncol(shannon_DF3_)){

    octaves_DF3[,l] <- octaves200(shannon_DF3_[,l])

}

octaves_DF3 <- octaves_DF3[1:(nrow(octaves_DF3)-(nrow(octaves_DF3)%%4)),]
octaves_DF3$octaves <- 1:nrow(octaves_DF3)


for(l in 1: nrow(octaves_DF3)){

  if(as.numeric(octaves_DF3[l,]$octaves) %% 4 != 0){
    
    octaves_DF3[l,]$octaves <- paste("Octave",paste(as.character(floor(as.numeric(octaves_DF3[l,]$octaves)/4)*4+1),as.character((floor(as.numeric(octaves_DF3[l,]$octaves)/4)+1)*4),sep ="-"), sep=" ")

  }else{

     octaves_DF3[l,]$octaves <- paste("Octave",paste(as.character((floor(as.numeric(octaves_DF3[l,]$octaves)/4)-1)*4+1),as.character(floor(as.numeric(octaves_DF3[l,]$octaves)/4)*4),sep ="-"), sep=" ")


  }

}

octaves_DF3_long <- reshape2::melt(octaves_DF3,"octaves",variable.name = "Year")


octaves_DF3 <- octaves_DF3 %>%
  group_by(octaves) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))


p300_ <- ggplot() +
  labs(title = "Rare Species Group",
       x = "Year",
       y = "Frequency") +
  theme_minimal() +
  theme_bw() +
  guides(color = guide_legend(title = "Octaves")) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 23, face = "bold"),
    axis.text = element_text(size = 23, face = "bold"),
    legend.title = element_text(size = 23.5, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22.5, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(1.5, "lines")
  )

# 假设数据集中年份为 1987 到 2001 年
years <- 1987:2012

# 合并所有行数据
all_data <- data.frame()

for(q in 1:nrow(octaves_DF3)) {
  # 将行数据转换为数据框
  row_data <- data.frame(
    Year = years,
    Value = as.numeric(octaves_DF3[q, 2:ncol(octaves_DF3)]),
    Octave = factor(rep(octaves_DF3[q,]$octaves, length(years)))  # 转换 Octave 为因子
  )
  
  # 合并所有数据
  all_data <- rbind(all_data, row_data)
}

# 绘制所有数据
p300_ <- p300_ + 
  geom_point(data = all_data, aes(x = Year, y = Value, color = Octave),size=4) +
  geom_line(data = all_data, aes(x = Year, y = Value, color = Octave),size=3)


octaves_DF3 <- data.frame(octaves_DF3) 
octaves_DF3 <- octaves_DF3[,-1]

for(n in 1:nrow(octaves_DF3)) {
  if(octaves_DF3[n,1]== max(octaves_DF3[,1])){
common_octave3 <- as.numeric(octaves_DF3[n,])

  }

}



for(h in 1:nrow(shannon_DF3_)){

  shannon_DF3_[h,] <- octaves(shannon_DF3_[h,])

}


shannon_DF_long3 <- melt(shannon_DF3_)

shannon_DF_long3_ <- shannon_DF_long3

shannon_DF_long3_ <- shannon_DF_long3_ %>%
  group_by(variable) %>%
  summarize(std_dev = sd(value,na.rm = T))


df_wide3 <- shannon_DF_long3_ %>%
  pivot_wider(names_from = variable, values_from = std_dev)

df_wide3 <- data.frame(df_wide3)
evenness_std3 <- as.numeric(df_wide3[1,])
mean_std3 <- mean(evenness_std3)



p30 <- ggplot(shannon_DF_long3, aes(x = value, fill = variable)) +
  geom_histogram(position = "dodge",  binwidth = 1.5) + 
  labs(title = "Rare Species Group",
       x = "Abundance Octave",
       y = "Frequency") +
  theme_minimal()+
  theme_bw() +
  guides(fill = guide_legend(title = "Year")) + 
  scale_fill_manual(values = colors) + 
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 23, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(2, "lines"),
    legend.position = "none"
  )




for(r in 1:nrow(selected_data4)){
  for (c in c(2:27)){
    shannon_DF3[r,c-1]<-selected_data4[r,c]/sum(na.omit(selected_data4[,c]))

  }
}


hill_DF3 <- shannon_DF3
simpson_DF3 <- shannon_DF3


for(r in 1:nrow(selected_data4)){
  for (c in c(2:27)){
    shannon_DF3[r,c-1]<-shannon_DF3[r,c-1]*log(shannon_DF3[r,c-1])
hill_DF3[r,c-1]<-(hill_DF3[r,c-1])^1.5
simpson_DF3[r,c-1]<-(simpson_DF3[r,c-1])^2

  }
}






shannon_index3<-rep(0,26)
hill_index3<-rep(0,26)
simpson_index3<-rep(0,26)
t_shannon_index3<-rep(0,26)
N0_3 <- rep(nrow(shannon_DF3),26)


for(m in 1:26){
shannon_index3[[m]] <- exp(-sum(na.omit(shannon_DF3[,m])))
hill_index3[[m]] <- 1/(sum(na.omit(hill_DF3[,m])))^2
simpson_index3[[m]] <- 1/(sum(na.omit(simpson_DF3[,m])))
t_shannon_index3[[m]] <- -sum(na.omit(shannon_DF3[,m]))

}

Ip1_3 <- 0.5* ((N0_3/simpson_index3)-1)
Ip0.5_3 <- 1/(0.5* 1.5) * ((N0_3/hill_index3) ^ 0.5 - 1)
pielou_evenness3 <- t_shannon_index3 / log(nrow(selected_data4))



ordered_columns <- c("Binomial","1987",colnames(selected_data5)[2:ncol(selected_data5)])


selected_data5$"1987"<-rep(NA,nrow(selected_data5))
for(k in 1:nrow(selected_data5)){
  if(!is.na(selected_data5[k,"1988"])){
     selected_data5$"1987"[k]<-1
  }
}

order<-rep(0,length(ordered_columns))

for(i in 1:length(ordered_columns)){
  order[[i]]<-which(colnames(selected_data5)==ordered_columns[[i]])
}

selected_data5<-selected_data5[,order]




consecutive_nonzero_count <- 2 



find_first_non_na_index <- function(row) {
  non_na_indices <- which(!is.na(row))
  
  if (length(non_na_indices) == 0) {
    return(NULL)
  }
  
  # 找到连续的非缺失值序列及其长度
  consecutive_sequence <- split(non_na_indices, cumsum(c(0, diff(non_na_indices) != 1)))
  max_sequence_index <- which.max(lengths(consecutive_sequence))
  first_index <- min(unlist(consecutive_sequence[which.max(lengths(consecutive_sequence))]))
  sequence_length <- max(lengths(consecutive_sequence)[max_sequence_index])
  
  return(list(first_index = first_index, sequence_length = sequence_length))
}


# 遍历每一行
for (row_index in 1:nrow(selected_data5)) {
  
  # 找到第一个连续的数值序列的起始索引
   indices_info <- find_first_non_na_index(selected_data5[row_index, 2:27])

# 如果找到了连续数值序列
if (!is.null(indices_info)) {
    start_index <- indices_info$first_index
    sequence_length <- indices_info$sequence_length
    end_index <- start_index + sequence_length - 1


    if(start_index==1){
      start_index<-start_index+1
    }
  
  selected_data5[,2:27][row_index, start_index - 1] <- 1
  
  # 计算It值
  for (i in start_index:end_index) {
    selected_data5[,2:27][row_index, i] <- selected_data5[,2:27][row_index, i - 1]*10^selected_data5[,2:27][row_index, i]
    }
  }
}


selected_data5$avg_I_value<-rep(NA,nrow(selected_data5))

for(t in 1:nrow(selected_data5)){
   selected_data5$avg_I_value[t]<-exp(sum(log(selected_data5[t,2:27][,!is.na(selected_data5[t,2:27])]))/length(selected_data5[t,2:27][,!is.na(selected_data5[t,2:27])]))
   selected_data5[t,2:27] <- selected_data5[t,2:27] * selected_data5$global_abundance[[t]]/selected_data5$avg_I_value[[t]]
}

selected_data5 <- selected_data5 [!is.nan(selected_data5$avg_I_value),]



shannon_DF4<-selected_data5[,2:27]

shannon_DF4_ <- shannon_DF4



octaves_DF4 <- data.frame(data.frame(matrix(0, nrow = length(numeric((ceiling(log2(max(shannon_DF4_, na.rm = TRUE)))))), ncol = 26)))
colnames(octaves_DF4) <- colnames(shannon_DF4_)



octaves2000 <- function(abundance_vector) {
  if (length(abundance_vector) == 0) {
    return(numeric(0))
  }

  max_abundance <- max(shannon_DF4_, na.rm = TRUE)

  if (!is.finite(max_abundance)) {
    stop("Invalid input: abundances vector contains non-numeric or NA/NaN values.")
  }

  num_octaves <- ceiling(log2(max_abundance))

  octave_counts <- numeric(num_octaves)

  for (i in 1:num_octaves) {
    lower_bound <- 2^(i - 1)
    upper_bound <- 2^i - 1
  octave_counts[i] <- sum(abundance_vector >= lower_bound & abundance_vector <= upper_bound,na.rm = TRUE)
  }

  return(octave_counts)
}


for(l in 1:ncol(shannon_DF4_)){

    octaves_DF4[,l] <- octaves2000(shannon_DF4_[,l])

}

octaves_DF4 <- octaves_DF4[1:(nrow(octaves_DF4)-(nrow(octaves_DF4)%%4)),]
octaves_DF4$octaves <- 1:nrow(octaves_DF4)

for(l in 1: nrow(octaves_DF4)){

  if(as.numeric(octaves_DF4[l,]$octaves) %% 4 != 0){
    
    octaves_DF4[l,]$octaves <- paste("Octave",paste(as.character(floor(as.numeric(octaves_DF4[l,]$octaves)/4)*4+1),as.character((floor(as.numeric(octaves_DF4[l,]$octaves)/4)+1)*4),sep ="-"), sep=" ")

  }else{

     octaves_DF4[l,]$octaves <- paste("Octave",paste(as.character((floor(as.numeric(octaves_DF4[l,]$octaves)/4)-1)*4+1),as.character(floor(as.numeric(octaves_DF4[l,]$octaves)/4)*4),sep ="-"), sep=" ")


  }

}



octaves_DF4_long <- reshape2::melt(octaves_DF4,"octaves",variable.name = "Year")


octaves_DF4 <- octaves_DF4 %>%
  group_by(octaves) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))


p400_ <- ggplot() +
  labs(title = "Less Dominant Species Group",
       x = "Year",
       y = "Frequency") +
  theme_minimal() +
  theme_bw() +
  guides(color = guide_legend(title = "Octaves")) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 23, face = "bold"),
    axis.text = element_text(size = 23, face = "bold"),
    legend.title = element_text(size = 23.5, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22.5, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(1.5, "lines")
  )

# 假设数据集中年份为 1987 到 2001 年
years <- 1987:2012

# 合并所有行数据
all_data <- data.frame()

for(q in 1:nrow(octaves_DF4)) {
  # 将行数据转换为数据框
  row_data <- data.frame(
    Year = years,
    Value = as.numeric(octaves_DF4[q, 2:ncol(octaves_DF4)]),
    Octave = factor(rep(octaves_DF4[q,]$octaves, length(years)))  # 转换 Octave 为因子
  )
  
  # 合并所有数据
  all_data <- rbind(all_data, row_data)
}

# 绘制所有数据
p400_ <- p400_ + 
  geom_point(data = all_data, aes(x = Year, y = Value, color = Octave),size=4) +
  geom_line(data = all_data, aes(x = Year, y = Value, color = Octave),size=3)


octaves_DF4 <- data.frame(octaves_DF4) 
octaves_DF4 <- octaves_DF4[,-1]

for(n in 1:nrow(octaves_DF4)) {
  if(octaves_DF4[n,1]== max(octaves_DF4[,1])){
common_octave4 <- as.numeric(octaves_DF4[n,])

  }

}



for(j in 1:nrow(shannon_DF4_)){

  shannon_DF4_[j,] <- octaves(shannon_DF4_[j,])
}

shannon_DF_long4 <- melt(shannon_DF4_)
shannon_DF_long4_ <- shannon_DF_long4

shannon_DF_long4_ <- shannon_DF_long4_ %>%
  group_by(variable) %>%
  summarize(std_dev = sd(value,na.rm = T))


df_wide4 <- shannon_DF_long4_ %>%
  pivot_wider(names_from = variable, values_from = std_dev)

df_wide4 <- data.frame(df_wide4)
evenness_std4 <- as.numeric(df_wide4[1,])
mean_std4 <- mean(evenness_std4)




  p40 <- ggplot(shannon_DF_long4, aes(x = value, fill = variable)) +
  geom_histogram(position = "dodge",  binwidth = 1.5) +
  labs(title = "Less Dominant Species Group",
       x = "Abundance Octave",
       y = "Frequency") +
  theme_minimal()+
  theme_bw() +
  guides(fill = guide_legend(title = "Year")) + 
  scale_fill_manual(values = colors) + 
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 23, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(2, "lines"),
    legend.position = "none"
  )




for(r in 1:nrow(selected_data5)){
  for (c in c(2:27)){
    shannon_DF4[r,c-1]<-selected_data5[r,c]/sum(na.omit(selected_data5[,c]))

  }
}


hill_DF4 <- shannon_DF4
simpson_DF4 <- shannon_DF4





for(r in 1:nrow(selected_data5)){
  for (c in c(2:27)){
    shannon_DF4[r,c-1]<-shannon_DF4[r,c-1]*log(shannon_DF4[r,c-1])
    hill_DF4[r,c-1]<-(hill_DF4[r,c-1])^1.5
    simpson_DF4[r,c-1]<-(simpson_DF4[r,c-1])^2

  }
}


shannon_index4<-rep(0,26)
hill_index4<-rep(0,26)
simpson_index4<-rep(0,26)
t_shannon_index4<-rep(0,26)
N0_4 <- rep(nrow(shannon_DF4),26)


for(m in 1:26){
shannon_index4[[m]] <- exp(-sum(na.omit(shannon_DF4[,m])))
hill_index4[[m]] <- 1/(sum(na.omit(hill_DF4[,m])))^2
simpson_index4[[m]] <- 1/(sum(na.omit(simpson_DF4[,m])))
t_shannon_index4[[m]] <- -sum(na.omit(shannon_DF4[,m]))

}


Ip1_4 <- 0.5* ((N0_4/simpson_index4)-1)
Ip0.5_4 <- 1/(0.5* 1.5) * ((N0_4/hill_index4) ^ 0.5 - 1)
pielou_evenness4 <- t_shannon_index4 / log(nrow(selected_data5))



ordered_columns <- c("Binomial","1987",colnames(selected_data6)[2:ncol(selected_data6)])


selected_data6$"1987"<-rep(NA,nrow(selected_data6))
for(k in 1:nrow(selected_data6)){
  if(!is.na(selected_data6[k,"1988"])){
     selected_data6$"1987"[k]<-1
  }
}

order<-rep(0,length(ordered_columns))

for(i in 1:length(ordered_columns)){
  order[[i]]<-which(colnames(selected_data6)==ordered_columns[[i]])
}

selected_data6<-selected_data6[,order]




consecutive_nonzero_count <- 2 



find_first_non_na_index <- function(row) {
  non_na_indices <- which(!is.na(row))
  
  if (length(non_na_indices) == 0) {
    return(NULL)
  }
  
  # 找到连续的非缺失值序列及其长度
  consecutive_sequence <- split(non_na_indices, cumsum(c(0, diff(non_na_indices) != 1)))
  max_sequence_index <- which.max(lengths(consecutive_sequence))
  first_index <- min(unlist(consecutive_sequence[which.max(lengths(consecutive_sequence))]))
  sequence_length <- max(lengths(consecutive_sequence)[max_sequence_index])
  
  return(list(first_index = first_index, sequence_length = sequence_length))
}


# 遍历每一行
for (row_index in 1:nrow(selected_data6)) {
  
  # 找到第一个连续的数值序列的起始索引
   indices_info <- find_first_non_na_index(selected_data6[row_index, 2:27])

# 如果找到了连续数值序列
if (!is.null(indices_info)) {
    start_index <- indices_info$first_index
    sequence_length <- indices_info$sequence_length
    end_index <- start_index + sequence_length - 1


    if(start_index==1){
      start_index<-start_index+1
    }
  
  selected_data6[,2:27][row_index, start_index - 1] <- 1
  
  # 计算It值
  for (i in start_index:end_index) {
    selected_data6[,2:27][row_index, i] <- selected_data6[,2:27][row_index, i - 1]*10^selected_data6[,2:27][row_index, i]
    }
  }
}


selected_data6$avg_I_value<-rep(NA,nrow(selected_data6))

for(t in 1:nrow(selected_data6)){
   selected_data6$avg_I_value[t]<-exp(sum(log(selected_data6[t,2:27][,!is.na(selected_data6[t,2:27])]))/length(selected_data6[t,2:27][,!is.na(selected_data6[t,2:27])]))
   selected_data6[t,2:27] <- selected_data6[t,2:27] * selected_data6$global_abundance[[t]]/selected_data6$avg_I_value[[t]]
}

selected_data6 <- selected_data6 [!is.nan(selected_data6$avg_I_value),]



shannon_DF5<-selected_data6[,2:27]

shannon_DF5_ <- shannon_DF5


octaves_DF5 <- data.frame(data.frame(matrix(0, nrow = length(numeric((ceiling(log2(max(shannon_DF5_, na.rm = TRUE)))))), ncol = 26)))
colnames(octaves_DF5) <- colnames(shannon_DF5_)



octaves20000 <- function(abundance_vector) {
  if (length(abundance_vector) == 0) {
    return(numeric(0))
  }

  max_abundance <- max(shannon_DF5_, na.rm = TRUE)

  if (!is.finite(max_abundance)) {
    stop("Invalid input: abundances vector contains non-numeric or NA/NaN values.")
  }

  num_octaves <- ceiling(log2(max_abundance))

  octave_counts <- numeric(num_octaves)

  for (i in 1:num_octaves) {
    lower_bound <- 2^(i - 1)
    upper_bound <- 2^i - 1
  octave_counts[i] <- sum(abundance_vector >= lower_bound & abundance_vector <= upper_bound,na.rm = TRUE)
  }

  return(octave_counts)
}

for(l in 1:ncol(shannon_DF5_)){

    octaves_DF5[,l] <- octaves20000(shannon_DF5_[,l])

}

octaves_DF5 <- octaves_DF5[1:(nrow(octaves_DF5)-(nrow(octaves_DF5)%%4)),]
octaves_DF5$octaves <- 1:nrow(octaves_DF5)

for(l in 1: nrow(octaves_DF5)){

  if(as.numeric(octaves_DF5[l,]$octaves) %% 4 != 0){
    
    octaves_DF5[l,]$octaves <- paste("Octave",paste(as.character(floor(as.numeric(octaves_DF5[l,]$octaves)/4)*4+1),as.character((floor(as.numeric(octaves_DF5[l,]$octaves)/4)+1)*4),sep ="-"), sep=" ")

  }else{

     octaves_DF5[l,]$octaves <- paste("Octave",paste(as.character((floor(as.numeric(octaves_DF5[l,]$octaves)/4)-1)*4+1),as.character(floor(as.numeric(octaves_DF5[l,]$octaves)/4)*4),sep ="-"), sep=" ")


  }

}

octaves_DF5_long <- reshape2::melt(octaves_DF5,"octaves",variable.name = "Year")


octaves_DF5 <- octaves_DF5 %>%
  group_by(octaves) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))



p500_ <- ggplot() +
  labs(title = "Less Rare Species Group",
       x = "Year",
       y = "Frequency") +
  theme_minimal() +
  theme_bw() +
  guides(color = guide_legend(title = "Octaves")) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 23, face = "bold"),
    axis.text = element_text(size = 23, face = "bold"),
    legend.title = element_text(size = 23.5, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size =22.5, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(1.5, "lines")
  )

# 假设数据集中年份为 1987 到 2001 年
years <- 1987:2012

# 合并所有行数据
all_data <- data.frame()

for(q in 1:nrow(octaves_DF5)) {
  # 将行数据转换为数据框
  row_data <- data.frame(
    Year = years,
    Value = as.numeric(octaves_DF5[q, 2:ncol(octaves_DF5)-1]),
    Octave = factor(rep(octaves_DF5[q,]$octaves, length(years)))  # 转换 Octave 为因子
  )
  
  # 合并所有数据
  all_data <- rbind(all_data, row_data)
}

# 绘制所有数据
p500_ <- p500_ + 
  geom_point(data = all_data, aes(x = Year, y = Value, color = Octave),size=4) +
  geom_line(data = all_data, aes(x = Year, y = Value, color = Octave),size=3)


octaves_DF5 <- data.frame(octaves_DF5) 
octaves_DF5 <- octaves_DF5[,-1]

for(n in 1:nrow(octaves_DF5)) {
  if(octaves_DF5[n,1]== max(octaves_DF5[,1])){
common_octave5 <- as.numeric(octaves_DF5[n,])

  }

}

for(j in 1: nrow(shannon_DF5_)){

  shannon_DF5_[j,] <- octaves(shannon_DF5_[j,])
}


shannon_DF_long5 <- melt(shannon_DF5_)

shannon_DF_long5_ <- shannon_DF_long5

shannon_DF_long5_ <- shannon_DF_long5_ %>%
  group_by(variable) %>%
  summarize(std_dev = sd(value,na.rm = T))

df_wide5 <- shannon_DF_long5_ %>%
  pivot_wider(names_from = variable, values_from = std_dev)
df_wide5 <- data.frame(df_wide5)

evenness_std5 <- as.numeric(df_wide5[1,])
mean_std5 <- mean(evenness_std5)



p50 <- ggplot(shannon_DF_long5, aes(x = value, fill = variable)) +
  geom_histogram(position = "dodge",  binwidth = 1.5) +
  labs(title = "Less Rare Species Group",
       x = "Abundance Octave",
       y = "Frequency") +
  theme_minimal()+
  theme_bw() +
  guides(fill = guide_legend(title = "Year")) + 
  scale_fill_manual(values = colors) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 23, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(1.6, "lines"),
    legend.position = "none"
  )







selected_data0 <- selected_data2[!(selected_data2$Binomial %in% selected_data3$Binomial) & !(selected_data2$Binomial %in% selected_data4$Binomial),]

selected_data0_ <- selected_data2_[!(selected_data2_$Binomial %in% selected_data3_$Binomial) & !(selected_data2_$Binomial %in% selected_data4_$Binomial),]


shannon_DF0<-selected_data0[,2:27]

shannon_DF0_ <- shannon_DF0


octaves_DF0 <- data.frame(data.frame(matrix(0, nrow = length(numeric((ceiling(log2(max(shannon_DF0_, na.rm = TRUE)))))), ncol = 26)))
colnames(octaves_DF0) <- colnames(shannon_DF0_)



octaves200000 <- function(abundance_vector) {
  if (length(abundance_vector) == 0) {
    return(numeric(0))
  }

  max_abundance <- max(shannon_DF0_, na.rm = TRUE)

  if (!is.finite(max_abundance)) {
    stop("Invalid input: abundances vector contains non-numeric or NA/NaN values.")
  }

  num_octaves <- ceiling(log2(max_abundance))

  octave_counts <- numeric(num_octaves)

  for (i in 1:num_octaves) {
    lower_bound <- 2^(i - 1)
    upper_bound <- 2^i - 1
  octave_counts[i] <- sum(abundance_vector >= lower_bound & abundance_vector <= upper_bound,na.rm = TRUE)
  }

  return(octave_counts)
}


for(l in 1:ncol(shannon_DF0_)){

    octaves_DF0[,l] <- octaves200000(shannon_DF0_[,l])

}

octaves_DF0 <- octaves_DF0[1:(nrow(octaves_DF0)-(nrow(octaves_DF0)%%4)),]
octaves_DF0$octaves <- 1:nrow(octaves_DF0)

for(l in 1: nrow(octaves_DF0)){

  if(as.numeric(octaves_DF0[l,]$octaves) %% 4 != 0){
    
    octaves_DF0[l,]$octaves <- paste("Octave",paste(as.character(floor(as.numeric(octaves_DF0[l,]$octaves)/4)*4+1),as.character((floor(as.numeric(octaves_DF0[l,]$octaves)/4)+1)*4),sep ="-"), sep=" ")

  }else{

     octaves_DF0[l,]$octaves <- paste("Octave",paste(as.character((floor(as.numeric(octaves_DF0[l,]$octaves)/4)-1)*4+1),as.character(floor(as.numeric(octaves_DF0[l,]$octaves)/4)*4),sep ="-"), sep=" ")


  }

}

octaves_DF0_long <- reshape2::melt(octaves_DF0,"octaves",variable.name = "Year")


octaves_DF0 <- octaves_DF0 %>%
  group_by(octaves) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))


p000_ <- ggplot() +
  labs(title = "Middle Species Group",
       x = "Year",
       y = "Frequency") +
  theme_minimal() +
  theme_bw() +
  guides(color = guide_legend(title = "Octaves")) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 23, face = "bold"),
    axis.text = element_text(size = 23, face = "bold"),
    legend.title = element_text(size = 23.5, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22.5, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(1.5, "lines")
  )

# 假设数据集中年份为 1987 到 2001 年
years <- 1987:2012

# 合并所有行数据
all_data <- data.frame()

for(q in 1:nrow(octaves_DF0)) {
  # 将行数据转换为数据框
  row_data <- data.frame(
    Year = years,
    Value = as.numeric(octaves_DF0[q, 2:ncol(octaves_DF0)]),
    Octave = factor(rep(octaves_DF0[q,]$octaves, length(years)))  # 转换 Octave 为因子
  )
  
  # 合并所有数据
  all_data <- rbind(all_data, row_data)
}

# 绘制所有数据
p000_ <- p000_ + 
  geom_point(data = all_data, aes(x = Year, y = Value, color = Octave),size=4) +
  geom_line(data = all_data, aes(x = Year, y = Value, color = Octave),size=3)



octaves_DF0 <- data.frame(octaves_DF0) 
octaves_DF0 <- octaves_DF0[,-1]

for(n in 1:nrow(octaves_DF0)) {
  if(octaves_DF0[n,1]== max(octaves_DF0[,1])){
common_octave0 <- as.numeric(octaves_DF0[n,])

  }

}


for(j in 1:nrow(shannon_DF0_)){

  shannon_DF0_[j,] <- octaves(shannon_DF0_[j,])

}

shannon_DF_long0 <- melt(shannon_DF0_)

shannon_DF_long0_ <- shannon_DF_long0


shannon_DF_long0_ <- shannon_DF_long0_ %>%
  group_by(variable) %>%
  summarize(std_dev = sd(value,na.rm = T))



df_wide0 <- shannon_DF_long0_ %>%
  pivot_wider(names_from = variable, values_from = std_dev)
df_wide0 <- data.frame(df_wide0)
evenness_std0 <- as.numeric(df_wide0[1,])
mean_std0 <- mean(evenness_std0)



p00 <- ggplot(shannon_DF_long0, aes(x = value, fill = variable)) +
  geom_histogram(position = "dodge",  binwidth = 1.5) +
  labs(title = "Middle Species Group",
       x = "Abundance Octave",
       y = "Frequency") +
  theme_minimal()+
  theme_bw() +
  guides(fill = guide_legend(title = "Year")) + 
  scale_fill_manual(values = colors) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 22, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 21, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(1.6, "lines")
    
  )








# 从其中一个子图提取图例

big_plot0 <- p10 + p20 + p30 + p40 + p50 + p00 + plot_layout(ncol = 2,guides = 'collect')
big_plot0 <- big_plot0 + 
  plot_annotation(title = "Distribution of Abundance of each Group in each Year",
                  theme = theme(
                    plot.title = element_text(size = 22, face = "bold"),
                    axis.title = element_text(size = 20, face = "bold"),
                    axis.text = element_text(size = 20, face = "bold"),
                    legend.title = element_text(size = 23, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 22, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(2, "lines") 
                  ))



ggsave("histogram_plot_for_distribution_of_abundance.png", plot = big_plot0, width = 15, height = 11,dpi = 300,limitsize = FALSE)





big_plot00_ <- p100_ + p200_ + p300_ + p400_ + p500_ + p000_ + plot_layout(ncol = 2)
big_plot00_ <- big_plot00_ + 
  plot_annotation(title = "Numbers of Species in each Abundance Octave of each Group in each Year",
                  theme = theme(
                    plot.title = element_text(size = 21, face = "bold"),
                    axis.title = element_text(size = 20, face = "bold"),
                    axis.text = element_text(size = 20, face = "bold"),
                    legend.title = element_text(size = 20.5, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 19.5, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(1.3, "lines") 
                  ))
ggsave("line_plot_for_abundance_octave_time_series.png", plot = big_plot00_, width = 18, height = 13,dpi = 300,limitsize = FALSE)







for(r in 1:nrow(selected_data6)){
  for (c in c(2:27)){
    shannon_DF5[r,c-1]<-selected_data6[r,c]/sum(na.omit(selected_data6[,c]))

  }
}

hill_DF5 <- shannon_DF5
simpson_DF5 <- shannon_DF5



for(r in 1:nrow(selected_data6)){
  for (c in c(2:27)){
    shannon_DF5[r,c-1]<-shannon_DF5[r,c-1]*log(shannon_DF5[r,c-1])
    hill_DF5[r,c-1]<-(hill_DF5[r,c-1])^1.5
    simpson_DF5[r,c-1]<-(simpson_DF5[r,c-1])^2

  }
}


shannon_index5<-rep(0,26)
hill_index5<-rep(0,26)
simpson_index5<-rep(0,26)
t_shannon_index5<-rep(0,26)
N0_5 <- rep(nrow(shannon_DF5),26)


for(m in 1:26){
shannon_index5[[m]] <- exp(-sum(na.omit(shannon_DF5[,m])))
hill_index5[[m]] <- 1/(sum(na.omit(hill_DF5[,m])))^2
simpson_index5[[m]] <- 1/(sum(na.omit(simpson_DF5[,m])))
t_shannon_index5[[m]] <- -sum(na.omit(shannon_DF5[,m]))

}


Ip1_5 <- 0.5* ((N0_5/simpson_index5)-1)
Ip0.5_5 <- 1/(0.5* 1.5) * ((N0_5/hill_index5) ^ 0.5 - 1)
pielou_evenness5 <- t_shannon_index5 / log(nrow(selected_data6))





for(r in 1:nrow(selected_data0)){
  for (c in c(2:27)){
    shannon_DF0[r,c-1]<-selected_data0[r,c]/sum(na.omit(selected_data0[,c]))

  }
}

hill_DF0 <- shannon_DF0
simpson_DF0 <- shannon_DF0



for(r in 1:nrow(selected_data0)){
  for (c in c(2:27)){
    shannon_DF0[r,c-1]<-shannon_DF0[r,c-1]*log(shannon_DF0[r,c-1])
    hill_DF0[r,c-1]<-(hill_DF0[r,c-1])^1.5
    simpson_DF0[r,c-1]<-(simpson_DF0[r,c-1])^2

  }
}


shannon_index0<-rep(0,26)
hill_index0<-rep(0,26)
simpson_index0<-rep(0,26)
t_shannon_index0<-rep(0,26)
N0_0 <- rep(nrow(shannon_DF0),26)

for(m in 1:26){
shannon_index0[[m]] <- exp(-sum(na.omit(shannon_DF0[,m])))
hill_index0[[m]] <- 1/(sum(na.omit(hill_DF0[,m])))^2
simpson_index0[[m]] <- 1/(sum(na.omit(simpson_DF0[,m])))
t_shannon_index0[[m]] <- -sum(na.omit(shannon_DF0[,m]))

}

Ip1_0 <- 0.5* ((N0_0/simpson_index0)-1)
Ip0.5_0 <- 1/(0.5* 1.5) * ((N0_0/hill_index0) ^ 0.5 - 1)

pielou_evenness0 <- t_shannon_index0 / log(nrow(selected_data0))

shannon_DF_ <- shannon_DF_[order(shannon_DF_[,1]),]
shannon_DF_ <- shannon_DF_[!is.na(shannon_DF_[,1]),]

  shannon_DF_$Initial_Octave <- shannon_DF_[,1]


p_ <- ggplot() +
  labs(title = "Time Series of Species Abundance",
       x = "Year",
       y = "Abundance Octave") +
  theme_minimal() +
  theme_bw() +
  guides(color = guide_legend(title = "Initial Octave"))

# 假设数据集中年份为 1987 到 2001 年
years <- 1987:2012

# 通过循环添加每一行的数据
for(q in 1:nrow(shannon_DF_)) {
  # 将行数据转换为数据框
  row_data <- data.frame(
    Year = years,
    Value = as.numeric(shannon_DF_[q, 1:(ncol(shannon_DF_) - 1)]),
    Initial_Octave = factor(rep(shannon_DF_[q, ncol(shannon_DF_)],26))
  )
  
  # 添加点和线到 ggplot 对象中
  p_ <- p_ + 
    geom_point(data = row_data, aes(x = Year, y = Value, color = Initial_Octave)) +
    geom_line(data = row_data, aes(x = Year, y = Value, color = Initial_Octave))+
    theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),  # 调整图例标题的大小和字体
    legend.text = element_text(size = 11, face = "bold"),                  # 调整图例文本的大小
    legend.key.size = unit(1, "lines") 
  )

}
  

ggsave("Absolute_Abundance_density_plot.png", plot = p_, width = 6, height = 4,limitsize = FALSE)






several_biodiversity_indexs <- data.frame(
  "index types" = c("unweighted LPI", "weighted LPI by abundance","unweighted LPI2", "weighted LPI by abundance2","unweighted LPI3", "weighted LPI by abundance3","half top unweighted LPI","half top weigted LPI","half low unweighted LPI","half low weighted LPI","half medium unweighted LPI","half medium weighted LPI"),
  matrix(NA, nrow = 12, ncol = 26)  # 创建一个 2 行 26 列的矩阵，初始值为 NA
)

# 为列添加年份标签
colnames(several_biodiversity_indexs)[-1] <- 1987:2012

for(k in 2:26){
   
  several_biodiversity_indexs[1,k+1]<-mean(na.omit(selected_data2_[,k]))

  several_biodiversity_indexs[2,k+1]<-sum(na.omit(selected_data2_[,k])*na.omit(selected_data2[,k])/sum(na.omit(selected_data2[,k])))

  

    several_biodiversity_indexs[3,k+1]<-mean(na.omit(selected_data3_[,k]))

  several_biodiversity_indexs[4,k+1]<-sum(na.omit(selected_data3_[,k])*na.omit(selected_data3[,k])/sum(na.omit(selected_data3[,k])))

    several_biodiversity_indexs[5,k+1]<-mean(na.omit(selected_data4_[,k]))

  several_biodiversity_indexs[6,k+1]<-sum(na.omit(selected_data4_[,k])*na.omit(selected_data4[,k])/sum(na.omit(selected_data4[,k])))

  several_biodiversity_indexs[7,k+1]<-mean(na.omit(selected_data5_[,k]))

  several_biodiversity_indexs[8,k+1]<-sum(na.omit(selected_data5_[,k])*na.omit(selected_data5[,k])/sum(na.omit(selected_data5[,k])))

  several_biodiversity_indexs[9,k+1]<-mean(na.omit(selected_data6_[,k]))

  several_biodiversity_indexs[10,k+1]<-sum(na.omit(selected_data6_[,k])*na.omit(selected_data6[,k])/sum(na.omit(selected_data6[,k])))

  several_biodiversity_indexs[11,k+1]<-mean(na.omit(selected_data0_[,k]))

  several_biodiversity_indexs[12,k+1]<-sum(na.omit(selected_data0_[,k])*na.omit(selected_data0[,k])/sum(na.omit(selected_data0[,k])))
}

several_biodiversity_indexs$"1987"<-1

for (i in 3:ncol(several_biodiversity_indexs)) {
    several_biodiversity_indexs[, i] <- several_biodiversity_indexs[, i - 1]*10^several_biodiversity_indexs[, i]
    }

lpi_index<-as.numeric(several_biodiversity_indexs[1,-1])
lpi_index2<-as.numeric(several_biodiversity_indexs[2,-1])


lpi_index_up<-as.numeric(several_biodiversity_indexs[3,-1])
lpi_index2_up<-as.numeric(several_biodiversity_indexs[4,-1])

lpi_index_low<-as.numeric(several_biodiversity_indexs[5,-1])
lpi_index2_low<-as.numeric(several_biodiversity_indexs[6,-1])

lpi_index_up_<-as.numeric(several_biodiversity_indexs[7,-1])
lpi_index2_up_<-as.numeric(several_biodiversity_indexs[8,-1])

lpi_index_low_<-as.numeric(several_biodiversity_indexs[9,-1])
lpi_index2_low_<-as.numeric(several_biodiversity_indexs[10,-1])

lpi_index_medium<-as.numeric(several_biodiversity_indexs[11,-1])
lpi_index2_medium<-as.numeric(several_biodiversity_indexs[12,-1])










lpi_rate<-rep(0,(length(lpi_index)-1))
shannon_rate<-rep(0,(length(shannon_index)-1))
shannon_rate2<-rep(0,(length(shannon_index2)-1))
shannon_rate3<-rep(0,(length(shannon_index3)-1))
shannon_rate4<-rep(0,(length(shannon_index4)-1))
shannon_rate5<-rep(0,(length(shannon_index5)-1))
shannon_rate0<-rep(0,(length(shannon_index0)-1))


simpson_rate<-rep(0,(length(simpson_index)-1))
simpson_rate2<-rep(0,(length(simpson_index2)-1))
simpson_rate3<-rep(0,(length(simpson_index3)-1))
simpson_rate4<-rep(0,(length(simpson_index4)-1))
simpson_rate5<-rep(0,(length(simpson_index5)-1))
simpson_rate0<-rep(0,(length(simpson_index0)-1))


hill_rate<-rep(0,(length(hill_index)-1))
hill_rate2<-rep(0,(length(hill_index2)-1))
hill_rate3<-rep(0,(length(hill_index3)-1))
hill_rate4<-rep(0,(length(hill_index4)-1))
hill_rate5<-rep(0,(length(hill_index5)-1))
hill_rate0<-rep(0,(length(hill_index0)-1))


pielou_rate<-rep(0,(length(pielou_evenness)-1))
pielou_rate2<-rep(0,(length(pielou_evenness2)-1))
pielou_rate3<-rep(0,(length(pielou_evenness3)-1))
pielou_rate4<-rep(0,(length(pielou_evenness4)-1))
pielou_rate5<-rep(0,(length(pielou_evenness5)-1))
pielou_rate0<-rep(0,(length(pielou_evenness0)-1))



lpi_rate2<-rep(0,(length(lpi_index2)-1))

lpi_rate2_up<-rep(0,(length(lpi_index2_up)-1))
lpi_rate_up<-rep(0,(length(lpi_index_up)-1))
lpi_rate2_low<-rep(0,(length(lpi_index2_low)-1))
lpi_rate_low<-rep(0,(length(lpi_index_low)-1))
lpi_rate2_up_<-rep(0,(length(lpi_index2_up_)-1))
lpi_rate_up_<-rep(0,(length(lpi_index_up_)-1))
lpi_rate2_low_<-rep(0,(length(lpi_index2_low_)-1))
lpi_rate_low_<-rep(0,(length(lpi_index_low_)-1))

lpi_rate2_medium<-rep(0,(length(lpi_index2_medium)-1))
lpi_rate_medium<-rep(0,(length(lpi_index_medium)-1))

for(h in 2:26){
  lpi_rate[[h-1]]<-(lpi_index[[h]]-lpi_index[[h-1]])/lpi_index[[h-1]]
  lpi_rate2[[h-1]]<-(lpi_index2[[h]]-lpi_index2[[h-1]])/lpi_index2[[h-1]]
  
    lpi_rate2_up[[h-1]]<-(lpi_index2_up[[h]]-lpi_index2_up[[h-1]])/lpi_index2_up[[h-1]]
  lpi_rate_up[[h-1]]<-(lpi_index_up[[h]]-lpi_index_up[[h-1]])/lpi_index_up[[h-1]]
   lpi_rate2_low[[h-1]]<-(lpi_index2_low[[h]]-lpi_index2_low[[h-1]])/lpi_index2_low[[h-1]]
  lpi_rate_low[[h-1]]<-(lpi_index_low[[h]]-lpi_index_low[[h-1]])/lpi_index_low[[h-1]]
     lpi_rate2_up_[[h-1]]<-(lpi_index2_up_[[h]]-lpi_index2_up_[[h-1]])/lpi_index2_up_[[h-1]]
  lpi_rate_up_[[h-1]]<-(lpi_index_up_[[h]]-lpi_index_up_[[h-1]])/lpi_index_up_[[h-1]]
   lpi_rate2_low_[[h-1]]<-(lpi_index2_low_[[h]]-lpi_index2_low_[[h-1]])/lpi_index2_low_[[h-1]]
  lpi_rate_low_[[h-1]]<-(lpi_index_low_[[h]]-lpi_index_low_[[h-1]])/lpi_index_low_[[h-1]]


   lpi_rate2_medium[[h-1]]<-(lpi_index2_medium[[h]]-lpi_index2_medium[[h-1]])/lpi_index2_medium[[h-1]]
  lpi_rate_medium[[h-1]]<-(lpi_index_medium[[h]]-lpi_index_medium[[h-1]])/lpi_index_medium[[h-1]]


  shannon_rate[[h-1]]<-(shannon_index[[h]]-shannon_index[[h-1]])/shannon_index[[h-1]]
    shannon_rate2[[h-1]]<-(shannon_index2[[h]]-shannon_index2[[h-1]])/shannon_index2[[h-1]]
  shannon_rate3[[h-1]]<-(shannon_index3[[h]]-shannon_index3[[h-1]])/shannon_index3[[h-1]]
  shannon_rate4[[h-1]]<-(shannon_index4[[h]]-shannon_index4[[h-1]])/shannon_index4[[h-1]]
  shannon_rate5[[h-1]]<-(shannon_index5[[h]]-shannon_index5[[h-1]])/shannon_index5[[h-1]]
  shannon_rate0[[h-1]]<-(shannon_index0[[h]]-shannon_index0[[h-1]])/shannon_index0[[h-1]]



  simpson_rate[[h-1]]<-(simpson_index[[h]]-simpson_index[[h-1]])/simpson_index[[h-1]]
    simpson_rate2[[h-1]]<-(simpson_index2[[h]]-simpson_index2[[h-1]])/simpson_index2[[h-1]]
  simpson_rate3[[h-1]]<-(simpson_index3[[h]]-simpson_index3[[h-1]])/simpson_index3[[h-1]]
  simpson_rate4[[h-1]]<-(simpson_index4[[h]]-simpson_index4[[h-1]])/simpson_index4[[h-1]]
  simpson_rate5[[h-1]]<-(simpson_index5[[h]]-simpson_index5[[h-1]])/simpson_index5[[h-1]]
  simpson_rate0[[h-1]]<-(simpson_index0[[h]]-simpson_index0[[h-1]])/simpson_index0[[h-1]]


  hill_rate[[h-1]]<-(hill_index[[h]]-hill_index[[h-1]])/hill_index[[h-1]]
    hill_rate2[[h-1]]<-(hill_index2[[h]]-hill_index2[[h-1]])/hill_index2[[h-1]]
  hill_rate3[[h-1]]<-(hill_index3[[h]]-hill_index3[[h-1]])/hill_index3[[h-1]]
  hill_rate4[[h-1]]<-(hill_index4[[h]]-hill_index4[[h-1]])/hill_index4[[h-1]]
  hill_rate5[[h-1]]<-(hill_index5[[h]]-hill_index5[[h-1]])/hill_index5[[h-1]]
  hill_rate0[[h-1]]<-(hill_index0[[h]]-hill_index0[[h-1]])/hill_index0[[h-1]]


  pielou_rate[[h-1]]<-(pielou_evenness[[h]]-pielou_evenness[[h-1]])/pielou_evenness[[h-1]]
    pielou_rate2[[h-1]]<-(pielou_evenness2[[h]]-pielou_evenness2[[h-1]])/pielou_evenness2[[h-1]]
  pielou_rate3[[h-1]]<-(pielou_evenness3[[h]]-pielou_evenness3[[h-1]])/pielou_evenness3[[h-1]]
  pielou_rate4[[h-1]]<-(pielou_evenness4[[h]]-pielou_evenness4[[h-1]])/pielou_evenness4[[h-1]]
  pielou_rate5[[h-1]]<-(pielou_evenness5[[h]]-pielou_evenness5[[h-1]])/pielou_evenness5[[h-1]]
  pielou_rate0[[h-1]]<-(pielou_evenness0[[h]]-pielou_evenness0[[h-1]])/pielou_evenness0[[h-1]]

}


avg_shannon_rate <- mean(shannon_rate)
avg_shannon_rate2 <- mean(shannon_rate2)
avg_shannon_rate3 <- mean(shannon_rate3)
avg_shannon_rate4 <- mean(shannon_rate4)
avg_shannon_rate5 <- mean(shannon_rate5)
avg_shannon_rate0 <- mean(shannon_rate0)


avg_simpson_rate <- mean(simpson_rate)
avg_simpson_rate2 <- mean(simpson_rate2)
avg_simpson_rate3 <- mean(simpson_rate3)
avg_simpson_rate4 <- mean(simpson_rate4)
avg_simpson_rate5 <- mean(simpson_rate5)
avg_simpson_rate0 <- mean(simpson_rate0)

sensitivity_rate <- abs ((avg_shannon_rate - avg_simpson_rate)/ avg_shannon_rate )
sensitivity_rate2 <- abs ((avg_shannon_rate2 - avg_simpson_rate2)/ avg_shannon_rate2 )
sensitivity_rate3 <- abs ((avg_shannon_rate3 - avg_simpson_rate3)/ avg_shannon_rate3 )
sensitivity_rate4 <- abs ((avg_shannon_rate4 - avg_simpson_rate4)/ avg_shannon_rate4 )
sensitivity_rate5 <- abs ((avg_shannon_rate5 - avg_simpson_rate5)/ avg_shannon_rate5 )
sensitivity_rate0 <- abs ((avg_shannon_rate0 - avg_simpson_rate0)/ avg_shannon_rate0 )





relative_sensitivity <- abs(simpson_rate / shannon_rate)
relative_sensitivity2 <- abs(simpson_rate2 / shannon_rate2)
relative_sensitivity3 <- abs(simpson_rate3 / shannon_rate3)
relative_sensitivity4 <- abs(simpson_rate4 / shannon_rate4)
relative_sensitivity5 <- abs(simpson_rate5 / shannon_rate5)
relative_sensitivity0 <- abs(simpson_rate0 / shannon_rate0)


sensitivity_rate_ <- mean(relative_sensitivity)
sensitivity_rate2_ <- mean(relative_sensitivity2)
sensitivity_rate3_ <- mean(relative_sensitivity3)
sensitivity_rate4_ <- mean(relative_sensitivity4)
sensitivity_rate5_ <- mean(relative_sensitivity5)
sensitivity_rate0_ <- mean(relative_sensitivity0)

relative_evenness <- mean(Ip1_)
relative_evenness2 <- mean(Ip1_2)
relative_evenness3 <- mean(Ip1_3)
relative_evenness4 <- mean(Ip1_4)
relative_evenness5 <- mean(Ip1_5)
relative_evenness0 <- mean(Ip1_0)


relative_evenness_ <- mean(Ip0.5_)
relative_evenness2_ <- mean(Ip0.5_2)
relative_evenness3_ <- mean(Ip0.5_3)
relative_evenness4_ <- mean(Ip0.5_4)
relative_evenness5_ <- mean(Ip0.5_5)
relative_evenness0_ <- mean(Ip0.5_0)



data <- data.frame(
  Year = 1987:2012,
  lpi_index2 = lpi_index2,
  shannon_index = shannon_index,

  lpi_index = lpi_index,
    shannon_index2 = shannon_index2,
  shannon_index3 = shannon_index3,
  shannon_index4 = shannon_index4,
  shannon_index5 = shannon_index5,
  shannon_index0 = shannon_index0,

 
  pielou_evenness = pielou_evenness,
  pielou_evenness2 = pielou_evenness2,
  pielou_evenness3 = pielou_evenness3,
  pielou_evenness4 = pielou_evenness4,
  pielou_evenness5 = pielou_evenness5,
  pielou_evenness0 = pielou_evenness0,


  hill_index2 = hill_index2,
  hill_index3 = hill_index3,
  hill_index4 = hill_index4,
  hill_index5 = hill_index5,
  hill_index0 = hill_index0,


  simpson_index = simpson_index,
  simpson_index2 = simpson_index2,
  simpson_index3 = simpson_index3,
  simpson_index4 = simpson_index4,
  simpson_index5 = simpson_index5,
  simpson_index5 = simpson_index5,


  lpi_index2_up = lpi_index2_up,
  lpi_index_up = lpi_index_up,
  lpi_index2_low = lpi_index2_low,
  lpi_index_low = lpi_index_low,
  lpi_index2_up_ = lpi_index2_up_,
  lpi_index_up_ = lpi_index_up_,
  lpi_index2_low_ = lpi_index2_low_,
  lpi_index_low_ = lpi_index_low_,
    lpi_index2_medium = lpi_index2_medium,
  lpi_index_medium = lpi_index_medium,


  evenness_std <- evenness_std,
    evenness_std2 <- evenness_std2,
  evenness_std3 <- evenness_std3,
  evenness_std4 <- evenness_std4,
  evenness_std5 <- evenness_std5,
  evenness_std0 <- evenness_std0,

  Ip1_ <- Ip1_,
  Ip1_2 <- Ip1_2,
  Ip1_3 <- Ip1_3,
  Ip1_4 <- Ip1_4,
  Ip1_5 <- Ip1_5,
  Ip1_0 <- Ip1_0,

  Ip0.5_ <- Ip0.5_,
  Ip0.5_2 <- Ip0.5_2,
  Ip0.5_3 <- Ip0.5_3,
  Ip0.5_4 <- Ip0.5_4,
  Ip0.5_5 <- Ip0.5_5,
  Ip0.5_0 <- Ip0.5_0

)

# 绘图
p_3 <- ggplot(data, aes(x = Year)) +

  geom_point(aes(y = shannon_index, color = "All Species Group"), size = 2.5) +
    geom_point(aes(y = shannon_index2, color = "Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = shannon_index3, color = "Rare Species Group"), size = 2.5) +
  geom_point(aes(y = shannon_index4, color = "Less Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = shannon_index5, color = "Less Rare Species Group"), size = 2.5) +
    geom_point(aes(y = shannon_index0, color = "Middle Species Group"), size = 2.5) +



  geom_line(aes(y = shannon_index, color = "All Species Group"), size = 2,se = FALSE) +
    geom_line(aes(y = shannon_index2, color = "Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = shannon_index3, color = "Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = shannon_index4, color = "Less Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = shannon_index5, color = "Less Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = shannon_index0, color = "Middle Species Group"), size = 2,se = FALSE) +

   



  scale_color_manual(values = c("#0072B2","#ff00a2","#6600ff","blue","red","#00fbff"), 
                     name = "Abundance-based Species Groups") +
  labs(x = "Year", y = "Index Value", title = "Different Hill numbers(q=1) Values") +
    theme_bw() +
   theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 19, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "right")




p_4 <- ggplot(data, aes(x = Year)) +

  geom_point(aes(y = simpson_index, color = "All Species Group"), size = 2.5) +
    geom_point(aes(y = simpson_index2, color = "Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = simpson_index3, color = "Rare Species Group"), size = 2.5) +
  geom_point(aes(y = simpson_index4, color = "Less Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = simpson_index5, color = "Less Rare Species Group"), size = 2.5) +
   geom_point(aes(y = simpson_index0, color = "Middle Species Group"), size = 2.5) +




  geom_line(aes(y = simpson_index, color = "All Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = simpson_index2, color = "Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = simpson_index3, color = "Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = simpson_index4, color = "Less Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = simpson_index5, color = "Less Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = simpson_index0, color = "Middle Species Group"), size = 2,se = FALSE) +

  



  scale_color_manual(values = c("#0072B2","#ff00a2","#6600ff","blue","red","#00fbff"), 
                     name = "Abundance-based Species Groups") +
  labs(x = "Year", y = "Hill Index Value", title = "Different Hill numbers(q=2) Values") +
    theme_bw() +
   theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 19, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "right")




p_5 <- ggplot(data, aes(x = Year)) +

  geom_point(aes(y = evenness_std, color = "All Species Group"), size = 2.5) +
    geom_point(aes(y = evenness_std2, color = "Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = evenness_std3, color = "Rare Species Group"), size = 2.5) +
  geom_point(aes(y = evenness_std4, color = "Less Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = evenness_std5, color = "Less Rare Species Group"), size = 2.5) +
   geom_point(aes(y = evenness_std0, color = "Middle Species Group"), size = 2.5) +




  geom_line(aes(y = evenness_std, color = "All Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = evenness_std2, color = "Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = evenness_std3, color = "Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = evenness_std4, color = "Less Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = evenness_std5, color = "Less Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = evenness_std0, color = "Middle Species Group"), size = 2,se = FALSE) +

  



  scale_color_manual(values = c("#0072B2","#ff00a2","#6600ff","blue","red","#00fbff"), 
                     name = "Abundamce-based Species Groups") +
  labs(x = "Year", y = "Standard Deviation Value", title = "Abundance Distribution in Each Species Group") +
    theme_bw() +
   theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 19, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "right")



p_6 <- ggplot(data, aes(x = Year)) +

  geom_point(aes(y = pielou_evenness, color = "All Species Group"), size = 2.5) +
    geom_point(aes(y = pielou_evenness2, color = "Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = pielou_evenness3, color = "Rare Species Group"), size = 2.5) +
  geom_point(aes(y = pielou_evenness4, color = "Less Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = pielou_evenness5, color = "Less Rare Species Group"), size = 2.5) +
   geom_point(aes(y = pielou_evenness0, color = "Middle Species Group"), size = 2.5) +




  geom_line(aes(y = pielou_evenness, color = "All Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = pielou_evenness2, color = "Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = pielou_evenness3, color = "Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = pielou_evenness4, color = "Less Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = pielou_evenness5, color = "Less Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = pielou_evenness0, color = "Middle Species Group"), size = 2,se = FALSE) +

  



  scale_color_manual(values = c("#0072B2","#ff00a2","#6600ff","blue","red","#00fbff"), 
                     name = "Abundance-based Species Groups") +
  labs(x = "Year", y = "Pielou Evenness Value", title = "Pielou Evenness in Each Species Group") +
    theme_bw() +
   theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 19, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "right")




p_7 <- ggplot(data, aes(x = Year)) +

  geom_point(aes(y = Ip1_, color = "All Species Group"), size = 2.5) +
    geom_point(aes(y = Ip1_2, color = "Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = Ip1_3, color = "Rare Species Group"), size = 2.5) +
  geom_point(aes(y = Ip1_4, color = "Less Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = Ip1_5, color = "Less Rare Species Group"), size = 2.5) +
   geom_point(aes(y = Ip1_0, color = "Middle Species Group"), size = 2.5) +




  geom_line(aes(y = Ip1_, color = "All Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = Ip1_2, color = "Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = Ip1_3, color = "Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = Ip1_4, color = "Less Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = Ip1_5, color = "Less Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = Ip1_0, color = "Middle Species Group"), size = 2,se = FALSE) +

  



  scale_color_manual(values = c("#0072B2","#ff00a2","#6600ff","blue","red","#00fbff"), 
                     name = "Abundance-based Species Groups") +
  labs(x = "Year", y = "Relative Evenness Value(λ=1)", title = "Relative Evenness in Each Species Group(λ=1)") +
    theme_bw() +
   theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 19, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "right")


p_8 <- ggplot(data, aes(x = Year)) +

  geom_point(aes(y = Ip0.5_, color = "All Species Group"), size = 2.5) +
    geom_point(aes(y = Ip0.5_2, color = "Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = Ip0.5_3, color = "Rare Species Group"), size = 2.5) +
  geom_point(aes(y = Ip0.5_4, color = "Less Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = Ip0.5_5, color = "Less Rare Species Group"), size = 2.5) +
   geom_point(aes(y = Ip0.5_0, color = "Middle Species Group"), size = 2.5) +




  geom_line(aes(y = Ip0.5_, color = "All Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = Ip0.5_2, color = "Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = Ip0.5_3, color = "Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = Ip0.5_4, color = "Less Dominant Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = Ip0.5_5, color = "Less Rare Species Group"), size = 2,se = FALSE) +
  geom_line(aes(y = Ip0.5_0, color = "Middle Species Group"), size = 2,se = FALSE) +

  



  scale_color_manual(values = c("#0072B2","#ff00a2","#6600ff","blue","red","#00fbff"), 
                     name = "Abundance-based Species Groups") +
  labs(x = "Year", y = "Relative Evenness Value(λ=0.5)", title = "Relative Evenness in Each Species Group(λ=0.5)") +
    theme_bw() +
   theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 19, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "right")



years <- 1988:2012
biodiv_data <- data.frame(
  year = years,
  lpi_abundance_weighted = lpi_rate2,
  shannon_entropy = shannon_rate,
    shannon_entropy2 = shannon_rate2,
  shannon_entropy3 = shannon_rate3,
  shannon_entropy4 = shannon_rate4,
  shannon_entropy5 = shannon_rate5,
   shannon_entropy0 = shannon_rate0,


 

 hill_rate = hill_rate,
    hill_rate2 = hill_rate2,
  hill_rate3 = hill_rate3,
  hill_rate4 = hill_rate4,
  hill_rate5 = hill_rate5,
   hill_rate0 = hill_rate0,


 simpson_rate = simpson_rate,
    simpson_rate2 = simpson_rate2,
  simpson_rate3 = simpson_rate3,
  simpson_rate4 = simpson_rate4,
  simpson_rate5 = simpson_rate5,
  simpson_rate0 = simpson_rate0,


pielou_evenness = pielou_rate,
    pielou_evenness2 = pielou_rate2,
    pielou_evenness3 = pielou_rate3,
    pielou_evenness4 = pielou_rate4,
    pielou_evenness5 = pielou_rate5,
    pielou_evenness0 = pielou_rate0,
 

  lpi_unweighted = lpi_rate,
  
  
  lpi_abundance_weighted_up = lpi_rate2_up,
  lpi_abundance_weighted_low = lpi_rate2_low,
  lpi_unweighted_up = lpi_rate_up,
  lpi_unweighted_low = lpi_rate_low,
  lpi_abundance_weighted_up_ = lpi_rate2_up_,
  lpi_abundance_weighted_low_ = lpi_rate2_low_,
  lpi_unweighted_up_ = lpi_rate_up_,
  lpi_unweighted_low_ = lpi_rate_low_,
lpi_abundance_weighted_medium = lpi_rate2_medium,
lpi_unweighted_medium = lpi_rate_medium,



relative_sensitivity <- relative_sensitivity,
relative_sensitivity2 <- relative_sensitivity2,
relative_sensitivity3 <- relative_sensitivity3,
relative_sensitivity4 <- relative_sensitivity4,
relative_sensitivity5 <- relative_sensitivity5,
relative_sensitivity0 <- relative_sensitivity0


)

# 使用 ggplot 绘制图形
p_1_ <- ggplot(biodiv_data, aes(x = year)) +
  geom_point(aes(y = lpi_unweighted, color = "All Species Group"), size = 2.5) +
  geom_point(aes(y = lpi_unweighted_up, color = "Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = lpi_unweighted_low, color = "Rare Species Group"), size = 2.5) +
  geom_point(aes(y = lpi_unweighted_up_, color = "Less Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = lpi_unweighted_low_, color = "Less Rare Species Group"), size = 2.5) +
  geom_point(aes(y = lpi_unweighted_medium, color = "Middle Species Group"), size = 2.5) +

  geom_line(aes(y = lpi_unweighted, color = "All Species Group"), size = 2, se = FALSE) +
  
    geom_line(aes(y = lpi_unweighted_up, color = "Dominant Species Group"), size = 2, se = FALSE) +
  geom_line(aes(y = lpi_unweighted_low, color = "Rare Species Group"), size = 2, se = FALSE) +
      geom_line(aes(y = lpi_unweighted_up_, color = "Less Dominant Species Group"), size = 2, se = FALSE) +
  geom_line(aes(y = lpi_unweighted_low_, color = "Less Rare Species Group"), size = 2, se = FALSE) +
  geom_line(aes(y = lpi_unweighted_medium, color = "Middle Species Group"), size = 2, se = FALSE) +

  labs(x = "Year", y = "LPI Growth Rate", title = "Growth Rates of Different Original LPI") +
  scale_color_manual(values = c("#0072B2","#ff00a2","#6600ff","blue","red","#00fbff")) +
  theme_minimal() +
  guides(color = guide_legend(title = "Abundance-based Species Groups")) +
  theme_bw()+
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text = element_text(size = 17, face = "bold"),
        legend.title = element_text(size = 19, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "right")





p_2_ <- ggplot(biodiv_data, aes(x = year)) +
  geom_point(aes(y = lpi_abundance_weighted, color = "All Species Group"), size = 2.5) +
  geom_point(aes(y = lpi_abundance_weighted_up, color = "Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = lpi_abundance_weighted_low, color = "Rare Species Group"), size = 2.5) +
    geom_point(aes(y = lpi_abundance_weighted_up_, color = "Less Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = lpi_abundance_weighted_low_, color = "Less Rare Species Group"), size = 2.5) +
  geom_point(aes(y = lpi_abundance_weighted_medium, color = "Middle Species Group"), size = 2.5) +


  geom_line(aes(y = lpi_abundance_weighted, color = "All Species Group"), size = 2, se = FALSE) +
  labs(x = "Year", y = "Weighted LPI Growth Rate", title = "Growth Rates of LPI Weighted by Abundance") +
    geom_line(aes(y = lpi_abundance_weighted_up, color = "Dominant Species Group"), size = 2, se = FALSE) +
  geom_line(aes(y = lpi_abundance_weighted_low, color = "Rare Species Group"), size = 2, se = FALSE) +
      geom_line(aes(y = lpi_abundance_weighted_up_, color = "Less Dominant Species Group"), size = 2, se = FALSE) +
  geom_line(aes(y = lpi_abundance_weighted_low_, color = "Less Rare Species Group"), size = 2, se = FALSE) +
  geom_line(aes(y = lpi_abundance_weighted_medium, color = "Middle Species Group"), size = 2, se = FALSE) +

  scale_color_manual(values = c("#0072B2","#ff00a2","#6600ff","blue","red","#00fbff")) +
  theme_minimal() +
    guides(color = guide_legend(title = "Abundance-based Species Groups")) +
  theme_bw()+
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text = element_text(size = 17, face = "bold"),
        legend.title = element_text(size = 19, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "right")
    


    big_plot2 <- p_1_ + p_2_ + plot_layout(ncol = 1)

ggsave("../Results/W&N_LPI_Growth_Rates.png", plot = big_plot2, width = 13, height = 7, dpi = 300, limitsize = FALSE)





p_3_ <- ggplot(biodiv_data, aes(x = year)) +
  geom_point(aes(y = relative_sensitivity, color = "All Species Group"), size = 2.5) +
  geom_point(aes(y = relative_sensitivity2, color = "Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = relative_sensitivity3, color = "Rare Species Group"), size = 2.5) +
  geom_point(aes(y = relative_sensitivity4, color = "Less Dominant Species Group"), size = 2.5) +
  geom_point(aes(y = relative_sensitivity5, color = "Less Rare Species Group"), size = 2.5) +
  geom_point(aes(y = relative_sensitivity0, color = "Middle Species Group"), size = 2.5) +

  geom_line(aes(y = relative_sensitivity, color = "All Species Group"), size = 2, se = FALSE) +
  
    geom_line(aes(y = relative_sensitivity2, color = "Dominant Species Group"), size = 2, se = FALSE) +
  geom_line(aes(y = relative_sensitivity3, color = "Rare Species Group"), size = 2, se = FALSE) +
      geom_line(aes(y = relative_sensitivity4, color = "Less Dominant Species Group"), size = 2, se = FALSE) +
  geom_line(aes(y = relative_sensitivity5, color = "Less Rare Species Group"), size = 2, se = FALSE) +
  geom_line(aes(y = relative_sensitivity0, color = "Middle Species Group"), size = 2, se = FALSE) +

  ylim(0,7.5)+

  labs(x = "Year", y = "Evenness Sensitivity", title = "Relative Evenness Sensitivity") +
  scale_color_manual(values = c("#0072B2","#ff00a2","#6600ff","blue","red","#00fbff")) +
  theme_minimal() +
  guides(color = guide_legend(title = "Abundance-based Species Groups")) +
  theme_bw()+
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text = element_text(size = 17, face = "bold"),
        legend.title = element_text(size = 19, face = "bold"),
        legend.text = element_text(size = 18, face = "bold"),
        legend.position = "right")



big_plot34 <- p_3 + p_4  + plot_layout(ncol = 1)

ggsave("../Results/Changing_Evenness_Values.png",plot = big_plot34, width = 9, height = 7, dpi = 300, limitsize = FALSE)




big_plot_evenness <- p_5 + p_3_  + plot_layout(ncol = 1)

ggsave("../Results/Evenness_Sensitivity.png",plot = big_plot_evenness, width = 9, height = 7.5, dpi = 300, limitsize = FALSE)




big_plot_relative <-p_6+ p_7 + p_8  + plot_layout(ncol = 1)

ggsave("../Results/Relative_Evenness.png",plot = big_plot_relative, width = 9, height = 10, dpi = 300, limitsize = FALSE)


years <- 1988:2012
biodiv_data <- data.frame(
  Year = years,
  lpi_abundance_weighted = lpi_rate2,
  shannon_entropy = shannon_rate,
    shannon_entropy2 = shannon_rate2,
  shannon_entropy3 = shannon_rate3,
  shannon_entropy4 = shannon_rate4,
  shannon_entropy5 = shannon_rate5,
  shannon_entropy0 = shannon_rate0,


hill_rate = hill_rate,
    hill_rate2 = hill_rate2,
  hill_rate3 = hill_rate3,
  hill_rate4 = hill_rate4,
  hill_rate5 = hill_rate5,
   hill_rate0 = hill_rate0,


 simpson_rate = simpson_rate,
    simpson_rate2 = simpson_rate2,
  simpson_rate3 = simpson_rate3,
  simpson_rate4 = simpson_rate4,
  simpson_rate5 = simpson_rate5,
  simpson_rate0 = simpson_rate0,



pielou_evenness = pielou_rate,
    pielou_evenness2 = pielou_rate2,
    pielou_evenness3 = pielou_rate3,
    pielou_evenness4 = pielou_rate4,
    pielou_evenness5 = pielou_rate5,
    pielou_evenness0 = pielou_rate0,


  lpi_unweighted = lpi_rate,
  
  
  lpi_abundance_weighted_up = lpi_rate2_up,
  lpi_abundance_weighted_low = lpi_rate2_low,
  lpi_unweighted_up = lpi_rate_up,
  lpi_unweighted_low = lpi_rate_low,
  lpi_abundance_weighted_up_ = lpi_rate2_up_,
  lpi_abundance_weighted_low_ = lpi_rate2_low_,
  lpi_unweighted_up_ = lpi_rate_up_,
  lpi_unweighted_low_ = lpi_rate_low_,
  lpi_abundance_weighted_medium = lpi_rate2_medium,
  lpi_unweighted_medium = lpi_rate_medium

)
p1_ <- ggplot(biodiv_data, aes(x = Year)) +
  geom_point(aes(y = lpi_unweighted, color = "rate of unweighted LPI(all species)"), size = 1.5) +
  geom_point(aes(y = lpi_abundance_weighted, color = "rate of weighted LPI by abundance(all species)"), size = 1.5) +
  geom_point(aes(y = shannon_rate, color = "rate of shannon index(all species)"), size = 1.5) +
    geom_point(aes(y = simpson_rate, color = "rate of simpson index(all species)"), size = 1.5) +
  geom_point(aes(y = pielou_evenness, color = "rate of pielou evenness(all species)"), size = 1.5) +


  geom_line(aes(y = lpi_unweighted, color = "rate of unweighted LPI(all species)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = lpi_abundance_weighted, color = "rate of weighted LPI by abundance(all species)"), size = 1.2,se = FALSE) +
  geom_line(aes(y = shannon_rate, color = "rate of shannon index(all species)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = simpson_rate, color = "rate of simpson index(all species)"), size = 1.2,se = FALSE) +
  geom_line(aes(y = pielou_evenness, color = "rate of pielou evenness(all species)"), size = 1.2,se = FALSE) +

  scale_color_manual(values = c("purple","blue","#ff00ff","#00fbff","black"), 
                     name = "growth rate of different indexes for all selected species") +
  labs(x = "Year", y = "Different Growth rate Value") +
  
  guides(color = guide_legend(title = "Rates of Different Indexes")) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.position = "right")
  



p2_ <- ggplot(biodiv_data, aes(x = Year)) +

  geom_point(aes(y = lpi_abundance_weighted_up, color = "rate of weighted LPI by abundance(top25%)"), size = 1.5) +
  geom_point(aes(y = lpi_unweighted_up, color = "rate of unweighted LPI(top25%)"), size = 1.5) +
  geom_point(aes(y = pielou_evenness2, color = "rate of pielou evenness(top25%)"), size = 1.5) +
   geom_point(aes(y = shannon_rate2, color = "rate of shannon index(top25%)"), size = 1.5) +
    geom_point(aes(y = simpson_rate2, color = "rate of simpson index(top25%)"), size = 1.5) +


  geom_line(aes(y = lpi_unweighted_up, color = "rate of unweighted LPI(top25%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = lpi_abundance_weighted_up, color = "rate of weighted LPI by abundance(top25%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = pielou_evenness2, color = "rate of pielou evenness(top25%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = shannon_rate2, color = "rate of shannon index(top25%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = simpson_rate2, color = "rate of simpson index(top25%)"), size = 1.2,se = FALSE) +


  scale_color_manual(values = c("purple","blue","#ff00ff","#00fbff","black"), 
                     name = "different growth rate of indexes for species with abundance over25%") +
  labs(x = "Year", y = "different growth rate values") +
    guides(color = guide_legend(title = "Rates of Different Indexes")) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.position = "right")
  





# 绘图
p3_ <- ggplot(biodiv_data, aes(x = Year)) +
  geom_point(aes(y = lpi_unweighted_low, color = "rate of unweighted LPI(bottom25%)"), size = 1.5) +
  geom_point(aes(y = lpi_abundance_weighted_low, color = "rate of weighted LPI by abundance(bottom25%)"), size = 1.5) +
  geom_point(aes(y = pielou_evenness3, color = "rate of pielou evenness(bottom25%)"), size = 1.5) +
   geom_point(aes(y = shannon_rate3, color = "rate of shannon index(bottom25%)"), size = 1.5) +
    geom_point(aes(y = simpson_rate3, color = "rate of simpson index(bottom25%)"), size = 1.5) +

  
  geom_line(aes(y = lpi_unweighted_low, color = "rate of unweighted LPI(bottom25%)"), size = 1.2,se = FALSE) +
  geom_line(aes(y = lpi_abundance_weighted_low, color = "rate of weighted LPI by abundance(bottom25%)"), size = 1.2,se = FALSE) +
  geom_line(aes(y = pielou_evenness3, color = "rate of pielou evenness(bottom25%)"), size = 1.2,se = FALSE) +
 geom_line(aes(y = shannon_rate3, color = "rate of shannon index(bottom25%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = simpson_rate3, color = "rate of simpson index(bottom25%)"), size = 1.2,se = FALSE) +


  scale_color_manual(values = c("purple","blue","#ff00ff","#00fbff","black"), 
                     name = "different growth rate of indexes for species with abundance below25%") +
  labs(x = "Year", y = "Different Growth Rate Value") +
      guides(color = guide_legend(title = "Rates of Different Indexes")) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.position = "right")
  



p4_ <- ggplot(biodiv_data, aes(x = Year)) +
  geom_point(aes(y = lpi_abundance_weighted_up_, color = "rate of weighted LPI by abundance(top50%)"), size = 1.5) +
    geom_point(aes(y = lpi_unweighted_up_, color = "rate of unweighted LPI(top50%)"), size = 1.5) +
  geom_point(aes(y = pielou_evenness4, color = "rate of pielou evenness(top50%)"), size = 1.5) +
  geom_point(aes(y = shannon_rate4, color = "rate of shannon index(top50%)"), size = 1.5) +
    geom_point(aes(y = simpson_rate4, color = "rate of simpson index(top50%)"), size = 1.5) +



     geom_line(aes(y = lpi_abundance_weighted_up_, color = "rate of weighted LPI by abundance(top50%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = lpi_unweighted_up_, color = "rate of unweighted LPI(top50%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = pielou_evenness4, color = "rate of pielou evenness(top50%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = shannon_rate4, color = "rate of shannon index(top50%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = simpson_rate4, color = "rate of simpson index(top50%)"), size = 1.2,se = FALSE) +


  scale_color_manual(values = c("purple","blue","#ff00ff","#00fbff","black"), 
                     name = "different growth rate of indexes for species with abundance over50%") +
  labs(x = "Year", y = "Different Growth Rate Value") +
        guides(color = guide_legend(title = "Rates of Different Indexes")) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.position = "right")
  



p5_ <- ggplot(biodiv_data, aes(x = Year)) +
  geom_point(aes(y = lpi_abundance_weighted_low_, color = "rate of weighted LPI by abundance(bottom50%)"), size = 1.5) +
    geom_point(aes(y = lpi_unweighted_low_, color = "rate of unweighted LPI(bottom50%)"), size = 1.5) +
  geom_point(aes(y = pielou_evenness5, color = "rate of pielou evenness(bottom50%)"), size = 1.5) +
  geom_point(aes(y = shannon_rate5, color = "rate of shannon index(bottom50%)"), size = 1.5) +
    geom_point(aes(y = simpson_rate5, color = "rate of simpson index(bottom50%)"), size = 1.5) +
  



     geom_line(aes(y = lpi_abundance_weighted_low_, color = "rate of weighted LPI by abundance(bottom50%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = lpi_unweighted_low_, color = "rate of unweighted LPI(bottom50%)"), size = 1.2,se = FALSE) +
  geom_line(aes(y = pielou_evenness5, color = "rate of pielou evenness(bottom50%)"), size = 1.2,se = FALSE) +
   geom_line(aes(y = shannon_rate5, color = "rate of shannon index(bottom50%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = simpson_rate5, color = "rate of simpson index(bottom50%)"), size = 1.2,se = FALSE) +

  scale_color_manual(values = c("purple","blue","#ff00ff","#00fbff","black"), 
                     name = "different growth rate of indexes for species with abundance below50%") +
  labs(x = "Year", y = "different growth rate value") +
          guides(color = guide_legend(title = "Rates of Different Indexes")) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.position = "right")
  

p6_ <- ggplot(biodiv_data, aes(x = Year)) +
  geom_point(aes(y = lpi_abundance_weighted_medium, color = "rate of weighted LPI by abundance(medium50%)"), size = 1.5) +
    geom_point(aes(y = lpi_unweighted_medium, color = "rate of unweighted LPI(medium50%)"), size = 1.5) +
  geom_point(aes(y = pielou_evenness0, color = "rate of pielou evenness(medium50%)"), size = 1.5) +
  geom_point(aes(y = shannon_rate0, color = "rate of shannon index(medium50%)"), size = 1.5) +
    geom_point(aes(y = simpson_rate0, color = "rate of simpson index(medium50%)"), size = 1.5) +
  



     geom_line(aes(y = lpi_abundance_weighted_medium, color = "rate of weighted LPI by abundance(medium50%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = lpi_unweighted_medium, color = "rate of unweighted LPI(medium50%)"), size = 1.2,se = FALSE) +
  geom_line(aes(y = pielou_evenness0, color = "rate of pielou evenness(medium50%)"), size = 1.2,se = FALSE) +
   geom_line(aes(y = shannon_rate0, color = "rate of shannon index(medium50%)"), size = 1.2,se = FALSE) +
    geom_line(aes(y = simpson_rate0, color = "rate of simpson index(medium50%)"), size = 1.2,se = FALSE) +

  scale_color_manual(values = c("purple","blue","#ff00ff","#00fbff","black"), 
                     name = "different growth rate of indexes for species with abundance below50%") +
  labs(x = "Year", y = "different growth rate value") +
          guides(color = guide_legend(title = "Rates of Different Indexes")) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.position = "right")


big_plot0 <- p1_ + p2_ + p3_ + p4_ + p5_ + p6_ + plot_layout(ncol = 1)

ggsave("../Results/different growth rate of indexes in different starting statuses.png",plot = big_plot0, width = 12, height = 17.5, dpi = 300, limitsize = FALSE)



standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}





fitting_data <- data.frame(matrix(0,25,18))
colnames(fitting_data) <- c("Ip1_","relative_sensitivity","evenness_std","Ip1_2","relative_sensitivity2","evenness_std2","Ip1_3","relative_sensitivity3","evenness_std3","Ip1_4","relative_sensitivity4","evenness_std4","Ip1_5","relative_sensitivity5","evenness_std5","Ip1_0","relative_sensitivity0","evenness_std0")


fitting_data[,1] <- Ip1_[-1]
fitting_data[,2] <- relative_sensitivity
fitting_data[,3] <- evenness_std[-1]

fitting_data[,4] <- Ip1_2[-1]
fitting_data[,5] <- relative_sensitivity2
fitting_data[,6] <- evenness_std2[-1]


fitting_data[,7] <- Ip1_3[-1]
fitting_data[,8] <- relative_sensitivity3
fitting_data[,9] <- evenness_std3[-1]

fitting_data[,10] <- Ip1_4[-1]
fitting_data[,11] <- relative_sensitivity4
fitting_data[,12] <- evenness_std4[-1]

fitting_data[,13] <- Ip1_5[-1]
fitting_data[,14] <- relative_sensitivity5
fitting_data[,15] <- evenness_std5[-1]

fitting_data[,16] <- Ip1_0[-1]
fitting_data[,17] <- relative_sensitivity0
fitting_data[,18] <- evenness_std0[-1]


evenness_fitting <- glm(Ip1_ ~ relative_sensitivity + evenness_std, data = fitting_data, family = gaussian)

evenness_fitting2 <- glm(Ip1_2 ~ relative_sensitivity2 + evenness_std2, data = fitting_data, family = gaussian)

evenness_fitting3 <- glm(Ip1_3 ~ relative_sensitivity3 + evenness_std3, data = fitting_data, family = gaussian)

evenness_fitting4 <- glm(Ip1_4 ~ relative_sensitivity4 + evenness_std4, data = fitting_data, family = gaussian)

evenness_fitting5 <- glm(Ip1_5 ~ relative_sensitivity5 + evenness_std5, data = fitting_data, family = gaussian)

evenness_fitting0 <- glm(Ip1_0 ~ relative_sensitivity0 + evenness_std0, data = fitting_data, family = gaussian)



shannon_index<-shannon_index/shannon_index[1]
shannon_index2<-shannon_index2/shannon_index2[1]
shannon_index3<-shannon_index3/shannon_index3[1]
shannon_index4<-shannon_index4/shannon_index4[1]
shannon_index5<-shannon_index5/shannon_index5[1]
shannon_index0<-shannon_index0/shannon_index0[1]


hill_index<-hill_index/hill_index[1]
hill_index2<-hill_index2/hill_index2[1]
hill_index3<-hill_index3/hill_index3[1]
hill_index4<-hill_index4/hill_index4[1]
hill_index5<-hill_index5/hill_index5[1]
hill_index0<-hill_index0/hill_index0[1]


simpson_index<-simpson_index/simpson_index[1]
simpson_index2<-simpson_index2/simpson_index2[1]
simpson_index3<-simpson_index3/simpson_index3[1]
simpson_index4<-simpson_index4/simpson_index4[1]
simpson_index5<-simpson_index5/simpson_index5[1]
simpson_index0<-simpson_index0/simpson_index0[1]


pielou_evenness<- pielou_evenness /pielou_evenness[1]
pielou_evenness2<-pielou_evenness2/pielou_evenness2[1]
pielou_evenness3<-pielou_evenness3/pielou_evenness3[1]
pielou_evenness4<-pielou_evenness4/pielou_evenness4[1]
pielou_evenness5<-pielou_evenness5/pielou_evenness5[1]
pielou_evenness0<-pielou_evenness0/pielou_evenness0[1]


Ip1_ <- Ip1_ /Ip1_[1]
Ip1_2 <- Ip1_2 /Ip1_2[1]
Ip1_3 <- Ip1_3 /Ip1_3[1]
Ip1_4 <- Ip1_4 /Ip1_4[1]
Ip1_5 <- Ip1_5 /Ip1_5[1]
Ip1_0 <- Ip1_0 /Ip1_0[1]

Ip0.5_ <- Ip0.5_ /Ip0.5_[1]
Ip0.5_2 <- Ip0.5_2 /Ip0.5_2[1]
Ip0.5_3 <- Ip0.5_3 /Ip0.5_3[1]
Ip0.5_4 <- Ip0.5_4 /Ip0.5_4[1]
Ip0.5_5 <- Ip0.5_5 /Ip0.5_5[1]
Ip0.5_0 <- Ip0.5_0 /Ip0.5_0[1]


data <- data.frame(
  Year = 1987:2012,
  lpi_index2 = lpi_index2,
  shannon_index = shannon_index,
  lpi_index = lpi_index,
    shannon_index2 = shannon_index2,
  shannon_index3 = shannon_index3,
  shannon_index4 = shannon_index4,
  shannon_index5 = shannon_index5,
   shannon_index0 = shannon_index0,


hill_index = hill_index,
hill_index2 = hill_index2,
  hill_index3 = hill_index3,
  hill_index4 = hill_index4,
  hill_index5 = hill_index5,
  hill_index0 = hill_index0,



simpson_index = simpson_index,
simpson_index2 = simpson_index2,
  simpson_index3 = simpson_index3,
  simpson_index4 = simpson_index4,
  simpson_index5 = simpson_index5,
  simpson_index0 = simpson_index0,

  pielou_evenness = pielou_evenness,
  pielou_evenness2 = pielou_evenness2,
  pielou_evenness3 = pielou_evenness3,
  pielou_evenness4 = pielou_evenness4,
  pielou_evenness5 = pielou_evenness5,
   pielou_evenness0 = pielou_evenness0,


  lpi_index2_up = lpi_index2_up,
  lpi_index_up = lpi_index_up,
  lpi_index2_low = lpi_index2_low,
  lpi_index_low = lpi_index_low,
  lpi_index2_up_ = lpi_index2_up_,
  lpi_index_up_ = lpi_index_up_,
  lpi_index2_low_ = lpi_index2_low_,
  lpi_index_low_ = lpi_index_low_,
    lpi_index2_medium = lpi_index2_medium,
  lpi_index_medium = lpi_index_medium,

    Ip1_ <- Ip1_,
  Ip1_2 <- Ip1_2,
  Ip1_3 <- Ip1_3,
  Ip1_4 <- Ip1_4,
  Ip1_5 <- Ip1_5,
  Ip1_0 <- Ip1_0,

  Ip0.5_ <- Ip0.5_,
  Ip0.5_2 <- Ip0.5_2,
  Ip0.5_3 <- Ip0.5_3,
  Ip0.5_4 <- Ip0.5_4,
  Ip0.5_5 <- Ip0.5_5,
  Ip0.5_0 <- Ip0.5_0
)



p11 <- ggplot(data, aes(x = Year)) +
  geom_point(aes(y = lpi_index, color = "Unweighted LPI"), size = 3.5) +
  geom_point(aes(y = lpi_index2, color = "Weighted LPI by Abundance"), size = 3.5) +
  geom_point(aes(y = shannon_index, color = "Hill number(q=1)"), size = 3.5) +
  geom_point(aes(y = simpson_index, color = "Hill number(q=2)"), size = 3.5) +


  geom_line(aes(y = lpi_index, color = "Unweighted LPI"), size = 3,se = FALSE) +
    geom_line(aes(y = lpi_index2, color = "Weighted LPI by Abundance"), size = 3,se = FALSE) +
  geom_line(aes(y = shannon_index, color = "Hill number(q=1)"), size = 3,se = FALSE) +
    geom_line(aes(y = simpson_index, color = "Hill number(q=2)"), size = 3,se = FALSE) +

    theme_bw() +

  scale_color_manual(values = c("purple","blue","#ff00ff","#00c8ff")
                    ) +
  labs(x = "Year", y = "Index Value",title="All Species Group") +
            guides(color = guide_legend(title = "Indexes")) +
  theme(plot.title = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 27, face = "bold"),
        axis.text = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 28, face = "bold"),
        legend.text = element_text(size = 27, face = "bold"),
        legend.position = "none")




p22 <- ggplot(data, aes(x = Year)) +

  geom_point(aes(y = lpi_index2_up, color = "Weighted LPI by Abundance"), size = 3.5) +
    geom_point(aes(y = shannon_index2, color = "Hill number(q=1)"), size = 3.5) +
  geom_point(aes(y = lpi_index_up, color = "Unweighted LPI"), size = 3.5) +
    geom_point(aes(y = simpson_index2, color = "Hill number(q=2)"), size = 3.5) +



  geom_line(aes(y = lpi_index_up, color = "Unweighted LPI"), size = 3,se = FALSE) +
    geom_line(aes(y = shannon_index2, color = "Hill number(q=1)"), size = 3,se = FALSE) +
    geom_line(aes(y = lpi_index2_up, color = "Weighted LPI by Abundance"), size = 3,se = FALSE) +
    geom_line(aes(y = simpson_index2, color = "Hill number(q=2)"), size = 3,se = FALSE) +


  scale_color_manual(values = c("purple","blue","#ff00ff","#00c8ff")
                     ) +
  labs(x = "Year", y = "Index Value",title="Dominant Species Group") +
            guides(color = guide_legend(title = "Indexes")) +
    theme_bw() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 27, face = "bold"),
        axis.text = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 28, face = "bold"),
        legend.text = element_text(size = 27, face = "bold"),
        legend.position = "none")
  





# 绘图
p33 <- ggplot(data, aes(x = Year)) +
  geom_point(aes(y = lpi_index_low, color = "Unweighted LPI"), size = 3.5) +
  geom_point(aes(y = lpi_index2_low, color = "Weighted LPI by Abundance"), size = 3.5) +
    geom_point(aes(y = shannon_index3, color = "Hill number(q=1)"), size = 3.5) +
  geom_point(aes(y = simpson_index3, color = "Hill number(q=2)"), size = 3.5) +

  
  geom_line(aes(y = lpi_index_low, color = "Unweighted LPI"), size = 3,se = FALSE) +
  geom_line(aes(y = lpi_index2_low, color = "Weighted LPI by Abundance"), size = 3,se = FALSE) +
    geom_line(aes(y = shannon_index3, color = "Hill number(q=1)"), size = 3,se = FALSE) +
    geom_line(aes(y = simpson_index3, color = "Hill number(q=2)"), size = 3,se = FALSE) +



  scale_color_manual(values = c("purple","blue","#ff00ff","#00c8ff") 
                     ) +
  labs(x = "Year", y = "Index Value",title="Rare Species Group") +
            guides(color = guide_legend(title = "Indexes")) +
    theme_bw() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 27, face = "bold"),
        axis.text = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 28, face = "bold"),
        legend.text = element_text(size = 27, face = "bold"),
        legend.position = "none")
  



p44 <- ggplot(data, aes(x = Year)) +
  geom_point(aes(y = lpi_index2_up_, color = "Weighted LPI by Abundance"), size = 3.5) +
    geom_point(aes(y = lpi_index_up_, color = "Unweighted LPI"), size = 3.5) +
      geom_point(aes(y = shannon_index4, color = "Hill number(q=1)"), size = 3.5) +
  geom_point(aes(y = simpson_index4, color = "Hill number(q=2)"), size = 3.5) +



     geom_line(aes(y = lpi_index2_up_, color = "Weighted LPI by Abundance"), size = 3,se = FALSE) +
    geom_line(aes(y = lpi_index_up_, color = "Unweighted LPI"), size = 3,se = FALSE) +
    geom_line(aes(y = shannon_index4, color = "Hill number(q=1)"), size = 3,se = FALSE) +
  geom_line(aes(y = simpson_index4, color = "Hill number(q=2)"), size = 3,se = FALSE) +


  scale_color_manual(values = c("purple","blue","#ff00ff","#00c8ff") 
                     ) +
  labs(x = "Year", y = "Index Value",title="Less Dominant Species Group") +
            guides(color = guide_legend(title = "Indexes")) +
  theme_bw() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 27, face = "bold"),
        axis.text = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 28, face = "bold"),
        legend.text = element_text(size = 27, face = "bold"),
        legend.position = "none")
  



p55 <- ggplot(data, aes(x = Year)) +
  geom_point(aes(y = lpi_index2_low_, color = "Weighted LPI by Abundance"), size = 3.5) +
    geom_point(aes(y = lpi_index_low_, color = "Unweighted LPI"), size = 3.5) +
  geom_point(aes(y = shannon_index5, color = "Hill number(q=1)"), size = 3.5) +
  geom_point(aes(y = simpson_index5, color = "Hill number(q=2)"), size = 3.5) +



     geom_line(aes(y = lpi_index2_low_, color = "Weighted LPI by Abundance"), size = 3,se = FALSE) +
    geom_line(aes(y = lpi_index_low_, color = "Unweighted LPI"), size = 3,se = FALSE) +
    geom_line(aes(y = shannon_index5, color = "Hill number(q=1)"), size = 3,se = FALSE) +
  geom_line(aes(y = simpson_index5, color = "Hill number(q=2)"), size = 3,se = FALSE) +


  scale_color_manual(values = c("purple","blue","#ff00ff","#00c8ff")
                     ) +
  labs(x = "Year", y = "Index Value",title="Less Rare Species Group") +
            guides(color = guide_legend(title = "Indexes")) +
  theme_bw() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 27, face = "bold"),
        axis.text = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 28, face = "bold"),
        legend.text = element_text(size = 27, face = "bold"),
        legend.position = "none")
  
p66 <- ggplot(data, aes(x = Year)) +
  geom_point(aes(y = lpi_index2_medium, color = "Weighted LPI by Abundance"), size = 3.5) +
    geom_point(aes(y = lpi_index_medium, color = "Unweighted LPI"), size = 3.5) +
  geom_point(aes(y = shannon_index0, color = "Hill number(q=1)"), size = 3.5) +
  geom_point(aes(y = simpson_index0, color = "Hill number(q=2)"), size = 3.5) +



     geom_line(aes(y = lpi_index2_medium, color = "Weighted LPI by Abundance"), size = 3,se = FALSE) +
    geom_line(aes(y = lpi_index_medium, color = "Unweighted LPI"), size = 3,se = FALSE) +
    geom_line(aes(y = shannon_index0, color = "Hill number(q=1)"), size = 3,se = FALSE) +
  geom_line(aes(y = simpson_index0, color = "Hill number(q=2)"), size = 3,se = FALSE) +


  scale_color_manual(values = c("purple","blue","#ff00ff","#00c8ff")
                    ,name="Different Indexes" ) +
  labs(x = "Year", y = "Index Value",title="Middle Species Group",color = "Different Indexes") +
  theme_bw() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 27, face = "bold"),
        axis.text = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 62, face = "bold"),
        legend.position = "right")
  

big_plot00 <- p11 + p22 + p33 + p44 + p55 + p66 + plot_layout(ncol = 2, guides = 'collect') &
  theme(legend.position = 'bottom') &
 guides(color = guide_legend(ncol=2, title = "Index Types"))
ggsave("../Results/different indexes in different statrting statuses.png",plot = big_plot00, width = 13, height = 15, dpi = 300, limitsize = FALSE)



