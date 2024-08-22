library(MASS)
library(zoo)
library(dplyr)
library(purrr)
library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)
library(patchwork)



proportion_data<-read_excel('../Data/Percentage_global_pop_LPI_20240410.xlsx')

proportion_data<- data.frame(proportion_data)

proportion_data<-proportion_data[proportion_data$Class=="Aves",]

proportion_data<-proportion_data[proportion_data$Percentage=="51-75%"|proportion_data$Percentage=="76-100%"|proportion_data$Percentage=="26-50%",]

selected_data<- read.csv("../Data/seems_useful_result.csv",header=T)



proportion_data$Binomial<- gsub("_"," ",proportion_data$Binomial)
selected_data<-selected_data[proportion_data$Binomial%in%selected_data$Binomial,]


write.csv(selected_data,"../Data/more_useful_result.csv",row.names=F)


silly_data<-read.csv("../Data/dt_values.csv",header=T)

silly_data<-silly_data[,c(2,73,74)]

silly_data<-silly_data[c(which(silly_data$Binomial%in%selected_data$Binomial)),]

merged_again_data <- merge(silly_data, selected_data, by = "Binomial", all.x = TRUE)


merged_again_data$Latitude<-abs(merged_again_data$Latitude)

point_size <- 3
point_alpha <- 0.7
color_palette <- c("#377eb8", "#e41a1c", "#4daf4a")

# Create individual plots
p1 <- ggplot(merged_again_data, aes(y = max_growth_rate, x = log(Body.Mass.gram.))) +
  geom_point(color = color_palette[1], size = point_size, alpha = point_alpha) +
  geom_smooth(color = color_palette[1], method = "glm", se = FALSE) +
  labs(x = "Logged body mass", y = "max lambda") +
  theme_minimal() + theme_bw()

p2 <- ggplot(merged_again_data, aes(y = max_growth_rate, x = log(global_abundance))) +
  geom_point(color = color_palette[2], size = point_size, alpha = point_alpha) +
  geom_smooth(color = color_palette[2], method = "glm", se = FALSE) +
  labs(x = "Logged global abundance", y = "max lambda") +
  theme_minimal() + theme_bw()

p3 <- ggplot(merged_again_data, aes(y = max_growth_rate, x = Latitude)) +
  geom_point(color = color_palette[3], size = point_size, alpha = point_alpha) +
  geom_smooth(color = color_palette[3], method = "glm", se = FALSE) +
  labs(x = "Latitude", y = "max lambda") +
  theme_minimal() + theme_bw()

# Save the individual plots
ggsave("../Results/hypothesis_test_logged_body_mass.png", p1, width = 6, height = 4, dpi = 300)
ggsave("../Results/hypothesis_test_logged_global_abundance.png", p2, width = 6, height = 4, dpi = 300)
ggsave("../Results/hypothesis_test_latitude.png", p3, width = 6, height = 4, dpi = 300)



fitting_again<-lm(max_growth_rate~log(Body.Mass.gram.)+log(global_abundance)+Latitude,data=merged_again_data)



final_fitting<-glm(max_growth_rate~log(Body.Mass.gram.)+log(global_abundance)+Latitude,data=merged_again_data,family = gaussian())

final_fitting2<-glm(max_growth_rate~log(global_abundance)+Latitude,data=merged_again_data,family = gaussian())

final_fitting3<-glm(max_growth_rate~log(Body.Mass.gram.)+Latitude,data=merged_again_data,family = gaussian())




fit_ <- fitdistr(selected_data$global_abundance, "Poisson")

# 计算均值和标准差
lambda_hat <- fit_$estimate # 拟合得到的lambda参数估计值
mean_val_ <- lambda_hat # Poisson分布的均值就是lambdasd_val_ <- fit_$estimate["sd"]
sd_val_ <- sqrt(lambda_hat) # Poisson分布标准差
alpha <- 0.5 # 置信水平为95%
lower <- qpois(alpha/2, lambda_hat)
upper <- qpois(1-alpha/2, lambda_hat)
conf_int_ <- c(lower, upper) # 95%置信区间

mean_ <- exp(mean(log(selected_data$global_abundance)))


filtered_data_up1 <- selected_data[selected_data$global_abundance >= mean_, ]
filtered_data_up1<-as.data.frame(filtered_data_up1)

filtered_data_low1 <- selected_data[selected_data$global_abundance <= mean_, ]
filtered_data_low1<-as.data.frame(filtered_data_low1)


fit_2 <- fitdistr(filtered_data_up1$global_abundance, "Poisson")

# 计算均值和标准差
lambda_hat2 <- fit_2$estimate # 拟合得到的lambda参数估计值
mean_val_2 <- lambda_hat2 # Poisson分布的均值就是lambdasd_val_ <- fit_$estimate["sd"]
sd_val_2 <- sqrt(lambda_hat2) # Poisson分布标准差
alpha2 <- 0.5 # 置信水平为95%
lower2 <- qpois(alpha2/2, lambda_hat2)
upper2 <- qpois(1-alpha2/2, lambda_hat2)
conf_int_2 <- c(lower2, upper2) # 95%置信区间


mean_1 <- exp(mean(log(filtered_data_up1$global_abundance)))


filtered_data_up2 <- filtered_data_up1[filtered_data_up1$global_abundance >= mean_1, ]
filtered_data_up2<-as.data.frame(filtered_data_up2)





fit_3 <- fitdistr(filtered_data_low1$global_abundance, "Poisson")

# 计算均值和标准差
lambda_hat3 <- fit_3$estimate # 拟合得到的lambda参数估计值
mean_val_3 <- lambda_hat3 # Poisson分布的均值就是lambdasd_val_ <- fit_$estimate["sd"]
sd_val_3 <- sqrt(lambda_hat3) # Poisson分布标准差
alpha3 <- 0.5 # 置信水平为95%
lower3 <- qpois(alpha3/2, lambda_hat3)
upper3 <- qpois(1-alpha3/2, lambda_hat3)
conf_int_3 <- c(lower3, upper3) # 95%置信区间


mean_2 <- exp(mean(log(filtered_data_low1$global_abundance)))


filtered_data_low2 <- filtered_data_low1[filtered_data_low1$global_abundance <= mean_2, ]
filtered_data_low2<-as.data.frame(filtered_data_low2)



write.csv(filtered_data_up2,"../Data/up_more_useful_result.csv",row.names=F)
write.csv(filtered_data_low2,"../Data/low_more_useful_result.csv",row.names=F)

write.csv(filtered_data_up1,"../Data/not_so_up_more_useful_result.csv",row.names=F)
write.csv(filtered_data_low1,"../Data/not_so_low_more_useful_result.csv",row.names=F)