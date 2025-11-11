############# 功能
############## con 12h ##################
rm(list = ls())
library(tidyverse)
data <- data.table::fread("data/dotplot_function/con_vs_t_12h/con_vs_t_12h_reporter_score.txt",data.table = F)
df <- data.table::fread("data/dotplot_function/con_vs_t_12h/con_vs_t_12h_ko_stat.txt",data.table = F)

data <- na.omit(data)
extract_top_bottom <- function(data, column_name) {
  # 对目标列排序
  sorted_data <- data[order(data[[column_name]]), ]
  
  # 检查数据框行数
  n <- nrow(data)
  
  # 定义提取的行数（防止超出范围）
  top_n <- min(5, n)
  bottom_n <- min(5, n)
  
  # 提取前5和后5的行
  top_data <- sorted_data[1:top_n, ]
  bottom_data <- sorted_data[rev((n - bottom_n + 1):n), ]
  
  # 合并结果
  result_data <- rbind(top_data, bottom_data)
  return(result_data)
}
# data_1 <- extract_top_bottom(data, "report_score")
pathway <- c("ABC transporters",
             "Phosphotransferase system (PTS)",
             "Biofilm formation - Escherichia coli",
             "Cationic antimicrobial peptide (CAMP) resistance",
             "Flagellar assembly",
             'Ribosome',
             "Biosynthesis of nucleotide sugars",
             "Biosynthesis of various nucleotide sugars",
             "Teichoic acid biosynthesis",
             "Cell cycle - Caulobacter")
data_1 <-filter(data,description %in% pathway)
data_1  <- data_1[c(4,5,6,8,9,1,2,3,7,10),]
data_1 <- data_1[,c(2,3)]
data_1$time <- ifelse(data_1$report_score>0,"12h","con")
data_1$group <- c("12h vs con")
############## 3d 5d ##################
data <- data.table::fread("data/dotplot_function/t_3d_vs_t_5d/t_3d_vs_t_5d_reporter_score.txt",data.table = F)
data <- na.omit(data)
extract_top_bottom <- function(data, column_name) {
  # 对目标列排序
  sorted_data <- data[order(data[[column_name]]), ]
  
  # 检查数据框行数
  n <- nrow(data)
  
  # 定义提取的行数（防止超出范围）
  top_n <- min(5, n)
  bottom_n <- min(5, n)
  
  # 提取前5和后5的行
  top_data <- sorted_data[1:top_n, ]
  bottom_data <- sorted_data[rev((n - bottom_n + 1):n), ]
  
  # 合并结果
  result_data <- rbind(top_data, bottom_data)
  return(result_data)
}
# data_2 <- extract_top_bottom(data, "report_score")
pathway <- c("O-Antigen repeat unit biosynthesis",
             "Other glycan degradation",
             "Sphingolipid metabolism",
             "Biosynthesis of amino acids",
             "2-Oxocarboxylic acid metabolism",
             'Glycine, serine and threonine metabolism',
             "Lipoic acid metabolism",
             "Glutathione metabolism",
             "Lysine degradation",
             "Microbial metabolism in diverse environments")
data_2 <-filter(data,description %in% pathway)
data_2  <- data_2[c(1,3,4,6,8,2,5,7,9,10),]
data_2 <- data_2[,c(2,3)]
data_2$time <- ifelse(data_2$report_score>0,"3d","5d")
data_2$group <- c("5d vs 3d")


data <- rbind(data_1,data_2)
data$report_score <- abs(data$report_score)
data$time <- factor(data$time,levels = c("con","12h","3d","5d"))
data$description <- factor(data$description,levels = data$description)
data$group <- factor(data$group,levels = c("5d vs 3d","12h vs con"))

ggplot(data, aes(time, description)) + 
  geom_point(aes(fill = report_score),
             shape = 21, color = "black",size = 6) + 
  facet_grid(group~., scales = "free", space='free', switch = "y") +
  # scale_size_continuous(breaks = seq(1, 6, 1), limits = c(1, 6),
  #                       range = c(1, 6)) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  #scale_fill_gradient(low = 'white', high = 'red') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.background.y = element_rect(color="white"),
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, color = "black", face = "bold"),
        axis.text.x = element_text(angle = 90, color = "black", size = 13, hjust = 1), 
        axis.text.y = element_text(color = "black", size = 13, vjust = 0.5),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 13),
        legend.text=element_text(size=13)) + 
  guides(fill = guide_colorbar(ticks = FALSE, 
                               barheight = 8, 
                               frame.colour = "black")) + 
  labs(x = "", y = "", 
       fill = "Pathway reporter score", 
       size = "Pathway reporter score") +
  ggpubr::rotate_x_text(hjust = 1)
ggsave("output/Fig.1/daotplot/dotplot.pdf",height = 7.5,width = 9)
