rm(list = ls())
data <- read.csv("data/a多样性指数结果所有.csv",row.names = 1)
data <- cbind(data,group = rownames(data))

############# Co
data <-  data[grepl("Co",data$group),]   
process_string <- function(s) {
  parts <- strsplit(s, "_")[[1]]
  if (length(parts) <= 2) {
    return(s)
  }
  return(paste(parts[1:(length(parts) - 2)], collapse = "_"))
}

data$group <- sapply(data$group, process_string)
data$group <- factor(data$group,levels = c("Con","Sham","SCI_0min","SCI_30min","SCI_1h",
                                                     "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                                     "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))

sobs_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(sobs, na.rm = TRUE), .groups = "drop")

ace_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(ace, na.rm = TRUE), .groups = "drop")

chao_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(chao, na.rm = TRUE), .groups = "drop")

shannon_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(shannon, na.rm = TRUE), .groups = "drop")

simpson_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(simpson, na.rm = TRUE), .groups = "drop")

p1 = ggplot(data,aes(x = group, y = sobs, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Sobs index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = sobs_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p1

p2 = ggplot(data,aes(x = group, y = ace, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="ace index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = ace_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p2

p3 = ggplot(data,aes(x = group, y = chao, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="chao index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = chao_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p3

p4 = ggplot(data,aes(x = group, y = shannon, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="shannon index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = shannon_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p4

p5 = ggplot(data,aes(x = group, y = simpson, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="simpson index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = simpson_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p5
library("gridExtra")
library("cowplot")
plot_grid(p1, p2, p3, p4,p5,ncol = 1)
ggsave('output/Fig.1/B/Co_Platform.pdf',width=10,height = 20)

############# Il
rm(list = ls())
data <- read.csv("data/a多样性指数结果所有.csv",row.names = 1)
data <- cbind(data,group = rownames(data))

data <- data[grepl("Il",data$group),]   
process_string <- function(s) {
  parts <- strsplit(s, "_")[[1]]
  if (length(parts) <= 2) {
    return(s)
  }
  return(paste(parts[1:(length(parts) - 2)], collapse = "_"))
}

data$group <- sapply(data$group, process_string)
data$group <- factor(data$group,levels = c("Con","Sham","SCI_0min","SCI_30min","SCI_1h",
                                           "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                           "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))

sobs_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(sobs, na.rm = TRUE), .groups = "drop")

ace_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(ace, na.rm = TRUE), .groups = "drop")

chao_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(chao, na.rm = TRUE), .groups = "drop")

shannon_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(shannon, na.rm = TRUE), .groups = "drop")

simpson_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(simpson, na.rm = TRUE), .groups = "drop")

p1 = ggplot(data,aes(x = group, y = sobs, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Sobs index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = sobs_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p1

p2 = ggplot(data,aes(x = group, y = ace, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="ace index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = ace_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p2

p3 = ggplot(data,aes(x = group, y = chao, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="chao index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = chao_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p3

p4 = ggplot(data,aes(x = group, y = shannon, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="shannon index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = shannon_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p4

p5 = ggplot(data,aes(x = group, y = simpson, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="simpson index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = simpson_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p5
library("gridExtra")
library("cowplot")
plot_grid(p1, p2, p3, p4,p5,ncol = 1)
ggsave('output/Fig.1/B/Il_Platform.pdf',width=10,height = 20)

############# Ce
rm(list = ls())
data <- read.csv("data/a多样性指数结果所有.csv",row.names = 1)
data <- cbind(data,group = rownames(data))
data <-  data[grepl("Ce",data$group),]   
process_string <- function(s) {
  parts <- strsplit(s, "_")[[1]]
  if (length(parts) <= 2) {
    return(s)
  }
  return(paste(parts[1:(length(parts) - 2)], collapse = "_"))
}

data$group <- sapply(data$group, process_string)
data$group <- factor(data$group,levels = c("Con","Sham","SCI_0min","SCI_30min","SCI_1h",
                                           "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                           "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))

sobs_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(sobs, na.rm = TRUE), .groups = "drop")

ace_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(ace, na.rm = TRUE), .groups = "drop")

chao_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(chao, na.rm = TRUE), .groups = "drop")

shannon_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(shannon, na.rm = TRUE), .groups = "drop")

simpson_avg <- data %>%
  group_by(group) %>%
  summarize(Value = mean(simpson, na.rm = TRUE), .groups = "drop")

p1 = ggplot(data,aes(x = group, y = sobs, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Sobs index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = sobs_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p1

p2 = ggplot(data,aes(x = group, y = ace, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="ace index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = ace_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p2

p3 = ggplot(data,aes(x = group, y = chao, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="chao index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = chao_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p3

p4 = ggplot(data,aes(x = group, y = shannon, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="shannon index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = shannon_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p4

p5 = ggplot(data,aes(x = group, y = simpson, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="simpson index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = simpson_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)
p5
library("gridExtra")
library("cowplot")
plot_grid(p1, p2, p3, p4,p5,ncol = 1)
ggsave('output/Fig.1/B/Ce_Platform.pdf',width=10,height = 20)



##### a多样性
rm(list = ls())
library(vegan)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

data <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_g_RPKM_percent.xls",data.table = F)
data <- data %>%
  separate(col = `#Taxonomy`, into = c("d", "k", "p","c","o","f","g"), sep = ";", remove = FALSE)
data <- data[,-c(1:7)]
rownames(data) <- data$g
data <- data[,-1]
##### Co
df <- as.vector(NULL)
aa <- c("Con_","Sham_","SCI_0min_","SCI_30min_","SCI_1h_",
        "SCI_6h_","SCI_12h_","SCI_1d_","SCI_3d_","SCI_5d_",
        "SCI_7d_","SCI_14d_","SCI_1m_","SCI_2m_","SCI_3m_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Co")
  df <- c(df,bb)
}
Co <- data[,df]
aa <- c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
        "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
        "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m")
group <- rep(aa,each = 6)
colnames(Co) <- group

# ⾏列转置
tdata = t(Co) 

# 转为数据框
a <- as.data.frame(tdata) 
a= as.data.frame(lapply(a,as.numeric)) #把⾥⾯的数字从字符串转为数值型(numeric)

# α多样性 - shannon、simpson、invsimpson指数
# 计算vegan包支持的多样性指数
shannon <- diversity(a, index = "shannon")
simpson <- diversity(a, index = "simpson")
invsimpson <- diversity(a, index = "invsimpson")

# 使用vegan包的estimateR函数计算Chao1和ACE指数,这俩是otu专属，还有Richness
# rare_indices <- estimateR(a)
# observed_species = rowSums(a) # 计算Richness，其实就是简单的对⾏求和
data_shannon = data.frame(shannon)
data_simpson = data.frame(simpson)
data_invsimpson = data.frame(invsimpson)

# 合并计算出来的三个指数
spe_alpha <- cbind(
  data_shannon,
  data_simpson,
  data_invsimpson,
  group # 或 group$group_column，如果group是数据框
)
spe_alpha$group <- factor(spe_alpha$group,levels = c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
                                                     "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                                     "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))

shannon_avg <- spe_alpha %>%
  group_by(group) %>%
  summarize(Value = mean(shannon, na.rm = TRUE), .groups = "drop")

simpson_avg <- spe_alpha %>%
  group_by(group) %>%
  summarize(Value = mean(simpson, na.rm = TRUE), .groups = "drop")

invsimpson_avg <- spe_alpha %>%
  group_by(group) %>%
  summarize(Value = mean(invsimpson, na.rm = TRUE), .groups = "drop")

p1 = ggplot(spe_alpha,aes(x = group, y = shannon, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Shannon index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = shannon_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)

p1

p2 = ggplot(spe_alpha,aes(x = group, y = simpson, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Simpson index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = simpson_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
# stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)

p2

p3 = ggplot(spe_alpha,aes(x = group, y = invsimpson, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Invsimpson index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = invsimpson_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
# stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)

p3

library("gridExtra")
library("cowplot")
plot_grid(p1, p2, p3, ncol = 1)
ggsave('output/Fig.1/B/Co.pdf',width=10,height = 13)

##### Ce
df <- as.vector(NULL)
aa <- c("Con_","Sham_","SCI_0min_","SCI_30min_","SCI_1h_",
        "SCI_6h_","SCI_12h_","SCI_1d_","SCI_3d_","SCI_5d_",
        "SCI_7d_","SCI_14d_","SCI_1m_","SCI_2m_","SCI_3m_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Ce")
  df <- c(df,bb)
}
Ce <- data[,df]
aa <- c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
        "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
        "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m")
group <- rep(aa,each = 6)
colnames(Ce) <- group

# ⾏列转置
tdata = t(Ce) 

# 转为数据框
a <- as.data.frame(tdata) 
a= as.data.frame(lapply(a,as.numeric)) #把⾥⾯的数字从字符串转为数值型(numeric)

# α多样性 - shannon、simpson、invsimpson指数
# 计算vegan包支持的多样性指数
shannon <- diversity(a, index = "shannon")
simpson <- diversity(a, index = "simpson")
invsimpson <- diversity(a, index = "invsimpson")

# 使用vegan包的estimateR函数计算Chao1和ACE指数,这俩是otu专属，还有Richness
# rare_indices <- estimateR(a)
# observed_species = rowSums(a) # 计算Richness，其实就是简单的对⾏求和
data_shannon = data.frame(shannon)
data_simpson = data.frame(simpson)
data_invsimpson = data.frame(invsimpson)

# 合并计算出来的三个指数
spe_alpha <- cbind(
  data_shannon,
  data_simpson,
  data_invsimpson,
  group # 或 group$group_column，如果group是数据框
)
spe_alpha$group <- factor(spe_alpha$group,levels = c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
                                                     "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                                     "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))

shannon_avg <- spe_alpha %>%
  group_by(group) %>%
  summarize(Value = mean(shannon, na.rm = TRUE), .groups = "drop")

simpson_avg <- spe_alpha %>%
  group_by(group) %>%
  summarize(Value = mean(simpson, na.rm = TRUE), .groups = "drop")

invsimpson_avg <- spe_alpha %>%
  group_by(group) %>%
  summarize(Value = mean(invsimpson, na.rm = TRUE), .groups = "drop")

p1 = ggplot(spe_alpha,aes(x = group, y = shannon, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Shannon index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = shannon_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)

p1

p2 = ggplot(spe_alpha,aes(x = group, y = simpson, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Simpson index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = simpson_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
# stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)

p2

p3 = ggplot(spe_alpha,aes(x = group, y = invsimpson, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Invsimpson index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = invsimpson_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
# stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)

p3

library("gridExtra")
library("cowplot")
plot_grid(p1, p2, p3, ncol = 1)
ggsave('output/Fig.1/B/Ce.pdf',width=10,height = 13)

##### Il
df <- as.vector(NULL)
aa <- c("Con_","Sham_","SCI_0min_","SCI_30min_","SCI_1h_",
        "SCI_6h_","SCI_12h_","SCI_1d_","SCI_3d_","SCI_5d_",
        "SCI_7d_","SCI_14d_","SCI_1m_","SCI_2m_","SCI_3m_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Il")
  df <- c(df,bb)
}
Il <- data[,df]
aa <- c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
        "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
        "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m")
group <- rep(aa,each = 6)
colnames(Il) <- group

# ⾏列转置
tdata = t(Il) 

# 转为数据框
a <- as.data.frame(tdata) 
a= as.data.frame(lapply(a,as.numeric)) #把⾥⾯的数字从字符串转为数值型(numeric)

# α多样性 - shannon、simpson、invsimpson指数
# 计算vegan包支持的多样性指数
shannon <- diversity(a, index = "shannon")
simpson <- diversity(a, index = "simpson")
invsimpson <- diversity(a, index = "invsimpson")

# 使用vegan包的estimateR函数计算Chao1和ACE指数,这俩是otu专属，还有Richness
# rare_indices <- estimateR(a)
# observed_species = rowSums(a) # 计算Richness，其实就是简单的对⾏求和
data_shannon = data.frame(shannon)
data_simpson = data.frame(simpson)
data_invsimpson = data.frame(invsimpson)

# 合并计算出来的三个指数
spe_alpha <- cbind(
  data_shannon,
  data_simpson,
  data_invsimpson,
  group # 或 group$group_column，如果group是数据框
)
spe_alpha$group <- factor(spe_alpha$group,levels = c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
                                                     "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                                     "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))

shannon_avg <- spe_alpha %>%
  group_by(group) %>%
  summarize(Value = mean(shannon, na.rm = TRUE), .groups = "drop")

simpson_avg <- spe_alpha %>%
  group_by(group) %>%
  summarize(Value = mean(simpson, na.rm = TRUE), .groups = "drop")

invsimpson_avg <- spe_alpha %>%
  group_by(group) %>%
  summarize(Value = mean(invsimpson, na.rm = TRUE), .groups = "drop")

p1 = ggplot(spe_alpha,aes(x = group, y = shannon, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Shannon index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = shannon_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
#stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)

p1

p2 = ggplot(spe_alpha,aes(x = group, y = simpson, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Simpson index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = simpson_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
# stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)

p2

p3 = ggplot(spe_alpha,aes(x = group, y = invsimpson, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="Invsimpson index")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))+    
  # 绘制连接平均值的线  
  geom_line(data = invsimpson_avg,
            aes(x = group, 
                y = Value,
                group = 1), 
            colour = "black",
            size = 1,  
            position = position_dodge(width = 0.9))
# stat_compare_means(comparisons = comparison, label = "p",size=5)+# label = "p.signif"显示显著性
# geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.7)

p3

library("gridExtra")
library("cowplot")
plot_grid(p1, p2, p3, ncol = 1)
ggsave('output/Fig.1/B/Il.pdf',width=10,height = 13)
