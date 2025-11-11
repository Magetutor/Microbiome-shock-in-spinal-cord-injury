############ 
rm(list = ls())
data <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_s_RPKM_percent.xls",data.table = F)
data <- data %>%
  separate(col = `#Taxonomy`, into = c("d", "k", "p","c","o","f","g","s"), sep = ";", remove = FALSE)
data <- data[,-c(1:8)]
rownames(data) <- data$s
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
Co = as.data.frame(t(Co))
Co <- cbind(group = group,Co)
# ⾏列转置
colnames(Co)[grep("Lactobacillus_acidophilus",colnames(Co))]
Co_1 <- Co[,c("group","s__Bacteroides_thetaiotaomicron")]
Co_2 <- Co[,c("group","s__Lactobacillus_acidophilus")]
Co_1$group <- factor(Co_1$group,levels = c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
                                       "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                       "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))
Co_1_mean <- Co_1 %>%
  group_by(group) %>%
  summarize(Value = mean(s__Bacteroides_thetaiotaomicron, na.rm = TRUE), .groups = "drop")

Co_2$group <- factor(Co_2$group,levels = c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
                                           "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                           "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))
Co_2_mean <- Co_2 %>%
  group_by(group) %>%
  summarize(Value = mean(s__Lactobacillus_acidophilus, na.rm = TRUE), .groups = "drop")


p1 <- ggplot(Co_1,aes(x = group, y = s__Bacteroides_thetaiotaomicron, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="s__Bacteroides_thetaiotaomicron")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))  +
  # 绘制连接平均值的线  
  geom_line(data = Co_1_mean,
            aes(x = group,
                y = Value,
                group = 1),
            colour = "black",
            size = 1,
            position = position_dodge(width = 0.9))

p2 <- ggplot(Co_2,aes(x = group, y = s__Lactobacillus_acidophilus, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="s__Lactobacillus_acidophilus")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))  +
  # 绘制连接平均值的线  
  geom_line(data = Co_2_mean,
            aes(x = group,
                y = Value,
                group = 1),
            colour = "black",
            size = 1,
            position = position_dodge(width = 0.9))

ggsave(p1,file = "output/Fig.2/B/Co/s__Bacteroides_thetaiotaomicron.pdf",height = 5,width = 10)

ggsave(p2,file = "output/Fig.2/B/Co/s__Lactobacillus_acidophilus.pdf",height = 5,width = 10)


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
Ce = as.data.frame(t(Ce))
Ce <- cbind(group = group,Ce)
# ⾏列转置
colnames(Ce)[grep("Lactobacillus_acidophilus",colnames(Ce))]
Ce_1 <- Ce[,c("group","s__Bacteroides_thetaiotaomicron")]
Ce_2 <- Ce[,c("group","s__Lactobacillus_acidophilus")]
Ce_1$group <- factor(Ce_1$group,levels = c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
                                           "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                           "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))
Ce_1_mean <- Ce_1 %>%
  group_by(group) %>%
  summarize(Value = mean(s__Bacteroides_thetaiotaomicron, na.rm = TRUE), .groups = "drop")

Ce_2$group <- factor(Ce_2$group,levels = c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
                                           "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                           "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))
Ce_2_mean <- Ce_2 %>%
  group_by(group) %>%
  summarize(Value = mean(s__Lactobacillus_acidophilus, na.rm = TRUE), .groups = "drop")


p1 <- ggplot(Ce_1,aes(x = group, y = s__Bacteroides_thetaiotaomicron, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="s__Bacteroides_thetaiotaomicron")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))  +
  # 绘制连接平均值的线  
  geom_line(data = Ce_1_mean,
            aes(x = group,
                y = Value,
                group = 1),
            colour = "black",
            size = 1,
            position = position_dodge(width = 0.9))

p2 <- ggplot(Ce_2,aes(x = group, y = s__Lactobacillus_acidophilus, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="s__Lactobacillus_acidophilus")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))  +
  # 绘制连接平均值的线  
  geom_line(data = Ce_2_mean,
            aes(x = group,
                y = Value,
                group = 1),
            colour = "black",
            size = 1,
            position = position_dodge(width = 0.9))

ggsave(p1,file = "output/Fig.2/B/Ce/s__Bacteroides_thetaiotaomicron.pdf",height = 5,width = 10)

ggsave(p2,file = "output/Fig.2/B/Ce/s__Lactobacillus_acidophilus.pdf",height = 5,width = 10)


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
Il = as.data.frame(t(Il))
Il <- cbind(group = group,Il)
# ⾏列转置
colnames(Il)[grep("Lactobacillus_acidophilus",colnames(Il))]
Il_1 <- Il[,c("group","s__Bacteroides_thetaiotaomicron")]
Il_2 <- Il[,c("group","s__Lactobacillus_acidophilus")]
Il_1$group <- factor(Il_1$group,levels = c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
                                           "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                           "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))
Il_1_mean <- Il_1 %>%
  group_by(group) %>%
  summarize(Value = mean(s__Bacteroides_thetaiotaomicron, na.rm = TRUE), .groups = "drop")

Il_2$group <- factor(Il_2$group,levels = c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
                                           "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                           "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))
Il_2_mean <- Il_2 %>%
  group_by(group) %>%
  summarize(Value = mean(s__Lactobacillus_acidophilus, na.rm = TRUE), .groups = "drop")


p1 <- ggplot(Il_1,aes(x = group, y = s__Bacteroides_thetaiotaomicron, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="s__Bacteroides_thetaiotaomicron")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))  +
  # 绘制连接平均值的线  
  geom_line(data = Il_1_mean,
            aes(x = group,
                y = Value,
                group = 1),
            colour = "black",
            size = 1,
            position = position_dodge(width = 0.9))

p2 <- ggplot(Il_2,aes(x = group, y = s__Lactobacillus_acidophilus, fill = group))+
  geom_boxplot(outlier.shape = NA)+ #箱线图
  labs(title="",  x="Group", y="s__Lactobacillus_acidophilus")+ 
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4","#f57d1f","#92a8af","#d42827","#8458a4",
                               "#8b554a","#d779b0","#b6bc60","#1cbdcf","#adc6e8",
                               "#fbb978","#b1c7c5","#f69595","#c5afd4","#0687ab")) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 30, hjust = 1,vjust = 1))  +
  # 绘制连接平均值的线  
  geom_line(data = Il_2_mean,
            aes(x = group,
                y = Value,
                group = 1),
            colour = "black",
            size = 1,
            position = position_dodge(width = 0.9))

ggsave(p1,file = "output/S_Fig.1/G/Il/s__Bacteroides_thetaiotaomicron.pdf",height = 5,width = 10)

ggsave(p2,file = "output/S_Fig.1/F/Il/s__Lactobacillus_acidophilus.pdf",height = 5,width = 10)