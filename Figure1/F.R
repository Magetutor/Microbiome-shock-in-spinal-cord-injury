########### Cecum
aa <- list.files("data/lefse/control/Cecum/")
aa <- paste0("data/lefse/control/Cecum/",aa)
df <- data.frame(NULL)
for (i in 1:length(aa)) {
  bb <- list.files(aa[i])
  bb <- paste0(aa[i],"/",bb)
  data <- data.table::fread(bb[2],data.table = F)
  jj <- str_split(str_split(aa[i],"/")[[1]][5],"_")[[1]][2]
  data <- subset(data,lda > 2 & pvalue < 0.05)
  data <- data[grep("s_",data$taxon),]
  data <- data %>%
    separate(col = `taxon`, into = c("p","c","o","f","g","s"), sep = "\\.", remove = FALSE)
  data <- data[,c(7,9,10)]
  data <- na.omit(data)
  data <- as.data.frame(table(data$group))
  data <- data %>%
    mutate(Var1 = case_when(
      Var1 == "con" ~ paste0(jj,"_down"),
      Var1 == paste0("t_",jj) ~ paste0(jj,"_up"),
      TRUE ~ Var1
    ))
  df <- rbind(df,data)
}
df_1 <- df[grepl("_down",df$Var1),]
df_1$Var1 <- sub("_[^_]*$", "", df_1$Var1)
df_2 <- df[-(grep("_down",df$Var1)),]
df_2$Var1 <- sub("_[^_]*$", "", df_2$Var1)
df_1$Var1 <- factor(df_1$Var1 ,levels = c("sham","0min","30min","1h","6h",
                                       "12h","1d","3d","5d","7d",
                                       "14d","1m","2m","3m"))
df_2$Var1 <- factor(df_2$Var1 ,levels = c("sham","0min","30min","1h","6h",
                                          "12h","1d","3d","5d","7d",
                                          "14d","1m","2m","3m"))

p1 <- ggplot(df_1,aes(Var1,Freq))+
  #柱状堆积图
  geom_col(fill = "#4DBBD5")+
  #标题
  labs(x=NULL,y=NULL,fill=NULL)+
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), hjust = 0.5)+
  ggtitle("Cecum_down")+
  #主题
  theme_classic()+
  theme(axis.text.x = element_text(color = "black",size=12,angle = 30,hjust = 1),
        axis.text.y = element_text(color = "black",size=12,hjust = 1),
        axis.line = element_line(color = "black",linewidth = 0.8),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.ticks = element_line(color = "black",linewidth = 0.8),
        legend.position = "right")+#图例设置
  scale_y_continuous(expand = c(0,0))#设置刻度从0开始
ggsave(p1,file = "output/Fig.1/F/Cecum_down.pdf",height = 5,width = 8)

p2 <- ggplot(df_2,aes(Var1,Freq))+
  #柱状堆积图
  geom_col(fill = "#E64B35")+
  #标题
  labs(x=NULL,y=NULL,fill=NULL)+
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), hjust = 0.5)+
  ggtitle("Cecum_up")+
  #主题
  theme_classic()+
  theme(axis.text.x = element_text(color = "black",size=12,angle = 30,hjust = 1),
        axis.text.y = element_text(color = "black",size=12,hjust = 1),
        axis.line = element_line(color = "black",linewidth = 0.8),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.ticks = element_line(color = "black",linewidth = 0.8),
        legend.position = "right")+#图例设置
  scale_y_continuous(expand = c(0,0))#设置刻度从0开始
ggsave(p2,file = "output/Fig.1/F/Cecum_up.pdf",height = 5,width = 8)


########### Colon
rm(list = ls())
aa <- list.files("data/lefse/control/Colon/")
aa <- paste0("data/lefse/control/Colon/",aa)
df <- data.frame(NULL)
for (i in 1:length(aa)) {
  bb <- list.files(aa[i])
  bb <- paste0(aa[i],"/",bb)
  data <- data.table::fread(bb[1],data.table = F)
  jj <- str_split(str_split(aa[i],"/")[[1]][5],"_")[[1]][2]
  data <- subset(data,lda > 2 & pvalue < 0.05)
  data <- data[grep("s_",data$taxon),]
  data <- data %>%
    separate(col = `taxon`, into = c("p","c","o","f","g","s"), sep = "\\.", remove = FALSE)
  data <- data[,c(7,9,10)]
  data <- na.omit(data)
  data <- as.data.frame(table(data$group))
  data <- data %>%
    mutate(Var1 = case_when(
      Var1 == "con" ~ paste0(jj,"_down"),
      Var1 == paste0("t_",jj) ~ paste0(jj,"_up"),
      TRUE ~ Var1
    ))
  df <- rbind(df,data)
}
df_1 <- df[grepl("_down",df$Var1),]
df_1$Var1 <- sub("_[^_]*$", "", df_1$Var1)
df_2 <- df[-(grep("_down",df$Var1)),]
df_2$Var1 <- sub("_[^_]*$", "", df_2$Var1)
df_1$Var1 <- factor(df_1$Var1 ,levels = c("sham","0min","30min","1h","6h",
                                          "12h","1d","3d","5d","7d",
                                          "14d","1m","2m","3m"))
df_2$Var1 <- factor(df_2$Var1 ,levels = c("sham","0min","30min","1h","6h",
                                          "12h","1d","3d","5d","7d",
                                          "14d","1m","2m","3m"))

p1 <- ggplot(df_1,aes(Var1,Freq))+
  #柱状堆积图
  geom_col(fill = "#4DBBD5")+
  #标题
  labs(x=NULL,y=NULL,fill=NULL)+
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), hjust = 0.5)+
  ggtitle("Colon_down")+
  #主题
  theme_classic()+
  theme(axis.text.x = element_text(color = "black",size=12,angle = 30,hjust = 1),
        axis.text.y = element_text(color = "black",size=12,hjust = 1),
        axis.line = element_line(color = "black",linewidth = 0.8),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.ticks = element_line(color = "black",linewidth = 0.8),
        legend.position = "right")+#图例设置
  scale_y_continuous(expand = c(0,0))#设置刻度从0开始
ggsave(p1,file = "output/Fig.1/F/Colon_down.pdf",height = 5,width = 8)

p2 <- ggplot(df_2,aes(Var1,Freq))+
  #柱状堆积图
  geom_col(fill = "#E64B35")+
  #标题
  labs(x=NULL,y=NULL,fill=NULL)+
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), hjust = 0.5)+
  ggtitle("Colon_up")+
  #主题
  theme_classic()+
  theme(axis.text.x = element_text(color = "black",size=12,angle = 30,hjust = 1),
        axis.text.y = element_text(color = "black",size=12,hjust = 1),
        axis.line = element_line(color = "black",linewidth = 0.8),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.ticks = element_line(color = "black",linewidth = 0.8),
        legend.position = "right")+#图例设置
  scale_y_continuous(expand = c(0,0))#设置刻度从0开始
ggsave(p2,file = "output/Fig.1/F/Colon_up.pdf",height = 5,width = 8)


########### Ileum
rm(list = ls())
aa <- list.files("data/lefse/control/Ileum/")
aa <- paste0("data/lefse/control/Ileum/",aa)
df <- data.frame(NULL)
for (i in 1:length(aa)) {
  bb <- list.files(aa[i])
  bb <- paste0(aa[i],"/",bb)
  data <- data.table::fread(bb[1],data.table = F)
  jj <- str_split(str_split(aa[i],"/")[[1]][5],"_")[[1]][2]
  data <- subset(data,lda > 2 & pvalue < 0.05)
  data <- data[grep("s_",data$taxon),]
  data <- data %>%
    separate(col = `taxon`, into = c("p","c","o","f","g","s"), sep = "\\.", remove = FALSE)
  data <- data[,c(7,9,10)]
  data <- na.omit(data)
  data <- as.data.frame(table(data$group))
  data <- data %>%
    mutate(Var1 = case_when(
      Var1 == "con" ~ paste0(jj,"_down"),
      Var1 == paste0("t_",jj) ~ paste0(jj,"_up"),
      TRUE ~ Var1
    ))
  df <- rbind(df,data)
}
df_1 <- df[grepl("_down",df$Var1),]
df_1$Var1 <- sub("_[^_]*$", "", df_1$Var1)
df_2 <- df[-(grep("_down",df$Var1)),]
df_2$Var1 <- sub("_[^_]*$", "", df_2$Var1)
df_1$Var1 <- factor(df_1$Var1 ,levels = c("sham","0min","30min","1h","6h",
                                          "12h","1d","3d","5d","7d",
                                          "14d","1m","2m","3m"))
df_2$Var1 <- factor(df_2$Var1 ,levels = c("sham","0min","30min","1h","6h",
                                          "12h","1d","3d","5d","7d",
                                          "14d","1m","2m","3m"))

p1 <- ggplot(df_1,aes(Var1,Freq))+
  #柱状堆积图
  geom_col(fill = "#4DBBD5")+
  #标题
  labs(x=NULL,y=NULL,fill=NULL)+
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), hjust = 0.5)+
  ggtitle("Ileum_down")+
  #主题
  theme_classic()+
  theme(axis.text.x = element_text(color = "black",size=12,angle = 30,hjust = 1),
        axis.text.y = element_text(color = "black",size=12,hjust = 1),
        axis.line = element_line(color = "black",linewidth = 0.8),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.ticks = element_line(color = "black",linewidth = 0.8),
        legend.position = "right")+#图例设置
  scale_y_continuous(expand = c(0,0))#设置刻度从0开始
ggsave(p1,file = "output/Fig.1/F/Ileum_down.pdf",height = 5,width = 8)

p2 <- ggplot(df_2,aes(Var1,Freq))+
  #柱状堆积图
  geom_col(fill = "#E64B35")+
  #标题
  labs(x=NULL,y=NULL,fill=NULL)+
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), hjust = 0.5)+
  ggtitle("Ileum_up")+
  #主题
  theme_classic()+
  theme(axis.text.x = element_text(color = "black",size=12,angle = 30,hjust = 1),
        axis.text.y = element_text(color = "black",size=12,hjust = 1),
        axis.line = element_line(color = "black",linewidth = 0.8),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.ticks = element_line(color = "black",linewidth = 0.8),
        legend.position = "right")+#图例设置
  scale_y_continuous(expand = c(0,0))#设置刻度从0开始
ggsave(p2,file = "output/Fig.1/F/Ileum_up.pdf",height = 5,width = 8)
