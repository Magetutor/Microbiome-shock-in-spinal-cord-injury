##### 比例柱状图
rm(list = ls())
library(tidyverse)
data <- data.table::fread("data/10.Taxon_Function_Composition/bar/nr/nr_Origin/taxa.percents.table.xls",data.table = F)

rownames(data) <- data$Phylum
data <- data[,-1]

metdata <- data.frame(sample = colnames(data))
metdata$group <- substr(metdata$sample, nchar(metdata$sample) - 1, nchar(metdata$sample))

########## Il
Il <- subset(metdata,group %in% "Il")
data_Il <- data[,Il$sample]
data_Il$sample <- rownames(data_Il)
data_Il <- data_Il %>%
  pivot_longer(cols = -"sample", names_to = "group", values_to = "num")
data_Il$group <- factor(data_Il$group,levels = c("Con_Il","Sham_Il","SCI_0min_Il","SCI_30min_Il","SCI_1h_Il","SCI_6h_Il",
                                                 "SCI_12h_Il","SCI_1d_Il","SCI_3d_Il","SCI_5d_Il","SCI_7d_Il","SCI_14d_Il",
                                                 "SCI_1m_Il","SCI_2m_Il","SCI_3m_Il"))

ggplot(data_Il,aes(group,num,fill=sample))+
  #柱状堆积图
  geom_col(position="fill")+
  #标题
  labs(x=NULL,y=NULL,fill=NULL)+
  #颜色
  scale_fill_manual(values = c("#ea544f","#eb6d73","#f8c6c4","#f18201","#f39b64",
                               "#b6bd61","#347988","#ed3122","#cccccc","#763395",
                               "#c1a2c1","#f18201","#00adaa","#b04929",
                               "#91d1c1","#e3907e","#acc6e8","#f9c059","#7a9ab0",
                               "#fab977","#f29a7f","#8491b3"))+
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
ggsave("output/D/Il.pdf",width = 11,height = 4)




