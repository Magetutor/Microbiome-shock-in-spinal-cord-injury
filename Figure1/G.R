################ 12h
rm(list = ls())
library(tidyverse)
data <- data.table::fread("data/lefse_12h/Ileum_12h_lefse_LDA.xls",data.table = F)
data <- subset(data,lda > 2 & pvalue < 0.05)
data <- data[grep("s_",data$taxon),]
data <- data %>%
  separate(col = `taxon`, into = c("p","c","o","f","g","s"), sep = "\\.", remove = FALSE)
data <- data[,-c(1:6)]
data <- data %>% 
  group_by(group) %>% 
  slice_max(lda, n = 10)

data <- data[,c(1,3,4)]
data <- data %>%
  mutate(lda = ifelse(group == "con", -lda, lda))
ggplot(data,aes(reorder(s, lda),lda,fill=group))+  
  geom_col()+  
  theme_bw()+  
  theme(panel.grid.major=element_blank(),        
        panel.grid.minor=element_blank(),       
        panel.border = element_blank(),   
        legend.title = element_blank(),   
        axis.text = element_text(color="black",size=10),   
        axis.line.x = element_line(color='black',size=0.05),   
        axis.ticks.y = element_blank(),      
        axis.text.y = element_blank(),       
        legend.position = 'right')+  
  coord_flip()+ 
  geom_segment(aes(y=0, yend=0,x=0,xend=18.5))+
  geom_text(data = data[which(data$lda>0),],aes(x=s, y=-0.1, label=s),  
            hjust=1, size=4,color="black")+ 
  geom_text(data = data[which(data$lda<0),],aes(x=s, y=0.1, label=s),      
            hjust=0, size=4,color="black")+ 
  scale_x_discrete(expand = expansion(mult = c(0,0)))+
  ylim(-6, 6)+ 
  labs(x='Species', y='LDA')
ggsave("output/Fig.1/G/barplot_2.pdf",height = 7.5,width = 9)

################ 5d
rm(list = ls())
library(tidyverse)
data <- data.table::fread("data/lefse_5d/Ileum_3d5d_lefse_LDA.xls",data.table = F)
data <- subset(data,lda > 2 & pvalue < 0.05)
data <- data[grep("s_",data$taxon),]
data <- data %>%
  separate(col = `taxon`, into = c("p","c","o","f","g","s"), sep = "\\.", remove = FALSE)
data <- data[,-c(1:6)]
data <- data %>% 
  group_by(group) %>% 
  slice_max(lda, n = 10)

data <- data[,c(1,3,4)]
data <- data %>%
  mutate(lda = ifelse(group == "t_3d", -lda, lda))
ggplot(data,aes(reorder(s, lda),lda,fill=group))+  
  geom_col()+  
  theme_bw()+  
  theme(panel.grid.major=element_blank(),        
        panel.grid.minor=element_blank(),       
        panel.border = element_blank(),   
        legend.title = element_blank(),   
        axis.text = element_text(color="black",size=10),   
        axis.line.x = element_line(color='black',size=0.05),   
        axis.ticks.y = element_blank(),      
        axis.text.y = element_blank(),       
        legend.position = 'right')+  
  coord_flip()+ 
  geom_segment(aes(y=0, yend=0,x=0,xend=18.5))+
  geom_text(data = data[which(data$lda>0),],aes(x=s, y=-0.1, label=s),  
            hjust=1, size=4,color="black")+ 
  geom_text(data = data[which(data$lda<0),],aes(x=s, y=0.1, label=s),      
            hjust=0, size=4,color="black")+ 
  scale_x_discrete(expand = expansion(mult = c(0,0)))+
  ylim(-6, 6)+ 
  labs(x='Species', y='LDA')
ggsave("output/Fig.1/G/barplot_2.pdf",height = 7.5,width = 9)

