rm(list = ls())
library(tidyverse)
library(pheatmap)
############ Colon
data <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_s_RPKM_percent.xls",data.table = F)
data <- data %>%
  separate(col = `#Taxonomy`, into = c("d", "k", "p","c","o","f","g","s"), sep = ";", remove = FALSE)
data <- data[,-c(1:8)]
rownames(data) <- data$s
data <- data[,-1]
##### Co
df <- as.vector(NULL)
aa <- c("Con_","SCI_0min_","SCI_30min_","SCI_1h_",
        "SCI_6h_","SCI_12h_","SCI_1d_","SCI_3d_","SCI_5d_",
        "SCI_7d_","SCI_14d_","SCI_1m_","SCI_2m_","SCI_3m_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Co")
  df <- c(df,bb)
}
Co <- data[,df]
Co <- Co %>% 
  mutate(totle = rowSums(.)) %>%
  top_n(10, wt = totle) %>%
  select(-totle)
Co <- as.data.frame(t(Co))
group <- rep(c("Con","SCI_0min","SCI_30min","SCI_1h",
               "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
               "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"),each = 6)
Co <- cbind(group,Co)
Co <- aggregate(. ~ group, data = Co, FUN = mean)
rownames(Co) <- Co$group
Co <- Co[,-1]
Co <- Co[c("Con","SCI_0min","SCI_30min","SCI_1h",
           "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
           "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"),]
Co <- as.data.frame(t(Co))
p <- pheatmap(Co,
         color = colorRampPalette(c("#3672b6", "white", "#e92e22"))(100),
         border="black",#边框颜色
         main = "Colon",#指定图表的标题
         show_rownames = T,#是否展示行名
         show_colnames = T,#是否展示列名
         cexCol = 1,#指定列标签的缩放比例。
         scale = 'row',
         cluster_col=F,
         cluster_rows=T,
         cellwidth = unit(0.5, "cm"),
         cellheight = unit(0.5, "cm"),
         border_color = "black",
         angle_col = "45",#指定列标签的角度。
         legend = T,#指定是否显示图例。
         fontsize_row = 8,#分别指定行标签和列标签的字体大小。
         fontsize_col = 8,
         legend_breaks = c(-2, 0, 2))
ggsave(p,file = "output/Fig.2/Colon.pdf")





############ Colon
data <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_s_RPKM_percent.xls",data.table = F)
data <- data %>%
  separate(col = `#Taxonomy`, into = c("d", "k", "p","c","o","f","g","s"), sep = ";", remove = FALSE)
data <- data[,-c(1:8)]
rownames(data) <- data$s
data <- data[,-1]
##### Co
df <- as.vector(NULL)
aa <- c("Con_","SCI_0min_","SCI_30min_","SCI_1h_",
        "SCI_6h_","SCI_12h_","SCI_1d_","SCI_3d_","SCI_5d_",
        "SCI_7d_","SCI_14d_","SCI_1m_","SCI_2m_","SCI_3m_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Il")
  df <- c(df,bb)
}
Co <- data[,df]
Co <- Co %>% 
  mutate(totle = rowSums(.)) %>%
  top_n(10, wt = totle) %>%
  select(-totle)
Co <- as.data.frame(t(Co))
group <- rep(c("Con","SCI_0min","SCI_30min","SCI_1h",
               "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
               "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"),each = 6)
Co <- cbind(group,Co)
Co <- aggregate(. ~ group, data = Co, FUN = mean)
rownames(Co) <- Co$group
Co <- Co[,-1]
Co <- Co[c("Con","SCI_0min","SCI_30min","SCI_1h",
           "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
           "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"),]
Co <- as.data.frame(t(Co))
p <- pheatmap(Co,
              color = colorRampPalette(c("#3672b6", "white", "#e92e22"))(100),
              border="black",#边框颜色
              main = "Intestine",#指定图表的标题
              show_rownames = T,#是否展示行名
              show_colnames = T,#是否展示列名
              cexCol = 1,#指定列标签的缩放比例。
              scale = 'row',
              cluster_col=F,
              cluster_rows=T,
              cellwidth = unit(0.5, "cm"),
              cellheight = unit(0.5, "cm"),
              border_color = "black",
              angle_col = "45",#指定列标签的角度。
              legend = T,#指定是否显示图例。
              fontsize_row = 8,#分别指定行标签和列标签的字体大小。
              fontsize_col = 8,
              legend_breaks = c(-2, 0, 2))
ggsave(p,file = "output/Fig.2/Intestine.pdf")
