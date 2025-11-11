########## 堆叠图
######### P1-control
library(tidyverse)
library(stringr)
library(ggplot2)
library(cowplot)

aa <- list.files("data/Differential gene analysis/")
df <- as.data.frame(NULL)
plot_data <- as.data.frame(NULL)
for (l in 1:length(aa)) {
  bb <- paste0("data/Differential gene analysis/",aa[l],"/")
  cc <- list.files(bb)
  for (i in 1:length(cc)) {
    dd <- paste0(bb,cc[i])
    jj <- str_split(cc[i],"_Control")[[1]][1]
    data <- read.csv(dd)
    data <- subset(data, abs(logFC) >= 1 & padj <= 0.05)
    df[i,1] <- nrow(data)
    df[i,2] <- jj
    df[i,3] <- aa[l]
  }
  plot_data <- rbind(plot_data,df)
}
colnames(plot_data) <- c("num","time","organ")
plot_data <- subset(plot_data,time != "Sham")
plot_data$time <- factor(plot_data$time,levels = c("SCI_0min","SCI_30min","SCI_1h","SCI_6h","SCI_12h",
                                                   "SCI_1d","SCI_3d","SCI_5d","SCI_7d","SCI_14d",
                                                   "SCI_1m","SCI_2m","SCI_3m"))
p1 <- ggplot(plot_data,aes(organ,num,fill=time))+
  #柱状堆积图
  geom_col(position = "fill")+
  #颜色
  coord_flip() +
  scale_fill_manual(values = c("#9f81b8","#e8c0ba","#f7b6d1","#855aa5","#f57f1f",
                               "#76c043","#d52928","#3388c9","#269e68","#1fbfd0",
                               "#fcba78","#aec7e8","#f69796"))+
  #主题
  labs(x = "", y = "Control Proportion", fill = 'Time') +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5),
        plot.margin = margin(15,0,15,0),
        legend.position = "none")


############ P3 control+双Y轴
library(tidyverse)
library(stringr)
library(ggplot2)
aa <- list.files("data/Differential gene analysis/")
df <- as.data.frame(NULL)
plot_data <- as.data.frame(NULL)
for (l in 1:length(aa)) {
  bb <- paste0("data/Differential gene analysis/",aa[l],"/")
  cc <- list.files(bb)
  for (i in 1:length(cc)) {
    dd <- paste0(bb,cc[i])
    jj <- str_split(cc[i],"_Control")[[1]][1]
    data <- read.csv(dd)
    data <- subset(data, abs(logFC) >= 1 & padj <= 0.05)
    df[i,1] <- nrow(data)
    df[i,2] <- nrow(subset(data, logFC >= 1))
    df[i,3] <- nrow(subset(data, logFC <= -1))
    df[i,4] <- jj
    df[i,5] <- aa[l]
  }
  plot_data <- rbind(plot_data,df)
}
colnames(plot_data) <- c("All","Up","Down","time","organ")
plot_data <- subset(plot_data,time != "Sham")
plot_data <- plot_data[,c("All","Up","Down","organ")]
plot_data <- plot_data %>%
  group_by(organ) %>%
  summarise(across(everything(), sum))

data_1 <- plot_data[,c("Up","Down","organ")]


p3 <- ggplot()+ 
  geom_point(data = data_1, aes(x = organ, Up,fill = "UP"),color="#d39ac1",size=1)+
  geom_line(data = data_1,aes(x = organ, Up,fill = "UP",group=1),color="#d39ac1",cex=0.8)+ 
  geom_point(data = data_1,aes(x = organ, Down,fill = "Down"), color="#decc33", size=1)+
  geom_line(data = data_1,aes(x = organ, Down,group=1,fill = "Down"),color="#decc33",cex=0.8)+
  coord_flip()+
  labs(x = "", y = "Control Number") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        plot.margin = margin(15,0,15,0),
        legend.position = "none")


# Extract legends
legend_p1 <- get_legend(p1 + theme(legend.position = "right"))
legend_p3 <- get_legend(p3 + theme(legend.position = "right"))

combined_legend <- gridExtra::arrangeGrob(legend_p1,legend_p3, ncol = 1)

# Combine plots and legends
combined_plot <- plot_grid(p1, p3,  nrow = 1, align = "h", rel_widths = c(1, 0.75))
final_plot <- plot_grid(combined_plot, combined_legend, nrow = 1, rel_widths = c(1, 0.2))

# Display the final plot
print(final_plot)

