rm(list = ls())
library(tidyverse)
############
cc <- c("Il6","Il10","Hk2","Pfkl")
bb <- c("spinal_tpm.csv","colon_tpm.csv","small_tpm.csv",
        "fat_tpm.csv","marrow_tpm.csv")
bb <- paste0("data/tpm_filter/",bb)

remove_last_part <- function(string) {
  sub("(.*?)(_[^_]*)$", "\\1", string)
}
for (pp in 1:length(cc)) {
  df <- data.frame(NULL)
  for (i in 1:length(bb)) {
    data <- read.csv(bb[i],row.names = 1)
    jj <- str_split(str_split(bb[i],"/")[[1]][4],"_")[[1]][1]
    data <- data[cc[pp],]
    data <- data[,-c(1:3,8:11)]
    rownames(data) <- jj
    colnames(data) <- sapply(colnames(data), remove_last_part)
    data <- cbind(organ = rownames(data),data)
    df <- rbind(df,data)
  }
  aa <- c("Control","SCI_0min","SCI_30min","SCI_1h","SCI_6h",
          "SCI_12h","SCI_1d","SCI_3d","SCI","SCI_7d","SCI_14d",
          "SCI_1m","SCI_2m","SCI_3m")
  df[,-1] <- log2(df[,-1] + 1)
  df <- df %>%
    pivot_longer(cols = -"organ", names_to = "group", values_to = "num")
  df$group <- sapply(df$group, remove_last_part)
  df$group <- factor(df$group,levels = aa)
  df$group <- as.numeric(df$group)
  
  
  # 数据准备：计算每组均值
  df_summary <- df %>%
    group_by(organ, group) %>%
    summarise(
      mean_num = mean(num),
      sd = sd(num)
    )
  
  # 创建折线图：以group为x轴，mean_num为y轴，按Group分组
  p <- ggplot(df_summary, aes(x = group, y = mean_num, group = organ, color = organ)) +
    
    # 添加折线、单点和趋势线
    # geom_line(linewidth = 1.2, algroupa = 0.8) +          # 固定线宽
    # geom_point(data = df_summary, aes(y = mean_num),      # 保留单点，使用原始num
    #            shape = 16, size = 2, algroupa = 0.6) +   # 固定点大小+
    # geom_errorbar(aes(ymin=mean_num-sd, 
    #                   ymax=mean_num+sd),
    #               size=1.05)+
    # 设置学术风格主题
    geom_smooth(aes(group = organ), method = "loess", 
                se = FALSE, linewidth = 1) + 
    theme_minimal(base_size = 14) +                     # 简洁主题，基础字体大小
    theme(
      panel.grid.major = element_line(color = "grey85", size = 0.3),  # 浅灰细网格线
      panel.grid.minor = element_blank(),                 # 移除次网格线
      axis.line = element_line(color = "black"),          # 黑色坐标轴
      axis.ticks = element_line(color = "black"),         # 黑色刻度线
      legend.position = "right",                          # 图例置右
      legend.box = "vertical",                            # 图例垂直排列
      legend.title = element_text(face = "bold"),         # 图例标题加粗
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 标题居中加粗
      plot.subtitle = element_text(hjust = 0.5, size = 12)  # 副标题居中
    ) +
    
    # 设置颜色和标签
    scale_color_manual(values = c("spinal" = "#bce2b5", 
                                  "colon" = "#fcb461",
                                  "small"  = "#fee0bf",
                                  "fat"  =  "#dec2df",
                                  "marrow"  = "#d69f9f"),  # 自定义颜色
                       name = "Group") +                      # 仅保留Group图例
    labs(
      title = cc[pp],
      y = "log2(TPM + 1)"
    )+    
    theme_classic()+
    scale_x_continuous(breaks = seq(1,14), labels = c("Control","SCI_0min","SCI_30min","SCI_1h","SCI_6h",
                                                    "SCI_12h","SCI_1d","SCI_3d","SCI_5d","SCI_7d","SCI_14d",
                                                    "SCI_1m","SCI_2m","SCI_3m"))
  
  
  # 显示图形
  print(p)
  dd <- paste0("output/Fig.2/hexinjiyin/",cc[pp],".pdf")
  # 可选：保存为高质量图片
  ggsave(p,file = dd,width = 9.5, height = 6.5)
}
