rm(list = ls())
library(ggplot2)
data <- data.frame(group = c("Public Data","Multi-Organ Data"),num = c(967,840))

theme_academic <- theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),  # 移除X轴主网格线
    panel.grid.minor = element_blank(),    # 移除次网格线
    axis.line = element_line(color = "black"),  # 添加黑色坐标轴线
    axis.text = element_text(size = 12, color = "black"),  # 字体大小和颜色
    axis.title = element_text(size = 14, face = "bold"),  # 标题加粗
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),  # 居中标题
    legend.position = "top",  # 图例置于顶部
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)  # 设置边距
  )

ggplot(data, aes(x = group, y = num, fill = group)) +
  # 使用geom_bar绘制分组柱状图，position_dodge使柱子并排放置
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.5, color = "black", size = 0.8) +
  # 自定义渐变填充
  scale_fill_manual(values = c("Public Data" = "#f7c495", "Multi-Organ Data" = "#99ce9a"), 
                    name = "Data Type") +
  # 在柱子上方添加数据标签，position_dodge确保标签与柱子对齐
  geom_text(aes(label = round(num, 1)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 4, color = "black") +
  # 设置标题和轴标签
  labs(
    y = "The Sample Number of SCIGAMA"
  ) +
  # 应用主题
  theme_academic +
  # 调整Y轴范围，确保标签不被裁剪
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

ggsave(file = "output/bar.pdf",width = 4.5,height = 4)
 