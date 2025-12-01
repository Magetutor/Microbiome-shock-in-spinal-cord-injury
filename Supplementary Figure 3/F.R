#########################################################
# 代码目的：
# 1.计算每个器官在不同时期的表达数量
#########################################################
rm(list = ls())
########################
library(tidyverse)
aa <- list.files("data/tpm/tpm_mean/coding/")
aa <- paste0("data/tpm/tpm_mean/coding/",aa)
df <- data.frame(1:15)

for (i in 1:length(aa)) {
  data <- read.csv(aa[i],row.names = 1)
  num = apply(data, 2, function(x){length(which(x > 0.1))})
  jj <- str_split(str_split(aa[i],"/")[[1]][5],"\\.")[[1]][1]
  df[,i] <-  num
  colnames(df)[i] <- jj
}
num <- as.data.frame(num)
rownames(df) <- rownames(num)

df <- cbind("time" = rownames(df),df)
subject_info <- df %>% pivot_longer(cols = !time, 
                            names_to = "organ",
                            values_to = "num")
subject_info$group <- paste0(subject_info$time,"_",subject_info$organ)

# 加载必要的包
library(tidyverse)
library(gghalves)
library(circlize)

# 设置颜色
age_color <- '#631879FF'
organ_color <- c("blood" = "#9f81b8","bone"= "#e8c0ba","colon"= "#f7b6d1","cortex"= "#855aa5","fat"= "#f57f1f",
                 "heart"= "#76c043","kidney"= "#d52928","liver"= "#3388c9","lung"= "#269e68","marrow"= "#1fbfd0",
                 "muscle"= "#fcba78","small"= "#aec7e8","spinal"= "#f69796","spleen"= "#cebf8f") #ABDDDE 文章配色

time_color <- c("Control" = "#1f77b4","Sham" = "#f57d1f","SCI_0min" = "#92a8af","SCI_30min" = "#d42827","SCI_1h" = "#8458a4",
                "SCI_6h" = "#8b554a","SCI_12h" = "#d779b0","SCI_1d" = "#b6bc60","SCI_3d" = "#1cbdcf","SCI_5d" = "#adc6e8",
                "SCI_7d" = "#fbb978","SCI_14d" = "#b1c7c5","SCI_1m" = "#f69595","SCI_2m" = "#c5afd4","SCI_3m" = '#74A089') #3B9AB2 文章配色


# 数据处理
df <-  data.frame(  
  factors = subject_info$group,    
  x = 1,  
  y = 1,  
  subject_info, 
  stringsAsFactors = TRUE 
  ) %>%  
  arrange(num) %>% 
  mutate(factors = factor(factors, levels = factors))

circos.clear()
circos.par( 
  "track.height" = 0.2,
  start.degree = 90, 
  clock.wise = TRUE,  
  gap.after = c(rep(0, nrow(df) - 1), 90), 
  circle.margin = c(0.1, 0.1, 0.1, 0.1), 
  cell.padding = c(0, 0, 0, 0))

circos.initialize(factors = df$factors, x = df$x, xlim = c(0.5, 1.5))

# 定义绘制函数
plot_circos_track <- function(temp_value, color, ylab, ylim_mult = c(0.8, 1.1), track_height = 0.2) { 
  circos.track(  
    factors = df$factors,    
    y = temp_value,   
    ylim = range(temp_value, na.rm = TRUE) * ylim_mult,   
    bg.border = "black",   
    track.height = track_height,    
    panel.fun = function(x, y) {    
      name = get.cell.meta.data("sector.index")    
      i = get.cell.meta.data("sector.numeric.index") 
      xlim = get.cell.meta.data("xlim")  
      ylim = get.cell.meta.data("ylim")   
      
      circos.yaxis(
        side = "left",
        at = c(ceiling(0.8*min(temp_value, na.rm = TRUE)),
               round((min(temp_value, na.rm = TRUE) + max(temp_value, na.rm = TRUE)) / 2, 0),
               round(max(temp_value, na.rm = TRUE), 0)),
        sector.index = get.all.sector.index()[1],
        labels.cex = 0.8,
        labels.niceFacing = FALSE
      )
      
      circos.lines(     
        x = mean(xlim, na.rm = TRUE),    
        y =  temp_value[i],    
        type = "h",      
        col = color,     
        lwd = 2   
      )          
      # if (ylab == "Age") {     
      #   circos.text(      
      #     x = 1,       
      #     y = 110,     
      #     labels = name,  
      #     facing = "clockwise",     
      #     niceFacing = TRUE,      
      #     cex = 0.75     
      #   )    
      # }     
      
      circos.points(   
        x = mean(xlim), 
        y =  temp_value[i],   
        pch = 16,   
        cex = 0.8,  
        col = color   
      )   
    }  
  )
  }
# 绘制  tracks
plot_circos_track(df$num, age_color, "Age")

# 定义绘制分类变量的函数
plot_categorical_track <- function(temp_var, colors, track_height = 0.1) {  
  temp_var[is.na(temp_var)] <- "grey" 
  mapped_colors <- colors[temp_var] 
  circos.track(  
    factors = df$factors,  
    y = df$y,    
    ylim = c(0, 1),   
    bg.border = "black",
    track.height = track_height,  
    panel.fun = function(x, y) {    
      i = get.cell.meta.data("sector.numeric.index")    
      xlim = get.cell.meta.data("xlim")   
      ylim = get.cell.meta.data("ylim")   
      circos.rect(    
        xleft = xlim[1],  
        ybottom = ylim[1],   
        xright = xlim[2],    
        ytop = ylim[2],      
        col = mapped_colors[i],   
        bg.border = "black"  
      )  
    } 
  )
  }
# 绘制分类 tracks
plot_categorical_track(df$organ, organ_color)
plot_categorical_track(df$time, time_color)
library(export)
graph2pdf(file = "circle.pdf",width = 10,height = 10)
# 定义绘制箱线图和点图的函数
plot_box_dot <- function(data, color, binwidth) { 
  ggplot(data.frame(class = "class", value = data), aes(x = class, y = value)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_dotplot(binaxis = "y", color = color, fill = color,
                 dotsize = 20, binwidth = binwidth, 
                 stackdir = "center") +  
    theme_bw() +   
    labs(x = "", y = "") +   
    scale_x_discrete(expand = expansion(mult = c(0, 0))) +  
    theme(panel.grid = element_blank(),   
          axis.text.x = element_blank(),     
          axis.ticks.x = element_blank())
  }
# 绘制年龄和BMI的箱线图和点图
plot_age <- plot_box_dot(df$num, age_color, 1)
ggsave(plot_age,file = "boxplot.pdf",width = 3,height = 4)

# 定义绘制条形图的函数
plot_bar <- function(data, colors, levels_order) { 
  ggplot(data.frame(class = "class", value = data), aes(x = class)) +  
    geom_bar(   
      aes(fill = factor(value, levels = levels_order)),    
      color = "black",     
      position = "stack",     
      width = 2  
    ) +   
    scale_fill_manual(values = colors) +  
    theme_bw() + 
    labs(x = "", y = "") +  
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +   
    theme(panel.grid = element_blank(),    
          axis.text.x = element_blank(),   
          axis.ticks.x = element_blank()   
    )}

# 绘制性别、种族和IRIS的条形图
plot_organ <- plot_bar(df$organ, organ_color, c("blood","bone","colon","cortex","fat",
                                                "heart","kidney","liver","lung","marrow",
                                                "muscle","small","spinal","spleen"))


ggsave(plot_organ,file = "bar_organ.pdf",width = 4,height = 4)
plot_time <- plot_bar(df$time, time_color, c("Control","Sham","SCI_0min","SCI_30min","SCI_1h",
                                             "SCI_6h","SCI_12h","SCI_1d","SCI_3d","SCI_5d",
                                             "SCI_7d","SCI_14d","SCI_1m","SCI_2m","SCI_3m"))

ggsave(plot_time,file = "bar_time.pdf",width = 4,height = 4)

