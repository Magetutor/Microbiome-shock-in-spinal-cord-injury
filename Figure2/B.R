######## 柱状图
rm(list = ls())
library(ggplot2)
library(tidyverse)
library(gground)
library(ggprism)
data_1 <- read.csv("data/SCI_12h_up.csv",row.names = 1)
aa <- c("regulation of lipid storage",
        'regulation of lipase activity',
        "regulation of leukocyte cell-cell adhesion",
        "regulation of chemokine production",
        "lipid catabolic process",
        "hormone transport",
        "regulation of mast cell activation",
        "leukocyte proliferation",
        "lymphocyte proliferation",
        "monocyte chemotaxis")
data_1 <- subset(data_1,Description %in% aa)
data_1 <- data_1[,c(2,6,9)]
data_1 <- cbind(group = "fat_12h",data_1)
data_1$Description <- paste0(data_1$Description,"_fat")

data_2 <- read.csv("data/SCI_2m_up.csv",row.names = 1)
aa <- c("morphogenesis of a branching epithelium",
        'immune response-activating cell surface receptor signaling pathway',
        "alpha-beta T cell differentiation",
        "regulation of leukocyte cell-cell adhesion",
        "tissue remodeling",
        "NK T cell differentiation",
        "peptidyl-tyrosine phosphorylation",
        "regulation of glucose metabolic process",
        "regulation of hormone secretion",
        "regulation of glycolytic process")
data_2 <- subset(data_2,Description %in% aa)
data_2 <- data_2[,c(2,6,9)]
data_2 <- cbind(group = "marrow_2m",data_2)
data_2$Description <- paste0(data_2$Description,"_marrow")
data <- rbind(data_2,data_1)


enrich_df <- data %>%
  mutate(part = factor(group, levels = c("marrow_2m","fat_12h"))) %>%
  group_by(group) %>%
  arrange(p.adjust) %>%
  mutate(
    group_rank = row_number(),
    alpha_value = scales::rescale(-log10(p.adjust), to = c(0.3,0.8))
  ) %>%
  ungroup() %>%
  mutate(
    Description = factor(
      Description,
      levels = with(., Description[order(part, -group_rank)])
    )
  )

width <-0.5

xaxis_max <- max(-log10(enrich_df$p.adjust)) +1

rect.data <- enrich_df %>%
  group_by(part) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(
    ymax = cumsum(n),
    ymin = lag(ymax, default =0) +1, 
    xmin = -0.8, 
    xmax = -0.4
  )
color_mapping <- c(
  "marrow_2m"="#4DBBD5",
  "fat_12h"="#E64B35"
)
enrich_df %>%
  ggplot(aes(-log10(p.adjust), y = Description, fill = part)) +
  geom_round_col(
    aes(y = Description,alpha = alpha_value), width =0.8) +
  geom_text(
    aes(x =0.05, label = Description),
    hjust =0, size =5
  ) +
  geom_round_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = part),
    data = rect.data,
    alpha = 0.8,
    radius = unit(2,'mm'),
    inherit.aes =FALSE
  ) +
  geom_text(
    aes(x = (xmin + xmax) /2, y = (ymin + ymax) /2, label = part),
    data = rect.data,
    angle = 90,
    hjust = 0.5,
    vjust = 0.5,
    size = 5,
    inherit.aes =FALSE
  ) +
  annotate(
    "segment",
    x = 0, y = 0, 
    xend = xaxis_max, yend = 0,
    linewidth = 1.5
  ) +
  labs(y =NULL) +
  scale_fill_manual(values = color_mapping,guide = "none") +
  scale_alpha_identity()+
  scale_x_continuous(
    breaks = seq(0, xaxis_max,1),
    expand = expansion(c(0,0))
  ) +
  theme_prism() +
  theme(
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_text()
  )
ggsave(file = "output/Fig.2/B/barplot.pdf",width = 9,height =8 )
