########## 相关性网络图绘制
### Ce
rm(list = ls())
library(tidyverse)
aa <- list.files("data/correlation/Ce_correlation/")
aa <- paste0("data/correlation/Ce_correlation/",aa)
cytoscape_postive_data <- data.frame(NULL)
point_postive_data <- data.frame(NULL)
cytoscape_negtive_data <- data.frame(NULL)
point_negtive_data <- data.frame(NULL)
organ_all_positive  <- data.frame(NULL)
organ_all_negtive  <- data.frame(NULL)
for (i in 1:length(aa)) {
  bb <- str_split(str_split(aa[i],"/")[[1]][4],".csv")[[1]][1]
  data <- data.table::fread(aa[i],data.table = F)
  data <- subset(data,meta != "s__Lactobacillus_sp..1")
  data <- data[,-1]
  df <- filter(data,abs(cor) > 0.7 & cor.pvalue < 0.05)
  
  df_positive <- filter(df,cor > 0)
  result_positive <- df_positive %>%
    group_by(meta) %>%          
    summarise(unique_cnt = n_distinct(gene), .groups = "drop")
  
  if (nrow(result_positive) > 0) {
    df_positive <- filter(df_positive,meta %in% result_positive$meta)
    result_positive <- cbind(result_positive,organ = paste0(bb,"_positive"))
    
    cytoscape_postive <- result_positive[,c(1,3)]
    mean <- df_positive %>%
      group_by(meta) %>% 
      summarise(mean_value = mean(cor))
    cytoscape_postive <- merge(cytoscape_postive,mean,by = "meta")
    
    point_postive <- as.data.frame(cbind(result_positive$meta,size = 100))
    colnames(point_postive) <- c("ID","size")
    point_postive <- rbind(point_postive,data.frame(ID = paste0(bb,"_positive"),size = length(unique(df_positive$gene))))
    cytoscape_postive_data <- rbind(cytoscape_postive_data,cytoscape_postive)
    point_postive_data <- rbind(point_postive_data,point_postive)
    point_postive_data <- point_postive_data %>%
      distinct(ID, .keep_all = TRUE)
  } else {
    print("None")
  }
  
  df_negtive <- filter(df,cor < 0)
  result_negtive <- df_negtive %>%
    group_by(meta) %>%          
    summarise(unique_cnt = n_distinct(gene), .groups = "drop")
  if (nrow(result_negtive) >0) {
    df_negtive <- filter(df_negtive,meta %in% result_negtive$meta)
    result_negtive <- cbind(result_negtive,organ = paste0(bb,"_negtive"))
    
    cytoscape_negtive <- result_negtive[,c(1,3)]
    mean <- df_negtive %>%
      group_by(meta) %>% 
      summarise(mean_value = mean(cor))
    cytoscape_negtive <- merge(cytoscape_negtive,mean,by = "meta")
    
    point_negtive <- as.data.frame(cbind(result_negtive$meta,size = 100))
    colnames(point_negtive) <- c("ID","size")
    point_negtive <- rbind(point_negtive,data.frame(ID = paste0(bb,"_negtive"),size = length(unique(df_negtive$gene))))
    cytoscape_negtive_data <- rbind(cytoscape_negtive_data,cytoscape_negtive)
    point_negtive_data <- rbind(point_negtive_data,point_negtive)
    point_negtive_data <- point_negtive_data %>%
      distinct(ID, .keep_all = TRUE)
  } else {
    print("None")
  }
  df_all_positive <- df_positive
  df_all_negtive <- df_negtive
  organ_all_positive <- rbind(organ_all_positive,df_all_positive)
  organ_all_negtive <- rbind(organ_all_negtive,df_all_negtive)
}

point_all_positive <- organ_all_positive %>%
  group_by(meta) %>%          
  summarise(unique_cnt = n_distinct(gene), .groups = "drop")

colnames(point_all_positive) <- c("ID","size")
point_all_positive <- as.data.frame(point_all_positive)

point_postive_data <- point_postive_data %>%
  distinct(ID, .keep_all = TRUE)

point_postive_data$size[match(point_all_positive$ID,point_postive_data$ID)] <- point_all_positive$size

point_postive_data$size <- as.numeric(unlist(point_postive_data$size))
f_postive <- point_postive_data %>%
  filter(str_detect(ID, "^s_"))
f_postive <- as.data.frame(f_postive[,1])

point_all_negtive <- organ_all_negtive %>%
  group_by(meta) %>%          
  summarise(unique_cnt = n_distinct(gene), .groups = "drop")
colnames(point_all_negtive) <- c("ID","size")
point_all_negtive <- as.data.frame(point_all_negtive)

point_negtive_data <- point_negtive_data %>%
  distinct(ID, .keep_all = TRUE)

point_negtive_data$size[match(point_all_negtive$ID,point_negtive_data$ID)] <- point_all_negtive$size
point_negtive_data$size <- as.numeric(unlist(point_negtive_data$size))
f_negtive <- point_negtive_data %>%
  filter(str_detect(ID, "^s_"))
f_negtive <- as.data.frame(f_negtive[,1])


write.table(f_postive,file ="output/cytoscape_data/Ce/s_postive.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(f_negtive,file ="output/cytoscape_data/Ce/s_negtive.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(cytoscape_postive_data,file ="output/cytoscape_data/Ce/cytoscape_postive_data.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(cytoscape_negtive_data,file ="output/cytoscape_data/Ce/cytoscape_negtive_data.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(point_postive_data,file ="output/cytoscape_data/Ce/point_postive_data.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(point_negtive_data,file ="output/cytoscape_data/Ce/point_negtive_data.txt", sep = "\t", quote = F,row.names = FALSE)

########## 相关性网络图绘制
### Co
rm(list = ls())
library(tidyverse)
aa <- list.files("data/correlation/Co_correlation/")
aa <- paste0("data/correlation/Co_correlation/",aa)
cytoscape_postive_data <- data.frame(NULL)
point_postive_data <- data.frame(NULL)
cytoscape_negtive_data <- data.frame(NULL)
point_negtive_data <- data.frame(NULL)
organ_all_positive  <- data.frame(NULL)
organ_all_negtive  <- data.frame(NULL)
for (i in 1:length(aa)) {
  bb <- str_split(str_split(aa[i],"/")[[1]][4],".csv")[[1]][1]
  data <- data.table::fread(aa[i],data.table = F)
  data <- subset(data,meta != "s__Lactobacillus_sp..1")
  data <- data[,-1]
  df <- filter(data,abs(cor) > 0.7 & cor.pvalue < 0.05)
  
  df_positive <- filter(df,cor > 0)
  result_positive <- df_positive %>%
    group_by(meta) %>%          
    summarise(unique_cnt = n_distinct(gene), .groups = "drop")
  
  if (nrow(result_positive) > 0) {
    df_positive <- filter(df_positive,meta %in% result_positive$meta)
    result_positive <- cbind(result_positive,organ = paste0(bb,"_positive"))
    
    cytoscape_postive <- result_positive[,c(1,3)]
    mean <- df_positive %>%
      group_by(meta) %>% 
      summarise(mean_value = mean(cor))
    cytoscape_postive <- merge(cytoscape_postive,mean,by = "meta")
    
    point_postive <- as.data.frame(cbind(result_positive$meta,size = 100))
    colnames(point_postive) <- c("ID","size")
    point_postive <- rbind(point_postive,data.frame(ID = paste0(bb,"_positive"),size = length(unique(df_positive$gene))))
    cytoscape_postive_data <- rbind(cytoscape_postive_data,cytoscape_postive)
    point_postive_data <- rbind(point_postive_data,point_postive)
    point_postive_data <- point_postive_data %>%
      distinct(ID, .keep_all = TRUE)
  } else {
    print("None")
  }
  
  df_negtive <- filter(df,cor < 0)
  result_negtive <- df_negtive %>%
    group_by(meta) %>%          
    summarise(unique_cnt = n_distinct(gene), .groups = "drop")
  if (nrow(result_negtive) >0) {
    df_negtive <- filter(df_negtive,meta %in% result_negtive$meta)
    result_negtive <- cbind(result_negtive,organ = paste0(bb,"_negtive"))
    
    cytoscape_negtive <- result_negtive[,c(1,3)]
    mean <- df_negtive %>%
      group_by(meta) %>% 
      summarise(mean_value = mean(cor))
    cytoscape_negtive <- merge(cytoscape_negtive,mean,by = "meta")
    
    point_negtive <- as.data.frame(cbind(result_negtive$meta,size = 100))
    colnames(point_negtive) <- c("ID","size")
    point_negtive <- rbind(point_negtive,data.frame(ID = paste0(bb,"_negtive"),size = length(unique(df_negtive$gene))))
    cytoscape_negtive_data <- rbind(cytoscape_negtive_data,cytoscape_negtive)
    point_negtive_data <- rbind(point_negtive_data,point_negtive)
    point_negtive_data <- point_negtive_data %>%
      distinct(ID, .keep_all = TRUE)
  } else {
    print("None")
  }
  df_all_positive <- df_positive
  df_all_negtive <- df_negtive
  organ_all_positive <- rbind(organ_all_positive,df_all_positive)
  organ_all_negtive <- rbind(organ_all_negtive,df_all_negtive)
}

point_all_positive <- organ_all_positive %>%
  group_by(meta) %>%          
  summarise(unique_cnt = n_distinct(gene), .groups = "drop")

colnames(point_all_positive) <- c("ID","size")
point_all_positive <- as.data.frame(point_all_positive)

point_postive_data <- point_postive_data %>%
  distinct(ID, .keep_all = TRUE)

point_postive_data$size[match(point_all_positive$ID,point_postive_data$ID)] <- point_all_positive$size

point_postive_data$size <- as.numeric(unlist(point_postive_data$size))
f_postive <- point_postive_data %>%
  filter(str_detect(ID, "^s_"))
f_postive <- as.data.frame(f_postive[,1])

point_all_negtive <- organ_all_negtive %>%
  group_by(meta) %>%          
  summarise(unique_cnt = n_distinct(gene), .groups = "drop")
colnames(point_all_negtive) <- c("ID","size")
point_all_negtive <- as.data.frame(point_all_negtive)

point_negtive_data <- point_negtive_data %>%
  distinct(ID, .keep_all = TRUE)

point_negtive_data$size[match(point_all_negtive$ID,point_negtive_data$ID)] <- point_all_negtive$size
point_negtive_data$size <- as.numeric(unlist(point_negtive_data$size))
f_negtive <- point_negtive_data %>%
  filter(str_detect(ID, "^s_"))
f_negtive <- as.data.frame(f_negtive[,1])


write.table(f_postive,file ="output/cytoscape_data/Co/s_postive.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(f_negtive,file ="output/cytoscape_data/Co/s_negtive.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(cytoscape_postive_data,file ="output/cytoscape_data/Co/cytoscape_postive_data.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(cytoscape_negtive_data,file ="output/cytoscape_data/Co/cytoscape_negtive_data.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(point_postive_data,file ="output/cytoscape_data/Co/point_postive_data.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(point_negtive_data,file ="output/cytoscape_data/Co/point_negtive_data.txt", sep = "\t", quote = F,row.names = FALSE)

########## 相关性网络图绘制
### Il
rm(list = ls())
library(tidyverse)
aa <- list.files("data/correlation/Il_correlation/")
aa <- paste0("data/correlation/Il_correlation/",aa)
cytoscape_postive_data <- data.frame(NULL)
point_postive_data <- data.frame(NULL)
cytoscape_negtive_data <- data.frame(NULL)
point_negtive_data <- data.frame(NULL)
organ_all_positive  <- data.frame(NULL)
organ_all_negtive  <- data.frame(NULL)
for (i in 1:length(aa)) {
  bb <- str_split(str_split(aa[i],"/")[[1]][4],".csv")[[1]][1]
  data <- data.table::fread(aa[i],data.table = F)
  data <- subset(data,meta != "s__Lactobacillus_sp..1")
  data <- data[,-1]
  df <- filter(data,abs(cor) > 0.7 & cor.pvalue < 0.05)
  
  df_positive <- filter(df,cor > 0)
  result_positive <- df_positive %>%
    group_by(meta) %>%          
    summarise(unique_cnt = n_distinct(gene), .groups = "drop")
  
  if (nrow(result_positive) > 0) {
    df_positive <- filter(df_positive,meta %in% result_positive$meta)
    result_positive <- cbind(result_positive,organ = paste0(bb,"_positive"))
    
    cytoscape_postive <- result_positive[,c(1,3)]
    mean <- df_positive %>%
      group_by(meta) %>% 
      summarise(mean_value = mean(cor))
    cytoscape_postive <- merge(cytoscape_postive,mean,by = "meta")
    
    point_postive <- as.data.frame(cbind(result_positive$meta,size = 100))
    colnames(point_postive) <- c("ID","size")
    point_postive <- rbind(point_postive,data.frame(ID = paste0(bb,"_positive"),size = length(unique(df_positive$gene))))
    cytoscape_postive_data <- rbind(cytoscape_postive_data,cytoscape_postive)
    point_postive_data <- rbind(point_postive_data,point_postive)
    point_postive_data <- point_postive_data %>%
      distinct(ID, .keep_all = TRUE)
  } else {
    print("None")
  }
  
  df_negtive <- filter(df,cor < 0)
  result_negtive <- df_negtive %>%
    group_by(meta) %>%          
    summarise(unique_cnt = n_distinct(gene), .groups = "drop")
  if (nrow(result_negtive) >0) {
    df_negtive <- filter(df_negtive,meta %in% result_negtive$meta)
    result_negtive <- cbind(result_negtive,organ = paste0(bb,"_negtive"))
    
    cytoscape_negtive <- result_negtive[,c(1,3)]
    mean <- df_negtive %>%
      group_by(meta) %>% 
      summarise(mean_value = mean(cor))
    cytoscape_negtive <- merge(cytoscape_negtive,mean,by = "meta")
    
    point_negtive <- as.data.frame(cbind(result_negtive$meta,size = 100))
    colnames(point_negtive) <- c("ID","size")
    point_negtive <- rbind(point_negtive,data.frame(ID = paste0(bb,"_negtive"),size = length(unique(df_negtive$gene))))
    cytoscape_negtive_data <- rbind(cytoscape_negtive_data,cytoscape_negtive)
    point_negtive_data <- rbind(point_negtive_data,point_negtive)
    point_negtive_data <- point_negtive_data %>%
      distinct(ID, .keep_all = TRUE)
  } else {
    print("None")
  }
  df_all_positive <- df_positive
  df_all_negtive <- df_negtive
  organ_all_positive <- rbind(organ_all_positive,df_all_positive)
  organ_all_negtive <- rbind(organ_all_negtive,df_all_negtive)
}

point_all_positive <- organ_all_positive %>%
  group_by(meta) %>%          
  summarise(unique_cnt = n_distinct(gene), .groups = "drop")

colnames(point_all_positive) <- c("ID","size")
point_all_positive <- as.data.frame(point_all_positive)

point_postive_data <- point_postive_data %>%
  distinct(ID, .keep_all = TRUE)

point_postive_data$size[match(point_all_positive$ID,point_postive_data$ID)] <- point_all_positive$size

point_postive_data$size <- as.numeric(unlist(point_postive_data$size))
f_postive <- point_postive_data %>%
  filter(str_detect(ID, "^s_"))
f_postive <- as.data.frame(f_postive[,1])

point_all_negtive <- organ_all_negtive %>%
  group_by(meta) %>%          
  summarise(unique_cnt = n_distinct(gene), .groups = "drop")
colnames(point_all_negtive) <- c("ID","size")
point_all_negtive <- as.data.frame(point_all_negtive)

point_negtive_data <- point_negtive_data %>%
  distinct(ID, .keep_all = TRUE)

point_negtive_data$size[match(point_all_negtive$ID,point_negtive_data$ID)] <- point_all_negtive$size
point_negtive_data$size <- as.numeric(unlist(point_negtive_data$size))
f_negtive <- point_negtive_data %>%
  filter(str_detect(ID, "^s_"))
f_negtive <- as.data.frame(f_negtive[,1])


write.table(f_postive,file ="output/cytoscape_data/Il/s_postive.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(f_negtive,file ="output/cytoscape_data/Il/s_negtive.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(cytoscape_postive_data,file ="output/cytoscape_data/Il/cytoscape_postive_data.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(cytoscape_negtive_data,file ="output/cytoscape_data/Il/cytoscape_negtive_data.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(point_postive_data,file ="output/cytoscape_data/Il/point_postive_data.txt", sep = "\t", quote = F,row.names = FALSE)
write.table(point_negtive_data,file ="output/cytoscape_data/Il/point_negtive_data.txt", sep = "\t", quote = F,row.names = FALSE)
