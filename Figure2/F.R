rm(list = ls())
library(tidyverse)
############ Colon
data <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_s_RPKM_percent.xls",data.table = F)
data <- data %>%
  separate(col = `#Taxonomy`, into = c("d", "k", "p","c","o","f","g","s"), sep = ";", remove = FALSE)
data <- data[,-c(1:8)]
rownames(data) <- data$s
data <- data[,-1]
##### Co
df <- as.vector(NULL)
aa <- c("Con_","Sham_","SCI_0min_","SCI_30min_","SCI_1h_",
        "SCI_6h_","SCI_12h_","SCI_1d_","SCI_3d_","SCI_5d_",
        "SCI_7d_","SCI_14d_","SCI_1m_","SCI_2m_","SCI_3m_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Co")
  df <- c(df,bb)
}
Co <- data[,df]
Co <- Co %>% 
  mutate(totle = rowSums(.)) %>%
  top_n(10, wt = totle)
Co <- data.frame(ID = rownames(Co),Num = Co$totle)
inflamation <- data.table::fread("data/cytoscape/Colon/inflamation.csv",data.table = F)
name <- c("negative regulation of lymphocyte proliferation",
          "basophil activation involved in immune response",
          "chronic inflammatory response to non-antigenic stimulus",
          "immunoglobulin V(D)J recombination",
          "neutrophil mediated cytotoxicity",
          "mast cell differentiation",
          "leukocyte migration involved in inflammatory response",
          "T cell receptor V(D)J recombination",
          'pro-B cell differentiation',
          "cytotoxic T cell pyroptotic process",
          "leukocyte chemotaxis involved in inflammatory response")
inflamation <- subset(inflamation,meta %in% name)
inflamation <- inflamation[,3:5]
colnames(inflamation) <- c("meta","pathway","cor")

metabolism <- data.table::fread("data/cytoscape/Colon/metabolism.csv",data.table = F)
name <- c("Arginine biosynthesis",
          'Cobalamin transport and metabolism',
          "Fatty acid metabolism",
          "Fructose and mannose metabolism",
          "Glycerophospholipid metabolism",
          "Pantothenate and CoA biosynthesis",
          "Phosphonate and phosphinate metabolism",
          "Riboflavin metabolism",
          "Starch and sucrose metabolism",
          "Valine, leucine and isoleucine biosynthesis")
metabolism <- subset(metabolism,meta %in% name)
metabolism <- metabolism[,3:5]
colnames(metabolism) <- c("meta","pathway","cor")

data <- rbind(inflamation,metabolism)
write.table(data,file ="output/Fig.2/F/cytoscape/Co_cytoscape_data.txt", sep = "\t", quote = F,row.names = FALSE)

ID <- c(unique(data$pathway))
Num <- c(1)
point <- data.frame(ID,Num)
point <- rbind(point,Co)
write.table(point,file ="output/Fig.2/F/cytoscape/Co_point_data.txt", sep = "\t", quote = F,row.names = FALSE)



############ Il
rm(list = ls())
library(tidyverse)
data <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_s_RPKM_percent.xls",data.table = F)
data <- data %>%
  separate(col = `#Taxonomy`, into = c("d", "k", "p","c","o","f","g","s"), sep = ";", remove = FALSE)
data <- data[,-c(1:8)]
rownames(data) <- data$s
data <- data[,-1]
##### Co
df <- as.vector(NULL)
aa <- c("Con_","Sham_","SCI_0min_","SCI_30min_","SCI_1h_",
        "SCI_6h_","SCI_12h_","SCI_1d_","SCI_3d_","SCI_5d_",
        "SCI_7d_","SCI_14d_","SCI_1m_","SCI_2m_","SCI_3m_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Il")
  df <- c(df,bb)
}
Co <- data[,df]
Co <- Co %>% 
  mutate(totle = rowSums(.)) %>%
  top_n(10, wt = totle) 
Co <- data.frame(ID = rownames(Co),Num = Co$totle)
inflamation <- data.table::fread("data/cytoscape/Il/inflamation.csv",data.table = F)
name <- c("regulation of type 2 immune response",
          "stimulatory killer cell immunoglobulin-like receptor signaling pathway",
          "mast cell proliferation",
          "acute inflammatory response to non-antigenic stimulus",
          "positive regulation of mast cell cytokine production",
          "neutrophil degranulation",
          "regulation of humoral immune response",
          "regulation of T-helper 17 type immune response",
          "positive regulation of acute inflammatory response to non-antigenic stimulus",
          "regulation of B cell chemotaxis",
          "NLRP3 inflammasome complex assembly",
          "susceptibility to T cell mediated cytotoxicity",
          "helper T cell diapedesis")
inflamation <- subset(inflamation,meta %in% name)
inflamation <- inflamation[,3:5]
colnames(inflamation) <- c("meta","pathway","cor")
metabolism <- data.table::fread("data/cytoscape/Il/metabolism.csv",data.table = F)
name <- c("Fructose and mannose metabolism",
          'Phenylalanine, tyrosine and tryptophan biosynthesis',
          "Folate biosynthesis",
          "Fatty acid biosynthesis",
          "Vitamin B6 metabolism",
          "Folate biosynthesis",
          "Riboflavin metabolism",
          "Pantothenate and CoA biosynthesis",
          "Caffeine metabolism",
          "Various types of N-glycan biosynthesis")
metabolism <- subset(metabolism,meta %in% name)
metabolism <- metabolism[,3:5]
colnames(metabolism) <- c("meta","pathway","cor")
data <- rbind(inflamation,metabolism)
write.table(data,file ="output/Fig.2/F/cytoscape/Il_cytoscape_data.txt", sep = "\t", quote = F,row.names = FALSE)
ID <- c(unique(data$pathway))
Num <- c(1)
point <- data.frame(ID,Num)
point <- rbind(point,Co)
write.table(point,file ="output/Fig.2/F/cytoscape/Il_point_data.txt", sep = "\t", quote = F,row.names = FALSE)








