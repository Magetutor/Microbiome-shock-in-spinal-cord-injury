####### 炎症
rm(list = ls())
library(ggplot2) #绘图使用
library(clusterProfiler) #数据处理使用
library(GSVA) #GSVA使用
library(GSEABase) #数据处理使用
library(dplyr) #数据处理使用
library(data.table) #数据读取使用
library(pheatmap)
library(tidyverse)

load("data/GSVA_kegg_bp_geneset/240331_latest_GOBP.Rda")
load("data/GSVA_kegg_bp_geneset/240331_latest_keggpath1BP.Rda")

gcSample <- c(bppath,keggpath1)
# 处理名字

gcSample <- lapply(
  split(gcSample, names(gcSample)),          # 按名字分组
  function(x) do.call(c, x)        # 合并同组内的向量；如为 data.frame 可改用 rbind 等
)

gs = lapply(gcSample, unique) #每个列表里面的基因去重
# gsc <- GeneSetCollection(mapply(function(geneIds, gsva_resultId) {
#   GeneSet(geneIds, geneIdType=EntrezIdentifier(),
#           collectionType=gsva_resultCollection(gsva_resultId),
#           setName=gsva_resultId)
# }, gs, names(gs)))
# gsc  #list和GeneSetCollection格式都可以
## 2.2直接下载的基因集
# gene_set <- getGmt("./data/Homo/fatty acid metabolism.gmt")  #对应功能基因集

aa <- list.files("data/tpm_mean_filter/")
aa <- paste0("data/tpm_mean_filter/",aa)
aa <- aa[c(3,5,6,7,8,10,12,13)]
for (i in 1:length(aa)) {
  gsva_data <- read.csv(aa[i],row.names = 1)
  jj <- str_split(str_split(aa[i],"/")[[1]][3],".csv")[[1]][1]
  #### GSVA富集分析
  gsva_result <- gsva(as.matrix(gsva_data),  # 输入的表达数据需要`matrix`格式的
                      gs,  # Gene sets provided either as a list object or as a GeneSetCollection object
                      method = "gsva", # GSVA算法的选择。这里选择了"gsva"，也可以选择"ssgsea"-一般ssgsea用于免疫浸润
                      min.sz=1, # 基因集中允许的最小基因数。默认为1，表示允许基因集中只包含一个基因。
                      max.sz=Inf,# 基因集中允许的最大基因数。默认为Inf，表示没有最大限制，可以包含任意多个基因
                      kcdf="Gaussian",# 输入counts值使用"Poisson"。输入标准化之后的值(tpm/fpkm)，需要使用"Gaussian"
                      parallel.sz=1L)# 并行计算的规模。设置为1L，表示使用单线程计算
  ## GSVA结果，其中行名就是富集的通路，列名为样本名，值就是对应通路的GSVA得分。
  #### 下一步可以做何种分析
  ##  聚类：通过GSVA得到的样本富集得分，可以用于对样本进行聚类分析，可以对样本进行分型。
  ##  差异分析：GSVA结果还可以使用limma包进行差异分析，获得差异通路。
  ##  机器学习/深度学习模型的输入：GSVA结果可以用于构建分类器，对样本进行预测，例如将肿瘤样本与正常样本进行区分，或预测不同临床亚型。
  ##  与表型关联：将GSVA得分与临床特征或表型数据相关联，可以发现基因集富集与特定生物学表型之间的关联。
  gsva_result <- as.data.frame(gsva_result)
  gsva_result <- gsva_result[,-2]
  pp <- stringr::str_to_title(jj)
  name <- c("Fatty acid biosynthesis",
            "Glycolysis / Gluconeogenesis",
            "Starch and sucrose metabolism",
            "Fructose and mannose metabolism",
            "Fatty acid metabolism",
            "Tryptophan metabolism",
            "Mucin type O-glycan biosynthesis",
            "2-Oxocarboxylic acid metabolism",
            "Terpenoid backbone biosynthesis",
            "D-Amino acid metabolism",
            "Inositol phosphate metabolism",
            "Phosphonate and phosphinate metabolism",
            "Lysine degradation",
            "Histidine metabolism",
            "Riboflavin metabolism",
            "Pantothenate and CoA biosynthesis",
            "adaptive immune memory response",
            "helper T cell extravasation",
            "negative regulation of NK T cell activation",
            "mast cell apoptotic process",
            "regulation of neutrophil degranulation",
            "mast cell proliferation",
            "cytotoxic T cell differentiation",
            "regulation of neutrophil extravasation",
            "regulation of chronic inflammatory response",
            "NK T cell differentiation",
            "extrathymic T cell differentiation",
            "positive regulation of T cell migration")
  gsva_result <- gsva_result[name,]
  p <- pheatmap(gsva_result,
                color = colorRampPalette(c("#3672b6", "white", "#e92e22"))(100),
                border="black",#边框颜色
                main = pp,#指定图表的标题
                show_rownames = T,#是否展示行名
                show_colnames = T,#是否展示列名
                cexCol = 1,#指定列标签的缩放比例。
                scale = 'row',#指定是否应按行方向或列方向居中和缩放，或不居中和缩放。对应的值为row, column和none。
                cluster_col=F,#分别指定是否按列和行http://127.0.0.1:22479/graphics/cfdda67e-d6be-4a3a-b75f-8a004a9ebe8f.png聚类。
                cluster_row=F,
                cellwidth = unit(0.5, "cm"),
                cellheight = unit(0.5, "cm"),
                border_color = "black",
                angle_col = "45",#指定列标签的角度。
                legend = T,#指定是否显示图例。
                fontsize_row = 8,#分别指定行标签和列标签的字体大小。
                fontsize_col = 8,
                legend_breaks = c(-2, 0, 2))
   jj <- paste0("output/",jj,".pdf")
  ggsave(p, file=jj, width=7, height=15,limitsize = FALSE)
}

