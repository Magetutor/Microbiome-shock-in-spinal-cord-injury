############# sankey _ Control ################
rm(list = ls())
library(tidyverse)
library(xlsx)
# 加载必要的包
library(networkD3)
library(tidyverse)
library(readxl)
library(htmlwidgets) 
library(webshot)


data_s <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_s_RPKM_percent.xls",data.table = F)
rownames(data_s) <- data_s$`#Taxonomy`
data_s <- data_s[,-1]
##### Il
df <- as.vector(NULL)
aa <- c("Con_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Il")
  df <- c(df,bb)
}
data_s <- data_s[,df]

data_s$sum <- rowSums(data_s)
data_s <- subset(data_s,sum >0)
data_s <- cbind("sample" = rownames(data_s),data_s)
df_split <- data_s %>%
  separate(col = sample, into = c("d", "k", "p","c","o","f","g","s"), sep = ";", remove = FALSE)
length(unique(df_split$s)) # 3969
length(unique(df_split$g)) # 1318

fe <- df_split$s[order(df_split$sum, decreasing = TRUE)[1:20]]
data <- subset(df_split,s %in% fe)

data <- data[,c(5:9,16)]
data <- data[,c(3:6)]


data$f <- substr(data$f, 4,nchar(data$f))
data$g <- substr(data$g, 4,nchar(data$g))
data$s <- substr(data$s, 4,nchar(data$s))

# data <- data %>%
#   group_by(c, `o`,`f`,`g`,`s`) %>%
#   summarise(Count = n(), .groups = 'drop')
colnames(data)[4] <- "Count"


data <- as.data.frame(data)


f_order <- sort(unique(data$f), na.last = TRUE)
g_order <- sort(unique(data$g), na.last = TRUE)
s_order <- sort(unique(data$s), na.last = TRUE)
# 创建节点数据并按自定义顺序排序
nodes <- data.frame(name = c(
  f_order,
  g_order,
  s_order))

# 创建链接数据
links <- data %>%
  select(f, `g`, Count) %>%
  rename(source = f, target = `g`, value = Count) %>%
  bind_rows(
    data %>% select(`g`, `s`, Count) %>%
      rename(source = `g`, target = `s`, value = Count)
  ) %>%
  group_by(source, target) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  ungroup() 
links <- as.data.frame(links)

# 将source和target转换为节点索引
links$IDsource <- match(links$source, nodes$name) - 1  # networkD3的索引从0开始
links$IDtarget <- match(links$target, nodes$name) - 1

# 自定义颜色列表
customColors <- c("#ea544f","#eb6d73","#f8c6c4","#f18201","#f39b64","#f9c059",
                  "#fbd474","#00adaa","#63c0ab","#00afcb","#00b1a6","#83bfea","#bad1ee","#63c0ab","#ec8171","#81cfe1","#4cbba9","#7586ab","#f6b8a4",
                  "#a8b1ca","#b1dfd3")

# 创建颜色比例字符串
colourScale <- sprintf('d3.scaleOrdinal().range(["%s"])', 
                       paste(customColors, collapse = '", "'))

p <- sankeyNetwork(Links = links, 
                   Nodes = nodes, 
                   Source = "IDsource", 
                   Target = "IDtarget", 
                   Value = "value", 
                   NodeID = "name", 
                   units = "T", 
                   fontSize = 0, 
                   nodeWidth = 30,
                   height = 600,
                   width = 700,
                   nodePadding = 20,
                   iterations = 0,
                   colourScale = colourScale)
p 


p1 <- htmlwidgets::onRender(p, '
  function(el, x) {
    var svg = d3.select(el).select("svg");

    // 创建一个合法的ID, 防止名称中有空格或者-等特殊字符
    function createValidID(name) {
      if (!name) {
        return "unknown"; // 如果名称是null或undefined，返回一个默认值
      }
      // 将非字母数字字符替换为下划线，包括空格和-
      return name.replace(/[^a-zA-Z0-9-]/g, "_");
    }
    
    
    // 收集要删除的节点名称（NA）
    var nodesToRemove = [];
    svg.selectAll(".node").each(function(d) {
      if (!d.name || d.name === "NA") {
        nodesToRemove.push(d.name);
      }
    });

    // 删除标签为NA的节点及其直接相关的链接
    nodesToRemove.forEach(function(nodeName) {
      // 找到并删除与该节点相关的所有直接链接
      svg.selectAll(".link").filter(function(link) {
        return link.target.name === nodeName;
      }).remove();
      
      // 删除节点
      svg.selectAll(".node").filter(function(node) {
        return node.name === nodeName;
      }).remove();
    });

    
    // 创建渐变色
    svg.selectAll(".link").each(function(d) {

      // 使用 source.name 和 target.name 作为渐变ID的一部分
      var gradientID = "gradient-" + createValidID(d.source.name) + "-" + createValidID(d.target.name);

      // 创建渐变
      var gradient = svg.append("defs")
        .append("linearGradient")
        .attr("id", gradientID)
        .attr("gradientUnits", "userSpaceOnUse")
        .attr("x1", d.source.x + d.source.dx / 2)
        .attr("y1", d.source.y + d.source.dy / 2)
        .attr("x2", d.target.x + d.target.dx / 2)
        .attr("y2", d.target.y + d.target.dy / 2);

      // 起始颜色（源节点颜色）
      var sourceColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.source.name; }).select("rect").style("fill");
      var targetColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.target.name; }).select("rect").style("fill");

      gradient.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", sourceColor);

      // 终止颜色（目标节点颜色）
      gradient.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", targetColor);

      // 设置链接的颜色为渐变色
      d3.select(this).style("stroke", "url(#" + gradientID + ")");
    }); 
    
    // 修改节点矩形框的颜色
    svg.selectAll(".node rect")
      .style("stroke", "black") // 设置矩形框的边框颜色
      .style("stroke-width", "1.5px"); // 设置矩形框的边框粗细
  }
')


p1
saveNetwork(p1,"output/Fig.1/sankey/sankey_con.html")
# 保存   
webshot("output/Fig.1/sankey/sankey_con.html" , "output/Fig.1/sankey/sankey_con.svg")
webshot("output/Fig.1/sankey/sankey_con.html", "output/Fig.1/sankey/sankey_con.pdf")




############ sankey _ Sham ################
rm(list = ls())
library(tidyverse)
library(xlsx)
# 加载必要的包
library(networkD3)
library(tidyverse)
library(readxl)
library(htmlwidgets) 
library(webshot)
data_s <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_s_RPKM_percent.xls",data.table = F)
rownames(data_s) <- data_s$`#Taxonomy`
data_s <- data_s[,-1]
##### Il
df <- as.vector(NULL)
aa <- c("Sham_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Il")
  df <- c(df,bb)
}
data_s <- data_s[,df]

data_s$sum <- rowSums(data_s)
data_s <- subset(data_s,sum >0)
data_s <- cbind("sample" = rownames(data_s),data_s)
df_split <- data_s %>%
  separate(col = sample, into = c("d", "k", "p","c","o","f","g","s"), sep = ";", remove = FALSE)
length(unique(df_split$s)) # 3969
length(unique(df_split$g)) # 1318

fe <- df_split$s[order(df_split$sum, decreasing = TRUE)[1:20]]
data <- subset(df_split,s %in% fe)

data <- data[,c(5:9,16)]
data <- data[,c(3:6)]


data$f <- substr(data$f, 4,nchar(data$f))
data$g <- substr(data$g, 4,nchar(data$g))
data$s <- substr(data$s, 4,nchar(data$s))

# data <- data %>%
#   group_by(c, `o`,`f`,`g`,`s`) %>%
#   summarise(Count = n(), .groups = 'drop')
colnames(data)[4] <- "Count"


data <- as.data.frame(data)


f_order <- sort(unique(data$f), na.last = TRUE)
g_order <- sort(unique(data$g), na.last = TRUE)
s_order <- sort(unique(data$s), na.last = TRUE)
# 创建节点数据并按自定义顺序排序
nodes <- data.frame(name = c(
  f_order,
  g_order,
  s_order))

# 创建链接数据
links <- data %>%
  select(f, `g`, Count) %>%
  rename(source = f, target = `g`, value = Count) %>%
  bind_rows(
    data %>% select(`g`, `s`, Count) %>%
      rename(source = `g`, target = `s`, value = Count)
  ) %>%
  group_by(source, target) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  ungroup() 
links <- as.data.frame(links)

# 将source和target转换为节点索引
links$IDsource <- match(links$source, nodes$name) - 1  # networkD3的索引从0开始
links$IDtarget <- match(links$target, nodes$name) - 1

# 自定义颜色列表
customColors <- c("#ea544f","#eb6d73","#f8c6c4","#f18201","#f39b64","#f9c059",
                  "#fbd474","#00adaa","#63c0ab","#00afcb","#00b1a6","#83bfea","#bad1ee","#63c0ab","#ec8171","#81cfe1","#4cbba9","#7586ab","#f6b8a4",
                  "#a8b1ca","#b1dfd3")

# 创建颜色比例字符串
colourScale <- sprintf('d3.scaleOrdinal().range(["%s"])', 
                       paste(customColors, collapse = '", "'))

p <- sankeyNetwork(Links = links, 
                   Nodes = nodes, 
                   Source = "IDsource", 
                   Target = "IDtarget", 
                   Value = "value", 
                   NodeID = "name", 
                   units = "T", 
                   fontSize = 0, 
                   nodeWidth = 30,
                   height = 600,
                   width = 700,
                   nodePadding = 20,
                   iterations = 0,
                   colourScale = colourScale)
p 


p1 <- htmlwidgets::onRender(p, '
  function(el, x) {
    var svg = d3.select(el).select("svg");

    // 创建一个合法的ID, 防止名称中有空格或者-等特殊字符
    function createValidID(name) {
      if (!name) {
        return "unknown"; // 如果名称是null或undefined，返回一个默认值
      }
      // 将非字母数字字符替换为下划线，包括空格和-
      return name.replace(/[^a-zA-Z0-9-]/g, "_");
    }
    
    
    // 收集要删除的节点名称（NA）
    var nodesToRemove = [];
    svg.selectAll(".node").each(function(d) {
      if (!d.name || d.name === "NA") {
        nodesToRemove.push(d.name);
      }
    });

    // 删除标签为NA的节点及其直接相关的链接
    nodesToRemove.forEach(function(nodeName) {
      // 找到并删除与该节点相关的所有直接链接
      svg.selectAll(".link").filter(function(link) {
        return link.target.name === nodeName;
      }).remove();
      
      // 删除节点
      svg.selectAll(".node").filter(function(node) {
        return node.name === nodeName;
      }).remove();
    });

    
    // 创建渐变色
    svg.selectAll(".link").each(function(d) {

      // 使用 source.name 和 target.name 作为渐变ID的一部分
      var gradientID = "gradient-" + createValidID(d.source.name) + "-" + createValidID(d.target.name);

      // 创建渐变
      var gradient = svg.append("defs")
        .append("linearGradient")
        .attr("id", gradientID)
        .attr("gradientUnits", "userSpaceOnUse")
        .attr("x1", d.source.x + d.source.dx / 2)
        .attr("y1", d.source.y + d.source.dy / 2)
        .attr("x2", d.target.x + d.target.dx / 2)
        .attr("y2", d.target.y + d.target.dy / 2);

      // 起始颜色（源节点颜色）
      var sourceColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.source.name; }).select("rect").style("fill");
      var targetColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.target.name; }).select("rect").style("fill");

      gradient.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", sourceColor);

      // 终止颜色（目标节点颜色）
      gradient.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", targetColor);

      // 设置链接的颜色为渐变色
      d3.select(this).style("stroke", "url(#" + gradientID + ")");
    }); 
    
    // 修改节点矩形框的颜色
    svg.selectAll(".node rect")
      .style("stroke", "black") // 设置矩形框的边框颜色
      .style("stroke-width", "1.5px"); // 设置矩形框的边框粗细
  }
')


p1
saveNetwork(p1,"output/Fig.1/sankey/sankey_Sham.html")
# 保存   
webshot("output/Fig.1/sankey/sankey_Sham.html" , "output/Fig.1/sankey/sankey_Sham.svg")
webshot("output/Fig.1/sankey/sankey_Sham.html", "output/Fig.1/sankey/sankey_Sham.pdf")




############ sankey _ 0min ################
rm(list = ls())
library(tidyverse)
library(xlsx)
# 加载必要的包
library(networkD3)
library(tidyverse)
library(readxl)
library(htmlwidgets) 
library(webshot)

data_s <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_s_RPKM_percent.xls",data.table = F)
rownames(data_s) <- data_s$`#Taxonomy`
data_s <- data_s[,-1]
##### Il
df <- as.vector(NULL)
aa <- c("SCI_0min_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Il")
  df <- c(df,bb)
}
data_s <- data_s[,df]

data_s$sum <- rowSums(data_s)
data_s <- subset(data_s,sum >0)
data_s <- cbind("sample" = rownames(data_s),data_s)
df_split <- data_s %>%
  separate(col = sample, into = c("d", "k", "p","c","o","f","g","s"), sep = ";", remove = FALSE)
length(unique(df_split$s)) # 3969
length(unique(df_split$g)) # 1318

fe <- df_split$s[order(df_split$sum, decreasing = TRUE)[1:20]]
data <- subset(df_split,s %in% fe)

data <- data[,c(5:9,16)]
data <- data[,c(3:6)]


data$f <- substr(data$f, 4,nchar(data$f))
data$g <- substr(data$g, 4,nchar(data$g))
data$s <- substr(data$s, 4,nchar(data$s))

# data <- data %>%
#   group_by(c, `o`,`f`,`g`,`s`) %>%
#   summarise(Count = n(), .groups = 'drop')
colnames(data)[4] <- "Count"


data <- as.data.frame(data)


f_order <- sort(unique(data$f), na.last = TRUE)
g_order <- sort(unique(data$g), na.last = TRUE)
s_order <- sort(unique(data$s), na.last = TRUE)
# 创建节点数据并按自定义顺序排序
nodes <- data.frame(name = c(
  f_order,
  g_order,
  s_order))

# 创建链接数据
links <- data %>%
  select(f, `g`, Count) %>%
  rename(source = f, target = `g`, value = Count) %>%
  bind_rows(
    data %>% select(`g`, `s`, Count) %>%
      rename(source = `g`, target = `s`, value = Count)
  ) %>%
  group_by(source, target) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  ungroup() 
links <- as.data.frame(links)

# 将source和target转换为节点索引
links$IDsource <- match(links$source, nodes$name) - 1  # networkD3的索引从0开始
links$IDtarget <- match(links$target, nodes$name) - 1

# 自定义颜色列表
customColors <- c("#ea544f","#eb6d73","#f8c6c4","#f18201","#f39b64","#f9c059",
                  "#fbd474","#00adaa","#63c0ab","#00afcb","#00b1a6","#83bfea","#bad1ee","#63c0ab","#ec8171","#81cfe1","#4cbba9","#7586ab","#f6b8a4",
                  "#a8b1ca","#b1dfd3")

# 创建颜色比例字符串
colourScale <- sprintf('d3.scaleOrdinal().range(["%s"])', 
                       paste(customColors, collapse = '", "'))

p <- sankeyNetwork(Links = links, 
                   Nodes = nodes, 
                   Source = "IDsource", 
                   Target = "IDtarget", 
                   Value = "value", 
                   NodeID = "name", 
                   units = "T", 
                   fontSize = 0, 
                   nodeWidth = 30,
                   height = 600,
                   width = 700,
                   nodePadding = 20,
                   iterations = 0,
                   colourScale = colourScale)
p 


p1 <- htmlwidgets::onRender(p, '
  function(el, x) {
    var svg = d3.select(el).select("svg");

    // 创建一个合法的ID, 防止名称中有空格或者-等特殊字符
    function createValidID(name) {
      if (!name) {
        return "unknown"; // 如果名称是null或undefined，返回一个默认值
      }
      // 将非字母数字字符替换为下划线，包括空格和-
      return name.replace(/[^a-zA-Z0-9-]/g, "_");
    }
    
    
    // 收集要删除的节点名称（NA）
    var nodesToRemove = [];
    svg.selectAll(".node").each(function(d) {
      if (!d.name || d.name === "NA") {
        nodesToRemove.push(d.name);
      }
    });

    // 删除标签为NA的节点及其直接相关的链接
    nodesToRemove.forEach(function(nodeName) {
      // 找到并删除与该节点相关的所有直接链接
      svg.selectAll(".link").filter(function(link) {
        return link.target.name === nodeName;
      }).remove();
      
      // 删除节点
      svg.selectAll(".node").filter(function(node) {
        return node.name === nodeName;
      }).remove();
    });

    
    // 创建渐变色
    svg.selectAll(".link").each(function(d) {

      // 使用 source.name 和 target.name 作为渐变ID的一部分
      var gradientID = "gradient-" + createValidID(d.source.name) + "-" + createValidID(d.target.name);

      // 创建渐变
      var gradient = svg.append("defs")
        .append("linearGradient")
        .attr("id", gradientID)
        .attr("gradientUnits", "userSpaceOnUse")
        .attr("x1", d.source.x + d.source.dx / 2)
        .attr("y1", d.source.y + d.source.dy / 2)
        .attr("x2", d.target.x + d.target.dx / 2)
        .attr("y2", d.target.y + d.target.dy / 2);

      // 起始颜色（源节点颜色）
      var sourceColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.source.name; }).select("rect").style("fill");
      var targetColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.target.name; }).select("rect").style("fill");

      gradient.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", sourceColor);

      // 终止颜色（目标节点颜色）
      gradient.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", targetColor);

      // 设置链接的颜色为渐变色
      d3.select(this).style("stroke", "url(#" + gradientID + ")");
    }); 
    
    // 修改节点矩形框的颜色
    svg.selectAll(".node rect")
      .style("stroke", "black") // 设置矩形框的边框颜色
      .style("stroke-width", "1.5px"); // 设置矩形框的边框粗细
  }
')


p1
saveNetwork(p1,"output/Fig.1/sankey/sankey_SCI_0min.html")
# 保存   
webshot("output/Fig.1/sankey/sankey_SCI_0min.html" , "output/Fig.1/sankey/sankey_SCI_0min.svg")
webshot("output/Fig.1/sankey/sankey_SCI_0min.html", "output/Fig.1/sankey/sankey_SCI_0min.pdf")




############ sankey _ 12h ################
rm(list = ls())
library(tidyverse)
library(xlsx)
# 加载必要的包
library(networkD3)
library(tidyverse)
library(readxl)
library(htmlwidgets) 
library(webshot)


data_s <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_s_RPKM_percent.xls",data.table = F)
rownames(data_s) <- data_s$`#Taxonomy`
data_s <- data_s[,-1]
##### Il
df <- as.vector(NULL)
aa <- c("SCI_12h_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Il")
  df <- c(df,bb)
}
data_s <- data_s[,df]

data_s$sum <- rowSums(data_s)
data_s <- subset(data_s,sum >0)
data_s <- cbind("sample" = rownames(data_s),data_s)
df_split <- data_s %>%
  separate(col = sample, into = c("d", "k", "p","c","o","f","g","s"), sep = ";", remove = FALSE)
length(unique(df_split$s)) # 3969
length(unique(df_split$g)) # 1318

fe <- df_split$s[order(df_split$sum, decreasing = TRUE)[1:20]]
data <- subset(df_split,s %in% fe)

data <- data[,c(5:9,16)]
data <- data[,c(3:6)]


data$f <- substr(data$f, 4,nchar(data$f))
data$g <- substr(data$g, 4,nchar(data$g))
data$s <- substr(data$s, 4,nchar(data$s))

# data <- data %>%
#   group_by(c, `o`,`f`,`g`,`s`) %>%
#   summarise(Count = n(), .groups = 'drop')
colnames(data)[4] <- "Count"


data <- as.data.frame(data)


f_order <- sort(unique(data$f), na.last = TRUE)
g_order <- sort(unique(data$g), na.last = TRUE)
s_order <- sort(unique(data$s), na.last = TRUE)
# 创建节点数据并按自定义顺序排序
nodes <- data.frame(name = c(
  f_order,
  g_order,
  s_order))

# 创建链接数据
links <- data %>%
  select(f, `g`, Count) %>%
  rename(source = f, target = `g`, value = Count) %>%
  bind_rows(
    data %>% select(`g`, `s`, Count) %>%
      rename(source = `g`, target = `s`, value = Count)
  ) %>%
  group_by(source, target) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  ungroup() 
links <- as.data.frame(links)

# 将source和target转换为节点索引
links$IDsource <- match(links$source, nodes$name) - 1  # networkD3的索引从0开始
links$IDtarget <- match(links$target, nodes$name) - 1

# 自定义颜色列表
customColors <- c("#ea544f","#eb6d73","#f8c6c4","#f18201","#f39b64","#f9c059",
                  "#fbd474","#00adaa","#63c0ab","#00afcb","#00b1a6","#83bfea","#bad1ee","#63c0ab","#ec8171","#81cfe1","#4cbba9","#7586ab","#f6b8a4",
                  "#a8b1ca","#b1dfd3")

# 创建颜色比例字符串
colourScale <- sprintf('d3.scaleOrdinal().range(["%s"])', 
                       paste(customColors, collapse = '", "'))

p <- sankeyNetwork(Links = links, 
                   Nodes = nodes, 
                   Source = "IDsource", 
                   Target = "IDtarget", 
                   Value = "value", 
                   NodeID = "name", 
                   units = "T", 
                   fontSize = 0, 
                   nodeWidth = 30,
                   height = 600,
                   width = 700,
                   nodePadding = 20,
                   iterations = 0,
                   colourScale = colourScale)
p 


p1 <- htmlwidgets::onRender(p, '
  function(el, x) {
    var svg = d3.select(el).select("svg");

    // 创建一个合法的ID, 防止名称中有空格或者-等特殊字符
    function createValidID(name) {
      if (!name) {
        return "unknown"; // 如果名称是null或undefined，返回一个默认值
      }
      // 将非字母数字字符替换为下划线，包括空格和-
      return name.replace(/[^a-zA-Z0-9-]/g, "_");
    }
    
    
    // 收集要删除的节点名称（NA）
    var nodesToRemove = [];
    svg.selectAll(".node").each(function(d) {
      if (!d.name || d.name === "NA") {
        nodesToRemove.push(d.name);
      }
    });

    // 删除标签为NA的节点及其直接相关的链接
    nodesToRemove.forEach(function(nodeName) {
      // 找到并删除与该节点相关的所有直接链接
      svg.selectAll(".link").filter(function(link) {
        return link.target.name === nodeName;
      }).remove();
      
      // 删除节点
      svg.selectAll(".node").filter(function(node) {
        return node.name === nodeName;
      }).remove();
    });

    
    // 创建渐变色
    svg.selectAll(".link").each(function(d) {

      // 使用 source.name 和 target.name 作为渐变ID的一部分
      var gradientID = "gradient-" + createValidID(d.source.name) + "-" + createValidID(d.target.name);

      // 创建渐变
      var gradient = svg.append("defs")
        .append("linearGradient")
        .attr("id", gradientID)
        .attr("gradientUnits", "userSpaceOnUse")
        .attr("x1", d.source.x + d.source.dx / 2)
        .attr("y1", d.source.y + d.source.dy / 2)
        .attr("x2", d.target.x + d.target.dx / 2)
        .attr("y2", d.target.y + d.target.dy / 2);

      // 起始颜色（源节点颜色）
      var sourceColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.source.name; }).select("rect").style("fill");
      var targetColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.target.name; }).select("rect").style("fill");

      gradient.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", sourceColor);

      // 终止颜色（目标节点颜色）
      gradient.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", targetColor);

      // 设置链接的颜色为渐变色
      d3.select(this).style("stroke", "url(#" + gradientID + ")");
    }); 
    
    // 修改节点矩形框的颜色
    svg.selectAll(".node rect")
      .style("stroke", "black") // 设置矩形框的边框颜色
      .style("stroke-width", "1.5px"); // 设置矩形框的边框粗细
  }
')


p1
saveNetwork(p1,"output/Fig.1/sankey/sankey_SCI_12h.html")
# 保存   
webshot("output/Fig.1/sankey/sankey_SCI_12h.html" , "output/Fig.1/sankey/sankey_SCI_12h.svg")
webshot("output/Fig.1/sankey/sankey_SCI_12h.html", "output/Fig.1/sankey/sankey_SCI_12h.pdf")




############ sankey _ 5d ################
rm(list = ls())
library(tidyverse)
library(xlsx)
# 加载必要的包
library(networkD3)
library(tidyverse)
library(readxl)
library(htmlwidgets) 
library(webshot)


data_s <- data.table::fread("data/2_Browse/Abundance_NR_RPKM_相对丰度/tax_s_RPKM_percent.xls",data.table = F)
rownames(data_s) <- data_s$`#Taxonomy`
data_s <- data_s[,-1]
##### Il
df <- as.vector(NULL)
aa <- c("SCI_5d_")
for (i in 1:length(aa)) {
  bb <- paste0(paste0(aa[i],c(1:6)),"_Il")
  df <- c(df,bb)
}
data_s <- data_s[,df]

data_s$sum <- rowSums(data_s)
data_s <- subset(data_s,sum >0)
data_s <- cbind("sample" = rownames(data_s),data_s)
df_split <- data_s %>%
  separate(col = sample, into = c("d", "k", "p","c","o","f","g","s"), sep = ";", remove = FALSE)
length(unique(df_split$s)) # 3969
length(unique(df_split$g)) # 1318

fe <- df_split$s[order(df_split$sum, decreasing = TRUE)[1:20]]
data <- subset(df_split,s %in% fe)

data <- data[,c(5:9,16)]
data <- data[,c(3:6)]


data$f <- substr(data$f, 4,nchar(data$f))
data$g <- substr(data$g, 4,nchar(data$g))
data$s <- substr(data$s, 4,nchar(data$s))

# data <- data %>%
#   group_by(c, `o`,`f`,`g`,`s`) %>%
#   summarise(Count = n(), .groups = 'drop')
colnames(data)[4] <- "Count"


data <- as.data.frame(data)


f_order <- sort(unique(data$f), na.last = TRUE)
g_order <- sort(unique(data$g), na.last = TRUE)
s_order <- sort(unique(data$s), na.last = TRUE)
# 创建节点数据并按自定义顺序排序
nodes <- data.frame(name = c(
  f_order,
  g_order,
  s_order))

# 创建链接数据
links <- data %>%
  select(f, `g`, Count) %>%
  rename(source = f, target = `g`, value = Count) %>%
  bind_rows(
    data %>% select(`g`, `s`, Count) %>%
      rename(source = `g`, target = `s`, value = Count)
  ) %>%
  group_by(source, target) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  ungroup() 
links <- as.data.frame(links)

# 将source和target转换为节点索引
links$IDsource <- match(links$source, nodes$name) - 1  # networkD3的索引从0开始
links$IDtarget <- match(links$target, nodes$name) - 1

# 自定义颜色列表
customColors <- c("#ea544f","#eb6d73","#f8c6c4","#f18201","#f39b64","#f9c059",
                  "#fbd474","#00adaa","#63c0ab","#00afcb","#00b1a6","#83bfea","#bad1ee","#63c0ab","#ec8171","#81cfe1","#4cbba9","#7586ab","#f6b8a4",
                  "#a8b1ca","#b1dfd3")

# 创建颜色比例字符串
colourScale <- sprintf('d3.scaleOrdinal().range(["%s"])', 
                       paste(customColors, collapse = '", "'))

p <- sankeyNetwork(Links = links, 
                   Nodes = nodes, 
                   Source = "IDsource", 
                   Target = "IDtarget", 
                   Value = "value", 
                   NodeID = "name", 
                   units = "T", 
                   fontSize = 0, 
                   nodeWidth = 30,
                   height = 600,
                   width = 700,
                   nodePadding = 20,
                   iterations = 0,
                   colourScale = colourScale)
p 


p1 <- htmlwidgets::onRender(p, '
  function(el, x) {
    var svg = d3.select(el).select("svg");

    // 创建一个合法的ID, 防止名称中有空格或者-等特殊字符
    function createValidID(name) {
      if (!name) {
        return "unknown"; // 如果名称是null或undefined，返回一个默认值
      }
      // 将非字母数字字符替换为下划线，包括空格和-
      return name.replace(/[^a-zA-Z0-9-]/g, "_");
    }
    
    
    // 收集要删除的节点名称（NA）
    var nodesToRemove = [];
    svg.selectAll(".node").each(function(d) {
      if (!d.name || d.name === "NA") {
        nodesToRemove.push(d.name);
      }
    });

    // 删除标签为NA的节点及其直接相关的链接
    nodesToRemove.forEach(function(nodeName) {
      // 找到并删除与该节点相关的所有直接链接
      svg.selectAll(".link").filter(function(link) {
        return link.target.name === nodeName;
      }).remove();
      
      // 删除节点
      svg.selectAll(".node").filter(function(node) {
        return node.name === nodeName;
      }).remove();
    });

    
    // 创建渐变色
    svg.selectAll(".link").each(function(d) {

      // 使用 source.name 和 target.name 作为渐变ID的一部分
      var gradientID = "gradient-" + createValidID(d.source.name) + "-" + createValidID(d.target.name);

      // 创建渐变
      var gradient = svg.append("defs")
        .append("linearGradient")
        .attr("id", gradientID)
        .attr("gradientUnits", "userSpaceOnUse")
        .attr("x1", d.source.x + d.source.dx / 2)
        .attr("y1", d.source.y + d.source.dy / 2)
        .attr("x2", d.target.x + d.target.dx / 2)
        .attr("y2", d.target.y + d.target.dy / 2);

      // 起始颜色（源节点颜色）
      var sourceColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.source.name; }).select("rect").style("fill");
      var targetColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.target.name; }).select("rect").style("fill");

      gradient.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", sourceColor);

      // 终止颜色（目标节点颜色）
      gradient.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", targetColor);

      // 设置链接的颜色为渐变色
      d3.select(this).style("stroke", "url(#" + gradientID + ")");
    }); 
    
    // 修改节点矩形框的颜色
    svg.selectAll(".node rect")
      .style("stroke", "black") // 设置矩形框的边框颜色
      .style("stroke-width", "1.5px"); // 设置矩形框的边框粗细
  }
')


p1
saveNetwork(p1,"output/Fig.1/sankey/sankey_SCI_5d.html")
# 保存   
webshot("output/Fig.1/sankey/sankey_SCI_5d.html" , "output/Fig.1/sankey/sankey_SCI_5d.svg")
webshot("output/Fig.1/sankey/sankey_SCI_5d.html", "output/Fig.1/sankey/sankey_SCI_5d.pdf")

