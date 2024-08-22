library(ggplot2)

# 创建背景图像
p_background <- ggplot() +
  theme_void() +
  theme(plot.background = element_rect(fill = "lightblue", color = NA),
        plot.margin = margin(0, 0, 0, 0)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "lightblue")

# 保存背景图像为 PNG
ggsave("background.png", plot = p_background, width = 12, height = 8, dpi = 300)





library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# 创建主要图表
graph <- grViz("
digraph flowchart {
  # 图表整体设置
  graph [bgcolor='#f0f5f9', fontname='Helvetica', layout=dot, rankdir=TB, nodesep=0.5, ranksep=0.7]
  
  # 节点样式
  node [shape=rectangle, style='filled,rounded', color='#2c3e50', penwidth=1.5, fontcolor='#2c3e50']
  
  # 蓝色区域节点（数据准备）
  node [fillcolor='#a8d8ea']
  LPD [label='Living Planet Dataset (LPD)']
  SLD [label='Species Lambda Dataset']
  IVS [label='I Value Sequences']
  GATS [label='Global Abundance\nTime Series']
  
  # 黄色区域节点（物种多样性分析）
  node [fillcolor='#fff7d1']
  WLPI [label='Weighted LPI by Abundance']
  HN [label='Hill Numbers\n(different q values)']
  SEA [label='Species Evenness Sensitivity\nAnalysis']
  REI [label='Relative Evenness Indexes']
  PCA [label='Principal Component\nAnalysis']
  
  # 连接线样式
  edge [color='#34495e', penwidth=1.2]
  
  # 连接
  LPD -> SLD -> IVS -> GATS
  GATS -> WLPI
  GATS -> HN
  HN -> SEA
  HN -> REI -> PCA
  
  # 节点层次结构
  {rank=same; LPD; SLD}
  {rank=same; IVS; GATS}
  {rank=same; WLPI; HN}
  {rank=same; SEA; REI}

  # 添加板块标题
  node [shape=plaintext, fontname='Helvetica', fontsize=36, color=black, width=0, height=0]
  DataPrepTitle [label='Data Preparation' shape=plaintext style=filled fillcolor=lightblue]
  SpeciesDiversityTitle [label='Species Diversity Analysis' shape=plaintext style=filled fillcolor=lightyellow]
  
  # 定义板块区域
  subgraph cluster_data_prep {
    color = lightblue
    DataPrepTitle
    LPD
    SLD
    IVS
    GATS
  }

  subgraph cluster_species_div {
    color = lightyellow
    SpeciesDiversityTitle
    WLPI
    HN
    SEA
    REI
    PCA
  }
}
")

# 导出为 SVG
svg_code <- export_svg(graph)
writeLines(svg_code, "flowchart.svg")

# 将 SVG 转换为 PNG
rsvg_png("flowchart.svg", "flowchart.png", width = 3600, height = 2400)




library(png)
library(grid)

# 读取背景图像
background_img <- readPNG("background.png")

# 读取主要图表图像
chart_img <- readPNG("flowchart.png")

# 创建 PNG 设备
png("final_flowchart.png", width = 3600, height = 2400, res = 300)

# 绘制背景图像
grid.raster(background_img, width = unit(1, "npc"), height = unit(1, "npc"))

# 绘制主要图表图像
grid.raster(chart_img, x = 0.5, y = 0.5, width = 0.9, height = 0.9)

# 关闭设备
dev.off()
