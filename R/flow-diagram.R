devtools::install_github('rich-iannone/DiagrammeR')
library(DiagrammeR)
library(dplyr)
ndf = create_node_df(n = 5, label = TRUE)
ndf$label = c("OSM", "PCT", "MasterMap", "Road Network", "Interventions")
ndf$shape = "square"

edf = create_edge_df(
  from = c(1, 2, 3, 4),
  to = c(4, 4, 4, 5)
  )
g = create_graph(nodes_df = ndf, edges_df = edf, graph_name = "cyipt", directed = TRUE)
render_graph(g)

mermaid("
graph LR
A(OSM)-->D[Road Network]
B[PCT]-->D[Road Network]
C[MasterMap]-->D[Road Network]
")
