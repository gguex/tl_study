library(igraph)

adj = matrix(c(0, 1, 0, 0, 0, 0,
               0, 0, 1, 0, 1, 0,
               0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 1, 0,
               0, 1, 0, 0, 0, 1,
               0, 0, 0, 0, 0, 0), 6, 6, byrow=T)

layout = matrix(c(0, 0, 
                  1, 0, 
                  2, 0, 
                  2, 1, 
                  1, 1, 
                  0, 1), 6, 2, byrow=T)

g = graph_from_adjacency_matrix(adj)

png("tuto_network.png")
par(mar=c(0,0,3,0)+.1, bg=NA)
plot(g, layout=layout.norm(layout), vertex.size=50, vertex.label.cex=6,
     edge.color=c("black", "black", "red", 
                         "black", "red", "black"), 
                         edge.label=c("", "", "", 
                                      "", "transfers", ""),
     edge.label.x=rep(-0.6, 6), edge.label.y=rep(0, 6), edge.label.cex=4,
     edge.label.color="red")
dev.off()
