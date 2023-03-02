#-------------------------------------------------------------------------------
#
# Preprocess data and save it
#
#-------------------------------------------------------------------------------

#--------------------------------
# Head
#--------------------------------

#------ Set working directory path
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("local_functions.R")

#--------------------------------
# Parameters
#--------------------------------

# Result folder
result_folder = "results/toys_graphs"

# Choose number of lines
n_lines_vec = c(2, 3, 4)
seed_vec = c(0, 2, 11)
theta_vec = c(0, -0.08,-0.051)

# Choose a seed

#--------------------------------
# Code
#--------------------------------

# --- loop
for(i in 1:length(n_lines_vec)){
  
  n_lines = n_lines_vec[i]
  n_stops = n_lines + 1
  set.seed(seed_vec[i])
  theta = theta_vec[i]
  
  # --- Network creation
  
  # Create the network
  network_data = network_prop(n_lines, n_stops)
  adj = network_data$adj
  
  # Stop names 
  letters = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
  line_letters = letters[1:(2*n_lines)]
  stop_numbers = 1:n_stops
  stop_names = paste0(rep(line_letters, each=n_stops), 
                      rep(stop_numbers, 2*n_lines))
  colnames(adj) = stop_names
  rownames(adj) = stop_names
  
  # Make the graph
  g = graph_from_adjacency_matrix(adj)
  
  # Make the colors
  new_palette = palette(rainbow(n_lines))
  stop_colors = rep(new_palette, each=2*n_stops)
  
  # Make the colors
  new_palette = palette(rainbow(n_lines))
  stop_colors = rep(new_palette, each=2*n_stops)
  
  # Layout
  if(n_lines == 2){
    layout = matrix(c(1, 2,
                      2, 2,
                      2, 1,
                      3, 1,
                      3, 2,
                      4, 2,
                      4, 3,
                      3, 3,
                      3, 4,
                      2, 4,
                      2, 3,
                      1, 3), 12, 2, byrow = T)
  } else {
    layout = layout_nicely(g, dim=2)
    rot_mat = matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
    layout = layout %*% rot_mat
  }
  
  # Plot
  pdf(paste0(result_folder, paste0("/toy_", n_lines,"_display.pdf")))
  par(mar=c(0,0,3,0)+.1)
  plot(g, layout=layout, vertex.color=stop_colors, edge.width=1,
       edge.arrow.size=0.7, edge.arrow.width=1.3, edge.color="black", 
       vertex.size=22, vertex.label.cex=2.5)
  title(paste0("p=", n_lines), cex.main=3)
  dev.off()
}