#-------------------------------------------------------------------------------
#
# Plot the graphic for MME error vs iterations
#
#-------------------------------------------------------------------------------

#--------------------------------
# Head
#--------------------------------

# Load packages 
library("tidyverse")

#--------------------------------
# Parameters
#--------------------------------

# The folder containing results
results_folder = "results/all_lines"

#--------------------------------
# Process
#--------------------------------

# Load files 
iterations_df = read_csv(paste0(results_folder, "/iterations_df.csv"))
iterations_df = iterations_df %>%
  mutate(mean_error = (error_in + error_out)/2) %>%
  filter(it_algo %in% 2:15)
  
iterations_df %>%
  ggplot() +
  geom_line(aes(x=it_algo, y=mean_error), color="blue") +
  xlab("Iteration") +
  ylab("MME") 

ggsave(paste0(results_folder, "/MME_vs_iteration.pdf"), width = 15, height = 5)
