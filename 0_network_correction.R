#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Data set lines correction
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Delete transports lines with a high difference between in and out passengers

# Set working directory path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read data
line_df = read.csv("/multilines_data/preprocessed_data/all_lines/line_df.csv")

# Sum passengers by line A and R
sum_table = data.frame(line=numeric(), a_in=numeric(), r_in=numeric(),
                       diff_a=numeric(), a_out=numeric(), r_out=numeric(),
                       diff_r=numeric())
for (i in unique(line_df$line_nbr)) {
  line = paste0(i)
  a_in = sum(ifelse(line_df$line_nbr == i &
                      line_df$direction == "A", line_df$passengers_in, 0))
  a_out = sum(ifelse(line_df$line_nbr == i & 
                       line_df$direction == "A", line_df$passengers_out, 0))
  r_in = sum(ifelse(line_df$line_nbr == i & 
                      line_df$direction == "R", line_df$passengers_in, 0))
  r_out = sum(ifelse(line_df$line_nbr == i & 
                       line_df$direction == "R", line_df$passengers_out, 0))
  
  diff_a = a_in/a_out
  diff_r = r_in/r_out
  
  new_row <- data.frame(line=line, a_in=a_in,a_out=a_out, diff_a=diff_a,
                        r_in=r_in, r_out=r_out, diff_r=diff_r)
  sum_table <- rbind(sum_table, new_row)
}


# Show lines which have more than 30% on difference in/out or only one direction
# or don't have way back
invalid_lines = sum_table[(sum_table$diff_a < 0.3 | sum_table$diff_a > 1.3) | 
                            (sum_table$diff_r < 0.7 | sum_table$diff_r > 1.3)| 
                            is.na(sum_table$diff_r) | is.na(sum_table$diff_a), ]

