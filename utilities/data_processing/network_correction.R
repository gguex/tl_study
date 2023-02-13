#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Data set lines correction
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Delete transports lines with a high difference between in and out passengers

# Read data
line_df = read.csv("multilines_data/formatted_data/all_lines_old/bus_df.csv")

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
  diff_tot_a = round((a_in - a_out) / (a_in + a_out + 1e-40), 4)
  diff_tot_r = round((r_in - r_out) / (r_in + r_out + 1e-40), 4)
  
  new_row <- data.frame(line=line, a_in=a_in,a_out=a_out, diff_a=diff_a,
                        r_in=r_in, r_out=r_out, diff_r=diff_r, 
                        diff_tot_a=diff_tot_a, diff_tot_r=diff_tot_r)
  sum_table <- rbind(sum_table, new_row)
}


# Show lines which have more than 30% on difference in/out or only one direction
# or don't have way back
invalid_lines = sum_table[(sum_table$diff_a < 0.3 | sum_table$diff_a > 1.3) | 
                            (sum_table$diff_r < 0.7 | sum_table$diff_r > 1.3)| 
                            is.na(sum_table$diff_r) | is.na(sum_table$diff_a), ]


threshold = 0.15
min_count = 10000
invalid_lines_2 = sum_table[abs(sum_table$diff_tot_a) > threshold | 
                              abs(sum_table$diff_tot_r) > threshold | 
                              (((sum_table$a_in+sum_table$a_out)/2 < min_count) & 
                               ((sum_table$r_in+sum_table$r_out)/2 < min_count)), ]


