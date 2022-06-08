library(readr)
A6 <- read_table2("dump/A6.csv", col_names = FALSE)

table_dif <- A6[,-1]-fig_a6_dat_wide[,-c(1,2)]

table_dif_perc <- table_dif/A6[,-1]


table_dif_long <- pivot_longer(data = tibble(row = 1:nrow(table_dif), table_dif),
                               cols = !row,
                               values_to = "value",
                               names_to = "cell")


A6_dif <- ggplot(table_dif_long, aes(x = factor(cell, levels = paste0("X",2:41)), 
                           y = row)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = value, label = round(value, digits = 3)), size = 2) + # write the values
  scale_fill_gradient2(low = "darkred", 
                       mid = "white", 
                       high = "midnightblue", 
                       midpoint = 0) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, 
                                   hjust = 1,
                                   vjust=1, 
                                   size = 10, 
                                   face = "bold"),
        plot.title = element_text(size=20, face="bold"),
        axis.text.y = element_text(size = 10, face = "bold")) + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_reverse()+
  labs(fill="Absolute Diff.")



# Table A7 ----------------------------------------------------------------

A7 <- read_table2("dump/A7.csv", col_names = FALSE)
A7 <- A7[,-42]

table_dif7 <- A7[,-1]-fig_a7_dat_wide[,-c(1,2)]


table_dif_long7 <- pivot_longer(data = tibble(row = 1:nrow(table_dif7), table_dif7),
                               cols = !row,
                               values_to = "value",
                               names_to = "cell")


A7_dif <- ggplot(table_dif_long7, aes(x = factor(cell, levels = paste0("X",2:41)), 
                                     y = row)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = value, label = round(value, digits = 3)), size = 2) + # write the values
  scale_fill_gradient2(low = "darkred", 
                       mid = "white", 
                       high = "midnightblue", 
                       midpoint = 0) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_reverse()+
  labs(fill="Absolute Diff.")
