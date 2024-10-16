#===========================================
# Load Libraries                           #
#==========================================-
library(tidyverse) #has dplyr and ggplot2
library(ggpubr) #some more visualizations 
library(janitor) #Tabyl function
library(ggimage) #image ggplot
library(gganimate)

## Week point data
# Full data
fantasy_data <- read.csv("All Data.csv", header = TRUE)

# Week 11 data
week_11_data <- read.csv("Week 11 CSV.csv", header = TRUE)


## Graph data
# Full data
ggplot(fantasy_data, aes(x = Points_For, y = Points_Against)) + 
  geom_image(aes(image=image), size = .08) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "red") + 
  ggtitle("Points For and Against in 2022 Friendship League") + theme(plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 1200, y = 1210, angle = 58, label = "More Against", color = "red") + 
  annotate("text", x = 1210, y = 1200, angle = 58, label = "More For", color = "red") + 
  theme(aspect.ratio=1, text = element_text(size = 18)) + 
  xlab("Points For") + 
  ylab("Points Against")

animated_plot <- ggplot(fantasy_data, aes(x = Points_For, y = Points_Against)) + 
  geom_image(aes(image=image), size = .08) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "red") + 
  ggtitle("Points For and Against in 2022 Friendship League") + theme(plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 1200, y = 1210, angle = 58, label = "More Against", color = "red") + 
  annotate("text", x = 1210, y = 1200, angle = 58, label = "More For", color = "red") + 
  theme(aspect.ratio=1, text = element_text(size = 18)) + 
  xlab("Points For") + 
  ylab("Points Against")

animated_plot + transition_time(Week)

animated_plot_noannotate <- ggplot(fantasy_data, aes(x = Points_For, y = Points_Against)) + 
  geom_image(aes(image=image), size = .08) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "red") + 
  ggtitle("Points For and Against in 2022 Friendship League") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(aspect.ratio=1, text = element_text(size = 18)) + 
  xlab("Points For") + 
  ylab("Points Against")

animated_plot_noannotate + transition_time(Week)



# Standardize data
fantasy_data <- fantasy_data %>%
  group_by(Week) %>%
  mutate(std_points_for = scale(Points_For),
         std_points_against = scale(Points_Against)) %>%
  ungroup()


# plot using std
animated_plot_std <- ggplot(fantasy_data, aes(x = std_points_for, y = std_points_against)) + 
  geom_image(aes(image=image), size = .08) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "red") + 
  theme(aspect.ratio=1, text = element_text(size = 18))

animated_plot_std + transition_time(Week) + 
  labs(title = 'Week: {frame_time}', x = 'Points For', y = 'Points Against')






# Week 11 data
# Plot with points
ggplot(week_11_data, aes(x = Points_For, y = Points_Against, color = Wins)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "red") + 
  ggtitle("Points For and Against in 2022 Friendship League") + theme(plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 1200, y = 1210, angle = 58, label = "More Against", color = "red") + 
  annotate("text", x = 1210, y = 1200, angle = 58, label = "More For", color = "red") + 
  theme(aspect.ratio=1) + 
  xlab("Points For") + 
  ylab("Points Against")

# plot with images
ggplot(week_11_data, aes(x = Points_For, y = Points_Against)) + 
  geom_image(aes(image=image), size = .08) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "red") + 
  ggtitle("Points For and Against in 2022 Friendship League") + theme(plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 1200, y = 1210, angle = 58, label = "More Against", color = "red") + 
  annotate("text", x = 1210, y = 1200, angle = 58, label = "More For", color = "red") + 
  theme(aspect.ratio=1, text = element_text(size = 18)) + 
  xlab("Points For") + 
  ylab("Points Against")
  

