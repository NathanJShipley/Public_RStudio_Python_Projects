# Fantasy Football Data Visualization

This R script is designed to analyze and visualize fantasy football data over multiple weeks. It includes both static and animated visualizations that explore the relationship between points scored and points against for different teams in the 2022 Friendship League. The script also includes standardization of data for easier comparison across weeks.

## Key Features

- **Data Loading**: The script loads two datasets, one containing full data for all weeks and the other specific to Week 11.
- **Visualization**: 
  - A scatter plot shows the relationship between "Points For" and "Points Against" using both static images and dynamic animations. 
  - The plots also use various visual elements like `geom_image` to display team logos and `geom_abline` to draw reference lines.
- **Animated Plots**: The script uses `gganimate` to create animations that show how teams' points evolve over the weeks.
- **Standardization**: The script standardizes the "Points For" and "Points Against" data, creating a better comparison between teams across weeks.
- **Plot Customization**: Several variations of the plots are created, some including annotations and others focusing on animated transitions between weeks.

## Workflow

1. **Load Libraries**: Essential libraries for data manipulation (`tidyverse`), visualizations (`ggplot2`, `ggpubr`), and animations (`gganimate`) are loaded.
2. **Data Loading**: The full dataset (`fantasy_data`) and the Week 11 data (`week_11_data`) are imported from CSV files.
3. **Plotting**: 
   - A static plot of "Points For" vs "Points Against" is created using team logos as markers.
   - Animated versions of the plot are generated to visualize the changes across weeks.
4. **Standardization**: The points data is standardized (z-scores) for better comparison across different weeks.
5. **Week 11 Specific Analysis**: A separate visualization is created for the Week 11 data to show team performance using points and images.

## Conclusion

This script provides an interactive and visual approach to analyzing fantasy football performance over multiple weeks. Through animated plots and standardization of data, it effectively illustrates how teams compare in terms of their offensive and defensive performances across the season.
