# Election 2020 Maps

This R script provides an analysis of the 2020 U.S. presidential election data, creating thematic maps to visualize the election results by county. It integrates census data, processes election vote percentages, and produces color-coded maps to showcase voting differences across counties.

## Features

- Retrieves U.S. Census data for counties using the `tidycensus` package.
- Loads and processes election data for the 2020 election from a CSV file.
- Joins election data with spatial data.
- Creates a map visualizing the voting differences between Democratic and Republican candidates across counties.
- Outputs maps as PNG files and provides centroid-based visualizations.
