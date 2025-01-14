# Racial Ethnicity Dot Map Using ACS Census Data

This repository contains an R script that maps out two counties in the United States—Dane County, WI and Johnson County, KS—using the American Community Survey (ACS) data. The script visualizes the distribution of racial and ethnic populations using dot maps. It uses the `tidycensus` package to retrieve the ACS data, the `ggplot2` and `sf` packages for visualization and mapping, and the `viridis` color palette for distinct color representation.

## Project Structure

The repository contains a single R script which does the following:
- Loads necessary libraries.
- Retrieves decennial census data for each county.
- Creates dot maps representing racial and ethnic distributions.

### Script Overview:
- **Data Retrieval**: The script uses the `tidycensus` library to access U.S. Census Bureau data via their API. It pulls demographic information for block groups within Dane County, WI and Johnson County, KS from the 2020 census.
- **Data Processing**: The data is processed to adjust for population size and combined for 'Other' race/ethnicity categories.
- **Mapping**: Dot maps are created for both counties, where each dot represents a person, and the color of the dot indicates a racial or ethnic group.
  
The script then outputs these maps as .jpg files, one for each county.
