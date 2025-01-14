# Spotify Listening Data Analysis

This project processes the listening data output from Spotify, analyzes it, and generates visualizations to give insights into a user's listening habits over time. Using a Python script, the raw JSON data from Spotify is transformed into a structured CSV format, which is then analyzed and visualized in R. The project includes various charts and graphs that highlight trends, including the user's most listened-to artists, albums, songs, and more.

## Project Overview

The goal of this project is to explore and visualize a user's listening history on Spotify. The process begins with extracting the raw JSON data from Spotify's listening history API. The Python script processes and consolidates this data into a single CSV file, which is then read into R for detailed analysis.

### Key Features:
- **Data Transformation:** The Python script takes the raw JSON data from Spotify, processes it, and turns it into a CSV file, making it easy to analyze in R.
- **Visualizations:** Various charts and graphs are generated to understand trends in listening behavior, including:
    - Top 50 most listened-to artists, albums, and songs.
    - Yearly analysis of the top 10 artists by year.
    - Monthly trends of the top artists across all years.
    - Weekly listening hours per year.
    - Daily listening hours for a specific year.

### Data Analysis & Visualizations:
The key analysis features include:
- **Top 50 Artists, Albums, and Songs:** A chart displaying the 50 most listened-to artists, albums, and songs based on data from all available time periods.
- **Top Artists by Year:** A chart showing the top 10 artists for each year in the dataset, allowing the user to see shifts in musical preferences over time.
- **Top Artists by Month:** A chart that shows which artists were listened to the most across each month of each year, providing a deeper insight into musical trends.
- **Total Hours per Year by Week:** A breakdown of listening hours by week for each year, showing patterns of listening intensity.
- **Daily Hours in a Single Year:** A graph that visualizes daily listening hours for one specific year, offering a detailed look at daily listening behavior.

### Next Steps:
- **Enhance the Analysis:** Further exploration into other variables such as listening time of day, playlists, or genre breakdowns.
- **Longer-Term Trends:** Analyze longer-term shifts in listening preferences by extending the dataset and incorporating more years of listening history.
- 
