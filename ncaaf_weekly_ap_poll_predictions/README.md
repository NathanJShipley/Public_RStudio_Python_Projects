# College Football Play-by-Play Data: AP Poll Prediction

This repository contains code and scripts used to predict the **AP Poll weekly ranking votes** based on college football **play-by-play data**. The project was developed to explore and better understand how to work with football data and to attempt predicting where teams would rank the following Sunday after all games were played on Saturday.

The final predictions are based on team performance data from the 2024 season, and the repository includes both Python and R scripts for exploratory analysis, data processing, and prediction modeling.

## Table of Contents

1. [Project Overview](#1-project-overview)  
2. [Repository Structure](#2-repository-structure)  
3. [Data](#3-data)  
4. [Code Descriptions](#4-code-descriptions)  

---

### 1. Project Overview

The goal of this project was to predict the **AP Poll rankings** by analyzing weekly college football data. By using play-by-play data and generating weekly statistics for each team, we aimed to predict their rank for the following week. The project focuses on:
- **Predicting AP Poll Votes**: Using team-level statistics to estimate where teams would rank in the weekly AP Poll.
- **Learning the Data**: The primary goal was to better understand the structure of football data and how various factors affect rankings.

---

### 2. Repository Structure

This repository is organized into the following key folders and files:

- **`2024_predictions/`**: Contains the last predictions made at the end of the 2024 season.
- **`scripts/`**: Contains all the code used to process the data, analyze the results, and generate predictions.
  - **`exploratory_python/`**: Python scripts for exploratory data analysis and initial experimentation.
  - **`r_scripts/`**: R scripts for data processing, feature engineering, and prediction modeling (majority of the code).

---

### 3. Data

The data used for predictions is play-by-play data for college football games. This includes:
- **Team statistics**: Weekly performance data (e.g., yards gained, turnovers, etc.).
- **AP Poll Votes**: The weekly rankings of teams.

Data should be available in the same directory as the scripts or can be linked to external sources depending on how it's structured.

---

### 4. Code Descriptions

- **R Scripts**:
  - **Initial Script**: This script loads the data using the `fastcfb` package and begins the data processing pipeline. It’s responsible for reading in play-by-play data, performing basic checks, and preparing the data for analysis.
  - **Data Feature Engineering**: The majority of the work was done in the R scripts, where various features were created for team-based analysis. Some examples of feature engineering include:
    - Calculating cumulative points, wins, losses, and total yards for each team.
    - Generating other team-level statistics over the course of the season, such as average yards per game, turnovers, and more.
  - **Model Stacking**: The R scripts were used to experiment with different stacking methods using a variety of machine learning algorithms. The final model used a stacked random forest that incorporated multiple models:
    - **XGBoost**
    - **SVM (Support Vector Machine)**
    - **GBM (Gradient Boosting Machine)**
    - **Random Forest**
    - **GLMNet** (Elastic Net Regularization)
  
  The goal was to compare different algorithms and their performances in predicting the AP Poll votes. While the final stacked model showed strong results, it still has room for refinement in order to improve accuracy and generalizability.

- **Python Scripts (TensorFlow)**:
  - **Exploratory Data Analysis**: Python scripts for initial analysis of the football data, including feature engineering and data visualization.
  - **TensorFlow Models**: Deep learning models built using TensorFlow to predict AP Poll rankings, with a focus on leveraging neural networks to capture complex relationships in the data.
  - **Model Training and Evaluation**: Scripts for training the TensorFlow models and evaluating their performance using various metrics.
