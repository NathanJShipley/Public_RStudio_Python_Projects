# Kaggle Transplant Survival Prediction


## Overview
This R script was developed for a Kaggle competition focused on improving survival predictions for patients undergoing allogeneic Hematopoietic Cell Transplantation (HCT). The goal of the competition is to build predictive models that not only enhance accuracy but also ensure fairness across diverse demographic groups. This project explores data preprocessing, feature engineering, and machine learning techniques to address disparities in survival predictions related to socioeconomic status, race, and geography.

## Key Features

- **Data Preprocessing:** The script handles missing values, encodes categorical variables, and performs feature engineering techniques like Principal Component Analysis (PCA) to enhance model performance.  
- **Modeling:** Several machine learning models, including XGBoost, Gradient Boosting Machine (GBM), and Random Forest (Ranger), are trained to predict patient survival outcomes.  
- **Fairness Considerations:** The approach focuses on reducing bias in predictive modeling to ensure equitable survival predictions across diverse patient populations.  
- **Model Evaluation:** Performance metrics are used to compare models, assessing both predictive accuracy and fairness.  

## Workflow

1. **Data Cleaning:** Handling missing data, encoding categorical variables, and filtering relevant features.  
2. **Feature Engineering:** Applying dimensionality reduction (PCA) and other transformations to improve model performance.  
3. **Model Training:** Implementing and tuning multiple machine learning models to predict survival rates.  
4. **Predictions & Evaluation:** Generating predictions and evaluating models based on accuracy and fairness metrics.  

## Conclusion

This script provides a structured approach to tackling survival prediction challenges in healthcare, emphasizing both predictive performance and fairness. While the competition remains ongoing, this work demonstrates key data science techniques for improving medical outcome predictions and addressing biases in healthcare models.
