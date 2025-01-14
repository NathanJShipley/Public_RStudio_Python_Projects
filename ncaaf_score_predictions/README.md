# NCAA College Football Playoff Prediction

This project leverages machine learning techniques to predict the winner of the NCAA College Football Playoffs. The pipeline includes data preprocessing, feature engineering, model fitting, and stacked model prediction. The models used in this project include linear regression, ridge regression, random forest, gradient boosting machines (GBM), XGBoost, and a TensorFlow-based neural network. 

## Project Overview

The overall objective of this project is to predict the winner of the NCAA College Football Playoffs using historical data. The data was preprocessed and cleaned, and several models were trained to provide predictions. The final output is a stacked model that combines the predictions of all the individual models to make the most accurate prediction.

### Data Preprocessing

The preprocessing script begins by taking the data from feature engineering performed in R using the **fast CFB** package. This script:
- Splits the data into training and test sets
- Cleans up categorical data
- Scales numerical data for modeling

### Model Training and Evaluation

The project includes separate functionalized scripts to train and evaluate the following models:
- **Linear Regression**
- **Ridge Regression**
- **Random Forest**
- **Gradient Boosting Machines (GBM)**
- **XGBoost**
- **TensorFlow Neural Network**

These models are evaluated based on their performance in predicting the playoff outcomes, and the most promising models are included in the stacking process.

### Stacked Model

The final prediction is made using a **stacked model**, which combines the outputs of all the models mentioned above. The stacked model uses a linear regression approach to weigh the outputs of each individual model and make a final prediction based on those weighted outputs.

## Notable Insights

- After exploring multiple models, it was observed that **Random Forest** had the best fitting training data. However, when applied to the test set, the model seemed to be overfitted, and thus it was not used as the final model in the stacking process.
- The final stacked model combines the predictions of linear regression, ridge regression, random forest, GBM, XGBoost, and TensorFlow to produce the final prediction.

## Additional Files

The repository contains Jupyter notebook files for each script. These notebooks provide a more interactive environment for:
- Running the individual models
- Performing additional exploratory analysis
- Experimenting with different stacking methods

## Next Steps

Some important areas for future work include:
1. **Exploring additional algorithms**: Testing models like support vector machines (SVM), lightGBM, or ensemble methods might yield better results.
2. **Hyperparameter tuning**: More detailed exploration of hyperparameters for each model can potentially improve accuracy.
3. **Residual analysis**: A deeper inspection of the residuals from the stacked models can help identify trends and areas for improvement.
4. **Data Feature Engineering**: The current dataset might not be comprehensive enough. The initial feature engineering focused on predicting AP votes, but further exploration into additional features that influence playoff outcomes is needed. More holistic data, possibly incorporating advanced metrics such as player statistics, injuries, and strength of schedule, could enhance model predictions.

## Final Prediction

After completing the entire pipeline, the final prediction for the 2025 NCAA College Football Playoffs is that **Notre Dame** will emerge as the winner. This prediction was determined through the stacked model that combined the outputs of several models and selected the most accurate predictions.
