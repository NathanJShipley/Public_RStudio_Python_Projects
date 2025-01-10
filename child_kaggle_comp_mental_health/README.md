# Kaggle Mental Health Model

This R script was designed for a Kaggle competition focused on predicting mental health outcomes. The project explores data cleaning, feature engineering, and machine learning techniques to develop predictive models. The final model achieved a relatively low score and was eventually abandoned, but the script showcases a series of steps commonly used in data science competitions.

## Key Features

- **Data Preprocessing**: The script cleans and transforms the data, handling missing values, converting categorical variables, and performing feature engineering like Principal Component Analysis (PCA).
- **Modeling**: Multiple machine learning models, including XGBoost, Gradient Boosting Machine (GBM), and Random Forest (Ranger), are trained to predict the target variable (`sii`).
- **Model Evaluation**: The models make predictions, which are stored for further evaluation.

## Workflow

1. **Data Cleaning**: Missing values are imputed, unnecessary columns are dropped, and categorical variables are converted to factors.
2. **Feature Engineering**: Dimensionality reduction techniques like PCA are applied to relevant features to enhance the models.
3. **Model Training**: Several machine learning models are trained, each using specific hyperparameters and evaluated based on their predictive performance.
4. **Predictions**: Predictions from all models are generated and stored for comparison.

## Conclusion

The script provides a comprehensive approach to tackling a Kaggle competition, with an emphasis on data cleaning, preprocessing, and testing different machine learning algorithms. While the final score was not high, the methodology demonstrates a solid workflow for data science tasks like feature engineering and model evaluation.
