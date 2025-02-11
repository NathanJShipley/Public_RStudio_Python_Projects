# import packages
import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV
import seaborn as sns
import matplotlib.pyplot as plt
import lightgbm as lgb

# read in train and test data
train_df = pd.read_csv(r"E:\github_repos\PRIVATE\Private_Active_Projects\post_HCT_survival_analysis\data\train.csv")
test_df = pd.read_csv(r"E:\github_repos\PRIVATE\Private_Active_Projects\post_HCT_survival_analysis\data\test.csv")

# First thing for this analysis, lets drop any columns from train not in test, easier for now
main_ivs = ['efs','efs_time']  # Replace with your actual column name
columns_to_drop_from_train = [col for col in train_df.columns if col not in test_df.columns and col not in main_ivs]
train_df = train_df.drop(columns_to_drop_from_train)

train_df.info()
train_df.head()
train_df.describe()
train_df.dtypes.value_counts()
print(train_df.isnull().mean() * 100)

missing_percentage = train_df.isnull().mean() * 100
train_df_filtered = train_df.loc[:, missing_percentage < 20]
test_df_filtered = test_df[train_df_filtered.columns.intersection(test_df.columns)] # grab variables shared in common with the filtered df

# Okay, lets set up a way to split numeric from categorical
numeric_columns = train_df_filtered.select_dtypes(include=['int64', 'float64']).columns
categorical_columns = train_df_filtered.select_dtypes(include=['object', 'category']).columns




corr = train_df_filtered[numeric_columns].corr()

# Create a heatmap
plt.figure(figsize=(15, 12))  # Adjust the size as needed
sns.heatmap(corr, annot=True, cmap='coolwarm', fmt=".2f")
plt.title("Correlation Matrix")
plt.show()


# Pairplot
sns.pairplot(train_df_filtered[numeric_columns], diag_kind="kde", kind="scatter", corner=True)


# before preprocessing, drop any columns that wont be used at all and sort into x and y
# Going to drop these columns from the data
columns_to_drop = ['efs','efs_time','cmv_status'] # Should go back and add the cmv status

# Dynamically drop columns if they exist
x_train = train_df_filtered.drop(columns=[col for col in columns_to_drop if col in train_df.columns])
y_train = train_df_filtered['efs']

x_test = test_df_filtered.drop(columns=[col for col in columns_to_drop if col in test_df.columns])
#y_test = test_df_filtered['efs'] # won't exist for a true test dataset


# now some preprocessing, lets first fill in missing data. do small imputation with just mean
# get new numeric columns for the new train data
numeric_train_cols = x_train.select_dtypes(include=['int64', 'float64']).columns

# Will apply same mean impute based on train, apply to test
for col in numeric_train_cols:
    mean_value = x_train[col].mean()  # Calculate mean from train_df
    x_train[col] = x_train[col].fillna(mean_value)  # Impute in train_df
    x_test[col] = x_test[col].fillna(mean_value)  # Impute in test_df with the same mean


# next, standardize numeric values
scaler = StandardScaler()
x_train[numeric_train_cols] = scaler.fit_transform(x_train[numeric_train_cols])
x_test[numeric_train_cols] = scaler.transform(x_test[numeric_train_cols])

# last, lets dummy code the categorical variables and build up the final train and test data
categorical_train_cols = x_train.select_dtypes(include=['object', 'category']).columns

x_train_encoded = pd.get_dummies(x_train, columns=categorical_train_cols, drop_first=True)
x_test_encoded = pd.get_dummies(x_test, columns=categorical_train_cols, drop_first=True)

# Check if any cat coded variables are missing from test, if so, add them with 0
missing_dummy_cols = set(x_train_encoded.columns) - set(x_test_encoded.columns)

for cols in missing_dummy_cols:
    x_test_encoded[cols] = 0

# Re-align columns in x_test_encoded to match the order of x_train_encoded
x_test_encoded = x_test_encoded[x_train_encoded.columns]

# Lastly, lets clean up column names
x_train_encoded.columns = x_train_encoded.columns.str.replace(r'[^a-zA-Z0-9]', '_', regex=True)
x_test_encoded.columns = x_test_encoded.columns.str.replace(r'[^a-zA-Z0-9]', '_', regex=True)


# Do one final check to make sure data align
print(x_train_encoded.shape)
print(x_test_encoded.shape)

# Lets make a model evaluation function
def model_evaluation(y_train_test, y_pred):
    # fit metrics
    r2 = r2_score(y_train_test, y_pred)
    mse = mean_squared_error(y_train_test, y_pred)
    mae = mean_absolute_error(y_train_test, y_pred)

    # print results
    print(f"R-squared: {r2:.3f}")
    print(f"Mean Squared Error (MSE): {mse:.3f}")
    print(f"Mean Absolute Error (MAE): {mae:.3f}")


# set inital regression model
lr_model = LinearRegression()

# fit model
lr_model.fit(x_train_encoded, y_train)

# evaluate fit
y_lm_pred_train = lr_model.predict(x_train_encoded)
model_evaluation(y_train, y_lm_pred_train)


# Step 1: Define hyperparameter grid for LightGBM
num_leaves_values = [25, 50, 100]  # Number of leaves in one tree
learning_rate_values = [0.01, 0.05, 0.1]  # Learning rate
n_estimators_values = [50, 100, 200]  # Number of boosting iterations (trees)

# Step 2: Create the parameter grid
lgb_param_grid = {
    'num_leaves': num_leaves_values,
    'learning_rate': learning_rate_values,
    'n_estimators': n_estimators_values
}

# Step 3: Set up regressor
lgb_regressor = lgb.LGBMRegressor()

# Step 4: Build the grid search model
lgb_grid_search = GridSearchCV(
    estimator=lgb_regressor,
    param_grid=lgb_param_grid,
    cv=5,  # 5-fold cross-validation
    scoring='neg_mean_squared_error',  # Use negative MSE as the scoring metric
    n_jobs=-3,  # Use all available cores
    verbose=1  # Output progress
)

# Step 5: Fit the grid search to find the best parameters
lgb_grid_search.fit(x_train_encoded, y_train)

# Extract and plot results from GridSearchCV for LightGBM
lgb_results = pd.DataFrame(lgb_grid_search.cv_results_)

# Plot performance for different num_leaves grouped by n_estimators
plt.figure(figsize=(10, 6))

for num_leaves in num_leaves_values:  # num_leaves: [31, 50, 100]
    subset = lgb_results[lgb_results['param_num_leaves'] == num_leaves]
    plt.plot(
        subset['param_n_estimators'],
        -subset['mean_test_score'],  # Convert Negative MSE to Positive MSE
        marker='o',
        label=f'Num Leaves: {num_leaves}'
    )

# Customize the plot
plt.xlabel('Number of Estimators (n_estimators)')
plt.ylabel('Mean Test Score (Positive MSE)')
plt.title('LightGBM: Performance vs. Number of Estimators')
plt.legend(title='Num Leaves')
plt.grid()
plt.show()

# Get the best parameters
best_lgb_num_leaves_value = lgb_grid_search.best_params_['num_leaves']
best_lgb_learning_rate_value = lgb_grid_search.best_params_['learning_rate']
best_lgb_n_estimators_value = lgb_grid_search.best_params_['n_estimators']

print(f"Best num_leaves: {best_lgb_num_leaves_value}")
print(f"Best learning_rate: {best_lgb_learning_rate_value}")
print(f"Best n_estimators: {best_lgb_n_estimators_value}")

# Step 8: Fit the final model using the best parameters from grid search
best_lgb_regressor = lgb.LGBMRegressor(
    num_leaves=best_lgb_num_leaves_value,
    learning_rate=best_lgb_learning_rate_value,
    n_estimators=best_lgb_n_estimators_value
)

best_lgb_regressor.fit(x_train_encoded, y_train)

# Model evaluation
y_lgb_pred_train = best_lgb_regressor.predict(x_train_encoded)

# evaluate model
model_evaluation(y_train, y_lgb_pred_train)



# Lets do a random forest now, seems best option for this question
# Step 1: Define hyperparameter grid
n_estimator_values = [50, 100, 200]  # Number of trees in the forest
max_depth_values = [None, 10, 20, 30]  # Maximum depth of the tree
max_features_values = ['auto', 'sqrt', 'log2']  # Number of features to consider at each split

# Step 2: Create the parameter grid
rf_param_grid = {
    'n_estimators': n_estimator_values,
    'max_depth': max_depth_values,
    'max_features': max_features_values
}

# Step 3: set up regressor
rf_regressor = RandomForestRegressor()

# Step 4: build the grid search model
rf_grid_search = GridSearchCV(
    estimator=rf_regressor,
    param_grid=rf_param_grid,
    cv=5,  # 5-fold cross-validation
    scoring='neg_mean_squared_error',  # Use negative MSE as the scoring metric
    n_jobs=-3,  # Use all available cores minus 2
    verbose=1  # Output progress
)

# fit the grid search
rf_grid_search.fit(x_train_encoded, y_train)

# Extract and plot results from GridSearchCV
rf_results = pd.DataFrame(rf_grid_search.cv_results_)

# Plot performance for different max_features grouped by n_estimators
plt.figure(figsize=(10, 6))

for max_feature in max_features_values:  # max_features: ['auto', 'sqrt', 'log2']
    subset = rf_results[rf_results['param_max_features'] == max_feature]
    plt.plot(
        subset['param_n_estimators'],
        -subset['mean_test_score'],  # Convert Negative MSE to Positive MSE
        marker='o',
        label=f'Max Features: {max_feature}'
    )

# Customize the plot
plt.xlabel('Number of Estimators (n_estimators)')
plt.ylabel('Mean Test Score (Positive MSE)')
plt.title('Random Forest Regressor: Performance vs. Number of Estimators')
plt.legend(title='Max Features')
plt.grid()
plt.show()

# Get the best parameters
best_n_estimator_value = rf_grid_search.best_params_['n_estimators']
best_max_depth_value = rf_grid_search.best_params_['max_depth']
best_max_features_value = rf_grid_search.best_params_['max_features']

# Print the best values
print(f"Best n_estimators: {best_n_estimator_value}")
print(f"Best max_depth: {best_max_depth_value}")
print(f"Best max_features: {best_max_features_value}")

# fit final model using the best param
best_rf_regressor = RandomForestRegressor(
    n_estimators=best_n_estimator_value,
    max_depth=best_max_depth_value,
    max_features=best_max_features_value
)

best_rf_regressor.fit(x_train_encoded, y_train)

# Model evaluation
y_rf_pred_train = best_rf_regressor.predict(x_train_encoded)

# evaluate model
model_evaluation(y_train, y_rf_pred_train)