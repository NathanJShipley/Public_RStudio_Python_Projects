from xgboost import XGBRegressor

def trained_xgb_model(x_train, y_train):
     # set best params from earlier testing
     best_n_estimators_value = 100
     best_max_depth_value = 3
     best_learning_rate_value = 0.1
     best_subsample_value = 0.8
     best_colsample_bytree_value = 1.0

     # fit rf
     best_xgb_regressor = XGBRegressor(
          n_estimators=best_n_estimators_value,
          max_depth=best_max_depth_value,
          learning_rate=best_learning_rate_value,
          subsample=best_subsample_value,
          colsample_bytree=best_colsample_bytree_value
     )

     best_xgb_regressor.fit(x_train, y_train)

     return best_xgb_regressor
