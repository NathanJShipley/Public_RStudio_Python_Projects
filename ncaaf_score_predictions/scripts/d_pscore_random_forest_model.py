from sklearn.ensemble import RandomForestRegressor

def trained_rf_model(x_train, y_train):
     # set best params from earlier testing
     best_n_estimator_value = 200
     best_max_depth_value = None
     best_max_features_value = 'sqrt'

     # fit rf
     best_rf_regressor = RandomForestRegressor(
          n_estimators=best_n_estimator_value,
          max_depth=best_max_depth_value,
          max_features=best_max_features_value
     )
     best_rf_regressor.fit(x_train, y_train)

     return best_rf_regressor
