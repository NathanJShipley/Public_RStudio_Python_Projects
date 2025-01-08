from a_pscore_data_reader_preproc import read_and_process_data, model_evaluation
from b_pscore_linear_reg_model import trained_linear_regression
from c_pscore_ridge_reg_model import trained_ridge_model
#from d_pscore_random_forest_model import trained_rf_model
from e_pscore_GBM_model import trained_gbm_model
from f_pscore_xgb_model import trained_xgb_model
from g_pscore_tf_dn_model import trained_tf_dn_model
from sklearn.linear_model import LinearRegression

def main():
    # Set the paths and if I want to dummy code the POS_Team variable
    # Also f means the full data, pull in the selected and full data
    train_path = r"E:\github_repos\Private_Projects\NCAA_FBS_AP_Ranking_Predictions\python_ap\scripts_and_data\data\score_pred_train_data.csv"
    test_path = r"E:\github_repos\Private_Projects\NCAA_FBS_AP_Ranking_Predictions\python_ap\scripts_and_data\data\score_pred_test_data.csv"

    print("Preprocessing selected data with dummy code for POS Team...")
    x_train, y_train, x_test, y_test, team_values = read_and_process_data(train_path, test_path, True)

    print("\nTraining Linear Regression on full preprocessed data...")
    lr_model = trained_linear_regression(x_train, y_train)

    ridge_model = trained_ridge_model(x_train, y_train)

    #rf_model = trained_rf_model(x_train, y_train)

    gbm_model = trained_gbm_model(x_train, y_train)

    xgb_model = trained_xgb_model(x_train, y_train)

    tf_model = trained_tf_dn_model(x_train, y_train)

    # Get model predictions and add back to x_train for stacked model predictions
    x_train_stacked = x_train.copy()
    x_train_stacked['lr_pred'] = lr_model.predict(x_train)
    x_train_stacked['ridge_pred'] = ridge_model.predict(x_train)
    #x_train_stacked['rf_pred'] = rf_model.predict(x_train)
    x_train_stacked['gbm_pred'] = gbm_model.predict(x_train)
    x_train_stacked['xgb_pred'] = xgb_model.predict(x_train)
    x_train_stacked['tf_pred'] = tf_model.predict(x_train)

    # Just select the preds for final modeling
    x_train_stacked = x_train_stacked[['lr_pred', 'ridge_pred', 'gbm_pred', 'xgb_pred', 'tf_pred']]

    # lets predict in the test for stacked predictions as well
    y_pred_test_lr = lr_model.predict(x_test)
    y_pred_test_ridge = ridge_model.predict(x_test)
    #y_pred_test_rf = rf_model.predict(x_test)
    y_pred_test_gbm = gbm_model.predict(x_test)
    y_pred_test_xgb = xgb_model.predict(x_test)
    y_pred_test_tf = tf_model.predict(x_test)

    # now
    x_test['lr_pred'] = y_pred_test_lr
    x_test['ridge_pred'] = y_pred_test_ridge
    #x_test['rf_pred'] = y_pred_test_rf
    x_test['gbm_pred'] = y_pred_test_gbm
    x_test['xgb_pred'] = y_pred_test_xgb
    x_test['tf_pred'] = y_pred_test_tf

    ####x_test_stacked = x_test[['lr_pred','ridge_pred','rf_pred','gbm_pred','xgb_pred','tf_pred']].copy()
    x_test_stacked = x_test[['lr_pred','ridge_pred','gbm_pred','xgb_pred','tf_pred']].copy() # RF is too over fitted

    # Lets do a stacked linear model as well
    lm_stacked = LinearRegression()

    # now fit
    lm_stacked.fit(x_train_stacked, y_train)

    return lm_stacked

if __name__ == "__main__":
    main()
