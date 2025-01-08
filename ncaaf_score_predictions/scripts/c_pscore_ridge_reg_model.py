from sklearn.linear_model import Ridge



def trained_ridge_model(x_train, y_train):
     # set alpha value already
     best_alpha = 2023.5896477251556

     # fit ridge
     best_ridge = Ridge(alpha=best_alpha)
     best_ridge.fit(x_train, y_train)

     return best_ridge
