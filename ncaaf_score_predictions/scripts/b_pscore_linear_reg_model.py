from sklearn.linear_model import LinearRegression



# Set up the simple linear regression function
def trained_linear_regression(x_train, y_train):
     lr_model = LinearRegression()
     lr_model.fit(x_train, y_train)

     return lr_model



