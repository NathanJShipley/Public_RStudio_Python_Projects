import pandas as pd                 # data manipulation
import matplotlib.pyplot as plt     # data viz
import numpy as np
import seaborn as sns               # data viz
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import Input, layers, models, metrics
from sklearn.preprocessing import StandardScaler
from tensorflow.keras.callbacks import EarlyStopping

# Small function to read in the csv data
def read_in_data(train_path, test_path):
    train_df = pd.read_csv(train_path)
    test_df = pd.read_csv(test_path)

    return train_df, test_df

# Read in the csv data. Note that the test data is based on the most recent week of data
train_df, test_df = read_in_data(
    r"E:\github_repos\Private_Projects\NCAA_FBS_AP_Ranking_Predictions\python_ap\scripts_and_data\data\train_data.csv",
    r"E:\github_repos\Private_Projects\NCAA_FBS_AP_Ranking_Predictions\python_ap\scripts_and_data\data\test_data.csv"
)

# Prepare the data
# Lets first take a look at make sure we have good data
#print(train_df.tail())
print(train_df.dtypes)

# Also some quick data viz
#snsplot = sns.pairplot(train_df[["log_votes","lagged_log_votes","cumulative_games_won"]], diag_kind="kde")
#plt.show()
#print(snsplot)



###### Preparing the data for real
# Split into x and y, drop log votes and analytical points from x data
x_train = train_df.drop(columns = ['log_votes','analytical_points'])
y_train = train_df['log_votes']

x_test = test_df.drop(columns = ['log_votes','analytical_points'])
y_test = test_df['log_votes']



# Scale data
columns_to_scale = x_train.drop(columns=['pos_team']).columns

# Initialize the scaler
scaler = StandardScaler()

# Fit and transform the training data
x_train[columns_to_scale] = scaler.fit_transform(x_train[columns_to_scale])

# Transform the test data using the parameters from training
x_test[columns_to_scale] = scaler.transform(x_test[columns_to_scale])



# So we do have a categorical variable that we should try to handle before working in TF. Are TEAMS, the pos_team variable
# Just going to dummy code for now
x_train_encoded = pd.get_dummies(x_train, columns=['pos_team'], drop_first=True)
x_test_encoded = pd.get_dummies(x_test, columns=['pos_team'], drop_first=True)

# Some teams are missing from the test data from the train data, so lets add them back real quick to the test data
missing_cols = set(x_train_encoded.columns) - set(x_test_encoded.columns)

for cols in missing_cols:
    x_test_encoded[cols] = 0

print(x_train_encoded.shape)  # Expected output should be (num_samples, 181)
print(x_test_encoded.shape) # Expected output should be (num_samples, 181)


### Time for TF!
# Build the inital tf model
tf_model = keras.Sequential([
    Input(shape=(x_train_encoded.shape[1],)),   # Explicit Input layer to define input shape. Seems to not be widely used online, but documentation suggests this is prefered method via keras
    layers.Dense(64, activation='relu'),        # hidden layer
    layers.Dense(32, activation='relu'),        # hidden layer, looks like having a 2nd is making the model fit better and having more nodes in this layer is helping
    layers.Dense(1, activation='linear')        # Output layer for regression
])

# Compile the model
# some other optimizers = tf.keras.optimizers.RMSprop(0.001), SGD, etc. Also try different learning rates in adam
# Also play around with different layers and neurons, so check the weights
# regularization techniques, dropout or L2
tf_model.compile(optimizer='adam', loss='mse', metrics=['mae','mse'])

# summary of the model, to check
tf_model.summary()

# Now train the model!
# Based on the plot below, looks like some really bad overfitting, so lets pull in early stopping
early_stopping = EarlyStopping(monitor='val_loss',  # You can change this to 'val_mae' or 'val_mse'
                               patience=10,  # Number of epochs with no improvement after which training will stop
                               restore_best_weights=True,  # Restore model weights from the epoch with the best validation loss
                               verbose=1)  # Show a message when early stopping is triggered

#history = tf_model.fit(x_train_encoded, y_train, epochs=200, batch_size=32, validation_split = 0.2) # note batch size is 32 by default, larger faster but more memory
#history = tf_model.fit(x_train_encoded, y_train, epochs=200, batch_size=32, validation_split = 0.2, callbacks = [early_stopping] ) # note batch size is 32 by default, larger faster but more memory
def plot_training_history(history, epochs=200):
    """
    Plots the loss, MAE, and MSE for both training and validation splits.

    Args:
        history: The training history returned by model.fit().
        epochs: The number of epochs (default is 200).
    """
    # Get the number of epochs from the history object
    epochs = len(history.history['loss'])  # Number of completed epochs

    # Extract training and validation metrics
    train_loss = history.history['loss']
    val_loss = history.history['val_loss']
    train_mae = history.history['mae']
    val_mae = history.history['val_mae']
    train_mse = history.history['mse']
    val_mse = history.history['val_mse']

    # Create subplots for the loss, MAE, and MSE
    fig, axs = plt.subplots(1, 3, figsize=(18, 5))

    # Plot training loss vs. validation loss
    axs[0].plot(range(1, epochs + 1), train_loss, label='Train Loss')
    axs[0].plot(range(1, epochs + 1), val_loss, label='Val Loss', linestyle='--')
    axs[0].set_title('Loss')
    axs[0].set_xlabel('Epochs')
    axs[0].set_ylabel('Loss')
    axs[0].legend()

    # Plot training MAE vs. validation MAE
    axs[1].plot(range(1, epochs + 1), train_mae, label='Train MAE')
    axs[1].plot(range(1, epochs + 1), val_mae, label='Val MAE', linestyle='--')
    axs[1].set_title('Mean Absolute Error (MAE)')
    axs[1].set_xlabel('Epochs')
    axs[1].set_ylabel('MAE')
    axs[1].legend()

    # Plot training MSE vs. validation MSE
    axs[2].plot(range(1, epochs + 1), train_mse, label='Train MSE')
    axs[2].plot(range(1, epochs + 1), val_mse, label='Val MSE', linestyle='--')
    axs[2].set_title('Mean Squared Error (MSE)')
    axs[2].set_xlabel('Epochs')
    axs[2].set_ylabel('MSE')
    axs[2].legend()

    # Display the plots
    plt.tight_layout()
    plt.show()
#plot_training_history(history)

# This seems to be the best fit so far, probably take out the validation split and set epoch around 10 for final model?
tf_model.fit(x_train_encoded, y_train, epochs=200, batch_size=32, validation_split = 0.2, callbacks = [early_stopping] )

# Lets compare to the test data and evaluate fit, looks like
test_loss, test_mae, test_mse = tf_model.evaluate(x_test_encoded, y_test)
print(f'Test Loss: {test_loss}')
print(f'Test MAE: {test_mae}')
print(f'Test MSE: {test_mse}')

# Predict values
y_pred = tf_model.predict(x_test_encoded)
y_pred = np.exp(y_pred)

# attached those values as a df
y_pred_df = pd.DataFrame(y_pred, columns=["Predicted_Votes"])

# Rejoin to the original test data, just grab the data of interest
output_df = test_df[["analytical_points","pos_team"]].copy()
output_df["Predicted_Votes"] = y_pred_df
output_df.to_csv('test.csv', index=False)


'''

test_loss, test_mae, test_mse = tf_model.evaluate(x_test_encoded, y_test)
print(f'Test Loss: {test_loss}')
print(f'Test MAE: {test_mae}')
print(f'Test MSE: {test_mse}')

y_pred = tf_model.predict(x_test_encoded)

tf_model.save('my_model.h5')
loaded_model = keras.models.load_model('my_model.h5')

'''


