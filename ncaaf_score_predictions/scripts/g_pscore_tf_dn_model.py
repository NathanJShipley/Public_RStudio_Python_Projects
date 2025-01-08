from tensorflow import keras
from tensorflow.keras import Input, layers, models, metrics, regularizers
from tensorflow.keras.optimizers import Adam

# see my jupyter notebook for development processes

def trained_tf_dn_model(x_train, y_train):
    # Build the model
    tf_model = keras.Sequential([
        Input(shape=(x_train.shape[1],)),   # Explicit Input layer to define input shape.
        layers.Dense(16, activation='relu', kernel_regularizer=regularizers.l2(0.01)),
        layers.Dense(16, activation='relu', kernel_regularizer=regularizers.l2(0.01)),
        layers.Dense(1, activation='linear')        # Output layer for regression
    ])

    # Compile the model, early iterations suggested adam optimizer with .01 learning rate
    optimizer = Adam(learning_rate=0.001)
    tf_model.compile(optimizer=optimizer, loss='mse', metrics=['mae', 'mse'])

    # Now fit the model
    tf_model.fit(x_train, y_train, epochs=25, batch_size=32)

    return tf_model
