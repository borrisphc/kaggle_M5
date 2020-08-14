import pandas as pd
from tensorflow.keras.layers import Dense
from tensorflow.keras.models import Sequential
import numpy as np
import random

df = pd.read_csv('/home/parsed_data2.csv')
df = df.dropna()


rm_list = ['Unnamed: 0','id','d','sales','date']
y_col = list(['sales'])
item_col  = list(['item_id'])
x_col = list(df.columns) 
[x_col.remove(i) for i in rm_list]
print(x_col)



train_df =  df[0:35000000]
val_df =  df[35000001:36500000]
#val_df =  df.loc[random.sample(list(df.index),1000)]
#train_df =  df.loc[random.sample(list(df.index),50000)]

x_data = np.array(train_df[x_col])
y_data = np.array(train_df[y_col])

val_x_data = np.array(val_df[x_col])
val_y_data = np.array(val_df[y_col])





from keras.layers import Embedding
from keras.layers import Flatten, Dense
from keras.layers import SimpleRNN
from keras.layers import BatchNormalization
from keras.models import Sequential
from keras.layers import Dropout
from keras.datasets import imdb
from keras.preprocessing.sequence import pad_sequences
from keras.callbacks import EarlyStopping
from keras.callbacks import ModelCheckpoint
from keras.callbacks import ReduceLROnPlateau
from keras.callbacks import TensorBoard
from keras.models import Sequential, Model
from keras import layers
from keras import Input
from keras.layers import LSTM
from keras.layers import Bidirectional
from keras.layers import Conv1D
from keras.utils import multi_gpu_model


DNA_input = Input( shape = (len(x_col),))
DNA_layer = Dense( units= 64,input_shape = (len(x_col),) , activation="relu") (DNA_input)
DNA_layer = Dense( units= 64 , activation="relu") (DNA_layer)
DNA_layer = Dense( units= 15 , activation="relu") (DNA_layer)
DNA_layer = Dense( units= 4 , activation="relu") (DNA_layer)

Out_put = Dense(1 )(DNA_layer)
model = Model( DNA_input, Out_put)
model.compile( optimizer= "rmsprop", loss='mse', metrics = ['mae'])





callbacks_list = [
    EarlyStopping( monitor = "mae", # 如果acc都沒有變就停止
                    patience = 50 ), # 多久沒有動就停
     ModelCheckpoint( filepath = "first_NN" + '.h5', # 存的檔名
                       monitor = 'mae',
                       save_best_only = True ),
    ReduceLROnPlateau( monitor = 'mae',
                       factor = 0.1 , # 減少的幅度，變成原先的十分之一
                       patience = 20 )#, # 多久沒動就開始變學習速度
    #TensorBoard( log_dir = "/src/notebooks/seed1024/my_log_dir",
    #             histogram_freq = 2,
      #           embeddings_freq = 2
#)
]


history = model.fit( x_data , y_data, epochs = 300, batch_size= 100, 
          validation_data=( val_x_data, val_y_data),
          callbacks = callbacks_list
         )



import matplotlib
#matplotlib.use('Agg')
import matplotlib.pyplot as plt
history_dict = history.history


acc_value = history_dict['mae']
val_acc_values = history.history['val_mae']


epochs = range(1,len(acc_value)+1 )
plt.plot(epochs, acc_value, 'b', label = "Training acc")
plt.plot(epochs, val_acc_values, 'b', label  = "Validation acc", color= "red")
plt.title('Training and Validation accuracy')
plt.xlabel('Epochs')
plt.ylabel('mae')
plt.legend()
plt.savefig( "history_val_acc_%s_acc_%s_id_%s.png"%( "tmp", "tmp", "tmp" ))
plt.clf()
#plt.show()