#!/usr/bin/env python
# coding: utf-8

# In[21]:


import autokeras as ak
import pandas as pd
import numpy as np
import tensorflow as tf

def fun(fn):
    test = pd.read_csv(fn)    
    test = test[['d','n1','n2','n3','n4','n5','n6','n7','n8']]
    return np.array(test)
def fun_id(fn):
    test = pd.read_csv(fn)    
    test = test[['id']]
    return np.array(test)
def funy(fn):
    uu = pd.read_csv(fn)
    return np.array((uu["ans"]))#!/usr/bin/env python
# coding: utf-8

# In[21]:


import autokeras as ak
import pandas as pd
import numpy as np
import tensorflow as tf




# In[23]:

ids = fun_id("/root/merge_data_train.csv")
x = fun("/root/merge_data_train.csv")
y =  funy("/root/merge_data_train.csv")

ids_val = fun_id("/root/merge_data_val.csv")
x_val =  fun("/root/merge_data_val.csv")
y_val =  funy("/root/merge_data_val.csv")

# In[24]:



from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(x, y, test_size=0.33, random_state=42)

print(X_train.shape)
print(y_train.shape)
print(y_test.shape)
print(y_val.shape)

print(X_train[1])
print(y_train[1])
# In[27]:


id_input = ak.StructuredDataInput()
id_den =  ak.CategoricalToNumerical()(id_input)
id_den =  ak.Embedding()(id_den)

x_input =  ak.Input()
layer = ak.DenseBlock()(x_input)


mer = ak.Merge()([id_den, layer])
output_node = ak.RegressionHead(metrics=['mae'])(mer)


# In[28]:


# auto_model = ak.AutoModel( inputs= x_input, 
#                            #project_name="categorical_model",
#                            outputs = output_node,
#                            objective="loss",
#                            tuner="bayesian", max_trials= 10 )
auto_model = ak.AutoModel( inputs= [id_input, x_input], 
                           #project_name="categorical_model",
                           outputs = output_node,
                           objective="loss",
                           tuner="bayesian", max_trials= 3 )



auto_model.fit( [ids, x],
                y, 
                epochs = 20, 
                batch_size = 256)





best = auto_model.export_model()
best.summary()
# In[ ]:
best.save('train_' +str(best.evaluate(X_train, y_train)) + '_new_'
          + str(best.evaluate(X_test, y_test))+ "val_"+ str(best.evaluate(x_val, y_val)) +
          ".h5")

# print("train")
# print(best.evaluate(X_train, y_train))
# print("val")
# print(best.evaluate(X_test, y_test))
# print("never seen")
# print(best.evaluate(x_val, y_val))


