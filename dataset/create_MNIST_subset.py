# Create a subset of MNIST dataset
# Select XX observations for each digit from MNIST training(test) dataset
# Boren Zheng

import pandas as pd
import torchvision
import matplotlib.pyplot as plt
import numpy as np

mnist = torchvision.datasets.MNIST(root='./mnist/',
    # training dataset
    #train=True,
    # test dataset
    train = False,
    transform=torchvision.transforms.ToTensor(),
    download = True)

# (60000, 28, 28) or (10000, 28, 28)  
data = mnist.data.numpy()  

# (60000, 784) or (10000, 28, 28)
flatten_data = data.reshape(-1, 28*28)

lables = mnist.targets.numpy()

# select 'num' observations for each digit from MNIST training(test) dataset, 
# if num = 100, the subset has 1000 observations,
# (num * 10, 784)
def select_data(num):
    index  = {k : [] for k in range(10)}
    for i in range(10000): # train:60000, test:10000
        if len(index[lables[i]]) < num:
          index[lables[i]].append(i)
    return index

index = select_data(200)

# print(select_data(10))
# print(index[0])

train_index = []
for i in range(10):
    train_index += index[i]
train_index.sort()

#print(train_index)

train_data = []
train_lable = []
for index in train_index:
    train_data.append(flatten_data[index])
    train_lable.append(lables[index])

attrs = pd.DataFrame(train_data)
mnist_lables = pd.DataFrame(train_lable)

# create custom MNIST train dataset in .csv
#attrs.to_csv(path_or_buf='./mnist_train.csv',index= False)
#mnist_lables.to_csv(path_or_buf='./mnist_train_lables.csv',index= False)

# create custom MNIST test dataset in .csv
attrs.to_csv(path_or_buf='./mnist_test.csv',index= False)
mnist_lables.to_csv(path_or_buf='./mnist_test_lables.csv',index= False)

    


