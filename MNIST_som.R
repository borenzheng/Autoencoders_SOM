library(popsom)
library(dplyr)
library(ggplot2)
library(som)
library(BBmisc)

mnist_test_label <- read.csv('mnist_test_lables.csv')

######## Original Data #########
mnist_test <- read.csv('MNIST_test_nonzero.csv')


# for loop -- train diff times
num_train <- c()
convergs <- c()
embeds <- c()
topos <- c()

for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,100000,500000))
  {
    ms = map.build(mnist_test, labels = mnist_test_label, xdim = 40, ydim = 40,alpha=0.95,train = i,algorithm="vsom")
    converg = map.convergence(ms)
    embed = map.embed(ms)
    topo = map.topo(ms)$val
    num_train = c(num_train,i)
    convergs = c(convergs,converg)
    embeds = c(embeds, embed)
    topos = c(topos,topo)
    print(converg)
  }
}

#plot(x = num_train,y = convergs, log = 'x', type = "o")
plotdata = data.frame(num_train,convergs, embeds, topos)
ggplot(plotdata,aes(x = log2(num_train), y = convergs)) + geom_point() + ylim(0,1)  +
  labs(title = 'SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata,aes(x = log2(num_train), y = embeds)) + geom_point() + ylim(0,1)  +
  labs(title = 'SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata,aes(x = log2(num_train), y = topos)) + geom_point() + ylim(0,1)  +
  labs(title = 'SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ms <- map.build(mnist_test, labels = mnist_test_label, xdim = 40, ydim = 30,alpha=0.8,
                train =10000,algorithm="vsom")
ms1 <- map.build(mnist_test, labels = mnist_test_label, xdim = 30, ydim = 30,alpha=0.95,
                 train =100000,algorithm="vsom")

map.embed(ms1)
map.convergence(ms1)
map.significance(ms1)
map.starburst(ms1)

########## Basic AE ############
mnist_AE <- read.csv('MNIST_AE_encoded.csv')

#mnist_AE_norm <- normalize(mnist_AE, method = "range", range = c(0, 1))

# for loop -- train diff times
num_train_ae <- c()
convergs_ae <- c()
embeds_ae <- c()
topos_ae <- c()


for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,100000,500000))
  {
    ms_ae = map.build(mnist_AE, labels = mnist_test_label, xdim = 40, ydim = 40,alpha=0.95,train = i,algorithm="vsom")
    converg = map.convergence(ms_ae)
    embed = map.embed(ms_ae)
    topo = map.topo(ms_ae)$val
    num_train_ae = c(num_train_ae,i)
    convergs_ae = c(convergs_ae,converg)
    embeds_ae = c(embeds_ae,embed)
    topos_ae = c(topos_ae,topo)
    print(converg)
    
  }
}

plotdata_ae = data.frame(num_train_ae,convergs_ae,embeds_ae,topos_ae)

ggplot(plotdata_ae,aes(x = log2(num_train_ae), y = convergs_ae)) + geom_point() + ylim(0,1)  +
  labs(title = 'AE_SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_ae,aes(x = log2(num_train_ae), y = embeds_ae)) + geom_point() + ylim(0,1)  +
  labs(title = 'AE_SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_ae,aes(x = log2(num_train_ae), y = topos_ae)) + geom_point() + ylim(0,1)  +
  labs(title = 'AE_SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

# ms_ae <- map.build(mnist_AE, labels = mnist_test_label, xdim = 30, ydim = 30,alpha=0.9,
#                    train =100000,algorithm="vsom")
# ms_ae1 <- map.build(mnist_AE, labels = mnist_test_label, xdim = 40, ydim = 35,alpha=0.9,
#                     train =100000,algorithm="vsom")
# map.embed(ms_ae1)
# map.convergence(ms_ae1)
# map.starburst(ms_ae1)
# map.significance(ms_ae)


########### SAE ############
mnist_SAE <- read.csv('MNIST_SAE_encoded.csv')

#mnist_SAE_norm <- normalize(mnist_SAE, method = "range", range = c(0, 1))

# for loop -- train diff times
num_train_sae <- c()
convergs_sae <- c()
embeds_sae <- c()
topos_sae <- c()


for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,100000,500000))
  {
    ms_sae = map.build(mnist_SAE, labels = mnist_test_label, xdim = 40, ydim = 40,alpha=0.95,train = i,algorithm="vsom")
    converg = map.convergence(ms_sae)
    embed = map.embed(ms_sae)
    topo = map.topo(ms_sae)$val
    num_train_sae = c(num_train_sae,i)
    convergs_sae = c(convergs_sae,converg)
    embeds_sae =c(embeds_sae, embed)
    topos_sae = c(topos_sae,topo)
    # print(i)
    # print(embed)
    print(converg)
  }
}

plotdata_sae = data.frame(num_train_sae,convergs_sae,embeds_sae,topos_sae)

ggplot(plotdata_sae,aes(x = log2(num_train_sae), y = convergs_sae)) + geom_point() + ylim(0,1)  +
  labs(title = 'SAE_SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4))  + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_sae,aes(x = log2(num_train_sae), y = embeds_sae)) + geom_point() + ylim(0,1)  +
  labs(title = 'SAE_SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3))  + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_sae,aes(x = log2(num_train_sae), y = topos_sae)) + geom_point() + ylim(0,1)  +
  labs(title = 'SAE_SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4))  + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

# ms_sae <- map.build(mnist_SAE_norm, labels = mnist_test_label, xdim = 30, ydim = 30,alpha=0.9,
#                     train =100000,algorithm="vsom")
# ms_sae1 <- map.build(mnist_SAE, labels = mnist_test_label, xdim = 40, ydim = 35,alpha=0.9,
#                      train =300000,algorithm="vsom")
# map.embed(ms_sae1)
# map.convergence(ms_sae1)
# map.starburst(ms_sae1)
# map.significance(ms_sae1)


############# CAE ################
mnist_CAE <- read.csv('MNIST_CAE_encoded.csv')

#mnist_CAE_norm <- normalize(mnist_CAE, method = "range", range = c(0, 1))

# for loop -- train diff times
num_train_cae <- c()
convergs_cae <- c()
embeds_cae <- c()
topos_cae <- c()


for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,100000,500000))
  {
    ms_cae = map.build(mnist_CAE, labels = mnist_test_label, xdim = 40, ydim = 40,alpha=0.95,train = i,algorithm="vsom")
    converg = map.convergence(ms_cae)
    embed = map.embed(ms_cae)
    topo = map.topo(ms_cae)$val
    num_train_cae = c(num_train_cae,i)
    convergs_cae = c(convergs_cae,converg)
    embeds_cae = c(embeds_cae,embed)
    topos_cae = c(topos_cae,topo)
    print(converg)
    
  }
}

plotdata_cae = data.frame(num_train_cae,convergs_cae,embeds_cae,topos_cae)
plotdata_cae = read.csv("./paper/mnist_plot/plot_CAE.csv",sep = "")

ggplot(plotdata_cae,aes(x = log2(num_train_cae), y = convergs_cae)) + geom_point() + ylim(0,1)  +
  labs(title = 'CAE_SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_cae,aes(x = log2(num_train_cae), y = embeds_cae)) + geom_point() + ylim(0,1)  +
  labs(title = 'CAE_SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_cae,aes(x = log2(num_train_cae), y = topos_cae)) + geom_point() + ylim(0,1)  +
  labs(title = 'CAE_SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


# ms_cae <- map.build(mnist_CAE, labels = mnist_test_label, xdim = 40, ydim = 35,alpha=0.9,
#                     train =10000,algorithm="vsom")
# ms_cae1 <- map.build(mnist_CAE, labels = mnist_test_label, xdim = 40, ydim = 35,alpha=0.9,
#                      train =300000,algorithm="vsom")
# map.embed(ms_cae1)
# map.convergence(ms_cae1)
# map.starburst(ms_cae)
# map.significance(ms_cae)

############# DCAE ################
mnist_DAE <- read.csv('MNIST_DCAE_encoded.csv')

#mnist_DAE_norm = normalize(mnist_DAE, byrow = TRUE)
#mnist_DAE_norm = normalize(mnist_DAE, method = "range", range = c(0, 1))

# for loop -- train diff times
num_train_dae <- c()
convergs_dae <- c()
embeds_dae <- c()
topos_dae <- c()

for( i in seq(1,5)){
  for(i in list(10,100,1000,10000,100000,500000))
  {
    ms_dae = map.build(mnist_DAE, labels = mnist_test_label, xdim = 40, ydim = 40,alpha=0.95,train = i,algorithm="vsom")
    converg = map.convergence(ms_dae)
    embed = map.embed(ms_dae)
    topo = map.topo(ms_dae)$val
    num_train_dae = c(num_train_dae,i)
    convergs_dae = c(convergs_dae,converg)
    embeds_dae = c(embeds_dae,embed)
    topos_dae = c(topos_dae,topo)
    print(converg)
  }
}

plotdata_dae = data.frame(num_train_dae,convergs_dae,embeds_dae,topos_dae)

ggplot(plotdata_dae,aes(x = log2(num_train_dae), y = convergs_dae)) + geom_point() + ylim(0,1)  +
  labs(title = 'DCAE_SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_dae,aes(x = log2(num_train_dae), y = embeds_dae)) + geom_point() + ylim(0,1)  +
  labs(title = 'DCAE_SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_dae,aes(x = log2(num_train_dae), y = topos_dae)) + geom_point() + ylim(0,1)  +
  labs(title = 'DCAE_SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 5)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ms_dae <- map.build(mnist_DAE, labels = mnist_test_label, xdim = 40, ydim = 35,alpha=0.6,
                    train =10000,algorithm="vsom")
ms_dae1 <- map.build(mnist_DAE, labels = mnist_test_label, xdim = 40, ydim = 40,alpha=0.95,
                     train =100000,algorithm="vsom")
map.embed(ms_dae1)
map.convergence(ms_dae1)
map.starburst(ms_dae1)
map.significance(ms_dae1)

############## ConvAE #############
mnist_conAE <- read.csv('MNIST_ConAE_encoded.csv')

#mnist_conAE_norm <- normalize(mnist_conAE, method = "range", range = c(0, 1))

# for loop -- train diff times
num_train_conae <- c()
convergs_conae <- c()
embeds_conae <- c()
topos_conae <- c()

for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,100000,500000))
  {
    ms_conae = map.build(mnist_conAE, labels = mnist_test_label, 
                         xdim = 40, ydim = 40,alpha=0.95,train = i,algorithm="vsom")
    converg = map.convergence(ms_conae)
    embed = map.embed(ms_conae)
    topo = map.topo(ms_conae)$val
    num_train_conae = c(num_train_conae,i)
    convergs_conae = c(convergs_conae,converg)
    embeds_conae = c(embeds_conae,embed)
    topos_conae = c(topos_conae,topo)
    print(converg)
  }
}


plotdata_conae = data.frame(num_train_conae,convergs_conae,embeds_conae,topos_conae)

ggplot(plotdata_conae,aes(x = log2(num_train_conae), y = convergs_conae)) + geom_point() + ylim(0,1)  +
  labs(title = 'ConvAE_SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 5)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_conae,aes(x = log2(num_train_conae), y = embeds_conae)) + geom_point() + ylim(0,1)  +
  labs(title = 'ConvAE_SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_conae,aes(x = log2(num_train_conae), y = topos_conae)) + geom_point() + ylim(0,1)  +
  labs(title = 'ConvAE_SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x,3)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


ms_conae <- map.build(mnist_conAE, labels = mnist_test_label, xdim = 40, ydim = 35,alpha=0.9,
                      train =50000,algorithm="vsom")
ms_conae1 <- map.build(mnist_conAE, labels = mnist_test_label, xdim = 30, ydim = 30,alpha=0.95,
                       train =100000,algorithm="vsom")
map.embed(ms_conae1)
map.convergence(ms_conae1)
map.starburst(ms_conae1)
map.topo(ms_conae1)$val
map.significance(ms_conae1)
