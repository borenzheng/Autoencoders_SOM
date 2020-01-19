library(popsom)
library(dplyr)
library(ggplot2)
library(BBmisc)
library(ggpubr)

sat_test_label <- read.csv('sat_test_label.csv')

######## Original Data #########
sat_test <- read.csv('sat_test_n.csv')

# for loop -- train diff times
num_train <- c()
convergs <- c()
embeds <- c()
topos <- c()
for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,50000,100000,200000,300000,400000))
  {
    ms = map.build(sat_test, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,train = i,algorithm="vsom")
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

plotdata = data.frame(num_train,convergs,embeds,topos)
plotdata = read.csv("./paper/Sat_plot/plot_SOM.csv",sep = "")

ggplot(plotdata,aes(x = log2(num_train), y = convergs)) + geom_point() + ylim(0,1) +
  labs(title = 'SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 2)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata,aes(x = log2(num_train), y = embeds)) + geom_point() + ylim(0,1) +
  labs(title = 'SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata,aes(x = log2(num_train), y = topos)) + geom_point() + ylim(0,1) +
  labs(title = 'SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 5)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ms <- map.build(sat_test, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                   train =10000,algorithm="vsom")
ms1 <- map.build(sat_test, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                train =200000,algorithm="vsom")

map.embed(ms1)
map.convergence(ms1)
map.starburst(ms1)
map.significance(ms1)

########## Basic AE ############
sat_AE <- read.csv('sat_AE_encoded.csv')
sat_AE_norm = normalize(sat_AE,method = 'range', range = c(0,1),margin = 1)

# for loop -- train diff times
num_train_ae <- c()
convergs_ae <- c()
embeds_ae <- c()
topos_ae <- c()

for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,50000,100000,200000,300000,400000))
  {
    ms_ae = map.build(sat_AE, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,train = i,algorithm="vsom")
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
plotdata_ae = read.csv("./paper/Sat_plot/plot_AE.csv",sep = "")

ggplot(plotdata_ae,aes(x = log2(num_train_ae), y = convergs_ae)) + geom_point() + ylim(0,1)  +
  labs(title = 'AE_SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 2)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_ae,aes(x = log2(num_train_ae), y = embeds_ae)) + geom_point() + ylim(0,1)  +
  labs(title = 'AE_SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 2)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_ae,aes(x = log2(num_train_ae), y = topos_ae)) + geom_point() + ylim(0,1)  +
  labs(title = 'AE_SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ms_ae <- map.build(sat_AE, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                train =10000,algorithm="vsom")
ms_ae1 <- map.build(sat_AE, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                   train =400000,algorithm="vsom")
map.embed(ms_ae1)
map.convergence(ms_ae1)
map.starburst(ms_ae1)
map.significance(ms_ae1)


########### SAE ############
sat_SAE <- read.csv('sat_SAE_encoded.csv')
#sat_SAE_norm = normalize(sat_SAE,method = 'range', range = c(0,1),margin = 1)

# for loop -- train diff times
num_train_sae <- c()
convergs_sae <- c()
embeds_sae <- c()
topos_sae <- c()

for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,50000,100000,200000,300000,400000))
  {
    ms_sae = map.build(sat_SAE, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,train = i,algorithm="vsom")
    converg = map.convergence(ms_sae)
    embed = map.embed(ms_sae)
    topo = map.topo(ms_sae)$val
    num_train_sae = c(num_train_sae,i)
    convergs_sae = c(convergs_sae,converg)
    embeds_sae = c(embeds_sae,embed)
    topos_sae = c(topos_sae, topo)
    print(converg)
  }
}

plotdata_sae = data.frame(num_train_sae,convergs_sae,embeds_sae,topos_sae)
plotdata_sae = read.csv("./paper/Sat_plot/plot_SAE.csv",sep = "")

ggplot(plotdata_sae,aes(x = log2(num_train_sae), y = convergs_sae)) + geom_point() + ylim(0,1)  +
  labs(title = 'SAE_SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3)) +  
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_sae,aes(x = log2(num_train_sae), y = embeds_sae)) + geom_point() + ylim(0,1)  +
  labs(title = 'SAE_SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 2)) +  
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_sae,aes(x = log2(num_train_sae), y = topos_sae)) + geom_point() + ylim(0,1)  +
  labs(title = 'SAE_SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) +  
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


ms_sae <- map.build(sat_SAE, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                      train =10000,algorithm="vsom")
ms_sae1 <- map.build(sat_SAE_norm, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                    train =400000,algorithm="vsom")
map.embed(ms_sae1)
map.convergence(ms_sae1)
map.starburst(ms_sae1)
map.significance(ms_sae1)


############# CAE ################
sat_CAE <- read.csv('sat_CAE_encoded.csv')
sat_CAE_norm = normalize(sat_CAE,method = 'range', range = c(0,1),margin = 1)

# for loop -- train diff times
num_train_cae <- c()
convergs_cae <- c()
embeds_cae <- c()
topos_cae <- c()

for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,50000,100000,200000,300000,400000))
  {
    ms_cae = map.build(sat_CAE, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,train = i,algorithm="vsom")
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
plotdata_cae = read.csv("./paper/Sat_plot/plot_CAE.csv",sep = "")

ggplot(plotdata_cae,aes(x = log2(num_train_cae), y = convergs_cae)) + geom_point() + ylim(0,1)  +
  labs(title = 'CAE_SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_cae,aes(x = log2(num_train_cae), y = embeds_cae)) + geom_point() + ylim(0,1)  +
  labs(title = 'CAE_SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 2)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_cae,aes(x = log2(num_train_cae), y = topos_cae)) + geom_point() + ylim(0,1)  +
  labs(title = 'CAE_SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 5)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


ms_cae <- map.build(sat_CAE, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                    train =10000,algorithm="vsom")
ms_cae1 <- map.build(sat_CAE_norm, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                     train =100000,algorithm="vsom")
map.embed(ms_cae1)
map.convergence(ms_cae1)
map.starburst(ms_cae1)
map.significance(ms_cae)

############# DAE ################
sat_DAE <- read.csv('sat_DAE_encoded.csv')
#sat_DAE_norm = normalize(sat_DAE,method = 'range', range = c(0,1),margin = 1)

# for loop -- train diff times
num_train_dae <- c()
convergs_dae <- c()
embeds_dae <- c()
topos_dae <-c()

for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,50000,100000,200000,300000,400000))
  {
    ms_dae = map.build(sat_DAE, labels = sat_test_label, 
                         xdim = 40, ydim = 35,alpha=0.6,train = i,algorithm="vsom")
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
plotdata_dae = read.csv("./paper/Sat_plot/plot_DAE.csv",sep = "")

ggplot(plotdata_dae,aes(x = log2(num_train_dae), y = convergs_dae)) + geom_point() + ylim(0,1)  +
  labs(title = 'DAE_SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 2)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_dae,aes(x = log2(num_train_dae), y = embeds_dae)) + geom_point() + ylim(0,1)  +
  labs(title = 'DAE_SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 2)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_dae,aes(x = log2(num_train_dae), y = topos_dae)) + geom_point() + ylim(0,1)  +
  labs(title = 'DAE_SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 5)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


ms_dae <- map.build(sat_DAE, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                    train =10000,algorithm="vsom")
ms_dae1 <- map.build(sat_DAE, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                     train =200000,algorithm="vsom")
map.embed(ms_dae1)
map.convergence(ms_dae1)
map.starburst(ms_dae1)
map.topo(ms_dae1)$val

map.significance(ms_dae)

############## ConvAE #############
sat_conAE <- read.csv('sat_ConAE_encoded.csv')

sat_conAE_norm = normalize(sat_conAE,method = 'range', range = c(0,1),margin = 1)

# for loop -- train diff times
num_train_conae <- c()
convergs_conae <- c()
embeds_conae <- c()
topos_conae <- c()

for(i in seq(1,5)){
  for(i in list(10,100,1000,10000,50000,100000,200000,300000,400000))
  {
    ms_conae = map.build(sat_conAE_norm, labels = sat_test_label, 
                         xdim = 40, ydim = 35,alpha=0.6,train = i,algorithm="vsom")
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
# for(i in seq(1,5)){
#   ms_conae = map.build(dim064_conAE, labels = dim064_test_label, 
#                        xdim = 40, ydim = 35,alpha=0.6,train = 300000,algorithm="vsom")
#   converg = map.convergence(ms_conae)
#   embed = map.embed(ms_conae)
#   topo = map.topo(ms_conae)$val
#   num_train_conae = c(num_train_conae,i)
#   convergs_conae = c(convergs_conae,converg)
#   embeds_conae = c(embeds_conae,embed)
#   topos_conae = c(topos_conae,topo)
#   print(converg)
# }


plotdata_conae = data.frame(num_train_conae,convergs_conae,embeds_conae,topos_conae)
plotdata_conae = read.csv("./paper/Sat_plot/plot_ConvAE.csv",sep = "")

ggplot(plotdata_conae,aes(x = log2(num_train_conae), y = convergs_conae)) + geom_point() + ylim(0,1)  +
  labs(title = 'ConvAE_SOM Convergence Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 3)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_conae,aes(x = log2(num_train_conae), y = embeds_conae)) + geom_point() + ylim(0,1)  +
  labs(title = 'ConvAE_SOM Embedding Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 4)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(plotdata_conae,aes(x = log2(num_train_conae), y = topos_conae)) + geom_point() + ylim(0,1)  +
  labs(title = 'ConvAE_SOM Estimated Topographic Accuracy',x= 'log2(Iterations)', y = 'Value') +
  stat_smooth(method = lm, formula = y ~ poly(x, 5)) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#ggarrange(embeds_plot,topos_plot,convergs_plot, ncol = 2, nrow = 2)

ms_conae <- map.build(sat_conAE, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                   train =10000,algorithm="vsom")
ms_conae1 <- map.build(sat_conAE_norm, labels = sat_test_label, xdim = 40, ydim = 35,alpha=0.6,
                      train =400000,algorithm="vsom")
map.embed(ms_conae1)
map.convergence(ms_conae1)
map.starburst(ms_conae1)
map.topo(ms_conae1)$val
map.significance(ms_conae1)
