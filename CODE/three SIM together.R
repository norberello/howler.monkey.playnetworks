######EDGE DENSITY


#simulated edge densities

library(igraph)
library(ggplot2)
library(brainGraph)
library("ggpubr")

setwd("~/Documents/R scripts and data/Play networks")

pla.data<-read.csv("PLAnet.csv",header=TRUE,row.names = 1)
pla.matrix<-as.matrix(pla.data)
pla.network<-graph.adjacency(pla.matrix,mode="undirected",
                             diag=FALSE,weighted=T)

aga.data<-read.csv("AGAnet.csv",header=TRUE,row.names = 1)
aga.matrix<-as.matrix(aga.data)
aga.network<-graph.adjacency(aga.matrix,mode="undirected",
                             diag=FALSE,weighted = TRUE)

liz.data<-read.csv("LIZnet.csv",header=TRUE,row.names = 1)
liz.matrix<-as.matrix(liz.data)
liz.network<-graph.adjacency(liz.matrix,mode="undirected",
                             diag=FALSE,weighted=T)

se.data<-read.csv("SEnet.csv",header=TRUE,row.names = 1)
se.matrix<-as.matrix(se.data)
se.network<-graph.adjacency(se.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

ch.data<-read.csv("CHnet.csv",header=TRUE,row.names = 1)
ch.matrix<-as.matrix(ch.data)
ch.network<-graph.adjacency(ch.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

sn.data<-read.csv("SNnet.csv",header=TRUE,row.names = 1)
sn.matrix<-as.matrix(sn.data)
sn.network<-graph.adjacency(sn.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

cp.data<-read.csv("CPnet.csv",header=TRUE,row.names = 1)
cp.matrix<-as.matrix(cp.data)
cp.network<-graph.adjacency(cp.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)


pla.sim <- vector('list',10000)
for(i in 1:10000){   
  pla.sim[[i]] <- erdos.renyi.game(
    n = gorder(pla.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
pla.sim.meanedge <- unlist(lapply(pla.sim,edge_density))
pla.sim.meanedge <- pla.sim.meanedge[!is.na(pla.sim.meanedge)]
pla.meanedge.h <- ggplot()+aes(x=pla.sim.meanedge)+geom_histogram()+
  labs(x="edge density")+theme_bw()+geom_vline(xintercept = edge_density(pla.network),
                                               color="red",
                                               linetype="dashed",
                                               linewidth=2)

aga.sim <- vector('list',10000)
for(i in 1:10000){   
  aga.sim[[i]] <- erdos.renyi.game(
    n = gorder(aga.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
aga.sim.meanedge <- unlist(lapply(aga.sim,edge_density))
aga.sim.meanedge <- aga.sim.meanedge[!is.na(aga.sim.meanedge)]
aga.meanedge.h <- ggplot()+aes(x=aga.sim.meanedge)+geom_histogram()+
  labs(x="edge density")+theme_bw()+geom_vline(xintercept = edge_density(aga.network),
                                               color="red",
                                               linetype="dashed",
                                               linewidth=2)

liz.sim <- vector('list',10000)
for(i in 1:10000){   
  liz.sim[[i]] <- erdos.renyi.game(
    n = gorder(liz.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
liz.sim.meanedge <- unlist(lapply(liz.sim,edge_density))
liz.sim.meanedge <- liz.sim.meanedge[!is.na(liz.sim.meanedge)]
liz.meanedge.h <- ggplot()+aes(x=liz.sim.meanedge)+geom_histogram()+
  labs(x="edge density")+theme_bw()+geom_vline(xintercept = edge_density(liz.network),
                                               color="red",
                                               linetype="dashed",
                                               linewidth=2)

cp.sim <- vector('list',10000)
for(i in 1:10000){   
  cp.sim[[i]] <- erdos.renyi.game(
    n = gorder(cp.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
cp.sim.meanedge <- unlist(lapply(cp.sim,edge_density))
cp.sim.meanedge  <- cp.sim.meanedge [!is.na(cp.sim.meanedge )]
cp.meanedge.h <- ggplot()+aes(x=cp.sim.meanedge )+geom_histogram()+
  labs(x="edge density")+theme_bw()+geom_vline(xintercept = edge_density(cp.network),
                                               color="red",
                                               linetype="dashed",
                                               linewidth=2)

sn.sim <- vector('list',10000)
for(i in 1:10000){   
  sn.sim[[i]] <- erdos.renyi.game(
    n = gorder(sn.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
sn.sim.meanedge <- unlist(lapply(sn.sim,edge_density))
sn.sim.meanedge  <- sn.sim.meanedge [!is.na(sn.sim.meanedge )]
sn.meanedge.h <- ggplot()+aes(x=sn.sim.meanedge )+geom_histogram()+
  labs(x="edge density")+theme_bw()+geom_vline(xintercept = edge_density(sn.network),
                                               color="red",
                                               linetype="dashed",
                                               linewidth=2)

ch.sim <- vector('list',10000)
for(i in 1:10000){   
  ch.sim[[i]] <- erdos.renyi.game(
    n = gorder(ch.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
ch.sim.meanedge <- unlist(lapply(ch.sim,edge_density))
ch.sim.meanedge  <- ch.sim.meanedge [!is.na(ch.sim.meanedge )]
ch.meanedge.h <- ggplot()+aes(x=ch.sim.meanedge )+geom_histogram()+
  labs(x="edge density")+theme_bw()+geom_vline(xintercept = edge_density(ch.network),
                                               color="red",
                                               linetype="dashed",
                                               linewidth=2)

se.sim <- vector('list',10000)
for(i in 1:10000){   
  se.sim[[i]] <- erdos.renyi.game(
    n = gorder(se.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
se.sim.meanedge <- unlist(lapply(se.sim,edge_density))
se.sim.meanedge  <- se.sim.meanedge [!is.na(se.sim.meanedge )]
se.meanedge.h <- ggplot()+aes(x=se.sim.meanedge )+geom_histogram()+
  labs(x="edge density")+theme_bw()+geom_vline(xintercept = edge_density(se.network),
                                               color="red",
                                               linetype="dashed",
                                               linewidth=2)

figure.aga <- ggarrange(aga.meanedge.h,ncol = 1, nrow = 1,labels = "AGA")
edge.g <- ggarrange(figure.aga, ggarrange(ch.meanedge.h,cp.meanedge.h,liz.meanedge.h,
                                          pla.meanedge.h,se.meanedge.h,sn.meanedge.h,
                                          labels = c("CH", "CP", "LIZ", "PLA", "SE", "SN"),
                                          ncol = 2, nrow = 3,legend="top"))
edge.g + ggtitle("Edge Density")

edge_density(aga.network)
edge_density(ch.network)
edge_density(cp.network)
edge_density(liz.network)
edge_density(pla.network)
edge_density(se.network)
edge_density(sn.network)


mean(aga.sim.meanedge>.34)
mean(ch.sim.meanedge>.71)
mean(sn.sim.meanedge>.9)











#####################################
ggplot()+aes(x=ch.sim.meanedge )+geom_histogram(bins=75)+
  labs(x="edge density")+theme_bw()+
  geom_vline(xintercept = edge_density(ch.network),
             color="red",
             linetype="dashed",
             linewidth=2)




####################
#####################AVERAGE PATH LENGTH

pla.data<-read.csv("PLAnet.csv",header=TRUE,row.names = 1)
pla.matrix<-as.matrix(pla.data)
pla.network<-graph.adjacency(pla.matrix,mode="undirected",
                             diag=FALSE,weighted=T)

aga.data<-read.csv("AGAnet.csv",header=TRUE,row.names = 1)
aga.matrix<-as.matrix(aga.data)
aga.network<-graph.adjacency(aga.matrix,mode="undirected",
                             diag=FALSE,weighted = TRUE)

liz.data<-read.csv("LIZnet.csv",header=TRUE,row.names = 1)
liz.matrix<-as.matrix(liz.data)
liz.network<-graph.adjacency(liz.matrix,mode="undirected",
                             diag=FALSE,weighted=T)

se.data<-read.csv("SEnet.csv",header=TRUE,row.names = 1)
se.matrix<-as.matrix(se.data)
se.network<-graph.adjacency(se.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

ch.data<-read.csv("CHnet.csv",header=TRUE,row.names = 1)
ch.matrix<-as.matrix(ch.data)
ch.network<-graph.adjacency(ch.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

sn.data<-read.csv("SNnet.csv",header=TRUE,row.names = 1)
sn.matrix<-as.matrix(sn.data)
sn.network<-graph.adjacency(sn.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

cp.data<-read.csv("CPnet.csv",header=TRUE,row.names = 1)
cp.matrix<-as.matrix(cp.data)
cp.network<-graph.adjacency(cp.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)


pla.sim <- vector('list',10000)
for(i in 1:10000){   
  pla.sim[[i]] <- erdos.renyi.game(
    n = gorder(pla.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
pla.sim.meand <- unlist(lapply(pla.sim,mean_distance))
pla.sim.meand <- pla.sim.meand[!is.na(pla.sim.meand)]
pla.meand.h <- ggplot()+aes(x=pla.sim.meand)+geom_histogram()+
  labs(x="average path length")+theme_bw()+geom_vline(xintercept = mean_distance(pla.network,weights=NA),
                                                      color="red",
                                                      linetype="dashed",
                                                      linewidth=2)

aga.sim <- vector('list',10000)
for(i in 1:10000){   
  aga.sim[[i]] <- erdos.renyi.game(
    n = gorder(aga.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
aga.sim.meand <- unlist(lapply(aga.sim,mean_distance))
aga.sim.meand <- aga.sim.meand[!is.na(aga.sim.meand)]
aga.meand.h <- ggplot()+aes(x=aga.sim.meand)+geom_histogram()+
  labs(x="average path length")+theme_bw()+geom_vline(xintercept = mean_distance(aga.network,weights = NA),
                                                      color="red",
                                                      linetype="dashed",
                                                      linewidth=2)

liz.sim <- vector('list',10000)
for(i in 1:10000){   
  liz.sim[[i]] <- erdos.renyi.game(
    n = gorder(liz.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
liz.sim.meand <- unlist(lapply(liz.sim,mean_distance))
liz.sim.meand <- liz.sim.meand[!is.na(liz.sim.meand)]
liz.meand.h <- ggplot()+aes(x=liz.sim.meand)+geom_histogram()+
  labs(x="average path length")+theme_bw()+geom_vline(xintercept = mean_distance(liz.network,weights = NA),
                                                      color="red",
                                                      linetype="dashed",
                                                      linewidth=2)

cp.sim <- vector('list',10000)
for(i in 1:10000){   
  cp.sim[[i]] <- erdos.renyi.game(
    n = gorder(cp.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
cp.sim.meand <- unlist(lapply(cp.sim,mean_distance))
cp.sim.meand  <- cp.sim.meand [!is.na(cp.sim.meand )]
cp.meand.h <- ggplot()+aes(x=cp.sim.meand )+geom_histogram()+
  labs(x="average path length")+theme_bw()+geom_vline(xintercept = mean_distance(cp.network,weights = NA),
                                                      color="red",
                                                      linetype="dashed",
                                                      linewidth=2)

sn.sim <- vector('list',10000)
for(i in 1:10000){   
  sn.sim[[i]] <- erdos.renyi.game(
    n = gorder(sn.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
sn.sim.meand <- unlist(lapply(sn.sim,mean_distance))
sn.sim.meand  <- sn.sim.meand [!is.na(sn.sim.meand )]
sn.meand.h <- ggplot()+aes(x=sn.sim.meand )+geom_histogram()+
  labs(x="average path length")+theme_bw()+geom_vline(xintercept = mean_distance(sn.network,weights = NA),
                                                      color="red",
                                                      linetype="dashed",
                                                      linewidth=2)

ch.sim <- vector('list',10000)
for(i in 1:10000){   
  ch.sim[[i]] <- erdos.renyi.game(
    n = gorder(ch.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
ch.sim.meand <- unlist(lapply(ch.sim,mean_distance))
ch.sim.meand  <- ch.sim.meand [!is.na(ch.sim.meand )]
ch.meand.h <- ggplot()+aes(x=ch.sim.meand )+geom_histogram()+
  labs(x="average path length")+theme_bw()+geom_vline(xintercept = mean_distance(ch.network,weights = NA),
                                                      color="red",
                                                      linetype="dashed",
                                                      linewidth=2)

se.sim <- vector('list',10000)
for(i in 1:10000){   
  se.sim[[i]] <- erdos.renyi.game(
    n = gorder(se.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
se.sim.meand <- unlist(lapply(se.sim,mean_distance))
se.sim.meand  <- se.sim.meand [!is.na(se.sim.meand )]
se.meand.h <- ggplot()+aes(x=se.sim.meand )+geom_histogram()+
  labs(x="average path length")+theme_bw()+geom_vline(xintercept = mean_distance(se.network,weights = NA),
                                                      color="red",
                                                      linetype="dashed",
                                                      linewidth=2)

figure.aga <- ggarrange(aga.meand.h,ncol = 1, nrow = 1,labels = "AGA")
apl.g <- ggarrange(figure.aga, ggarrange(ch.meand.h,cp.meand.h,liz.meand.h,pla.meand.h,se.meand.h,sn.meand.h,
                                labels = c("CH", "CP", "LIZ", "PLA", "SE", "SN"),
                                ncol = 2, nrow = 3,legend="right"))

apl.g


# Calculate the proportion of graphs with an average path length lower than our observed
mean(se.sim.meand < mean_distance(se.network,weights = NA))
mean(aga.sim.meand < mean_distance(aga.network,weights = NA))
mean(ch.sim.meand < mean_distance(ch.network,weights = NA))
mean(cp.sim.meand < mean_distance(cp.network,weights = NA))
mean(liz.sim.meand < mean_distance(liz.network,weights = NA))
mean(pla.sim.meand < mean_distance(pla.network,weights = NA))
mean(se.sim.meand < mean_distance(pla.network,weights = NA))




#Great work! As you can see, 
#the Forrest Gump network is far 
#more interconnected than we would 
#expect by chance as zero random 
#networks have an average path
#length smaller than the Forrest
#Gump network's average path length.
#como el valor mínimo es 1 lo de arriba calcula la proporción
#de sim distances menor que 1.1

pla.data<-read.csv("PLAnet.csv",header=TRUE,row.names = 1)
pla.matrix<-as.matrix(pla.data)
pla.network<-graph.adjacency(pla.matrix,mode="undirected",
                             diag=FALSE,weighted=T)

aga.data<-read.csv("AGAnet.csv",header=TRUE,row.names = 1)
aga.matrix<-as.matrix(aga.data)
aga.network<-graph.adjacency(aga.matrix,mode="undirected",
                             diag=FALSE,weighted = TRUE)

liz.data<-read.csv("LIZnet.csv",header=TRUE,row.names = 1)
liz.matrix<-as.matrix(liz.data)
liz.network<-graph.adjacency(liz.matrix,mode="undirected",
                             diag=FALSE,weighted=T)

se.data<-read.csv("SEnet.csv",header=TRUE,row.names = 1)
se.matrix<-as.matrix(se.data)
se.network<-graph.adjacency(se.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

ch.data<-read.csv("CHnet.csv",header=TRUE,row.names = 1)
ch.matrix<-as.matrix(ch.data)
ch.network<-graph.adjacency(ch.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

sn.data<-read.csv("SNnet.csv",header=TRUE,row.names = 1)
sn.matrix<-as.matrix(sn.data)
sn.network<-graph.adjacency(sn.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

cp.data<-read.csv("CPnet.csv",header=TRUE,row.names = 1)
cp.matrix<-as.matrix(cp.data)
cp.network<-graph.adjacency(cp.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)


pla.sim <- vector('list',10000)
for(i in 1:10000){   
  pla.sim[[i]] <- erdos.renyi.game(
    n = gorder(pla.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
pla.sim.transi <- unlist(lapply(pla.sim,transitivity))
pla.sim.transi <- pla.sim.transi[!is.na(pla.sim.transi)]
pla.transi.h <- ggplot()+aes(x=pla.sim.transi)+geom_histogram()+
  labs(x="clustering coefficient")+theme_bw()+geom_vline(xintercept = transitivity(pla.network),
                                                         color="red",
                                                         linetype="dashed",
                                                         linewidth=2)

aga.sim <- vector('list',10000)
for(i in 1:10000){   
  aga.sim[[i]] <- erdos.renyi.game(
    n = gorder(aga.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
aga.sim.transi <- unlist(lapply(aga.sim,transitivity))
aga.sim.transi <- aga.sim.transi[!is.na(aga.sim.transi)]
aga.transi.h <- ggplot()+aes(x=aga.sim.transi)+geom_histogram()+
  labs(x="clustering coefficient")+theme_bw()+geom_vline(xintercept = transitivity(aga.network),
                                                         color="red",
                                                         linetype="dashed",
                                                         linewidth=2)
mean(aga.sim.transi>.55)


liz.sim <- vector('list',10000)
for(i in 1:10000){   
  liz.sim[[i]] <- erdos.renyi.game(
    n = gorder(liz.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
liz.sim.transi <- unlist(lapply(liz.sim,transitivity))
liz.sim.transi <- liz.sim.transi[!is.na(liz.sim.transi)]
liz.transi.h <- ggplot()+aes(x=liz.sim.transi)+geom_histogram()+
  labs(x="clustering coefficient")+theme_bw()+geom_vline(xintercept = transitivity(liz.network),
                                                         color="red",
                                                         linetype="dashed",
                                                         linewidth=2)

cp.sim <- vector('list',10000)
for(i in 1:10000){   
  cp.sim[[i]] <- erdos.renyi.game(
    n = gorder(cp.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
cp.sim.transi <- unlist(lapply(cp.sim,transitivity))
cp.sim.transi  <- cp.sim.transi [!is.na(cp.sim.transi )]
cp.transi.h <- ggplot()+aes(x=cp.sim.transi )+geom_histogram()+
  labs(x="clustering coefficient")+theme_bw()+geom_vline(xintercept = transitivity(cp.network),
                                                         color="red",
                                                         linetype="dashed",
                                                         linewidth=2)

sn.sim <- vector('list',10000)
for(i in 1:10000){   
  sn.sim[[i]] <- erdos.renyi.game(
    n = gorder(sn.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
sn.sim.transi <- unlist(lapply(sn.sim,transitivity))
sn.sim.transi  <- sn.sim.transi [!is.na(sn.sim.transi )]
sn.transi.h <- ggplot()+aes(x=sn.sim.transi )+geom_histogram()+
  labs(x="clustering coefficient")+theme_bw()+geom_vline(xintercept = transitivity(sn.network),
                                                         color="red",
                                                         linetype="dashed",
                                                         linewidth=2)

ch.sim <- vector('list',10000)
for(i in 1:10000){   
  ch.sim[[i]] <- erdos.renyi.game(
    n = gorder(ch.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
ch.sim.transi <- unlist(lapply(ch.sim,transitivity))
ch.sim.transi  <- ch.sim.transi [!is.na(ch.sim.transi )]
ch.transi.h <- ggplot()+aes(x=ch.sim.transi )+geom_histogram()+
  labs(x="clustering coefficient")+theme_bw()+geom_vline(xintercept = transitivity(ch.network),
                                                         color="red",
                                                         linetype="dashed",
                                                         linewidth=2)

se.sim <- vector('list',10000)
for(i in 1:10000){   
  se.sim[[i]] <- erdos.renyi.game(
    n = gorder(se.network),   
    p.or.m = runif(1),  # the prob of interacting with anyone is uniform from 0 to 1
    type = "gnp")
}
se.sim.transi <- unlist(lapply(se.sim,transitivity))
se.sim.transi  <- se.sim.transi [!is.na(se.sim.transi )]
se.transi.h <- ggplot()+aes(x=se.sim.transi )+geom_histogram()+
  labs(x="clustering coefficient")+theme_bw()+geom_vline(xintercept = transitivity(se.network),
                                                         color="red",
                                                         linetype="dashed",
                                                         linewidth=2)

figure.aga <- ggarrange(aga.transi.h,ncol = 1, nrow = 1,labels = "AGA")
clu.g <- ggarrange(figure.aga, ggarrange(ch.transi.h,cp.transi.h,liz.transi.h,pla.transi.h,se.transi.h,sn.transi.h,
                                labels = c("CH", "CP", "LIZ", "PLA", "SE", "SN"),
                                ncol = 2, nrow = 3,legend="right"))

clu.g

transitivity(aga.network)
transitivity(ch.network)
transitivity(cp.network)
transitivity(pla.network)
transitivity(se.network)
transitivity(sn.network)


mean(aga.sim.transi>.55)
mean(ch.sim.transi>.83)
mean(cp.sim.transi>.74)
mean(sn.sim.transi>.9)



###############

ggarrange(edge.g,apl.g,clu.g,
        labels = c("a", "b", "c"),
      ncol = 1, nrow = 3,legend="right")



###########
ggarrange(edge.g, apl.g, clu.g,
          labels = c("edge density", 
                     "average path length",
                     "clustering coefficient"),
          ncol = 1, nrow = 3,
          common.legend = TRUE,
          legend = "right")


###################

library(ggpubr)

# Create plots with titles
edge_plot <- edge.g + labs(title = "Edge Density")
apl_plot <- apl.g + labs(title = "Average Path Length")
clu_plot <- clu.g + labs(title = "Clustering Coefficient")

# Arrange plots
ggarrange(edge_plot, apl_plot, clu_plot, ncol = 1, nrow = 3)


#############################
library(cowplot)

# Create individual titles for the plots
titles <- c("Edge Density", "Average Path Length", "Clustering Coefficient")

# Combine plots with titles vertically
combined_plots <- plot_grid(edge.g + ggtitle(titles[1]),
                            apl.g + ggtitle(titles[2]),
                            clu.g + ggtitle(titles[3]),
                            ncol = 1, align = "v", axis = "l")

# Print the combined plot
print(combined_plots)



###################################
library(patchwork)

(edge.g + plot_annotation(title = 'a) Edge Density')) /
  (apl.g + plot_annotation(title = 'b) Average Path Length')) /
  (clu.g + plot_annotation(title = 'c) Clustering Coefficient'))

###########
# Create a data frame with a single row to use for plotting
df <- data.frame(x = 0, y = 0)

# Create plots with just titles
title_edge <- ggplot(df, aes(x, y)) +
  geom_blank() +
  labs(title = "a) Edge Density") +
  theme_void() +  # Remove axis and background
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),  # Reduce margin
        plot.title = element_text(size = 14, face = "bold"))  # Increase font size and make it bold

title_apl <- ggplot(df, aes(x, y)) +
  geom_blank() +
  labs(title = "b) Average Path Length") +
  theme_void() +  # Remove axis and background
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),  # Reduce margin
        plot.title = element_text(size = 14, face = "bold"))  # Increase font size and make it bold

title_clu <- ggplot(df, aes(x, y)) +
  geom_blank() +
  labs(title = "c) Clustering Coefficient") +
  theme_void() +  # Remove axis and background
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),  # Reduce margin
        plot.title = element_text(size = 14, face = "bold"))  # Increase font size and make it bold

# Arrange plots
ggarrange(title_edge, edge.g, title_apl, apl.g, title_clu, clu.g,
          ncol = 1, nrow = 6, heights = c(0.2, 1, 0.2, 1, 0.2, 1))
