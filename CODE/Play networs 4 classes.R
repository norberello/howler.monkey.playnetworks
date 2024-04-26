####hemen networks eta glmms daude



rm(list=ls())

setwd("~/Documents/R scripts and data/Play networks")
pla.data<-read.csv("PLAnet.csv",header=TRUE,row.names = 1)
head(pla.data)


pla.matrix<-as.matrix(pla.data)
library(igraph)
pla.network2<-graph.adjacency(pla.matrix,mode="undirected",
                              diag=FALSE,weighted = TRUE)
#naming vertices, class of individuals (name is by default entered)

V(pla.network2)$name

V(pla.network2)$class<- c("adult male", "adult male",
                          "adult female", "adult female",
                          "juvenile", "infant", "infant")

V(pla.network2)$color=V(pla.network2)$class #assign the "Sex" attribute as the vertex color
V(pla.network2)$color=gsub("adult female","purple",V(pla.network2)$color) #Females will be red
V(pla.network2)$color=gsub("adult male","black",V(pla.network2)$color) #Males will be blue
V(pla.network2)$color=gsub("juvenile","yellow",V(pla.network2)$color) #Males will be blue
V(pla.network2)$color=gsub("infant","green",V(pla.network2)$color) #Males will be blue


pla.clust <- cluster_louvain(pla.network2)

plot(pla.network2,
     main="PLA",
     vertex.size=5+eigen_centrality(pla.network2)$vector*15,
     vertex.label.dist=1.5,
     edge.width=E(pla.network2)$weight/2,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(pla.clust),
     vertex.label=NA)

###############AGA

aga.data<-read.csv("AGAnet.csv",header=TRUE,row.names = 1)
aga.matrix<-as.matrix(aga.data)

aga.network<-graph.adjacency(aga.matrix,mode="undirected",
                             diag=FALSE,weighted = TRUE)

V(aga.network)$name

V(aga.network)$class<- c("adult female", "adult female", "subadult female", "adult female",
                         "adult female", "adult female", "adult female", "adult female",
                         "subadult female", "adult female", "adult female", "adult female",
                         "adult female", "adult female", 
                         "adult male","adult male","adult male","adult male",
                         "adult male","adult male","adult male","adult male",
                         "adult male","adult male","adult male","adult male",
                         "infant", "infant", "infant","juvenile",
                         "infant", "juvenile", "juvenile","juvenile",
                         "infant", "infant", "juvenile","juvenile",
                         "infant","juvenile")

V(aga.network)$class#check

V(aga.network)$color=as.factor(V(aga.network)$class) #assign the "Sex" attribute as the vertex color
V(aga.network)$color=gsub("1","red",V(aga.network)$color) 
V(aga.network)$color=gsub("5","orange",V(aga.network)$color)
V(aga.network)$color=gsub("2","black",V(aga.network)$color) 
V(aga.network)$color=gsub("4","yellow",V(aga.network)$color) 
V(aga.network)$color=gsub("3","green",V(aga.network)$color) 

aga.network <- delete.vertices(aga.network,6)#remove nueva




aga.clust <- cluster_louvain(aga.network)

plot(aga.network,mark.groups=communities(aga.clust),
     vertex.size=3+eigen_centrality(aga.network)$vector*10,
     vertex.label.dist=0.75,
     vertex.label=NA,
     edge.width=E(aga.network)$weight/2,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     main="AGA")

#####
#####LIZ

liz.data<-read.csv("LIZnet.csv",header=TRUE,row.names = 1)
liz.matrix<-as.matrix(liz.data)

liz.network<-graph.adjacency(liz.matrix,mode="undirected",
                             diag=FALSE,weighted = TRUE)

V(liz.network)$name
V(liz.network)$class<- c("adult female", "adult female", "adult female", "adult female",
                         "adult male", "juvenile")



V(liz.network)$color=V(liz.network)$class #assign the "Sex" attribute as the vertex color
V(liz.network)$color=gsub("adult female","red",V(liz.network)$color) #Females will be red
V(liz.network)$color=gsub("adult male","black",V(liz.network)$color) #Males will be blue
V(liz.network)$color=gsub("juvenile","yellow",V(liz.network)$color) #Immature will be blue

liz.clust <- cluster_louvain(liz.network)


####################### this one, fast and greedy #################
plot(liz.network,
     main="LIZ",
     vertex.size=5+eigen_centrality(liz.network)$vector*15,
     vertex.label.dist=2,
     vertex.label=NA,
     edge.width=E(liz.network)$weight/2,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(liz.clust))



#######
#####SE
se.data<-read.csv("SEnet.csv",header=TRUE,row.names = 1)
se.matrix<-as.matrix(se.data)

se.network<-graph.adjacency(se.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)


V(se.network)$class<- c("adult female", "adult female",
                        "adult female", "adult male")
V(se.network)$color=V(se.network)$class #assign the "Sex" attribute as the vertex color
V(se.network)$color=gsub("adult female","red",V(se.network)$color) #Females will be red
V(se.network)$color=gsub("adult male","black",V(se.network)$color) #Males will be blue

se.clust <- cluster_louvain(se.network)


####################### this one, fast and greedy #################
plot(se.network,
     main="SE",
     vertex.size=5+eigen_centrality(se.network)$vector*15,
     vertex.label.dist=2,
     vertex.label=NA,
     edge.width=E(se.network)$weight*5,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(se.clust))

##############CH
###CH
#####CH
ch.data<-read.csv("CHnet.csv",header=TRUE,row.names = 1)
ch.matrix<-as.matrix(ch.data)

ch.network<-graph.adjacency(ch.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

V(ch.network)$name


V(ch.network)$class<- c("adult female", "adult female", 
                        "adult female", "adult female",
                        "adult male","adult male","adult male",
                        "subadult male","infant","juvenile",
                        "juvenile","juvenile","juvenile")

V(ch.network)$color=as.factor(V(ch.network)$class) #assign the "Sex" attribute as the vertex color


V(ch.network)$color=gsub("1","red",V(ch.network)$color) #Females will be red
V(ch.network)$color=gsub("5","brown",V(ch.network)$color) #Males will be blue
V(ch.network)$color=gsub("2","black",V(ch.network)$color) #Males will be blue
V(ch.network)$color=gsub("3","yellow",V(ch.network)$color)
V(ch.network)$color=gsub("4","green",V(ch.network)$color)


ch.clust <- cluster_louvain(ch.network)


####################### this one, fast and greedy #################
plot(ch.network,
     main="CH",
     vertex.size=5+eigen_centrality(ch.network)$vector*20,
     vertex.label.dist=2,
     vertex.label=NA,
     edge.width=E(ch.network)$weight/2,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(ch.clust))

###############
#####SN
sn.data<-read.csv("SNnet.csv",header=TRUE,row.names = 1)
sn.matrix<-as.matrix(sn.data)

sn.network<-graph.adjacency(sn.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

V(sn.network)$class<- c("adult female", "adult female", "subadult female", 
                        "adult male",
                        "adult male","infant","juvenile")
V(sn.network)$name

V(sn.network)$color=as.factor(V(sn.network)$class) #assign the "Sex" attribute as the vertex color
V(sn.network)$color=gsub("1","red",V(sn.network)$color) #Females will be red
V(sn.network)$color=gsub("5","orange",V(sn.network)$color) #Females will be red
V(sn.network)$color=gsub("2","black",V(sn.network)$color) #Males will be blue
V(sn.network)$color=gsub("3","yellow",V(sn.network)$color)
V(sn.network)$color=gsub("4","green",V(sn.network)$color)



sn.clust <- cluster_louvain(sn.network)


####################### this one, fast and greedy #################
plot(sn.network,
     main="SN",
     vertex.size=5+eigen_centrality(sn.network)$vector*15,
     vertex.label.dist=2,
     vertex.label=NA,
     edge.width=E(sn.network)$weight/4,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(sn.clust))



#######################################
######################################

#CP
#CP
#####
cp.data<-read.csv("CPnet.csv",header=TRUE,row.names = 1)
cp.matrix<-as.matrix(cp.data)

cp.network<-graph.adjacency(cp.matrix,mode="undirected",
                            diag=FALSE,weighted = TRUE)

V(cp.network)$name

V(cp.network)$class<- c("adult female", "subadult female", "subadult male", "adult female",
                        "adult female","adult female","adult female","adult male","adult male",
                        "infant","infant","infant","juvenile")

V(cp.network)$color=as.factor(V(cp.network)$class) #assign the "Sex" attribute as the vertex color
V(cp.network)$color

V(cp.network)$color=gsub("1","red",V(cp.network)$color) #Females will be red
V(cp.network)$color=gsub("6","orange",V(cp.network)$color) #Females will be red
V(cp.network)$color=gsub("7","brown",V(cp.network)$color) #Females will be red
V(cp.network)$color=gsub("2","black",V(cp.network)$color) #Males will be blue
V(cp.network)$color=gsub("4","green",V(cp.network)$color)
V(cp.network)$color=gsub("5","yellow",V(cp.network)$color)

cp.clust <- cluster_louvain(cp.network)


####################### this one, fast and greedy #################
plot(cp.network,
     main="CP",
     vertex.size=5+eigen_centrality(cp.network)$vector*20,
     vertex.label.dist=2,
     vertex.label=NA,
     edge.width=E(cp.network)$weight/2,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(cp.clust))

#############################################################################
#######################################
##########ALL THE 7, alphabetically
########****************************

par(mar = c(0, 0, 1.5, 0) + 0.0)
b <- layout(matrix(c(1,1,1,1,2,2,3,3,4,4,
                     1,1,1,1,2,2,3,3,4,4,
                     1,1,1,1,5,5,6,6,7,7,
                     1,1,1,1,5,5,6,6,7,7
), 4, 10, byrow = TRUE))
b
layout.show(7)

plot(aga.network,mark.groups=communities(aga.clust),
     vertex.size=3+eigen_centrality(aga.network)$vector*10,
     vertex.label.dist=0.75,
     vertex.label=NA,
     edge.width=E(aga.network)$weight/2,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA)

legend("topright",
       title = "communities",
       legend = c(""," "," "," "),
       fill = c("pink","light blue","lightgreen","darkorchid1"),       # Color of the squares
       border = c("red","blue","darkgreen","purple"),
       bty = "n",cex=2.5,
       horiz=T) # Colr of the border of the squares

legend("topleft", 
       legend=c("infant",
                            "juvenile",
                            "subadult female",
                            "subadult male",
                            "adult female",
                            "adult male"),
       col=c("green", "yellow","orange","brown","red","black"),
       pch = c(19, 19, 19,19,19,19),cex=2.5,
       bty = "n")
legend("top",
       title="",
       legend=c("low centrality",
                               "medium centrality",
                               "high centrality"),
       pt.cex=c(1.5,3,5),
       pch = c(19, 19, 19),
       bty = "n",
       cex=2)

legend("left",as.expression(bquote(bold("AGA"))),
       bty="n",
       cex=1.25)


plot(ch.network,
     main="CH",
     vertex.size=5+eigen_centrality(ch.network)$vector*20,
     vertex.label.dist=2,
     vertex.label=NA,
     edge.width=E(ch.network)$weight/8,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(ch.clust))

plot(cp.network,
     main="CP",
     vertex.size=5+eigen_centrality(cp.network)$vector*20,
     vertex.label.dist=2,
     vertex.label=NA,
     edge.width=E(cp.network)$weight/7,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(cp.clust))

plot(liz.network,
     main="LIZ",
     vertex.size=5+eigen_centrality(liz.network)$vector*15,
     vertex.label.dist=2,
     vertex.label=NA,
     edge.width=E(liz.network)$weight/7,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(liz.clust))

plot(pla.network2,
     main="PLA",
     vertex.size=5+eigen_centrality(pla.network2)$vector*15,
     vertex.label.dist=1.5,
     edge.width=E(pla.network2)$weight/10,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(pla.clust),
     vertex.label=NA)


plot(se.network,
     main="SE",
     vertex.size=5+eigen_centrality(se.network)$vector*15,
     vertex.label.dist=2,
     vertex.label=NA,
     edge.width=E(se.network)$weight,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(se.clust))


plot(sn.network,
     main="SN",
     vertex.size=5+eigen_centrality(sn.network)$vector*15,
     vertex.label.dist=2,
     vertex.label=NA,
     edge.width=E(sn.network)$weight/4,
     edge.curved=F,
     edge.color="gray",
     vertex.frame.color=NA,
     mark.groups=communities(sn.clust))


#################################################################
#################################################################

class <- c(
  V(aga.network)$class,
  V(ch.network)$class,
  V(cp.network)$class,
  V(liz.network)$class,
  V(pla.network2)$class,
  V(se.network)$class,
  V(sn.network)$class
)

str(class)

eigen <- c(
  eigen_centrality(aga.network)$vector,
  eigen_centrality(ch.network)$vector,
  eigen_centrality(cp.network)$vector,
  eigen_centrality(liz.network)$vector,
  eigen_centrality(pla.network2)$vector,
  eigen_centrality(se.network)$vector,
  eigen_centrality(sn.network)$vector
)

str(eigen)

V(aga.network)$degree <- degree(aga.network)
V(ch.network)$degree <- degree(ch.network)
V(cp.network)$degree <- degree(cp.network)
V(liz.network)$degree <- degree(liz.network)
V(pla.network2)$degree <- degree(pla.network2)
V(se.network)$degree <- degree(se.network)
V(sn.network)$degree <- degree(sn.network)

degree <- c(
  V(aga.network)$degree,
  V(ch.network)$degree,
  V(cp.network)$degree,
  V(liz.network)$degree,
  V(pla.network2)$degree,
  V(se.network)$degree,
  V(sn.network)$degree
)

str(degree)

#betweeness
V(aga.network)$btw <- betweenness(aga.network,normalized=T)
V(ch.network)$btw <- betweenness(ch.network,normalized=T)
V(cp.network)$btw <- betweenness(cp.network,normalized=T)
V(liz.network)$btw <- betweenness(liz.network,normalized=T)
V(pla.network2)$btw <- betweenness(pla.network2,normalized=T)
V(se.network)$btw <- betweenness(se.network,normalized=T)
V(sn.network)$btw <- betweenness(sn.network,normalized=T)

btw <- c(
  V(aga.network)$btw,
  V(ch.network)$btw,
  V(cp.network)$btw,
  V(liz.network)$btw,
  V(pla.network2)$btw,
  V(se.network)$btw,
  V(sn.network)$btw
)

str(btw)

#closeness
V(aga.network)$close <- closeness(aga.network)
V(ch.network)$close <- closeness(ch.network)
V(cp.network)$close <- closeness(cp.network)
V(liz.network)$close <- closeness(liz.network)
V(pla.network2)$close <- closeness(pla.network2)
V(se.network)$close <- closeness(se.network)
V(sn.network)$close <- closeness(sn.network)

close <- c(
  V(aga.network)$close,
  V(ch.network)$close,
  V(cp.network)$close,
  V(liz.network)$close,
  V(pla.network2)$close,
  V(se.network)$close,
  V(sn.network)$close
)


str(close)


av.players <- c(
rep(gorder(aga.network),gorder(aga.network)),
rep(gorder(ch.network),gorder(ch.network)),
rep(gorder(cp.network),gorder(cp.network)),
rep(gorder(liz.network),gorder(liz.network)),
rep(gorder(pla.network2),gorder(pla.network2)),
rep(gorder(se.network),gorder(se.network)),
rep(gorder(sn.network),gorder(sn.network))
)

str(av.players)

group.name <- c(
  rep("aga",gorder(aga.network)),
  rep("ch",gorder(ch.network)),
  rep("cp",gorder(cp.network)),
  rep("liz",gorder(liz.network)),
  rep("pla",gorder(pla.network2)),
  rep("se",gorder(se.network)),
  rep("sn",gorder(sn.network))
)

str(group.name)

im.ad.r <- c(
  rep(19/40,gorder(aga.network)),#check, cause the availability changes, but not sure if fairer like this
  rep(0.625,gorder(ch.network)),
  rep(0.44,gorder(cp.network)),
  rep(0.2,gorder(liz.network)),
  rep(0.75,gorder(pla.network2)),
  rep(0,gorder(se.network)),
  rep(0.5,gorder(sn.network))
)

str(im.ad.r)

obs.sec<- c(
  c(496800,118800,331200,136800,496800,303300,511200,612000,208800,212400,136800,187200,198000,#aga females
    176040,75600,165600,252000,338400,342000,288000,313200,374400,378000,324000,421200,#males
    416800,416800,416800,108800,187200,496800,303300,612000,612000,208800,208800,212400,187200,198000#immatures
    ),#AGA
  c(984960,984960,984960,984960,984960,984960,984960,984960,984960,522240,522240,522240,522240),
  rep(746100,gorder(cp.network)),#all the same obs time
  c(1000440,1000440,1000440,1000440,1000440,750330),
  rep(1000440,gorder(pla.network2)),
  rep(394200,gorder(se.network)),
  rep(1000440,gorder(sn.network))
)

str(obs.sec)

play.sec <- c(
  c(12705,2043,3990,1770,2250,1310,2130,7920,2655,2790,2580,2425,4200,#aga females
    725,360,665,1400,677,3383,2515,2625,530,1615,1550,1845,#males		
    770,5030,1020,120,1260,19925,8340,14247,735,5485,4590,2350,180,1010#immatures
    ),#AGA
  c(4247,7751,1744,4147,2849,3027,1483,1744,11145,9418,12538,8096,13960),#CH
  c(988,3762,1357,1591,1594,907,3148,1084,3897,19881,7246,17070,17660),#CP
  c(7880,7185,1935,3360,2855,6496),#LIZ
  c(3420,2255,10000,12435,37578,36765,29062),#PLA
  c(1000,953,617,539),#SE
  c(1403,974,4302,10303,648,2828,13649)#SN
)

str(play.sec)

eigen.df <- data.frame(
  age.sex.class=class,
  group.name,
  eigen,
  degree,
  play.sec,
  obs.sec,
  im.ad.r,
  av.players,
  close,
  btw
)
head(eigen.df)
str(eigen.df)

eigen.df$age.sex.class2 <- eigen.df$age.sex.class

eigen.df$age.sex.class2<-replace(eigen.df$age.sex.class2, eigen.df$age.sex.class2=="subadult female", "adult female")
eigen.df$age.sex.class2<-replace(eigen.df$age.sex.class2, eigen.df$age.sex.class2=="subadult male", "adult male")
eigen.df$age.sex.class2<-replace(eigen.df$age.sex.class2, eigen.df$age.sex.class2=="infant", "immature")
eigen.df$age.sex.class2<-replace(eigen.df$age.sex.class2, eigen.df$age.sex.class2=="juvenile", "immature")

table(eigen.df$age.sex.class2)
boxplot(eigen~age.sex.class2,eigen.df)


levels(eigen.df$age.sex.class)
eigen.df$age.sex.class <- factor(eigen.df$age.sex.class,
                                 levels=c('infant',"juvenile",'subadult female',
                                          'subadult male',
                                          "adult female","adult male"))


eigen.df$play.rate <- (eigen.df$play.sec/eigen.df$obs.sec)*3600#secods per hr

eigen.df$prop.players <- eigen.df$degree/eigen.df$av.players

################################
######it is not about available players is about players the id did not play with!!!!!!


eigen.df$num.not.players <- eigen.df$av.players-eigen.df$degree
hist(eigen.df$num.not.players)

################
#write.csv(eigen.df,"eigen.df.csv")
######################################HERE FOR ANALYSES





# Save an object to a file
#saveRDS(eigen.df, file = "eigen.df.rds")


#############ready**********************
##################################################
##################################################
#################################################
##################################################
###################################################
###################################################
###################################################

# Restore the object
eigen.df <- readRDS(file = "eigen.df.rds")
head(eigen.df)
table(eigen.df$age.sex.class2)

hist(eigen.df$play.rate)
hist(eigen.df$btw)

boxplot(eigen~age.sex.class,eigen.df,
        col=c("green","yellow","orange","brown","red","grey"),
        ylab="eigenvector centrality",
        outline=FALSE,xaxt="n",
        cex.text=1.5,
        xlab="",
        varwidth = TRUE,
        cex.lab = 1.5)

means1 <- tapply(eigen.df$eigen,eigen.df$age.sex.class,mean)
points(means1,pch=3,
       cex=1.5)


legend("top",
       title = "",
       fill = c("green","yellow","orange","brown","red","grey"),
       legend = c("infant","juvenile","subadult female","subadult male",
                  "adult female","adult male"),       # Color of the squares
       border = c("black"),
       bty = "n",cex=1.5) # Color of the border of the squares

library(ggplot2)

ggplot(eigen.df,aes(age.sex.class,eigen,fill=age.sex.class))+
  geom_boxplot()+theme_bw()+ 
  scale_fill_manual(values=c("green","yellow","orange","brown","red","grey"))+
  theme(legend.position="top")+
  ylab("eigenvector centrality")+
theme(legend.position="top")+
  ylab("eigenvector centrality")+
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 3,
  )

ggplot(eigen.df,aes(age.sex.class,close,fill=age.sex.class))+
  geom_boxplot()+theme_bw()+ 
  scale_fill_manual(values=c("green","yellow","orange","brown","red","grey"))+
  theme(legend.position="top")+
  ylab("eigenvector centrality")+
  theme(legend.position="top")+
  ylab("eigenvector centrality")+
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 3,
  )

ggplot(eigen.df,aes(age.sex.class,btw,fill=age.sex.class))+
  geom_boxplot()+theme_bw()+ 
  scale_fill_manual(values=c("green","yellow","orange","brown","red","grey"))+
  theme(legend.position="top")+
  ylab("eigenvector centrality")+
  theme(legend.position="top")+
  ylab("eigenvector centrality")+
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 3,
  )





ggplot(eigen.df,aes(age.sex.class,play.rate,fill=age.sex.class))+
  geom_boxplot()+theme_bw()+ 
  scale_fill_manual(values=c("green","yellow","orange","brown","red","grey"))+
  theme(legend.position="top")+
  ylab("play rate (sc per h)")+
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 3,
  )



eigen.df$age.sex.class2 <- factor(eigen.df$age.sex.class2,                                    # Change ordering manually
                  levels = c("immature", "adult female", "adult male"))

eigen.fig <- ggplot(eigen.df,aes(age.sex.class2,eigen,fill=age.sex.class2))+
  geom_boxplot(alpha=.4)+
  scale_fill_manual(values=c("yellow","red","grey"))+
  ylab("eigenvector centrality")+
  xlab("age-sex class")+
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 3,
  )+theme_bw()+theme(legend.position="none")  
  
eigen.fig
  

play.fig <- ggplot(eigen.df,aes(age.sex.class2,play.rate,fill=age.sex.class2))+
  geom_boxplot(alpha=.4)+
  scale_fill_manual(values=c("yellow","red","grey"))+
  ylab("play effort (sec per hr)")+
  xlab("age-sex class")+
stat_summary(
  geom = "point",
  fun = "mean",
  size = 3,
  shape = 3,
)+theme_bw()+theme(legend.position="none") 


play.fig


ggarrange(play.fig,eigen.fig,
          labels = c("a", "b"),
          ncol = 2, nrow = 1)


















boxplot(play.rate~age.sex.class,eigen.df,
        col=c("green","yellow","orange","brown","red","grey"),
        ylab="eigenvector centrality",
        outline=FALSE,xaxt="n",
        cex.text=1.5,
        xlab="",
        varwidth = TRUE,
        cex.lab = 1.5)

means1 <- tapply(eigen.df$play.rate,eigen.df$age.sex.class,mean)
points(means1,pch=3,
       cex=1.5)

legend("top",
       title = "",
       fill = c("green","yellow","orange","brown","red","grey"),
       legend = c("infant","juvenile","subadult female","subadult male",
                  "adult female","adult male"),       # Color of the squares
       border = c("black"),
       bty = "n",cex=1.5) # Color of the border of the squares

boxplot(eigen~group.name,eigen.df)
boxplot(eigen~age.sex.class2,eigen.df)
boxplot(prop.players~age.sex.class2,eigen.df)
boxplot(prop.players~age.sex.class,eigen.df)



plot(eigen.df$eigen,eigen.df$prop.players)

fit <- lm(prop.players~poly(eigen,2,raw=TRUE),eigen.df)
plot(prop.players~eigen,eigen.df)
curve(predict(fit,newdata=data.frame(eigen=x)),add=T)

library(ggplot2)
ggplot(eigen.df, aes(x=eigen, y=prop.players))+
  geom_point()+
  stat_smooth(se=T, method='lm', formula=y~poly(x,2))

ggplot(eigen.df, aes(x=play.rate, y=prop.players))+
  geom_point()+
  stat_smooth(se=T, method='lm', formula=y~poly(x,2))

ggplot(eigen.df, aes(x=av.players, y=prop.players))+
  geom_point()+
  stat_smooth(se=T, method='lm', formula=y~poly(x,2))


#tiene mas sentido medir cuando juegas
#es decir la energia usada en juego
#a la centralidad
#y a ese % de jugadores
#la fama cuesta!!!
#la cohesion cuesta!!!!
#bueno, esas son las hipotesis
#ver medida de gibones quizas, segundos por hora!! o algo asi!!

plot(eigen.df$av.players,eigen.df$prop.players)

library(ggplot2)

ggplot(eigen.df,aes(eigen,fill=age.sex.class))+
  geom_density(alpha=0.4)

ggplot(eigen.df,aes(eigen,fill=age.sex.class2))+
  geom_density(alpha=0.4)

a <- kruskal.test(eigen~age.sex.class,eigen.df)
b <- kruskal.test(eigen~age.sex.class2,eigen.df)

c <- kruskal.test(play.rate~age.sex.class2,eigen.df)



#poshoc tests1 (PT)
library(DescTools)

PT = NemenyiTest(x = eigen.df$eigen,
                 g = eigen.df$age.sex.class2,
                 dist="tukey")
PT

PT2 = NemenyiTest(x = eigen.df$play.rate,
                 g = eigen.df$age.sex.class2,
                 dist="tukey")
PT2


library(FSA)

PT3 = dunnTest(eigen ~ age.sex.class2,
              data=eigen.df,
              method="bh") 
PT3

PT4 = dunnTest(eigen ~ age.sex.class,
               data=eigen.df,
               method="bh") 
PT4

#####################################
######################################EXPLORING what explains eigen centrality and %players
library(ggplot2)

ggplot(eigen.df,aes(group.name,av.players))+geom_boxplot()
ggplot(eigen.df,aes(group.name,eigen))+geom_boxplot()

#order groups by group size
eigen.df$group.name <- as.factor(eigen.df$group.name)
levels(eigen.df$group.name)

eigen.df$group.name <- factor(eigen.df$group.name,
                                 levels=c('se',"liz",'sn',
                                          'pla',
                                          "cp","ch","aga"))

ggplot(eigen.df,aes(group.name,eigen))+geom_boxplot()


#https://rstudio-pubs-static.s3.amazonaws.com/490532_5b04406f4d154f2d9b5a99ff488d644a.html
#put together played with and available players (play spread, play efficiency...?)





##############*********************************
###########
#glmm binomial
# Restore the object
eigen.df <- readRDS(file = "eigen.df.rds")
names(eigen.df)
library(ggplot2)

ggplot(eigen.df,aes(eigen,btw))+
  geom_point()+geom_smooth()

ggplot(eigen.df,aes(close,btw))+
  geom_point()+geom_smooth()

ggplot(eigen.df,aes(close,eigen))+
  geom_point()+geom_smooth()

cor(eigen.df$btw,eigen.df$eigen)



cb.play<-cbind(eigen.df$degree,eigen.df$num.not.players)


eigen.df$z.eigen <- scale(eigen.df$eigen)
eigen.df$z.close <- scale(eigen.df$close)
eigen.df$z.btw <- scale(eigen.df$btw)
eigen.df$z.ir <- scale(eigen.df$im.ad.r)
eigen.df$z.playrate <- scale(eigen.df$play.rate)
eigen.df$z.group <- scale(eigen.df$av.players)


library(lme4)
library(lmerTest)

eigen.df$age.sex.class2 <- as.factor(eigen.df$age.sex.class2)
#order age.sex.class2 by age-sex: immature, female, male
levels(eigen.df$age.sex.class2)
#carefull, here I am doing something wrong!!!
#eigen.df$age.sex.class2 <- factor(eigen.df$age.sex.class2,
#                              levels=c('immature',"adult female",'adult male'))
#me lo salto por si caso, asi ademas mantengo los colores q quiero

# Remove the installed lme4 package
remove.packages("lme4")

# Install lme4 from source
install.packages("lme4", type = "source")
library(lme4)
install.packages("Matrix", version = "1.6.3") 
library(Matrix)

glm.play<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.eigen*age.sex.class2+
                  z.ir*age.sex.class2+
                  age.sex.class2+z.group+
                  offset(obs.sec)
                  +(1|group.name),
                family=binomial,data=eigen.df)
summary(glm.play)



glm.play<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.eigen*age.sex.class2+
                  z.ir*age.sex.class2+
                  z.group*age.sex.class2+
                  offset(obs.sec)
                +(1|group.name),
                family=binomial,data=eigen.df)
summary(glm.play)





test.play<-glmer(cb.play~z.playrate +(1|group.name),
                family=binomial,data=eigen.df)


glm.play<-glmer(cb.play~z.playrate*age.sex.class2+
                z.eigen*age.sex.class2+
                  z.ir*age.sex.class2+
                  age.sex.class2+z.group+
                  offset(obs.sec)+(1|group.name),
              family=binomial,data=eigen.df)#it is only fair to put
summary(glm.play)


library(glmmTMB)

glm.play <- glmmTMB(cb.play ~ z.playrate * age.sex.class2 +
                      z.eigen * age.sex.class2 +
                      z.ir * age.sex.class2 +
                      age.sex.class2 + z.group +
                      offset(obs.sec) + (1 | group.name),
                    family = binomial, data = eigen.df)

summary(glm.play)


install.packages("lme4", dependencies = TRUE)



#######this one!!!
library(lme4)
glm.play<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.eigen*age.sex.class2+z.group+
                  z.ir*age.sex.class2+(1|group.name),
                family=binomial,data=eigen.df)#it is only fair to put
summary(glm.play)


glm.play.simple<-glmer(cb.play~z.playrate+age.sex.class2+
                  z.eigen+z.group+(1|group.name),
                family=binomial,data=eigen.df)#it is only fair to put
summary(glm.play.simple)




glm.play2<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.eigen*age.sex.class2+z.group*age.sex.class2+
                  z.ir*age.sex.class2+(1|group.name),
                family=binomial,data=eigen.df)#it is only fair to put
summary(glm.play2)

glm.play3<-glmer(cb.play~z.playrate*age.sex.class2+
                   z.eigen*age.sex.class2+z.group*age.sex.class2+
                   z.ir*age.sex.class2+age.sex.class2+(1|group.name),
                 family=binomial,data=eigen.df)#it is only fair to put
summary(glm.play3)



glm.playz<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.eigen+age.sex.class2+z.group+
                  z.ir*age.sex.class2+(1|group.name),
                family=binomial,data=eigen.df)#it is only fair to put


glm.play.simple<-glmer(cb.play~z.playrate+age.sex.class2+
                   z.eigen+z.group+
                   z.ir+(1|group.name),
                 family=binomial,data=eigen.df)#it is only fair to put



summary(glm.play.simple)

shapiro.test(sqrt(eigen.df$obs.sec))

#two things here on is to put obser time as offset
#included the total observation time of each individual as an offset variable in the corresponding models.
#and 2) n order to gauge effects on proportion of players
#the proportion of macaque-initiated HM interactions (v) were all over-dispersed. 
#Thus, we ran negative binomial models in these cases.


glm.play2<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.eigen+z.group+
                  z.ir+(1|group.name),
                family=binomial,data=eigen.df)#it is only fair to put

glm.play3<-glmer(cb.play~z.playrate+
                   z.eigen+z.group+age.sex.class2+
                   z.ir+(1|group.name),
                 family=binomial,data=eigen.df)#it is only fair to put

summary(glm.play3)




help('isSingular')
summary(glm.play)

library(MuMIn)
r.squaredGLMM(glm.play)
#?

library(lmerTest)#p balioak ikusteko
summary(glm.play)
library(car)
vif(glm.play)

library(equatiomatic)
equatiomatic::extract_eq(glm.play)


drop1(glm.play, test = "Chisq")
anova(glm.play)

null.glm<-glmer(cb.play~1+(1|group.name),
                family=binomial,data=eigen.df)#NULL model

anova(glm.play,null.glm)


#it is interesting that more play rate did not significantly increase the number
#of playmates of a group someone plays with.... however, more centrality does!
#which means that they compensate with centrality!!!!Q yes!!!! fuckers!!!! well,
#that is all then!!! no need to do what explains centrality this is enough to tell




#######################easy effect plots
library(ggeffects)
library(ggplot2)



glm.play<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.eigen*age.sex.class2+z.group+
                  z.ir*age.sex.class2+(1|group.name),
                family=binomial,data=eigen.df)#it is only fair to put
summary(glm.play)

glm.play<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.eigen*age.sex.class2+z.group+
                  z.ir*age.sex.class2+(1|group.name),
                family=binomial,data=eigen.df)#it is only fair to put
summary(glm.play)

glm.play<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.eigen*age.sex.class2+z.group+
                  z.ir*age.sex.class2+(1|group.name),
                family=binomial,data=eigen.df)#it is only fair to put
summary(glm.play)


?ggpredict

pr.eigen <- ggpredict(glm.play,
                      c("z.eigen [all]","age.sex.class2"),
                      type="fixed")
plot(pr.eigen,colors = c(1,2,3))

pr.eigen <- ggpredict(
  glm.play,
  c("z.eigen[all]", "age.sex.class2"),
  type = "fixed"  # Set color of confidence intervals
)
plot(pr.eigen,
     colors = c("red","blue","green"))







pr.play <- ggpredict(glm.play,
                      c("z.playrate [all]","age.sex.class2"),
                      type="fixed")
plot(pr.play)


pr.group <- ggpredict(glm.play, c("z.group [all]"),
                      type="fixed")
plot(pr.group)

pr.ir <- ggpredict(glm.play, c("z.ir [all]","age.sex.class2"), type="fixed",
                   back.transform = T)
plot(pr.ir)

plot(pr.ir)+
  theme(text = element_text(size=13))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
  theme(strip.background = element_rect(fill = "white", color = "white", size = 1))



age.sex.class2
pr.class <- ggpredict(glm.play, c("age.sex.class2"), type="fixed")
plot(pr.class,colors = "circus")
plot(pr.class,colors = "circus",connect.lines = TRUE)


eigen.df
ggplot(eigen.df,aes(age.sex.class2,eigen))+geom_boxplot()
ggplot(eigen.df,aes(age.sex.class2,prop.players))+geom_boxplot()

pr.class2 <- ggpredict(glm.play, terms="age.sex.class2")
plot(pr.class2,facet = TRUE)+
  scale_color_manual(values = c("adult female" = "red",
                                "adult male" = "blue",
                                "immature" = "darkgreen"))

plot(pr.class2,colors = c(1,2,3))+                                       # Applying two fill functions
  scale_color_manual(values = c("#1b98e0", "yellow", "#353436")) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE))

dat <- ggpredict(glm.play, c("age.sex.class2"), type = "fixed")
ggplot(dat, aes(x = x, y = predicted, colour = age.sex.class2)) + 
  geom_line()








library(ggpubr)
library(gridExtra)
grid.arrange(plot(pr.class)+ ggtitle("a")+
               xlab("age-sex class") +
               ylab("proportion of playmates"),
  plot(pr.play)+ ggtitle("b")+ 
    theme(legend.position = c(0.25, 0.8),legend.title = element_blank())+
    xlab("standarized play effort (sec/hr)") + ylab("proportion of playmates"),
  plot(pr.eigen)+ ggtitle("c")+ 
    xlab("standarized eigen-vector centrality") + ylab("proportion of playmates")+
    theme(legend.position = c(0.25, 0.8),legend.title = element_blank()),
  plot(pr.ir)+ ggtitle("d")+
    xlab("standarized immature to adult ratio") + ylab("proportion of playmates")+ 
    theme(legend.position = c(0.25, 0.8),legend.title = element_blank()),
  plot(pr.group)+ ggtitle("e")+
    xlab("standarized group size") + ylab("proportion of playmates")+ 
    theme(legend.position = c(0.8, 0.8),legend.title = element_blank()),
  nrow=2, ncol=3
  )


four.f <- grid.arrange(plot(pr.class)+ ggtitle("b")+
                         xlab("age-sex class") +
                         ylab("play efficiency"),
             plot(pr.eigen)+ ggtitle("c")+ 
               xlab("standarized eigen-vector centrality") + ylab("play efficiency")+
               theme(legend.position = c(0.25, 0.8),legend.title = element_blank()),
             plot(pr.ir)+ ggtitle("d")+
               xlab("standarized immature to adult ratio") + ylab("play efficiency")+ 
               theme(legend.position = c(0.25, 0.8),legend.title = element_blank()),
             plot(pr.group)+ ggtitle("e")+
               xlab("standarized group size") + ylab("play efficiency")+ 
               theme(legend.position = c(0.8, 0.8),legend.title = element_blank()),
             nrow=2, ncol=3
)

############just the 3 significant, this is the plot!!!*******
grid.arrange(plot(pr.class)+ ggtitle("a")+
               xlab("age-sex class") +
               ylab("% playmates")+
               theme(text = element_text(size=13))+
               theme(panel.background = element_blank())+
               theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
               theme(strip.background = element_rect(fill = "white", color = "white", size = 1)+
                       theme_minimal())
             ,
              plot(pr.eigen,colors = c("red","blue","green"))+ ggtitle("b")+ 
               xlab("standarized eigen-vector centrality") + 
               ylab("% playmates")+
               theme(legend.position = c(0.25, 0.8),legend.title = element_blank())+
               theme(text = element_text(size=13))+
               theme(panel.background = element_blank())+
               theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
               theme(strip.background = element_rect(fill = "white", color = "white", size = 1)+
                       theme_minimal())
             ,
             plot(pr.group)+ ggtitle("c")+
               xlab("standarized group size") + ylab("% playmates")+ 
               theme(legend.position = c(0.8, 0.8),legend.title = element_blank())+
               theme(text = element_text(size=13))+
               theme(panel.background = element_blank())+
               theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
               theme(strip.background = element_rect(fill = "white", color = "white", size = 1)+
                       theme_minimal())
               ,nrow=1, ncol=3)


######try from beginnig just for significant values
#######################
#######################
theme_set(theme_bw())
library(ggeffects)

pr.class <- ggpredict(glm.play, c("age.sex.class2"), type="fixed")
pr.eigen <- ggpredict(glm.play,
                      c("z.eigen [all]","age.sex.class2"),
                      type="fixed")
pr.group <- ggpredict(glm.play, c("z.group [all]"),
                      type="fixed")


# Adjust the ggpredict function with ggtheme argument
pr.class <- ggpredict(glm.play, c("age.sex.class2"), type = "fixed", ggtheme = theme_minimal())
pr.eigen <- ggpredict(glm.play, c("z.eigen [all]", "age.sex.class2"), type = "fixed", ggtheme = theme_minimal())
pr.group <- ggpredict(glm.play, c("z.group"), type = "fixed", ggtheme = theme_minimal())











summary(glm.play)







grid.arrange(plot(pr.play)+ ggtitle("a")+
               xlab("standarized play effort (sec/hr)") +
               ylab("play efficiency")+ 
               theme(legend.position = c(0.25, 0.8),
                     legend.title = element_blank(),
                     ),
             four.f,nrow=1, ncol=2
             )



plot.test <- ggpredict(glm.play,
                      c("z.eigen [all]","age.sex.class2"),
                      type="fixed")

ggplot(plot.test, aes(x = x, y = predicted)) +
  geom_line()+
  facet_wrap(~group)

ggplot(plot.test, aes(x = x, y = predicted,color=group)) +
  geom_line()


plot(plot.test)+theme(axis.title.y = element_text(size=16, 
                                                  color="black",
                                                  angle=90),
                      axis.title.x = element_text(size=16, 
                                                  color="black"))
#ggsave
ggplot(plot.test,aes(z.eigen,cb.play))

head(plot.test)

##################################
## I would like to have a figure Ossoff, the estimate notes


library(sjPlot)
library(sjmisc)

plot_model(glm.play)
plot_model(glm.play,show.intercept=T,show.values = TRUE, value.offset = .3)


 
plot_model(glm.play)+geom_hline(yintercept = 1,
                                color="green",
                                linetype="dashed",
                                linewidth=1)+theme_bw()+
  labs(title = "")+ 
  scale_x_discrete(labels= c("immature : imm. ad. ratio",
                             "adult male : imm. ad. ratio",
                             "immature : eigenvector c.",
                             "adult male : eigenvector c.",
                             "immature : play effort",
                             "adult male : play effort",
                             "group size",
                             "imm. ad. ratio",
                             "eigenvector centrality",
                             "immature",
                             "adult male",
                             "play effort"))+
  scale_y_continuous(trans='exp')

#http://www.strengejacke.de/sjPlot/reference/plot_model.html
plot_model(glm.play,show.intercept=T,show.values = TRUE, value.offset = .3)+
  geom_hline(yintercept = 1,
                                color="green",
                                linetype="dashed",
                                linewidth=1)+theme_bw()+
  labs(title = "")+ 
  scale_x_discrete(labels= c("immature*imm. ad. ratio",
                             "adult male*imm. ad. ratio",
                             "immature*eigenvector c.",
                       "adult male*eigenvector c.",
                             "immature*play effort",
                             "adult male*play effort",
                             "group size",
                             "imm. ad. ratio",
                             "eigenvector centrality",
                             "immature",
                             "adult male",
                             "play effort",
                             "intercept (adult female)"))+ylim(0.25,2.25)
#el 1 es el 0
exp(0)
#hum porque no ponerlo en probabilidades
#Figure xxx. odds ratios from a GLMM for predictors of interest, 

#http://www.strengejacke.de/sjPlot/reference/plot_model.html
plot_model(glm.play,show.intercept=T,transform = "plogis")+geom_hline(yintercept = 0.5,
                                                 color="green",
                                                 linetype="dashed",
                                                 linewidth=1)+theme_bw()+
  labs(title = "")+ 
  scale_x_discrete(labels= c("immature : imm. ad. ratio",
                             "adult male : imm. ad. ratio",
                             "immature : eigenvector c.",
                             "adult male : eigenvector c.",
                             "immature : play effort",
                             "adult male : play effort",
                             "group size",
                             "imm. ad. ratio",
                             "eigenvector centrality",
                             "immature",
                             "adult male",
                             "play effort",
                             "intercept (adult female)"))+ylim(0,1)
#el 1 es el 0

#http://www.strengejacke.de/sjPlot/reference/plot_model.html
plot_model(glm.play,show.intercept=T,transform = "plogis",
           show.values = TRUE, value.offset = .3)+geom_hline(yintercept = 0.5,
                                                                      color="green",
                                                                      linetype="dashed",
                                                                      linewidth=1)+theme_bw()+
  labs(title = "")+ 
  scale_x_discrete(labels= c("immature : imm. ad. ratio",
                             "adult male : imm. ad. ratio",
                             "immature : eigenvector c.",
                             "adult male : eigenvector c.",
                             "immature : play effort",
                             "adult male : play effort",
                             "group size",
                             "imm. ad. ratio",
                             "eigenvector centrality",
                             "immature",
                             "adult male",
                             "play effort",
                             "intercept (adult female)"))+ylim(0,1)
#el 1 es el 0

library(dplyr)

eigen.df %>%
  group_by(group.name) %>%
  summarise_at(vars(obs.sec), list(name = mean, stdv =sd))

307288/(3600)
147893/3600


summary(glm.play)


#http://www.strengejacke.de/sjPlot/reference/plot_model.html
plot_model(glm.play,show.intercept=T,transform = NULL,
           show.values = TRUE, value.offset = .4)+geom_hline(yintercept = 0,
                                                             color="green",
                                                             linetype="dashed",
                                                             linewidth=.5)+theme_bw()+
  labs(title = "")+ 
  scale_x_discrete(labels= c("immature*imm. ad. ratio",
                             "adult male*imm. ad. ratio",
                             "immature*eigenvector c.",
                             "adult male*eigenvector c.",
                             "immature*play effort",
                             "adult male*play effort",
                             "group size",
                             "imm. ad. ratio",
                             "eigenvector centrality",
                             "immature",
                             "adult male",
                             "play effort",
                             "intercept (adult female)"))+ylim(-1,1.5)+
  ylab("Log Odds Ratios")
#el 1 es el 0


summary(glm.play)








####ordered by you need to change
#http://www.strengejacke.de/sjPlot/reference/plot_model.html
plot_model(glm.play,show.intercept=T,sort.est = TRUE)+
  geom_hline(yintercept = 1,
                                                 color="green",
                                                 linetype="dashed",
                                                 linewidth=1)+theme_bw()+
  labs(title = "")+ 
  scale_x_discrete(labels= c("immature : imm. ad. ratio",
                             "adult male : imm. ad. ratio",
                             "immature : eigenvector c.",
                             "adult male : eigenvector c.",
                             "immature : play effort",
                             "adult male : play effort",
                             "group size",
                             "imm. ad. ratio",
                             "eigenvector centrality",
                             "immature",
                             "adult male",
                             "play effort",
                             "intercept (adult female)"))+ylim(0.25,2.25)
#el 1 es el 0

#####
#####
plot_model(glm.play, type = "std")#they have been standarized already, so this is actually the raw data

tab_model(glm.play,show.df = F)


#https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_mixed.html

#random effects
library(glmmTMB)
plot_model(glm.play,type = "re")


#pretty names


data.play <- data.frame(play.effort=eigen.df$z.playrate,
                        age.sex.class=eigen.df$age.sex.class2,
                        eigenvector.c=eigen.df$z.eigen,
                        im.ad.r=eigen.df$z.ir,
                        group=eigen.df$z.group,
                        obs.time=eigen.df$obs.sec,
                        group.name=eigen.df$group.name,
                        degree=eigen.df$degree,
                        num.not.players=eigen.df$num.not.players)

play.efficiency<-cbind(eigen.df$degree,eigen.df$num.not.players)


glm.play.names<-glmer(play.efficiency~play.effort*age.sex.class+
                  eigenvector.c*age.sex.class+
                  im.ad.r*age.sex.class+
                  age.sex.class+group+
                  offset(obs.time)+(1|group.name),
                family=binomial,data=data.play)
summary(glm.play.names)



















#https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html
#https://strengejacke.github.io/ggeffects/reference/plot.html
#https://bookdown.org/isubirana/longitudinal_data_analyses/ModelosNoNormal.html

#####glmm

ggplot(eigen.df,aes(eigen))+geom_density()

ggplot(eigen.df,aes(play.rate,eigen))+geom_point()+
  stat_smooth()+facet_wrap(~age.sex.class2)

ggplot(eigen.df,aes(z.playrate,z.eigen))+geom_point()+
  stat_smooth(se=T, method='lm')+
  facet_wrap(~age.sex.class2)

ggplot(eigen.df,aes(age.sex.class2,z.eigen))+geom_boxplot()

ggplot(eigen.df,aes(log(play.rate),log(eigen)))+geom_point()+
  stat_smooth(se=T, method='lm')+
  facet_wrap(~age.sex.class2)

eigen.df$log.eigen <- log(eigen.df$eigen)

#log improves residual fit!!
lmm.eigen<-lmer(log.eigen~z.playrate*age.sex.class2+
                  z.ir*age.sex.class2+age.sex.class2+
                  z.group+
                  +(1|group.name),
                data=eigen.df)#it is only fair to put
vif(lmm.eigen)
#use GVIF 1/2DF
summary(lmm.eigen)


res.lmm <- resid(lmm.eigen)
fit.lmm <- fitted(lmm.eigen)

par(mfrow=c(2,2))
plot(fit.lmm,res.lmm,ylab="residuals",xlab="fitted values")
abline(h=0,lty=2)#if eigen is logged, two outliers are back to OK


boxplot(res.lmm~group.name,data=eigen.df,
        xlab="group",ylab="residuals",
        col="lightgreen")
abline(h=0,lty=2)

boxplot(res.lmm~age.sex.class2,data=eigen.df,
        xlab="group",ylab="residuals",
        col="lightgreen")
abline(h=0,lty=2)

hist(res.lmm,col="lightgreen")

ggplot()+geom_histogram(aes(res.lmm))
shapiro.test(res.lmm)


pr.play2 <- ggpredict(lmm.eigen,
                      c("z.playrate [all]","age.sex.class2"),
                      type="fixed")
plot(pr.play2)

pr.groupb <- ggpredict(lmm.eigen,
                     c("z.group [all]","age.sex.class2"),
                     type="fixed")
plot(pr.groupb)

pr.ir2 <- ggpredict(lmm.eigen,
                     c("z.ir [all]","age.sex.class2"),
                     type="fixed")
plot(pr.ir2)

pr.class2<- ggpredict(lmm.eigen,
                   c("age.sex.class2"),
                   type="fixed")
plot(pr.class2)


grid.arrange(plot(pr.class2)+ ggtitle("a")+
               xlab("age-sex class") +
               ylab("log(eigenvector centrality"),
             plot(pr.play2)+ ggtitle("b")+ 
               theme(legend.position = c(0.25, 0.8),legend.title = element_blank())+
               xlab("standarized play effort (sec/hr)") +
               ylab("log(eigenvector centrality"),
             plot(pr.ir2)+ ggtitle("c")+ 
               xlab("standarized immature to adult ratio") +
               ylab("log(eigenvector centrality")+
               theme(legend.position = c(0.7, 0.8),
                     legend.title = element_blank()),
             plot(pr.groupb)+ ggtitle("d")+
               xlab("standarized group size") +
               ylab("log(eigenvector centrality")+ 
               theme(legend.position = c(0.8, 0.8),legend.title = element_blank()),
             nrow=2, ncol=2
             )

#mean fitted eigenvector centrality of howler monkeys and confidence intervals
#agains age-sex class (a), play effort (b),
#immature to adult ratio (c) and group sice (d)
#modelled with a gaussian GLMM, with
#group id fitted as a random term in the model. Shaded areas represent
#the confidence interval of the model



summary(glm.play)

######broom
library(broom)
library(dplyr)
library(tidyr)
# Make a grid of explanatory data
explanatory_data <- expand.grid(
  z.eigen=seq(-1:3),
  age.sex.class2=unique(eigen.df$age.sex.class2),
  z.group=seq(-1.25,2),
  z.ir=seq(-1,3),
  z.playrate=seq(-3,3.25),
  group.name=unique(eigen.df$group.name)
)

# See the result
explanatory_data

# Add predictions to the data frame
prediction_data <- explanatory_data %>% 
  mutate(
    prediction=predict(
      glm.play,explanatory_data
    )
  )

head(prediction_data)

ggplot(prediction_data,aes(z.eigen,prediction)) +
  geom_line()+facet_grid(~age.sex.class2) 


############################glm close
glm.play<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.close*age.sex.class2+
                  z.ir*age.sex.class2+
                  age.sex.class2+z.group+
                  (1|group.name),
                family=binomial,data=eigen.df)
summary(glm.play)


#####################################
#######################################
#####################################
##################################
###################################
############################AICc
glm.btw<-glmer(cb.play~z.playrate*age.sex.class2+
                  z.btw*age.sex.class2+
                  z.ir*age.sex.class2+
                  age.sex.class2+z.group+
                  +(1|group.name),
                family=binomial,data=eigen.df)

glm.cls<-glmer(cb.play~z.playrate*age.sex.class2+
                 z.close*age.sex.class2+
                 z.ir*age.sex.class2+
                 age.sex.class2+z.group+
                 +(1|group.name),
               family=binomial,data=eigen.df)

glm.eigen<-glmer(cb.play~z.playrate*age.sex.class2+
                 z.eigen*age.sex.class2+
                 z.ir*age.sex.class2+
                 age.sex.class2+z.group+
                 +(1|group.name),
               family=binomial,data=eigen.df)

glm.null <- glmer(cb.play~1
                    +(1|group.name),
                  family=binomial,data=eigen.df)

library(car)
vif(glm.eigen)
vif(glm.cls)
vif(glm.btw)

#https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_mixed.html
tab_model(glm.eigen,glm.btw,glm.btw)
tab_model(glm.eigen,glm.btw,glm.btw,transform = NULL)

tab_model(glm.eigen,transform = NULL)
tab_model(glm.btw
          ,transform = NULL)
tab_model(glm.cls
          ,transform = NULL)



vif(glm.play)

apa_print(glm.play)
tab_model(glm.play)

library(AICcmodavg)
cand.set <- c(glm.eigen,glm.btw,glm.cls,glm.null)
modnames <- c("glmm.eigen","glmm.btw","glmm.cls","glmm.null")

# S3 method for AICglmerMod
aictab(cand.set, modnames = modnames,
       second.ord = TRUE, nobs = NULL, sort = TRUE) 

summary(glm.eigen)
summary(glm.btw)
summary(glm.cls)
anova(glm.null,glm.cls,glm.eigen,glm.btw)

##########btw plots

#######################easy effect plots btw
library(ggeffects)
library(ggplot2)

summary(glm.btw)


?ggpredict

pr.btw <- ggpredict(glm.btw,
                      c("z.btw [all]","age.sex.class2"),
                      type="fixed")
plot(pr.btw,colors = c(1,"grey","red"))
plot(pr.btw)

pr.play <- ggpredict(glm.btw,
                     c("z.playrate [all]","age.sex.class2"),
                     type="fixed")
plot(pr.play)


pr.group <- ggpredict(glm.btw, c("z.group [all]"),
                      type="fixed")
plot(pr.group)

pr.ir <- ggpredict(glm.btw, c("z.ir [all]","age.sex.class2"),
                      type="fixed")
plot(pr.ir)


pr.class <- ggpredict(glm.btw, c("age.sex.class2"), type="fixed")
plot(pr.class)


library(gridExtra)
grid.arrange(plot(pr.class)+ ggtitle("a")+
               xlab("age-sex class") +
               ylab("% playmates")+
               theme(text = element_text(size=13))+
               theme(panel.background = element_blank())+
               theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
               theme(strip.background = element_rect(fill = "white", color = "white", size = 1)),
             plot(pr.btw)+ ggtitle("b")+ 
               xlab("standarized betweenness centrality") + ylab("% playmates")+
               theme(text = element_text(size=13))+
               theme(panel.background = element_blank())+
               theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
               theme(strip.background = element_rect(fill = "white", color = "white", size = 1))+
               xlim(-.5,2)+
               theme(legend.position = c(0.19, 0.8),legend.title = element_blank()),
             plot(pr.ir)+ ggtitle("c")+
               xlab("standarized immature to adult ratio") + ylab("% playmates")+
                 theme(text = element_text(size=13))+
                 theme(panel.background = element_blank())+
                 theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
                 theme(strip.background = element_rect(fill = "white", color = "white", size = 1)) +
               theme(legend.position = c(0.75, 0.25),legend.title = element_blank()),
             plot(pr.group)+ ggtitle("d")+
               xlab("standarized group size") + ylab("% playmates")+
                 theme(text = element_text(size=13))+
                 theme(panel.background = element_blank())+
                 theme(panel.border = element_rect(fill = NA, colour = "black", size = 1))+
                 theme(strip.background = element_rect(fill = "white", color = "white", size = 1)) +
               theme(legend.position = c(0.8, 0.8),legend.title = element_blank()),
             nrow=2, ncol=2
)







library(gridExtra)
grid.arrange(plot(pr.class)+ ggtitle("a")+
               xlab("age-sex class") +
               ylab("proportion of playmates"),
             plot(pr.play)+ ggtitle("b")+ 
               theme(legend.position = c(0.25, 0.7),legend.title = element_blank())+
               xlab("standarized play effort (sec/hr)") + ylab("proportion of playmates"),
             plot(pr.btw)+ ggtitle("c")+ 
               xlab("standarized eigen-vector centrality") + ylab("proportion of playmates")+
               theme(legend.position = c(0.25, 0.8),legend.title = element_blank()),
             plot(pr.ir)+ ggtitle("d")+
               xlab("standarized immature to adult ratio") + ylab("proportion of playmates")+ 
               theme(legend.position = c(0.25, 0.8),legend.title = element_blank()),
             plot(pr.group)+ ggtitle("e")+
               xlab("standarized group size") + ylab("proportion of playmates")+ 
               theme(legend.position = c(0.8, 0.8),legend.title = element_blank()),
             nrow=2, ncol=3
)



four.f <- grid.arrange(plot(pr.class)+ ggtitle("b")+
                         xlab("age-sex class") +
                         ylab("play efficiency"),
                       plot(pr.play)+ ggtitle("c")+ 
                         xlab("standarized play effort") + ylab("play efficiency")+
                         theme(legend.position = c(0.25, 0.75),legend.title = element_blank()),
                       plot(pr.ir)+ ggtitle("d")+
                         xlab("standarized immature to adult ratio") + ylab("play efficiency")+ 
                         theme(legend.position = c(0.7, 0.2),legend.title = element_blank()),
                       plot(pr.group)+ ggtitle("e")+
                         xlab("standarized group size") + ylab("play efficiency"),
                       nrow=2, ncol=2
)


############just the 3 significant
grid.arrange(plot(pr.btw)+ ggtitle("a")+xlab("standarized betweenness")+
               ylab("play efficiency")+
               theme(legend.position = c(0.25, 0.9),legend.title = element_blank()),
               four.f,
             nrow=1, ncol=2
)


summary(glm.btw)

tab_model(glm.btw,glm.eigen,glm.cls)



# plot multiple models
plot_models(glm.btw,glm.eigen,glm.cls, grid = TRUE)


#plot multiple models with legend labels and
# point shapes instead of value labels
plot_models(
  glm.btw,glm.eigen,glm.cls,
  axis.labels = c(
    "Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"
  ),
  m.labels = c("Barthel Index", "Negative Impact", "Services used"),
  show.values = FALSE, show.p = FALSE, p.shape = TRUE
)


# plot multiple models from nested lists argument
all.models <- list()




all.models[[1]] <- glm.btw
all.models[[2]] <- glm.eigen
all.models[[3]] <- glm.cls





plot_models(all.models)
#http://strengejacke.de/sjPlot/reference/plot_models.html

plot_models(glm.btw,glm.eigen,glm.cls,grid=TRUE)


plot_models(glm.btw,glm.eigen,glm.cls,grid=TRUE,
            axis.labels = c(
              "immature*closeness",
              "adult male*closeness",
              "closeness",
              "immature*eigenvector",
              "adult male*eigenvector",
              "eigenvector",
              "immature*im. ad. ratio",
              "adult male*im. ad. ratio",
              "immature*betweenness",
              "adult male*betweenness",
              "immature*play effort",
              "adult male*play effort",
              "group size",
              "im. ad. ratio",
              "betweenness",
              "immature",
              "adult male",
              "play effort",
              "intercept (adult female)"),
            m.labels = c("betweenness model",
                         "eigenvector model",
                         "closeness model"),
            show.intercept = T,transform = NULL,
            show.values = TRUE)+geom_hline(yintercept = 0,
                                           color="grey",
                                           linetype="dashed",
                                           linewidth=1)+theme_bw()+ylim(-1,3.1)+
  ylab("Log Odds Ratios")+
  theme(legend.position="none")+
   theme(text = element_text(size = 16))


####hum, good, but I want the ORs since I have put the on the text as such
plot_models(glm.btw,glm.eigen,glm.cls,grid=TRUE,
            axis.labels = c(
              "immature*closeness",
              "adult male*closeness",
              "closeness",
              "immature*eigenvector",
              "adult male*eigenvector",
              "eigenvector",
              "immature*im. ad. ratio",
              "adult male*im. ad. ratio",
              "immature*betweenness",
              "adult male*betweenness",
              "immature*play effort",
              "adult male*play effort",
              "group size",
              "im. ad. ratio",
              "betweenness",
              "immature",
              "adult male",
              "play effort",
              "intercept (adult female)"),
            m.labels = c("betweenness model", "eigenvector model", "closeness model"),
            show.intercept = T,
            show.values = TRUE)+geom_hline(yintercept = 1,
                                           color="grey",
                                           linetype="dashed",
                                           linewidth=1)+theme_bw()+
  ylim(0,18)+
  ylab("Odds Ratios")+
  theme(legend.position="none")


plot_models(glm.btw)+geom_hline(yintercept = 0,
                                color="green",
                                linetype="dashed",
                                linewidth=1)+theme_bw()+
  labs(title = "")+ylim(-1,1.5)

plot_models(glm.btw,glm.eigen,glm.cls,grid=TRUE,
           show.intercept=T,transform = NULL,
           show.values = TRUE, value.offset = .4)+geom_hline(yintercept = 0,
                                                             color="green",
                                                             linetype="dashed",
                                                             linewidth=1)+theme_bw()+
  labs(title = "")+ylim(-1,1.5)+
  ylab("Log Odds Ratios")+
  scale_x_discrete(labels= c("immature*imm. ad. ratio",
                             "adult male*imm. ad. ratio",
                             "immature*eigenvector c.",
                             "adult male*eigenvector c.",
                             "immature*play effort",
                             "adult male*play effort",
                             "group size",
                             "imm. ad. ratio",
                             "eigenvector centrality",
                             "immature",
                             "adult male",
                             "play effort",
                             "intercept (adult female)"))
  
  
  
  
  
  scale_x_discrete(labels= c("immature*imm. ad. ratio",
                             "adult male*imm. ad. ratio",
                             "immature*eigenvector c.",
                             "adult male*eigenvector c.",
                             "immature*play effort",
                             "adult male*play effort",
                             "group size",
                             "imm. ad. ratio",
                             "eigenvector centrality",
                             "immature",
                             "adult male",
                             "play effort",
                             "intercept (adult female)"))+ylim(-1,1.5)+
  ylab("Log Odds Ratios")
#el 1 es el 0

  anova(glm.btw,null.glm)
  anova(glm.cls,null.glm)

  
  table(eigen.df$age.sex.class2)
  
  
  
  
  #just the eigenvector, the lowest AIC
  
  
  tab_model(glm.eigen)
  
  plot_model(glm.eigen,grid=T,
              axis.labels = c(
                "immature*imm. ad. ratio",
                "adult male*imm. ad. ratio",
                "immature*eigen",
                "adult male*eigen",
                "immature*play effort",
                "adult male*play effort",
                "group size",
                "imm. ad. ratio.",
                "eigen centrality",
                "immature",
                "adult male",
                "play effort",
                "intercept (adult female)"),
              m.labels = c("eigenvector model"),
              show.intercept = T,transform = NULL,
              show.values = TRUE,
             value.offset=.25)+
    geom_hline(yintercept = 0,color="grey",
    linetype="dashed",
    linewidth=1)+theme_bw()+ylim(-1,1.5)+
    ylab("Log Odds Ratios")+
    theme(legend.position="none")+
    theme(text = element_text(size = 12))+
    labs(title = "Eigen GLMM on play efficiency")
  
  
####TRYING
  
  
  
  
  
  
  