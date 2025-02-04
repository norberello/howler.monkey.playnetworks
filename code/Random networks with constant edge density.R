####Null network simulations using as null edge density and same number of nodes as observed playtime network!!
###Libraries
library(igraph)
library(ggplot2)
library(brainGraph)
library(ggpubr)

### Playtime igraph for each group
#### PLA Network
pla.data <- read.csv2("PLAnettime.csv", header = TRUE, row.names = 1)
pla.matrix <- as.matrix(pla.data)
pla.network <- graph.adjacency(pla.matrix, mode = "undirected",
                               diag = FALSE, weighted = TRUE)

#### AGA Network
aga.data <- read.csv2("AGAnettime.csv", header = TRUE, row.names = 1)
aga.matrix <- as.matrix(aga.data)
aga.network <- graph.adjacency(aga.matrix, mode = "undirected",
                               diag = FALSE, weighted = TRUE)

#### LIZ Network
liz.data <- read.csv2("LIZnettime.csv", header = TRUE, row.names = 1)
liz.matrix <- as.matrix(liz.data)
liz.network <- graph.adjacency(liz.matrix, mode = "undirected",
                               diag = FALSE, weighted = TRUE)

#### CH Network
ch.data <- read.csv2("CHnettime.csv", header = TRUE, row.names = 1)
ch.matrix <- as.matrix(ch.data)
ch.network <- graph.adjacency(ch.matrix, mode = "undirected",
                              diag = FALSE, weighted = TRUE)

#### CP Network
cp.data <- read.csv2("CPnettime.csv", header = TRUE, row.names = 1)
cp.matrix <- as.matrix(cp.data)
cp.network <- graph.adjacency(cp.matrix, mode = "undirected",
                              diag = FALSE, weighted = TRUE)

#### SE Network #check check
se.data <- read.csv2("SEnettime.csv", header = TRUE, row.names = 1)
se.matrix <- as.matrix(se.data)
se.network <- graph.adjacency(se.matrix, mode = "undirected",
                              diag = FALSE, weighted = TRUE)

#### SN Network
sn.data <- read.csv2("SNnettime.csv", header = TRUE, row.names = 1)
sn.matrix <- as.matrix(sn.data)
sn.network <- graph.adjacency(sn.matrix, mode = "undirected",
                              diag = FALSE, weighted = TRUE)



####################
#####################AVERAGE PATH LENGTH SIMs
#######################AVERAGE PATH LENGTH SIMs

# Set up Monte Carlo simulations
num_edges <- gsize(pla.network)  # Number of edges in the observed network
non_na_playtimes <- pla.matrix[!is.na(pla.matrix)]  # Get non-NA playtimes
log_non_na_playtimes <- log(non_na_playtimes)
mean_log_playtime <- mean(log_non_na_playtimes)
sd_log_playtime <- sd(log_non_na_playtimes)

pla.sim <- vector('list', 10000)  # List to store simulated networks

# Loop to generate 10,000 random graphs
for (i in 1:10000) {   
  pla.sim[[i]] <- erdos.renyi.game(
    n = gorder(pla.network),  # Set number of nodes (group size)
    p.or.m = edge_density(pla.network),  # Edge probability for random graph
    type = "gnp"
  )
  
  # Get number of edges in the generated random graph
  num_edges_simulated <- gsize(pla.sim[[i]])
  
  # Ensure there are edges in the graph (biologically reasonable)
  if (num_edges_simulated == 0) {
    next  # Skip this simulation if no edges are present
  }
  
  # Generate random playtimes from the log-normal distribution, matching number of edges
  random_playtimes_log_normal <- round(rlnorm(num_edges_simulated, meanlog = mean_log_playtime, sdlog = sd_log_playtime), 0)
  
  # Assign random playtimes as edge weights
  E(pla.sim[[i]])$weight <- 1/random_playtimes_log_normal
}

pla.sim.meand <- unlist(lapply(pla.sim,mean_distance))
pla.sim.meand <- pla.sim.meand[!is.na(pla.sim.meand)]
#here we need to recalculate pla.network with 1/playtime
#so mean_distance(pla.network) is comparable
#assign reverse the playtime weights of pla.network
# Reverse the playtime weights (1/playtime) while keeping zeros as zeros
inv_pla_matrix <- ifelse(pla.matrix > 0, 1 / pla.matrix, 0)  # Reverse the weights

# Now create the network with the reversed weights
pla.network <- graph.adjacency(inv_pla_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)


pla.meand.h <- ggplot()+aes(x=pla.sim.meand)+geom_histogram()+
  labs(x="Average path length",y = "")+theme_bw()+geom_vline(xintercept = mean_distance(pla.network),
                                                             color="red",
                                                             linetype="dashed",
                                                             linewidth=2)

#  scale_x_continuous(limits = c(0, 1000))
  #scale_x_continuous(limits = c(0, 10000))



######AGA

# Number of edges in the observed network
num_edges <- gsize(aga.network)

# Non-NA playtimes for the observed network
non_na_playtimes <- aga.matrix[!is.na(aga.matrix)]
# Apply log transformation, but replace log(0) with 0
log_playtimes <- ifelse(non_na_playtimes > 0, log(non_na_playtimes), 0)
# Calculate mean and standard deviation for the log-transformed playtimes
mean_log_playtime <- mean(log_playtimes)
sd_log_playtime <- sd(log_playtimes)


# List to store simulations
aga.sim <- vector('list', 10000)

# Loop to generate 10,000 random graphs for aga.network
for (i in 1:10000) {   
  aga.sim[[i]] <- erdos.renyi.game(
    n = gorder(aga.network),  # Set number of nodes (group size)
    p.or.m = edge_density(pla.network),  # Edge probability for random graph
    type = "gnp"
  )
  
  # Get number of edges in the generated random graph
  num_edges_simulated <- gsize(aga.sim[[i]])
  
  # Ensure there are edges in the graph (biologically reasonable)
  if (num_edges_simulated == 0) {
    next  # Skip this simulation if no edges are present
  }
  
  # Generate random playtimes from the log-normal distribution
  random_playtimes_log_normal <- round(rlnorm(num_edges_simulated, meanlog = mean_log_playtime, sdlog = sd_log_playtime), 0)
  
  # Assign random playtimes as edge weights
  E(aga.sim[[i]])$weight <- 1/random_playtimes_log_normal
}



aga.sim.meand <- unlist(lapply(aga.sim,mean_distance))
aga.sim.meand <- aga.sim.meand[!is.na(aga.sim.meand)]

# Reverse the playtime weights (1/playtime) while keeping zeros as zeros
inv_aga_matrix <- ifelse(aga.matrix > 0, 1 / aga.matrix, 0)  # Reverse the weights

# Now create the network with the reversed weights
aga.network <- graph.adjacency(inv_aga_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)



aga.meand.h <- ggplot()+aes(x=aga.sim.meand)+geom_histogram(bins = 50)+
  labs(x="Average path length",y = "")+theme_bw()+geom_vline(xintercept = mean_distance(aga.network),
                                                             color="red",
                                                             linetype="dashed",
                                                             linewidth=2)

# Observed network: liz.network

num_edges <- gsize(liz.network)
non_na_playtimes <- liz.matrix[!is.na(liz.matrix)]  # Non-NA playtimes for the observed network
# Apply log transformation, but replace log(0) with 0
log_playtimes <- ifelse(non_na_playtimes > 0, log(non_na_playtimes), 0)
# Calculate mean and standard deviation for the log-transformed playtimes
mean_log_playtime <- mean(log_playtimes)
sd_log_playtime <- sd(log_playtimes)


# List to store simulations
liz.sim <- vector('list', 10000)

# Loop to generate 10,000 random graphs for liz.network
for (i in 1:10000) {   
  liz.sim[[i]] <- erdos.renyi.game(
    n = gorder(liz.network),  # Set number of nodes (group size)
    p.or.m = edge_density(pla.network),  # Edge probability for random graph
    type = "gnp"
  )
  
  # Get number of edges in the generated random graph
  num_edges_simulated <- gsize(liz.sim[[i]])
  
  # Ensure there are edges in the graph (biologically reasonable)
  if (num_edges_simulated == 0) {
    next  # Skip this simulation if no edges are present
  }
  
  # Generate random playtimes from the log-normal distribution
  random_playtimes_log_normal <- round(rlnorm(num_edges_simulated, meanlog = mean_log_playtime, sdlog = sd_log_playtime), 0)
  
  # Assign random playtimes as edge weights
  E(liz.sim[[i]])$weight <- 1/random_playtimes_log_normal
}


liz.sim.meand <- unlist(lapply(liz.sim,mean_distance))
liz.sim.meand <- liz.sim.meand[!is.na(liz.sim.meand)]

# Reverse the playtime weights (1/playtime) while keeping zeros as zeros
inv_liz_matrix <- ifelse(liz.matrix > 0, 1 / liz.matrix, 0)  # Reverse the weights

# Now create the network with the reversed weights
liz.network <- graph.adjacency(inv_liz_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)


liz.meand.h <- ggplot()+aes(x=liz.sim.meand)+geom_histogram()+
  labs(x="Average path length",y = "")+theme_bw()+geom_vline(xintercept = mean_distance(liz.network),
                                                             color="red",
                                                             linetype="dashed",
                                                             linewidth=2)
  #scale_x_continuous(limits = c(0, 10000))



#CP
# Observed network: cp.network

# Number of edges in the observed network
num_edges <- gsize(cp.network)
non_na_playtimes <- cp.matrix[!is.na(cp.matrix)]  # Non-NA playtimes for the observed network
# Apply log transformation, but replace log(0) with 0
# Apply log transformation, but replace log(0) with 0
log_playtimes <- ifelse(non_na_playtimes > 0, log(non_na_playtimes), 0)
mean_log_playtime <- mean(log_playtimes)
sd_log_playtime <- sd(log_playtimes)
# List to store simulations
cp.sim <- vector('list', 10000)

# Loop to generate 10,000 random graphs for cp.network
for (i in 1:10000) {   
  cp.sim[[i]] <- erdos.renyi.game(
    n = gorder(cp.network),  # Set number of nodes (group size)
    p.or.m = edge_density(pla.network),  # Edge probability for random graph
    type = "gnp"
  )
  
  # Get number of edges in the generated random graph
  num_edges_simulated <- gsize(cp.sim[[i]])
  
  # Ensure there are edges in the graph (biologically reasonable)
  if (num_edges_simulated == 0) {
    next  # Skip this simulation if no edges are present
  }
  
  # Generate random playtimes from the log-normal distribution
  random_playtimes_log_normal <- round(rlnorm(num_edges_simulated, meanlog = mean_log_playtime, sdlog = sd_log_playtime), 0)
  
  # Assign random playtimes as edge weights
  E(cp.sim[[i]])$weight <- 1/random_playtimes_log_normal
}



cp.sim.meand <- unlist(lapply(cp.sim,mean_distance))
cp.sim.meand  <- cp.sim.meand [!is.na(cp.sim.meand )]

# Reverse the playtime weights (1/playtime) while keeping zeros as zeros
inv_cp_matrix <- ifelse(cp.matrix > 0, 1 / cp.matrix, 0)  # Reverse the weights

# Now create the network with the reversed weights
cp.network <- graph.adjacency(inv_cp_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)



cp.meand.h <- ggplot()+aes(x=cp.sim.meand )+geom_histogram()+
  labs(x="Average path length",y = "")+theme_bw()+geom_vline(xintercept = mean_distance(cp.network),
                                                             color="red",
                                                             linetype="dashed",
                                                             linewidth=2)

#SN

# Number of edges in the observed network
num_edges <- gsize(sn.network)
non_na_playtimes <- sn.matrix[!is.na(sn.matrix)]  # Non-NA playtimes for the observed network
# Apply log transformation, but replace log(0) with 0
log_playtimes <- ifelse(non_na_playtimes > 0, log(non_na_playtimes), 0)
mean_log_playtime <- mean(log_playtimes)
sd_log_playtime <- sd(log_playtimes)

# List to store simulations
sn.sim <- vector('list', 10000)

# Loop to generate 10,000 random graphs for sn.network
for (i in 1:10000) {   
  sn.sim[[i]] <- erdos.renyi.game(
    n = gorder(sn.network),  # Set number of nodes (group size)
    p.or.m = edge_density(sn.network),  # Edge probability for random graph
    type = "gnp"
  )
  
  # Get number of edges in the generated random graph
  num_edges_simulated <- gsize(sn.sim[[i]])
  
  # Ensure there are edges in the graph (biologically reasonable)
  if (num_edges_simulated == 0) {
    next  # Skip this simulation if no edges are present
  }
  
  # Generate random playtimes from the log-normal distribution
  random_playtimes_log_normal <- round(rlnorm(num_edges_simulated, meanlog = mean_log_playtime, sdlog = sd_log_playtime), 0)
  
  # Assign random playtimes as edge weights
  E(sn.sim[[i]])$weight <- 1/random_playtimes_log_normal
}

sn.sim.meand <- unlist(lapply(sn.sim,mean_distance))
sn.sim.meand  <- sn.sim.meand [!is.na(sn.sim.meand )]

# Reverse the playtime weights (1/playtime) while keeping zeros as zeros
inv_sn_matrix <- ifelse(sn.matrix > 0, 1 / sn.matrix, 0)  # Reverse the weights

# Now create the network with the reversed weights
sn.network <- graph.adjacency(inv_sn_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)



sn.meand.h <- ggplot()+aes(x=sn.sim.meand )+geom_histogram()+
  labs(x="Average path length",y = "")+theme_bw()+geom_vline(xintercept = mean_distance(sn.network),
                                                             color="red",
                                                             linetype="dashed",
                                                             linewidth=2)


# Observed network: ch.network

# Number of edges in the observed network
num_edges <- gsize(ch.network)
non_na_playtimes <- ch.matrix[!is.na(ch.matrix)]  # Non-NA playtimes for the observed network
# Apply log transformation, but replace log(0) with 0
log_playtimes <- ifelse(non_na_playtimes > 0, log(non_na_playtimes), 0)
mean_log_playtime <- mean(log_playtimes)
sd_log_playtime <- sd(log_playtimes)

# List to store simulations
ch.sim <- vector('list', 10000)

# Loop to generate 10,000 random graphs for ch.network
for (i in 1:10000) {   
  ch.sim[[i]] <- erdos.renyi.game(
    n = gorder(ch.network),  # Set number of nodes (group size)
    p.or.m = edge_density(ch.network),  # Edge probability for random graph
    type = "gnp"
  )
  
  # Get number of edges in the generated random graph
  num_edges_simulated <- gsize(ch.sim[[i]])
  
  # Ensure there are edges in the graph (biologically reasonable)
  if (num_edges_simulated == 0) {
    next  # Skip this simulation if no edges are present
  }
  
  # Generate random playtimes from the log-normal distribution
  random_playtimes_log_normal <- round(rlnorm(num_edges_simulated, meanlog = mean_log_playtime, sdlog = sd_log_playtime), 0)
  
  # Assign random playtimes as edge weights
  E(ch.sim[[i]])$weight <- 1/random_playtimes_log_normal
}


ch.sim.meand <- unlist(lapply(ch.sim,mean_distance))
ch.sim.meand  <- ch.sim.meand [!is.na(ch.sim.meand )]

# Reverse the playtime weights (1/playtime) while keeping zeros as zeros
inv_ch_matrix <- ifelse(ch.matrix > 0, 1 / ch.matrix, 0)  # Reverse the weights

# Now create the network with the reversed weights
ch.network <- graph.adjacency(inv_ch_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)



ch.meand.h <- ggplot()+aes(x=ch.sim.meand )+geom_histogram()+
  labs(x="Average path length",y = "")+theme_bw()+geom_vline(xintercept = mean_distance(ch.network),
                                                             color="red",
                                                             linetype="dashed",
                                                             linewidth=2)

####SE

# Observed network: se.network

# Number of edges in the observed network
num_edges <- gsize(se.network)
non_na_playtimes <- se.matrix[!is.na(se.matrix)]  # Non-NA playtimes for the observed network
# Apply log transformation, but replace log(0) with 0
log_playtimes <- ifelse(non_na_playtimes > 0, log(non_na_playtimes), 0)
mean_log_playtime <- mean(log_playtimes)
sd_log_playtime <- sd(log_playtimes)
# List to store simulations
se.sim <- vector('list', 10000)

# Loop to generate 10,000 random graphs for se.network
for (i in 1:10000) {   
  se.sim[[i]] <- erdos.renyi.game(
    n = gorder(se.network),  # Set number of nodes (group size)
    p.or.m = edge_density(se.network),  # Edge probability for random graph
    type = "gnp"
  )
  
  # Get number of edges in the generated random graph
  num_edges_simulated <- gsize(se.sim[[i]])
  
  # Ensure there are edges in the graph (biologically reasonable)
  if (num_edges_simulated == 0) {
    next  # Skip this simulation if no edges are present
  }
  
  # Generate random playtimes from the log-normal distribution
  random_playtimes_log_normal <- round(rlnorm(num_edges_simulated, meanlog = mean_log_playtime, sdlog = sd_log_playtime), 0)
  
  # Assign random playtimes as edge weights
  E(se.sim[[i]])$weight <- 1/random_playtimes_log_normal
}



se.sim.meand <- unlist(lapply(se.sim,mean_distance))
se.sim.meand  <- se.sim.meand [!is.na(se.sim.meand )]

# Reverse the playtime weights (1/playtime) while keeping zeros as zeros
inv_se_matrix <- ifelse(se.matrix > 0, 1 / se.matrix, 0)  # Reverse the weights

# Now create the network with the reversed weights
se.network <- graph.adjacency(inv_se_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)




se.meand.h <- ggplot()+aes(x=se.sim.meand )+geom_histogram()+
  labs(x="Average path length",y = "")+theme_bw()+geom_vline(xintercept = mean_distance(se.network),
                                                             color="red",
                                                             linetype="dashed",
                                                             linewidth=2)
  #scale_x_continuous(limits = c(0, 10000))

figure.aga <- ggarrange(aga.meand.h,ncol = 1, nrow = 1,labels = "AGA")
apl.g <- ggarrange(figure.aga, ggarrange(ch.meand.h,cp.meand.h,liz.meand.h,pla.meand.h,se.meand.h,sn.meand.h,
                                         labels = c("CH", "CP", "LIZ", "PLA", "SE", "SN"),
                                         ncol = 2, nrow = 3,legend="right"))

apl.g


# Calculate the proportion of graphs with an average
#path greater than the one of the observed network
mean(se.sim.meand > mean_distance(se.network))
mean(aga.sim.meand > mean_distance(aga.network))
mean(ch.sim.meand > mean_distance(ch.network))
mean(cp.sim.meand > mean_distance(cp.network))
mean(liz.sim.meand > mean_distance(liz.network))
mean(pla.sim.meand > mean_distance(pla.network))
mean(se.sim.meand > mean_distance(se.network))
mean(sn.sim.meand > mean_distance(sn.network))

