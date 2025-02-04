#######8 graph simulations!!!
library(igraph)

#### PLA network playtime data as weight
pla.data <- read.csv2("PLAnettime.csv", header = TRUE, row.names = 1)
pla.matrix <- as.matrix(pla.data)# Convert the data to a matrix
pla.network <- graph.adjacency(pla.matrix, mode = "undirected", diag = FALSE, weighted = TRUE)

### Getting the dyadic playtimes observed
non_na_playtimes <- pla.matrix[!is.na(pla.matrix)]
log_non_na_playtimes <- log(non_na_playtimes)
mean_log_playtime <- mean(log_non_na_playtimes)
sd_log_playtime <- sd(log_non_na_playtimes)

# Set up plotting parameters
par(mar = c(1.5, 1.5, 4, 1.5) + 0.1)
par(mfrow = c(3, 3))

# Plot the observed play network
plot(pla.network,
     layout = layout_in_circle,
     vertex.label = NA,  # No labels
     vertex.color = "red",  # Color of the nodes
     vertex.frame.color = "black",  # Frame color of the nodes
     vertex.size = 15,  # Size of the nodes
     edge.color = "black",  # Color of the edges
     edge.width = (E(pla.network)$weight / max(E(pla.network)$weight)) * 5,  # Scale edge width by playtime
     main = c(
       paste("Pla Network"),
       paste("Avg. path length = 0.000061")  # Fixed rounding
     )
)

# Loop to generate 8 simulations
for (i in 1:8) {
  # Step 1: Generate the random graph
  rg1 <- erdos.renyi.game(
    n = gorder(pla.network), 
    p.or.m = edge_density(pla.network), 
    type = "gnp"
  )
  
  # Step 2: Get the number of edges in the random graph
  num_edges <- gsize(rg1)  # Count of actual edges
  
  # Ensure there are edges in the graph
  if (num_edges == 0) {
    next  # Skip this iteration if no edges are present
  }
  
  # Step 3: Generate random playtimes based on the log-normal distribution
  random_playtimes_log_normal <- round(rlnorm(num_edges, meanlog = mean_log_playtime, sdlog = sd_log_playtime), 0)
  
  # Step 4: Assign random playtimes as weights to the edges in the graph
  E(rg1)$weight <- random_playtimes_log_normal  # Original weights for plotting
  
  # Step 4.1: Create a temporary graph with inverted weights for mean_distance calculation
  rg1_inv <- rg1  # Copy the graph
  E(rg1_inv)$weight <- 1 / E(rg1_inv)$weight  # Invert weights
  
  # Step 5: Plot the graph
  plot(rg1,
       layout = layout_in_circle,
       vertex.label = NA,  # No labels
       vertex.color = "grey",  # Color of the nodes
       vertex.frame.color = "black",  # Frame color of the nodes
       vertex.size = 15,  # Size of the nodes
       edge.color = "black",  # Color of the edges
       edge.width = E(rg1)$weight / max(E(rg1)$weight) * 5,  # Scale edge width by playtime
       main = c(
         paste("Simulation", i),
         paste("Avg. path length =", round(mean_distance(rg1_inv, weights = E(rg1_inv)$weight), 5))  # Use the inverted weight graph
       ))
}

