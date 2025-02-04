library(igraph)



#### PLA network playtime data as weight
pla.data <- read.csv2("PLAnettime.csv", header = TRUE, row.names = 1)
pla.matrix <- as.matrix(pla.data)# Convert the data to a matrix
# Create the network for eigenvector centrality
pla.network <- graph.adjacency(pla.matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
# Invert the weights for betweenness and closeness centrality
pla.matrix_inv <- ifelse(pla.matrix == 0, Inf, 1 / pla.matrix)
# Create the network using the inverted weights (for betweenness and closeness)
pla.network_inv <- graph.adjacency(pla.matrix_inv, mode = "undirected", diag = FALSE, weighted = TRUE)


##PLA centrality measures
betweenness_centrality <- betweenness(pla.network_inv, 
                                      directed = FALSE, 
                                      weights = E(pla.network_inv)$weight,
                                      normalized = T)
closeness_centrality <- closeness(pla.network_inv, 
                                  weights = E(pla.network_inv)$weight, 
                                  normalized = TRUE) 
eigenvector_centrality <- eigen_centrality(pla.network, 
                                           directed = FALSE, 
                                           scale = TRUE, 
                                           weights = E(pla.network)$weight)$vector

centrality_measures_PLA <- data.frame(
  Betweenness = betweenness_centrality,
  Closeness = closeness_centrality,
  Eigenvector = eigenvector_centrality
)

centrality_measures_PLA

#### AGA network playtime data as weight
aga.data <- read.csv2("AGAnettime.csv", header = TRUE, row.names = 1)
aga.matrix <- as.matrix(aga.data) # Convert the data to a matrix
# Create the network for eigenvector centrality
aga.network <- graph.adjacency(aga.matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
# Invert the weights for betweenness and closeness centrality
aga.matrix_inv <- ifelse(aga.matrix == 0, Inf, 1 / aga.matrix)
# Create the network using the inverted weights (for betweenness and closeness)
aga.network_inv <- graph.adjacency(aga.matrix_inv, mode = "undirected", diag = FALSE, weighted = TRUE)

## AGA centrality measures
betweenness_centrality_AGA <- betweenness(aga.network_inv, 
                                          directed = FALSE, 
                                          weights = E(aga.network_inv)$weight,
                                          normalized = TRUE)
closeness_centrality_AGA <- closeness(aga.network_inv, 
                                      weights = E(aga.network_inv)$weight, 
                                      normalized = TRUE) 
eigenvector_centrality_AGA <- eigen_centrality(aga.network, 
                                               directed = FALSE, 
                                               scale = TRUE, 
                                               weights = E(aga.network)$weight)$vector

centrality_measures_AGA <- data.frame(
  Betweenness = betweenness_centrality_AGA,
  Closeness = closeness_centrality_AGA,
  Eigenvector = eigenvector_centrality_AGA
)

#### LIZ network playtime data as weight
liz.data <- read.csv2("LIZnettime.csv", header = TRUE, row.names = 1)
liz.matrix <- as.matrix(liz.data)
liz.network <- graph.adjacency(liz.matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
liz.matrix_inv <- ifelse(liz.matrix == 0, Inf, 1 / liz.matrix)
liz.network_inv <- graph.adjacency(liz.matrix_inv, mode = "undirected", diag = FALSE, weighted = TRUE)

## LIZ centrality measures
betweenness_centrality_LIZ <- betweenness(liz.network_inv, directed = FALSE, weights = E(liz.network_inv)$weight, normalized = TRUE)
closeness_centrality_LIZ <- closeness(liz.network_inv, weights = E(liz.network_inv)$weight, normalized = TRUE) 
eigenvector_centrality_LIZ <- eigen_centrality(liz.network, directed = FALSE, scale = TRUE, weights = E(liz.network)$weight)$vector

centrality_measures_LIZ <- data.frame(
  Betweenness = betweenness_centrality_LIZ,
  Closeness = closeness_centrality_LIZ,
  Eigenvector = eigenvector_centrality_LIZ
)

#### CH network playtime data as weight
ch.data <- read.csv2("CHnettime.csv", header = TRUE, row.names = 1)
ch.matrix <- as.matrix(ch.data)
ch.network <- graph.adjacency(ch.matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
ch.matrix_inv <- ifelse(ch.matrix == 0, Inf, 1 / ch.matrix)
ch.network_inv <- graph.adjacency(ch.matrix_inv, mode = "undirected", diag = FALSE, weighted = TRUE)

## CH centrality measures
betweenness_centrality_CH <- betweenness(ch.network_inv, directed = FALSE, weights = E(ch.network_inv)$weight, normalized = TRUE)
closeness_centrality_CH <- closeness(ch.network_inv, weights = E(ch.network_inv)$weight, normalized = TRUE) 
eigenvector_centrality_CH <- eigen_centrality(ch.network, directed = FALSE, scale = TRUE, weights = E(ch.network)$weight)$vector

centrality_measures_CH <- data.frame(
  Betweenness = betweenness_centrality_CH,
  Closeness = closeness_centrality_CH,
  Eigenvector = eigenvector_centrality_CH
)

#### CP network playtime data as weight
cp.data <- read.csv2("CPnettime.csv", header = TRUE, row.names = 1)
cp.matrix <- as.matrix(cp.data)
cp.network <- graph.adjacency(cp.matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
cp.matrix_inv <- ifelse(cp.matrix == 0, Inf, 1 / cp.matrix)
cp.network_inv <- graph.adjacency(cp.matrix_inv, mode = "undirected", diag = FALSE, weighted = TRUE)

## CP centrality measures
betweenness_centrality_CP <- betweenness(cp.network_inv, directed = FALSE, weights = E(cp.network_inv)$weight, normalized = TRUE)
closeness_centrality_CP <- closeness(cp.network_inv, weights = E(cp.network_inv)$weight, normalized = TRUE) 
eigenvector_centrality_CP <- eigen_centrality(cp.network, directed = FALSE, scale = TRUE, weights = E(cp.network)$weight)$vector

centrality_measures_CP <- data.frame(
  Betweenness = betweenness_centrality_CP,
  Closeness = closeness_centrality_CP,
  Eigenvector = eigenvector_centrality_CP
)

#### SE network playtime data as weight
se.data <- read.csv2("SEnettime.csv", header = TRUE, row.names = 1)
se.matrix <- as.matrix(se.data)
se.network <- graph.adjacency(se.matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
se.matrix_inv <- ifelse(se.matrix == 0, Inf, 1 / se.matrix)
se.network_inv <- graph.adjacency(se.matrix_inv, mode = "undirected", diag = FALSE, weighted = TRUE)

## SE centrality measures
betweenness_centrality_SE <- betweenness(se.network_inv, directed = FALSE, weights = E(se.network_inv)$weight, normalized = TRUE)
closeness_centrality_SE <- closeness(se.network_inv, weights = E(se.network_inv)$weight, normalized = TRUE) 
eigenvector_centrality_SE <- eigen_centrality(se.network, directed = FALSE, scale = TRUE, weights = E(se.network)$weight)$vector

centrality_measures_SE <- data.frame(
  Betweenness = betweenness_centrality_SE,
  Closeness = closeness_centrality_SE,
  Eigenvector = eigenvector_centrality_SE
)

#### SN network playtime data as weight
sn.data <- read.csv2("SNnettime.csv", header = TRUE, row.names = 1)
sn.matrix <- as.matrix(sn.data)
sn.network <- graph.adjacency(sn.matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
sn.matrix_inv <- ifelse(sn.matrix == 0, Inf, 1 / sn.matrix)
sn.network_inv <- graph.adjacency(sn.matrix_inv, mode = "undirected", diag = FALSE, weighted = TRUE)

## SN centrality measures
betweenness_centrality_SN <- betweenness(sn.network_inv, directed = FALSE, weights = E(sn.network_inv)$weight, normalized = TRUE)
closeness_centrality_SN <- closeness(sn.network_inv, weights = E(sn.network_inv)$weight, normalized = TRUE) 
eigenvector_centrality_SN <- eigen_centrality(sn.network, directed = FALSE, scale = TRUE, weights = E(sn.network)$weight)$vector

centrality_measures_SN <- data.frame(
  Betweenness = betweenness_centrality_SN,
  Closeness = closeness_centrality_SN,
  Eigenvector = eigenvector_centrality_SN
)


# Function to create a formatted data frame with centrality measures
create_metrics_df <- function(metrics_df, group_name) {
  data.frame(
    id = rownames(metrics_df),
    group = group_name,
    btw = metrics_df$Betweenness,
    close = metrics_df$Closeness,
    eigen = metrics_df$Eigenvector
  )
}

# Create centrality measure data frames for each group
centrality_measures_PLA <- data.frame(
  Betweenness = betweenness_centrality,
  Closeness = closeness_centrality,
  Eigenvector = eigenvector_centrality
)

centrality_measures_AGA <- data.frame(
  Betweenness = betweenness_centrality_AGA,
  Closeness = closeness_centrality_AGA,
  Eigenvector = eigenvector_centrality_AGA
)

centrality_measures_LIZ <- data.frame(
  Betweenness = betweenness_centrality_LIZ,
  Closeness = closeness_centrality_LIZ,
  Eigenvector = eigenvector_centrality_LIZ
)

centrality_measures_CH <- data.frame(
  Betweenness = betweenness_centrality_CH,
  Closeness = closeness_centrality_CH,
  Eigenvector = eigenvector_centrality_CH
)

centrality_measures_CP <- data.frame(
  Betweenness = betweenness_centrality_CP,
  Closeness = closeness_centrality_CP,
  Eigenvector = eigenvector_centrality_CP
)

centrality_measures_SE <- data.frame(
  Betweenness = betweenness_centrality_SE,
  Closeness = closeness_centrality_SE,
  Eigenvector = eigenvector_centrality_SE
)

centrality_measures_SN <- data.frame(
  Betweenness = betweenness_centrality_SN,
  Closeness = closeness_centrality_SN,
  Eigenvector = eigenvector_centrality_SN
)

# Apply the function to create final metrics data frames for each group
metrics_PLA <- create_metrics_df(centrality_measures_PLA, "PLA")
metrics_AGA <- create_metrics_df(centrality_measures_AGA, "AGA")
metrics_LIZ <- create_metrics_df(centrality_measures_LIZ, "LIZ")
metrics_CH <- create_metrics_df(centrality_measures_CH, "CH")
metrics_CP <- create_metrics_df(centrality_measures_CP, "CP")
metrics_SE <- create_metrics_df(centrality_measures_SE, "SE")
metrics_SN <- create_metrics_df(centrality_measures_SN, "SN")

# Combine all into a single data frame
all_metrics <- rbind(metrics_PLA, metrics_AGA, metrics_LIZ, metrics_CH, 
                     metrics_CP, metrics_SE, metrics_SN)




#resulting data frame
head(all_metrics)

##############################################PLOTTING 7 networks

# Define the custom 'triangle' shape for females
mytriangle <- function(coords, v = NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1 / 200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  # Plot the triangle shape
  symbols(
    x = coords[, 1], y = coords[, 2], bg = vertex.color,
    stars = cbind(vertex.size, vertex.size, vertex.size),
    add = TRUE, inches = FALSE
  )
}

# Register the custom 'triangle' shape
add_shape("triangle", 
          clip = shapes("circle")$clip, 
          plot = mytriangle)

################
################
################


# Assigning different shapes based on age and sex class
V(pla.network)$class <- c("adult male", "adult male", 
                          "adult female", "adult female", 
                          "immature", "immature", "immature")

# Map the class to vertex shapes, with custom triangle, square, and circle
V(pla.network)$shape <- ifelse(V(pla.network)$class == "adult male", "square", 
                               ifelse(V(pla.network)$class == "adult female", "triangle", 
                                      ifelse(V(pla.network)$class == "immature", "circle", "triangle")))

colorblind_colors <- c("adult male" = "#56B4E9",  # Orange (Color Universal Design)
                       "adult female" = "#E69F00",  # Light Blue (Color Universal Design)
                       "immature" = "#009E73")  # Green (Color Universal Design)

# Assign colors to nodes based on their class
V(pla.network)$color <- colorblind_colors[V(pla.network)$class]

# Apply clustering and degree calculation
pla.clust <- cluster_louvain(pla.network)


plot(pla.network)

###############AGA2

# Assigning the updated class names
V(aga.network)$class <- c(rep("adult female", 13), 
                          rep("adult male", 12), 
                          rep("immature", 14))

# Set the colors (colorblind-friendly)
V(aga.network)$color <- c(rep("#E69F00", 13),   # Adult females (orange)
                          rep("#56B4E9", 12),   # Adult males (light blue)
                          rep("#009E73", 14))   # Immatures (green)

# Assigning the shapes
V(aga.network)$shape <- c(rep("triangle", 13),    # Adult females
                          rep("square", 12),     # Adult males
                          rep("circle", 14))     # Immatures

# Plot the AGA network with the updated class names and closeness centrality
plot(aga.network)


#####
#####LIZ3

# Assigning the updated class names for LIZ network
V(liz.network)$class <- c("adult female", "adult female", "adult female", "adult female",
                          "adult male", "immature")

# Set the colors (colorblind-friendly)
V(liz.network)$color <- c(rep("#E69F00", 4),   # Adult females (orange)
                          rep("#56B4E9", 1),   # Adult males (light blue)
                          rep("#009E73", 1))   # Immatures (green)

# Assigning the shapes
V(liz.network)$shape <- c(rep("triangle", 4),    # Adult females
                          rep("square", 1),     # Adult males
                          rep("circle", 1))     # Immatures

# Apply clustering and degree calculation
liz.clust <- cluster_louvain(liz.network)

plot(liz.network)


#######
#####SE4


# Assigning the updated class names for SE network
V(se.network)$class <- c("adult female", "adult female", 
                         "adult female", "adult male")

# Set the colors (colorblind-friendly)
V(se.network)$color <- c(rep("#E69F00", 3),   # Adult females (orange)
                         rep("#56B4E9", 1))   # Adult male (light blue)

# Assigning the shapes
V(se.network)$shape <- c(rep("triangle", 3),    # Adult females
                         rep("square", 1))     # Adult male

# Apply clustering and degree calculation
se.clust <- cluster_louvain(se.network)
degree(se.network)

# Plot the SE network with the updated class names and closeness centrality
plot(se.network) 


##############CH
###CH
#####CH5


# Assigning the updated class names for CH network (without subadult)
V(ch.network)$class <- c("adult female", "adult female", 
                         "adult female", "adult female", 
                         "adult male", "adult male", "adult male",
                         "adult male", "immature", "immature",
                         "immature", "immature", "immature")



# Set the colors (colorblind-friendly)
V(ch.network)$color <- c(rep("#E69F00", 4),  # Adult females (orange)
                         rep("#56B4E9", 4),  # Adult males (light blue)
                         rep("#009E73", 5))  # Immature (yellow)

# Assigning the shapes
V(ch.network)$shape <- c(rep("triangle", 4),  # Adult females (triangles)
                         rep("square", 4),    # Adult males (squares)
                         rep("circle", 5))    # Immature (circles)

# Apply clustering and degree calculation
ch.clust <- cluster_louvain(ch.network)
degree(ch.network)

# Plot the CH network with the updated class names and closeness centrality
plot(ch.network) 




###############
#####SN6

## Assigning the updated class names for SN network
V(sn.network)$class <- c("adult female", "adult female", "adult female", 
                         "adult male", "adult male", "immature", "immature")



# Set the colors (colorblind-friendly)
V(sn.network)$color <- c(rep("#E69F00", 3),  # Adult females (orange)
                         rep("#56B4E9", 2),  # Adult males (light blue)
                         rep("#009E73", 2))  # Immature (yellow)

# Assigning the shapes
V(sn.network)$shape <- c(rep("triangle", 3),  # Adult females (triangles)
                         rep("square", 2),    # Adult males (squares)
                         rep("circle", 2))    # Immature (circles)

# Apply clustering and degree calculation
sn.clust <- cluster_louvain(sn.network)
degree(sn.network)

# Plot the SN network with the updated class names and closeness centrality
plot(sn.network)


#######################################
######################################

#CP
#CP
#####

# Assigning the updated class names for CP network
V(cp.network)$class <- c("adult female", "adult female", "adult female", "adult female",
                         "adult female", "adult female", "adult male", "adult male", 
                         "adult male", "immature", "immature", "immature", "immature")

# Set the colors (colorblind-friendly)
V(cp.network)$color <- c(rep("#E69F00", 6),   # Adult females (orange)
                         rep("#56B4E9", 3),   # Adult males (light blue)
                         rep("#009E73", 4))   # Immature (yellow)

# Assigning the shapes
V(cp.network)$shape <- c(rep("triangle", 6),  # Adult females (triangles)
                         rep("square", 3),    # Adult males (squares)
                         rep("circle", 4))    # Immature (circles)

# Apply clustering and degree calculation
cp.clust <- cluster_louvain(cp.network)
degree(cp.network)

# Plot the CP network with the updated class names and closeness centrality
plot(cp.network)

######
#############################################################################
#######################################
##########ALL THE 7, alphabetically, and use eigenvector as vertex size
########****************************

par(mar = c(0, 0, 1.5, 0) + 0.0)
b <- layout(matrix(c(1,1,1,1,2,2,3,3,4,4,
                     1,1,1,1,2,2,3,3,4,4,
                     1,1,1,1,5,5,6,6,7,7,
                     1,1,1,1,5,5,6,6,7,7
), 4, 10, byrow = TRUE))
b
layout.show(7)

plot(aga.network, 
     vertex.size = 5 + eigen_centrality(aga.network)$vector * 10,  # Scale based on eigenvector centrality
     vertex.label = NA,  # Remove node labels
     edge.width = E(aga.network)$weight/500,  # Use original edge weights
     edge.color = "black",  # Set edge color to black
     vertex.color = V(aga.network)$color,  # Apply colors based on class
     vertex.shape = V(aga.network)$shape,  # Apply shapes based on class
     vertex.frame.color = NA,  # Remove borders around nodes
     mark.groups = communities(aga.clust))  # Mark communities based on Louvain clustering


legend("top",
       title = "age-sex class",
       legend=c("immature", "adult female", "adult male"),
       col=c("#009E73", "#E69F00","#56B4E9"),
       pch = c(19,17,15),cex=2,
       bty = "n")

legend("left",as.expression(bquote(bold("AGA"))),
       bty="n",
       cex=2)

# Apply the same updates to all other networks
plot(pla.network, 
     vertex.size = 10 + eigen_centrality(pla.network)$vector * 10,
     vertex.label = NA,
     edge.width = E(pla.network)$weight/800,
     edge.color = "black",
     vertex.frame.color = NA,
     mark.groups = communities(pla.clust))
title(main = "PLA", cex.main = 2)

plot(liz.network, 
     vertex.size = 10 + eigen_centrality(liz.network)$vector * 10,
     vertex.label = NA,
     edge.width = E(liz.network)$weight/500,
     edge.color = "black",
     vertex.color = V(liz.network)$color,
     vertex.shape = V(liz.network)$shape,
     vertex.frame.color = NA,
     mark.groups = communities(liz.clust))
title(main = "LIZ", cex.main = 2)

plot(se.network, 
     vertex.size = 10 + eigen_centrality(se.network)$vector * 10,
     vertex.label = NA,
     edge.width = E(se.network)$weight/100,
     edge.color = "black",
     vertex.color = V(se.network)$color,
     vertex.shape = V(se.network)$shape,
     vertex.frame.color = NA,
     mark.groups = communities(se.clust))
title(main = "SE", cex.main = 2)

plot(ch.network, 
     vertex.size = 10 + eigen_centrality(ch.network)$vector * 10,
     vertex.label = NA,
     edge.width = E(ch.network)$weight/500,
     edge.color = "black",
     vertex.color = V(ch.network)$color,
     vertex.shape = V(ch.network)$shape,
     vertex.frame.color = NA,
     mark.groups = communities(ch.clust))
title(main = "CH", cex.main = 2)

plot(sn.network, 
     vertex.size = 10 + eigen_centrality(sn.network)$vector * 10,
     vertex.label = NA,
     edge.width = E(sn.network)$weight/500,
     edge.color = "black",
     vertex.color = V(sn.network)$color,
     vertex.shape = V(sn.network)$shape,
     vertex.frame.color = NA,
     mark.groups = communities(sn.clust))
title(main = "SN", cex.main = 2)

plot(cp.network, 
     vertex.size = 10 + eigen_centrality(cp.network)$vector * 10,
     vertex.label = NA,
     edge.width = E(cp.network)$weight/600,
     edge.color = "black",
     vertex.color = V(cp.network)$color,
     vertex.shape = V(cp.network)$shape,
     vertex.frame.color = NA,
     mark.groups = communities(cp.clust))
title(main = "CP", cex.main = 2)
##################################################





  
  
  
  
  