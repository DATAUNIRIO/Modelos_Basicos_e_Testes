
# social network methods
 
library(igraph)
library(readr)
dev.off()

nomes <- c("Steven", "Joao", "Manu","AAA","BBB")

nomes <- c("Ciro" , "Eymael" , "Felipe" , "Jair" , 
"Léo" , "Luiz" , "Kelmon" , "Simone" , "Sofia" , "Vera")  

dados <- data.frame(
  from = sample(nomes, 50, TRUE),
  to = sample(nomes, 50, TRUE),
  weight = runif(50)
)

class(nomes)
class(dados)

net <- graph.data.frame(dados, directed=T)
class(net)
V(net)
E(net)

net[] # adjacency matrix
net[1,] # first row of adjacency matrix

plot(net)

# distancia
distances(net, mode="all")

# Transitivity
transitivity(net)

set.seed(12345)

V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

plot(net)

plot(net,
     vertex.color = 'green',
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)

# Layout multidimensional scaling - MDS
plot(net,
     vertex.color = 'skyblue',
     vertex.size = 2,
     edge.arrow.size = 0.1,
     layout=layout_with_mds(net))

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.graphopt)

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.kamada.kawai)

#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------

degree(net)
degree(net, mode="all")
degree(net, mode="in")
degree(net, mode="out")

closeness(net)
closeness(net, mode="all")
closeness(net, mode="in")
closeness(net, mode="out")

betweenness(net)
betweenness(net,
  v = V(net),
  directed = TRUE,
  normalized = FALSE)

eigen_centrality(net)$vector

## Authority and Hub scores
AS <- authority_score(net)$vector
HS <- hub_score(net)$vector

plot(net,
     vertex.size=HS*30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size=AS*30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

#--------------------------------------------------------
# Community Detection
# To detect densely connected nodes
#--------------------------------------------------------

cnet <- cluster_edge_betweenness(net)
cnet
plot(cnet,
     net,
     edge.arrow.size=0.1,
     vertex.size = 10,
     vertex.label.cex = 0.8)
# Now you can see within the groups there are dense connections and between the groups with sparse connection


#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------

## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
actors <- data.frame(nome=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     idade=c(48,33,45,34,21),
                     sexo=c("F","M","F","M","F"))

relations <- data.frame(de=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        para=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        amizade=c(7,5,5,2,1,1))

net2 <- graph.data.frame(relations, directed=T)
plot(net2)

closeness(net2)
betweenness(net2)

#igraph::centr_clo(net2)
#igraph::centr_betw(net2)
#igraph::centr_degree(net2)
#igraph::centr_eigen(net2)

degree(net2)
eigen_centrality(net2)
## Authority and Hub scores
AS2 <- authority_score(net2)$vector
HS2 <- hub_score(net2)$vector




load('C:/Users/Hp/Documents/GitHub/Modelos_Basicos_e_Testes/social_network_methods/dados/star/star.Rdata')



net3 <- graph_from_data_frame(relacoes, directed=TRUE)
plot(net3)
plot(net3,edge.arrow.size=0.1)

V(net3)
E(net3)


vertex_attr(net3) # all attributes of the nodes
edge_attr(net3) # all attributes of the edges
net3[] # adjacency matrix
net3[1,] # first row of adjacency matrix

par(mar=c(0,0,0,0))
plot(net3)
plot(net3,
     vertex.color = "grey", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .75, # change size of labels to 75% of original size
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20") # change edge color to grey

V(net3)$size <- strength(net3)
plot(net3)
# taking the log to improve it
V(net3)$size <- log(strength(net3)) 

V(net3)$label <- V(net3)$name
V(net3)$degree <- degree(net3)
V(net3)$size <- degree(net3)
plot(net3)

V(net3)$label <- ifelse(strength(net3)>=10, V(net3)$name, NA)
plot(net3)

# create vectors with characters in each side
dark_side <- c("DARTH VADER", "MOTTI", "TARKIN")
light_side <- c("R2-D2", "CHEWBACCA", "C-3PO", "LUKE", "CAMIE", "BIGGS",
                "LEIA", "BERU", "OWEN", "OBI-WAN", "HAN", "DODONNA",
                "GOLD LEADER", "WEDGE", "RED LEADER", "RED TEN", "GOLD FIVE")
other <- c("GREEDO", "JABBA")

# node we'll create a new color variable as a node property
V(net3)$color <- NA
V(net3)$color[V(net3)$name %in% dark_side] <- "red"
V(net3)$color[V(net3)$name %in% light_side] <- "lightblue"
V(net3)$color[V(net3)$name %in% other] <- "grey20"
vertex_attr(net3)
plot(net3)
legend(x=.75, y=.75, legend=c("Dark side", "Light side", "Other"), 
       pch=21, pt.bg=c("red", "lightblue", "grey20"), pt.cex=2, bty="n")

E(net3)$width <- log(E(net3)$weight) + 1
edge_attr(net3)

par(mfrow=c(2, 3), mar=c(0,0,1,0))
plot(net3, layout=layout_randomly, main="Random")
plot(net3, layout=layout_in_circle, main="Circle")
plot(net3, layout=layout_as_star, main="Star")
plot(net3, layout=layout_as_tree, main="Tree")
plot(net3, layout=layout_on_grid, main="Grid")
plot(net3, layout=layout_with_fr, main="Force-directed")
# The most popular layouts are force-directed. These algorithms, such as Fruchterman-Reingold, try to position the nodes so that the edges have similar length and there are as few crossing edges as possible. The idea is to generate “clean” layouts, where nodes that are closer to each other share more connections in common that those that are located further apart. Note that this is a non-deterministic algorithm: choosing a different seed will generate different layouts.

par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
fr <- layout_with_fr(net3, niter=1000)
plot(net3, layout=fr)

#------------------------------------------------------------
set.seed(12345)
par(mfrow=c(2,2))

plot(net3,
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)

plot(net3,
     vertex.color = rainbow(52),
     vertex.size = V(net3)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)

plot(net3,
     vertex.size=30,
     vertex.color = "skyblue", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = 1.5, # change size of labels to 75% of original size
     #edge.curved=.25, # add a 25% curve to the edges
     edge.arrow.size = 0.01,
     edge.color="grey20") # change edge color to grey

plot(net3,
     vertex.size=30,
     vertex.color = "lightgreen", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = 1.5, # change size of labels to 75% of original size
     #edge.curved=.25, # add a 25% curve to the edges
     edge.arrow.size = 0.8,
     edge.color="grey20") # change edge color to grey


#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Load package
library(networkD3)

# Create data
nomes <- c("Ciro" , "Eymael" , "Felipe" , "Jair" , 
           "Léo" , "Luiz" , "Kelmon" , "Simone" , "Sofia" , "Vera")  

dados <- data.frame(
  src = sample(nomes, 50, TRUE),
  target = sample(nomes, 50, TRUE),
  weight = runif(50)
)
# Plot
simpleNetwork(dados)

# Load data
data(MisLinks)
data(MisNodes)
View(MisNodes)
View(MisLinks)
# Plot

forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)


#### Os Miseraveis (Vitor Hugo)
load('C:/Users/Hp/Documents/GitHub/Modelos_Basicos_e_Testes/social_network_methods/dados/star/os_miseraveis.Rdata')

# Create graph with node text faintly visible when no hovering
forceNetwork(Links = os_miseraveis$links, Nodes = os_miseraveis$nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4, bounded = TRUE,
             opacityNoHover = TRUE)


## Specify colours for specific edges
# Find links to Valjean (11)
which(MisNodes == "Valjean", arr = TRUE)[1] - 1
ValjeanInds = which(MisLinks == 11, arr = TRUE)[, 1]
# Create a colour vector
ValjeanCols = ifelse(1:nrow(MisLinks) %in% ValjeanInds, "#bf3eff", "#666")

forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8, linkColour = ValjeanCols)


#--------------------------------------------------------------------
# Use igraph to make the graph and find membership
karate <- make_graph("Zachary")
wc <- cluster_walktrap(karate)
members <- membership(wc)
# Convert to object suitable for networkD3
karate_d3 <- igraph_to_networkD3(karate, group = members)

# Create force directed network plot
forceNetwork(Links = karate_d3$links, Nodes = karate_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')

#simpleNetwork(networkData) %>%
#  saveNetwork(file = 'Net1.html')



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

cliques(net3)
# this usually contains cliques of size six
cliques(net3, min=6)
largest_cliques(net3)


### A real little network, Zachary's karate club data
karate <- make_graph("Zachary")
class(karate)
plot(karate)

hub_score(karate)$vector
authority_score(karate)$vector

grupos <- igraph::cluster_optimal(karate)
grupos

### Greedy algorithm
fc <- cluster_fast_greedy(karate)

memb <- membership(fc)

plot(karate, vertex.color=memb)
#ord1 <- order(membership(fc))

### Comparision of algorithms
communities <- list()

### cluster_edge_betweenness
ceb <- cluster_edge_betweenness(karate)
communities$`Edge betweenness` <- ceb
### cluster_fast_greedy
fc <- cluster_fast_greedy(karate)
communities$`Fast greedy` <- fc
### cluster_leading_eigen
lec <- cluster_leading_eigen(karate)
communities$`Leading eigenvector` <- lec
### cluster_spinglass
sc <- cluster_spinglass(karate, spins=10)
communities$`Spinglass` <- sc
### cluster_walktrap
wt <- cluster_walktrap(karate)
communities$`Walktrap` <- wt
### cluster_label_prop
labprop <- cluster_label_prop(karate)
communities$`Label propagation` <- labprop

