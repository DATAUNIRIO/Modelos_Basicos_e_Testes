
# social network methods
  
#  The hub scores of the vertices are defined as the principal eigenvector of At(A), where A is the adjacency matrix of the graph
#  The authority scores of the vertices are defined as the principal eigenvector of t(A)A, where A is the adjacency matrix of the graph. 
  
http://pablobarbera.com/big-data-upf/html/02a-networks-intro-visualization.html
https://stats.stackexchange.com/questions/175492/how-to-test-statistically-whether-my-network-graph-is-a-small-world-network

library(igraph)
library(readr)
dev.off()
#data <- read_csv('https://raw.githubusercontent.com/finnstats/finnstats/main/socialnetworkdata.csv')
#y <- data.frame(data$first, data$second)

nomes <- c("Steven", "Joao", "Manu","AAA","BBB")

nomes <- c("Ciro" , "Eymael" , "Felipe" , "Jair" , 
"Léo" , "Luiz" , "Kelmon" , "Simone" , "Sofia" , "Vera")  

dados <- data.frame(
  from = sample(nomes, 50, TRUE),
  to = sample(nomes, 50, TRUE),
  weight = runif(50)
)

net <- graph.data.frame(dados, directed=T)

V(net)
E(net)

V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

set.seed(12345)
plot(net,
     vertex.color = 'green',
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)

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


hs <- hub_score(net)$vector
as <- authority.score(net)$vector
set.seed(12345)
plot(net,
     vertex.size=hs*30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size=as*30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)


# Community Detection
# To detect densely connected nodes

#net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)
cnet
plot(cnet,
     net,
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

nome=c("Alice", "Bob", "Cecil", "David","Esmeralda")

relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        friendship=c(7,5,5,2,1,1))
graph <- graph_from_data_frame(relations, directed=TRUE, vertices=nome)

plot(graph)

# distancia geodesica
distances(graph, mode="all")
# Transitivity
transitivity(graph)

closeness(graph, mode="all")
closeness(graph)
closeness(graph, mode="in")
closeness(graph, mode="out")
closeness(graph, mode="all")

igraph::centr_clo(graph,mode = c("all"))

igraph::centr_betw(graph)
igraph::centr_degree(graph)
igraph::centr_eigen(graph)


betweenness(
  graph,
  v = V(graph),
  directed = TRUE,
  normalized = FALSE
)
degree(graph)

eigen_centrality(graph)

## Authority and Hub scores
AS <- authority_score(g)$vector
HS <- hub_score(g)$vector



library(readr)
relacoes <- read_csv('https://raw.githubusercontent.com/pablobarbera/data-science-workshop/master/sna/data/star-wars-network-edges.csv')
#save(relacoes,seres,file = 'C:/Users/Hp/Documents/GitHub/Modelos_Basicos_e_Testes/social_network_methods/dados/star/star.Rdata')

load('C:/Users/Hp/Documents/GitHub/Modelos_Basicos_e_Testes/social_network_methods/dados/star/star.Rdata')

graph <- graph_from_data_frame(relacoes, directed=TRUE)
plot(graph)


V(graph)
E(graph)

V(graph)$name # names of each node

vertex_attr(graph) # all attributes of the nodes
edge_attr(graph) # all attributes of the edges
graph[] # adjacency matrix
graph[1,] # first row of adjacency matrix

par(mar=c(0,0,0,0))
plot(graph)
plot(graph,
     vertex.color = "grey", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .75, # change size of labels to 75% of original size
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20") # change edge color to grey

V(graph)$size <- strength(graph)
plot(graph)
# taking the log to improve it
V(graph)$size <- log(strength(graph)) 

V(graph)$label <- V(graph)$name
V(graph)$degree <- degree(graph)
V(graph)$size <- degree(graph)
plot(graph)

V(graph)$label <- ifelse( strength(graph)>=10, V(graph)$name, NA )
plot(graph)

# create vectors with characters in each side
dark_side <- c("DARTH VADER", "MOTTI", "TARKIN")
light_side <- c("R2-D2", "CHEWBACCA", "C-3PO", "LUKE", "CAMIE", "BIGGS",
                "LEIA", "BERU", "OWEN", "OBI-WAN", "HAN", "DODONNA",
                "GOLD LEADER", "WEDGE", "RED LEADER", "RED TEN", "GOLD FIVE")
other <- c("GREEDO", "JABBA")

# node we'll create a new color variable as a node property
V(graph)$color <- NA
V(graph)$color[V(graph)$name %in% dark_side] <- "red"
V(graph)$color[V(graph)$name %in% light_side] <- "lightblue"
V(graph)$color[V(graph)$name %in% other] <- "grey20"
vertex_attr(graph)
plot(graph)
legend(x=.75, y=.75, legend=c("Dark side", "Light side", "Other"), 
       pch=21, pt.bg=c("red", "gold", "grey20"), pt.cex=2, bty="n")

E(graph)$width <- log(E(graph)$weight) + 1
edge_attr(graph)

par(mfrow=c(2, 3), mar=c(0,0,1,0))
plot(graph, layout=layout_randomly, main="Random")
plot(graph, layout=layout_in_circle, main="Circle")
plot(graph, layout=layout_as_star, main="Star")
plot(graph, layout=layout_as_tree, main="Tree")
plot(graph, layout=layout_on_grid, main="Grid")
plot(graph, layout=layout_with_fr, main="Force-directed")

# The most popular layouts are force-directed. These algorithms, such as Fruchterman-Reingold, try to position the nodes so that the edges have similar length and there are as few crossing edges as possible. The idea is to generate “clean” layouts, where nodes that are closer to each other share more connections in common that those that are located further apart. Note that this is a non-deterministic algorithm: choosing a different seed will generate different layouts.

par(mfrow=c(1,2))
fr <- layout_with_fr(graph, niter=1000)
par(mar=c(0,0,0,0)); plot(graph, layout=fr)

#------------------------------------------------------------
set.seed(12345)
plot(graph,
     vertex.color = 'green',
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)

plot(graph,
     vertex.color = rainbow(52),
     vertex.size = V(graph)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)

plot(graph,
     vertex.size=30,
     vertex.color = "skyblue", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = 1.5, # change size of labels to 75% of original size
     #edge.curved=.25, # add a 25% curve to the edges
     edge.arrow.size = 0.01,
     edge.color="grey20") # change edge color to grey

plot(graph,
     vertex.size=30,
     vertex.color = "lightgreen", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = 1.5, # change size of labels to 75% of original size
     #edge.curved=.25, # add a 25% curve to the edges
     edge.arrow.size = 0.8,
     edge.color="grey20") # change edge color to grey


#load('C:/Users/Hp/Documents/GitHub/Modelos_Basicos_e_Testes/social_network_methods/dados/star/star.Rdata')
g <- graph_from_data_frame(relacoes, directed=TRUE, vertices=seres)



cliques(graph)
# this usually contains cliques of size six
cliques(graph, min=6)

largest_cliques(graph)

### A real little network, Zachary's karate club data
karate <- make_graph("Zachary")
class(karate)
hub_score(karate)$vector
authority_score(karate)$vector

grupos <- igraph::cluster_optimal(graph)
grupos <- igraph::cluster_optimal(karate)

### Greedy algorithm
fc <- cluster_fast_greedy(karate)
memb <- membership(fc)
plot(karate, vertex.color=memb)
#ord1 <- order(membership(fc))

cluster_edge_betweenness(graph)

### Comparision of algorithms
communities <- list()
### cluster_edge_betweenness
ebc <- cluster_edge_betweenness(karate)
communities$`Edge betweenness` <- ebc
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
### Plot everything
layout(rbind(1:3, 4:6))
coords <- layout_with_kk(karate)
plot(coords)




# Load package
library(networkD3)

# Create fake data
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

#### JSON Data Example
# Load data JSON formated data into two R data frames
# Create URL. paste0 used purely to keep within line width.
URL <- paste0("https://cdn.rawgit.com/christophergandrud/networkD3/",
              "master/JSONdata/miserables.json")

MisJson <- jsonlite::fromJSON(URL)
# Create graph with node text faintly visible when no hovering
forceNetwork(Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
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


library(readr)
relacoes <- read_csv('https://raw.githubusercontent.com/pablobarbera/data-science-workshop/master/sna/data/star-wars-network-edges.csv')
seres <- unique(relacoes$source)
seres2 <- unique(relacoes$target)
seres <- append(seres,seres2)
seres <- data.frame(seres)
seres <- unique(seres)

forceNetwork(Links = relacoes, Nodes = seres,
             Source = "source", Target = "target",
             Value = "weight", NodeID = "seres",
             Group = "group", opacity = 0.8)

class(MisJson)
# Load igraph
library(igraph)

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

