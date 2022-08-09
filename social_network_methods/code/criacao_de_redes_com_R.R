
Parte 1 - Teoria & Aplicação no R
1. Núcleos & ligações
2. Revisão de matriz (matriz quadrada e matriz simétrica)
3. Matriz de ligações (matriz de adjacência)
4. Graficos de rede
5. Conecção and distância
6. A distância geodésica
7. Medidas de centralidade e poder
7.1 closeness
7.2 betweenness 
7.3 degrees 
7.4 eigenvector centrality
8. Grupos e comunidades 

Parte 2 - Aplicada com R
1 - construindo redes com o R
2 - criação de matrizes com o R
3 - distância geodésica com o R
4 - as quatro medidas de centralidade e poder com o R
5 - identificando grupos com o R

Parte 3 - O estado da arte, desafios e principais problemas que encontrei
  aprendendo a usar a melhor ferramenta de redes (gephi network analysis)
  
  
  
  social network methods
  
  The hub scores of the vertices are defined as the principal eigenvector of At(A), where A is the adjacency matrix of the graph
  The authority scores of the vertices are defined as the principal eigenvector of t(A)A, where A is the adjacency matrix of the graph. 
  smallworldness(x, B = 1000, up = 0.995, lo = 0.005)
  indice mundo pequeno
  sample_smallworld(dim, size, nei, p, loops = FALSE, multiple = FALSE)
  smallworld(...)
  

  http://pablobarbera.com/big-data-upf/html/02a-networks-intro-visualization.html

library(igraph)
library(readr)

data <- read_csv('https://raw.githubusercontent.com/finnstats/finnstats/main/socialnetworkdata.csv')
y <- data.frame(data$first, data$second)

net <- graph.data.frame(y, directed=T)

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

net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)
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
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        friendship=c(7,5,5,2,1,1))
graph <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plot(graph)

# distancia geodesica
distances(graph, mode="all")
closeness(graph, mode="all")

closeness(graph)
closeness(graph, mode="in")
closeness(graph, mode="out")
closeness(graph, mode="all")
igraph::centr_clo(graph)
igraph::centr_betw(graph)
igraph::centr_degree(graph)
igraph::centr_eigen(graph)


betweenness(
  graph,
  v = V(graph),
  directed = TRUE,
  nobigint = TRUE,
  normalized = FALSE
)
degree(graph)
eigen_centrality(graph)

igraph::cluster_optimal(graph)

library(readr)
relacoes <- read_csv('https://raw.githubusercontent.com/pablobarbera/data-science-workshop/master/sna/data/star-wars-network-edges.csv')
pessoas <- data.frame(relacoes$source)
pessoas2<- data.frame(relacoes$target)
names(pessoas) <- 'pessoas'
names(pessoas2) <- 'pessoas'
library(dplyr)
pessoas <- pessoas %>% add_row(pessoas2)
seres <- unique(pessoas)
remove(pessoas,pessoas2)

save(relacoes,seres,file = 'C:/Users/Hp/Documents/GitHub/Modelos_Basicos_e_Testes/social_network_methods/dados/star/star.Rdata')

load('C:/Users/Hp/Documents/GitHub/Modelos_Basicos_e_Testes/social_network_methods/dados/star/star.Rdata')

graph <- graph_from_data_frame(relacoes, directed=TRUE, vertices=seres)
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




