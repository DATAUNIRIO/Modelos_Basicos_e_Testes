
# https://kateto.net/netscix2016.html
library(igraph)
library(readxl)
library(tidyr)
library(dplyr)


board_2009 <- read_excel("dados/Boards RAW 2009.xlsx")
names(board_2009) <- c('Codigo','Nome','CPF','Code')
names(board_2009)
board_2009 <- board_2009 %>% select(-CPF)

board_2009 <- board_2009 %>% fill(Codigo)

board_2009$teste <- ifelse(board_2009$Nome=="Name",1,0)
table(board_2009$teste)
board_2009 <- board_2009[board_2009$teste==0,]
board_2009 <- board_2009 %>% drop_na(Nome)

board <- board_2009 %>% select(-teste)
board <- board %>% select(-Code) 

dim(board)[1] - length(unique(board$Nome))

board$teste <- ifelse(board$Codigo==board$Nome,1,0)
table(board$teste)
board <- board[board$teste==0,]
board <- board %>% select(-teste) 

dim(board)[1] - length(unique(board$Nome))

board2 <- table(board$Nome) %>% data.frame()
board2 <- board2[board2$Freq>2,]

board$maisdedois <- ifelse(board$Nome %in% board2$Var1,1,0)

board_amostra <- board[board$maisdedois==1,]
my_data2 <- my_data[, c(5, 4, 1, 2, 3)]

board_amostra <- board_amostra[,c(2,1)]

dev.off()

net <- graph.data.frame(board_amostra, directed=T)
V(net)
E(net)

V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

set.seed(12345)
plot(net,
     vertex.color = 'yellow',
     vertext.size = 0.6,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.5)

centralidade <- closeness(net, mode="all")
centralidade <- data.frame(centralidade)


degree(net)
eigen_centrality(net)

board_amostra <- sample_n(board,50)

net <- graph.data.frame(board_amostra, directed=T)
V(net)
E(net)

V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

set.seed(12345)
plot(net,
     vertex.color = 'yellow',
     vertext.size = 0.3,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.3)


dim(board)[1] - length(unique(board$Nome))

board_2 <- board 
  
  
board_2 <- sample_n(board,50)
  dev.off()
  
  
