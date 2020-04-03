

#Livros Gratuitos da Springer

#Acabei de saber na lista de emails da Associação Brasileira de Estatística 
#que a editora Springer disponibilizou mais de 500 livros gratuitamente. 
#Basta visitar o site e baixar, sem sequer necessitar fazer cadastro. 
#A lista completa de livros está nesta planilha do Excel, mas abaixo eu coloquei 
#a listagem de livros de Matemática e Estatística, colocados em ordem alfabética do título. Aproveitem.


library(rvest)     
page <- read_html("https://marcusnunes.me/posts/livros-gratuitos-da-springer/")
links<-page %>% html_nodes("a") %>% html_attr('href')
links<-links[20:76]
links

nomes<-page %>% html_nodes("a") %>% html_text()
nomes<-nomes[20:76]

Livros_de_graca<-tibble::tibble(nomes,links)


#save(Livros_de_graca,file="Livros_de_graca.RData")

paste0("[",Livros_de_graca$nomes,"](",Livros_de_graca$links,")")
