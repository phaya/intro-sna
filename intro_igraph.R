# R Scripting I
library(igraph)
rm(list=ls())

# Representaciones más habituales de un grafo

# Matriz de adjacencia: cada posición ij con un valor distinto de cero representa un enlace entre el nodo-i y el nodo-j. En el caso de grafos simples, el enlace se representa con un 1. Si la matriz es simétrica, el grafo es no dirigido. El valor de cada posición también se puede interpretar como el peso del enlace. 

adjmatrix <- matrix( 
    c(0, 1, 1, 1, 0, 0, 0, 0, 
      0, 0, 1, 0, 1, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 0,
      1, 0, 0, 0, 0, 0, 0, 0,
      0, 1, 1, 0, 0, 1, 0, 0,
      1, 0, 1, 0, 0, 0, 0, 1,
      0, 1, 1, 1, 0, 1, 0, 1,
      0, 0, 0, 1, 1, 1, 1, 0), 
    nrow=8,              
    ncol=8,
    byrow=TRUE)        
g <- graph_from_adjacency_matrix(adjmatrix)
str(g)
plot(g, edge.arrow.size=0.01, edge.arrow.width=0.01)
as_adjacency_matrix(g)
V(g)
E(g)

# Lista de enlaces (edgelist):
# Cada enlace se representa con una tupla (i,j) que indica que el nodo i está conectado al nodo j. El conjunto de enlaces forman una matriz o data frame de 2 columnas por tantas filas como enlaces tenga el grafo 

el <- matrix(c(1,5, 
               1,4, 
               2,1, 
               2,3,
               2,4,
               3,5,
               4,2,
               4,3,
               4,6,
               5,1,
               5,2), ncol=2, byrow=TRUE)
g <- graph_from_edgelist(el)
plot(g, edge.arrow.size=0.1, edge.arrow.width=0.1)
g <- graph_from_edgelist(el, directed = FALSE)
plot(g, edge.arrow.size=0.1, edge.arrow.width=0.1)
g <- simplify(g)
plot(g, edge.arrow.size=0.1, edge.arrow.width=0.1)

# Lista de enlaces y descripción de nodos (extendida)
# Los enlaces se representa como un data frame tal como sea explicado anteriormente teniendo en cuenta que columnas adicionales a las dos primeras se tratan como propiedades del enlace. Los nodos se representan como un data frame donde la primera columna incluye el identificador del mismo, y las restantes columnas representan propiedades del nodo 


edges <- data.frame(from=c("andres", "andres","maria", "ana","daniel", "daniel"),
                    to=c("maria","daniel", "ana","andres","andres","ana"),
                    weight=c(0.3, 0.3, 0.5, 0.7, 0.8, 0.1))
nodes <- data.frame(name=c("andres", "maria", "ana", "daniel"),
                    gender=c("M","F","F","M"))

g <- graph_from_data_frame(edges, vertices = nodes)
str(g)
V(g)$gender
plot(g, vertex.color=ifelse(V(g)$gender=="M", "green", "yellow"), edge.width = E(g)$weight*10, edge.arrow.size=0.01, edge.arrow.width=0.01)

# Podemos crear grafos siguiendo un estructura determinada
g <- make_full_graph(10)
plot(g)
g <- make_full_graph(10, directed = TRUE)
plot(g, edge.arrow.size=0.3, edge.arrow.width=0.3)
g <- make_ring(10)   
plot(g)
g <- make_star(10)
plot(g,edge.arrow.size=0.3, edge.arrow.width=0.3)
g <- make_tree(10)
plot(g,edge.arrow.size=0.01, edge.arrow.width=0.01)
g <- make_lattice(c(2,2,3)) #lattice graphs
plot(g)

# Grafos famosos
# Social network of friendships between 34 members of a karate club at a US university in the 1970s. See W. W. Zachary, An information flow model for conflict and fission in small groups, Journal of Anthropological Research 33, 452-473 (1977). 
g <- make_graph("Zachary")
plot(g)

# Leer un grafo desde un fichero
# edglist: el formato más simple que existe. El id de los nodos empieza en cero, y se numeran secuencialmente 
g <- read_graph("el.txt", directed = FALSE)
plot(g,edge.arrow.size=0.01, edge.arrow.width=0.01)
# formatos soportados
#c("edgelist", "pajek", "ncol", "lgl", "graphml",
#  "dimacs", "graphdb", "gml", "dl")

# ncol: los nodos se identifican con etiquetas 
g <- read_graph("ncol.txt", format="ncol", directed=TRUE)
plot(g)

# Leer desde un fichero de enlaces y nodos
edges <- read.csv("edges.csv")
nodes <- read.csv("nodes.csv")
g <- graph_from_data_frame(edges, vertices = nodes)

E(g)$weight <- as.numeric(E(g)$weight)

# Operaciones básicas
V(g)
V(g)[V(g)$gender=="F"]

E(g)
E(g)[2:4 %--% 2:4] 
plot(g,edge.arrow.size=0.01, edge.arrow.width=0.01)

E(g)["andres" %--% 1:4]
incident(g, 1)

E(g)[1 %->% 1:4]
incident(g, 1, mode="out")

E(g)[E(g)$weight > 0.4]

neighbors(g, 1)
neighbors(g, 1, mode="all")

g <- set_edge_attr(g, "polarity", value = c("pos","pos","neg","pos","neg","neg"))
E(g)[E(g)$polarity == "pos"]

#Adding vertices/edges
g <- g + vertex("carla", gender="F")
g <- g + edge("carla", "maria", weight=0.2, polarity="pos")
g <- g + edge("carla", "andres", weight=0.8, polarity="neg")
plot(g, vertex.color=ifelse(V(g)$gender=="M", "green", "yellow"), edge.width = E(g)$weight*10, edge.arrow.size=0.01, edge.arrow.width=2)

g <- delete_vertices(g, (V(g)$name=="carla"))

gfem <- induced.subgraph(g, V(g)$gender=="F")
gfem

# R Scripting II
################################################################################
# Propiedades de las redes
################################################################################
set.seed(0)
grand <- erdos.renyi.game(1000, 0.1)
plot(degree.distribution(grand), xlab="degree", ylab="fraction of nodes",
     pch=1, col=1, type="b")

gpa <- sample_pa(1000, power = 1.5) # old barabasi game
plot(degree.distribution(gpa), xlab="degree", ylab="fraction of nodes",
     pch=1, col=1, type="b", log="x")

gsw <- sample_smallworld(1, 1000, 10, 0.1) # old watts.strogatz.game
mean_distance(gsw)
transitivity(gsw, type="average")

mean_distance(grand)
transitivity(grand, type="average")
