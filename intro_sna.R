# igraph es la biblioteca que nos permite manipular redes facilmente. 
library(igraph)
rm(list=ls())

# Leemos la red de ejemplo. Esta red se compone de usuarios de Twitter que 
# han retuiteado a otros usuarios
# La red se define como una lista de enlaces dirigidos donde cada enlace define 
# que un usuario (retweeter) retuitea a otro usuario (retweeted).
df <- read.csv("https://dl.dropboxusercontent.com/u/686390/network.txt", sep=" ")

g <- graph_from_data_frame(df, directed=TRUE)
rm(df)
g

############################################################################################################
# ¿Cuántos nodos y cuantos enlaces tiene la red?
############################################################################################################
# V(g) nos devuelve la sequencia de nodos que componen la red. De la misma manera E(g) nos devuelve una
# sequencia de enlaces
N <- gorder(g) # vcount
M <- gsize(g)  # ecount
# N = 67561, M = 192986

############################################################################################################
# Distribución de grado
############################################################################################################
# Una de las propiedades más interesantes de una red es su distribución de grado. Esta nos aporta
# información valiosa sobre su estructura, en particular, si la comparamos con la estructura que
# tendría una red aleatoria equivalente en nodos y enlaces. 

# degree(g) nos devuelve un vector con el grado de cada nodo, mientras que degree_distribution nos devuelve 
# la frecuencia relativa de cada grado.
# Asi,
degree(g, mode="in")[100]
# nos devuelve el grado del nodo de índice 100, mientras que
degree_distribution(g, mode="in")[2]
# nos dice la frecuencia relativa de los nodos que han sido retuiteados una vez

# El grado medio de la red, y su desviación estandar se pueden calcular facilmente
g.kmean <- mean(degree(g, mode="in"))
g.sd <- sd(degree(g, mode="in"))
# g.kmean = 2.84 y g.sd = 32.033

# Vamos a comparar la distribución de grado de esta red con su equivalente aleatorio
# Nos creamos una red aleatoria que tenga N nodos y M enlaces escogiéndose estos M enlaces uniformente
# entre el conjunto de todos los posibles enlaces.
grand <- erdos.renyi.game(N, M, type="gnm", directed=TRUE)

grand.kmean <- mean(degree(grand, mode="in"))
grand.sd <- sd(degree(grand, mode="in"))

# Preg:
# ¿Qué similitudes y diferencias existen entre la media y desviación estandar de ambas redes?
# Razone su respuesta.

# Las diferencias entre la distribuci?n se pueden observar si calculamos los diferentes cuantiles de ambas
summary(degree(g, mode="in"))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    0.000    0.000    2.856    1.000 2434.000 
summary(degree(grand, mode="in"))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   2.000   3.000   2.856   4.000  14.000 

# y de forma más evidente si las pintamos
plot(degree.distribution(g, mode="in"), xlab="node", ylab="fraction of nodes",
     pch=1, col=1, type="b", log="xy")
points(degree.distribution(grand, mode="in"), pch=2, col=2,type="b")

legend("bottomleft", xjust=1, yjust=1,
       c("network","random"), pch=1:2, col=1:2, lty=1, bty="n")

# nótese que los ejes están representados en escala logarítmica

# Preg:
# ¿Qué diferencias se aprecian en ambas distribuciones y a qué son debidas?
# Razone su respuesta.

# Diferencia entre indegree y outdegree

# usuario más veces retuiteado
degree(g)[which.max(degree(g, mode="in"))]
# usuario con más retuits realizados
degree(g)[which.max(degree(g, mode="out"))]

in.degree <- degree(g, mode="in")
out.degree <- degree(g, mode="out")
summary(in.degree)
summary(out.degree)

plot(degree.distribution(g, mode="in"), xlab="node", ylab="fraction of nodes",
     pch=1, col=1, type="b", log="xy")
points(degree.distribution(g, mode="out"), pch=2, col=2,type="b")

legend("bottomleft", xjust=1, yjust=1,
       c("in-degree","out-degree"), pch=1:2, col=1:2, lty=1, bty="n")

deg <- data.frame("in"=in.degree, "out"=out.degree)
cor(deg)

# Si nos fijamos en la distribuciones son muy parecidas existiendo una diferencia
# entre el grado máximo. #De una explicación de porqué el in-degree es mucho mayor
# que el out-degree

# Finalmente, vamos a comporar el grado con la ley de Pareto tanto para el
# grado de entrada como de salida
# El 20% de los nodos con más enlaces cuantos enlaces tienen respecto al total
# Obtenemos los índices del 20% de los nodos con grado mayor
in.nodes.hubs <- V(g)[in.degree > quantile(in.degree, 0.80)]
# Calculamos cuantos enlaces llegan a estos nodos
in.nedges.hubs <- sum(degree(g, mode="in")[in.nodes.hubs])
#in.nedges.hubs <- length(E(g)[to(in.nodes.hubs)]) # Otra manera de calcularlo
in.nedges.hubs/M
# 95.9%

out.nodes.hubs <- V(g)[out.degree > quantile(out.degree, 0.80)]
# Calculamos cuantos enlaces salen de estos nodos
out.nedges.hubs <- sum(degree(g, mode="out")[out.nodes.hubs])
#out.nedges.hubs <- length(E(g)[from(out.nodes.hubs)])
out.nedges.hubs/M
# 65.7%

# Preg:
# ¿Cuánto vale estas mismas cantidades para la red aleatoria? ¿Tiene sentido la diferencia?

############################################################################################################
# Small Worlds
# Coef. de clustering (transitividad) y el promedio de la longitud de los camino mínimos
############################################################################################################

# El coeficiente de clustering de la red indica la fracción de triangulos existente
# frente al total posible (Un triángulo se produce cuando dos vecinos de un nodo están
# conectados entre si)
g.cc <- transitivity(g, type="global")
grand.cc <- transitivity(grand, type="global")
# 0.0073, 7.89478e-05 

# El promedido de la longitud de los caminos m?nimos por su parte  
g.l <- average.path.length(g, directed=TRUE)
grand.l <- average.path.length(grand, directed=TRUE)
# 7.16, 10.5

g.l <- 7.16
grand.l <- 10.5

# Preg:
# Explique los resultados anteriores en función de la naturaleza de cada uno de los grafos
# y relacione los resultados obtenidos con las propiedades de Small-World descritas por
# Watts y Strogatz
# Watts, Duncan J.; Strogatz, Steven H. (June 1998). "Collective dynamics of 'small-world' networks". 
# Nature 393 (6684): 440???442

############################################################################################################
# Propiedades locales:
# Centralidad: Degree, Closeness, betweenness, Eigenvector y Pagerank
# Transitividad
############################################################################################################
g.degree <- degree(g)
g.closeness <- closeness(g)
g.betweenness <- betweenness(g)
g.eigenvector <- eigen_centrality(g)$vector

g.lcc <- transitivity(g, type="local")
g.local <- data.frame("degree"= g.degree,
                      "closeness"= g.closeness,
                      "betweenness"= g.betweenness,
                      "eigenvector"= g.eigenvector,
                      "cc"= g.lcc)

# ¿Cuáles son los diez nodos con el grado más alto?
sort(g.degree, decreasing=TRUE)[1:10]
sort(g.closeness, decreasing=TRUE)[1:10]

# Preg: 
# Encuentre cuales son los nodos que puntuan m?s alto en la mayor?a de las medidas
# de centralidad

# Preg:
# Partiendo de la matriz de correlaci?n entre la cinco medidas de centralidad razone
# las diferencias y similitudes entre ambas.
cor(g.local[1:5])

plot(g.local$eigenvector, g.local$degree,
     xlab="Eigenvector", ylab="log(Degree)", log="y")

# Preg:
# Encontrar nodos con eigenvector / degree
# Alta / Alta 
# Baja / Alta
# Alta / Baja
# ¿Qué diferencias crees que existen en los roles que juegan estos tres tipos de nodos?

# Transitivity vs. degree
plot(g.local$cc, g.local$degree, 
     xlab="Clustering", ylab="log(Degree)", log="y")

# Preg:
# Si comparamos la centralidad  con el coeficiente de clustering
# ¿qué patrón se cumple en este caso con los nodos de más elevada centralidad?
# En general, ¿cómo varía en este caso la centralidad con el coeficiente de clustering?
# ¿qué tipo de nodos serán estos últimos?

############################################################################################################
# Componentes
############################################################################################################
# Las componentes de una red son subredes que están desconectadas entre si. La componente gigante
# es aquella subred con el mayor número de nodos
plot(component_distribution(g), xlab="size of component", ylab="fraction of components",
     pch=1, col=1, type="b", log="x")

comp <- components(g, mode="weak")
# El número de componentes es
comp$no
# 2311
# Y el porcentaje de nodos de la componente gigante será
max(comp$csize) / N
# 90.19%

# Nos quedamos con la componete gigante
# así que eliminamos aquellos nodos que no están en la misma
node_selected <- comp$membership != which.max(comp$csize)
ggc <- delete_vertices(g, node_selected)

# Cambiar a no dirigido
# nos quedamos con los nodos que tienen un grado mayor que 5
ggc_plot <- induced.subgraph(ggc, V(ggc)[degree(ggc) > 10])
vcount(ggc_plot)/vcount(ggc)

# asignamos etiquetas sólo aquellos nodos con grado elevado
degree.ggc_plot <- degree(ggc_plot)
vlabel <- ifelse(degree.ggc_plot>5*mean(degree.ggc_plot),V(ggc_plot)$name," ")

# calculamos el layout (esta operación puede tardar minutos)
ll <- layout.drl(ggc_plot,options=list(simmer.attraction=0))
ll <- layout.norm(ll,-1,1,-1,1)

# Esta operación puede tardar minutos
plot(ggc_plot, layout=ll,
     vertex.size=log(degree(ggc_plot)), vertex.label=vlabel, 
     vertex.label.family="sans", vertex.label.cex=0.4,
     edge.arrow.mode=".", edge.curved=T)