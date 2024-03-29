#Laboratorio 3 - An�lisis de Datos
#Integrantes:
#            -Hugo Arenas
#            -Juan Arredondo


library(cluster)
library(fpc)
library(NbClust)
library(factoextra)
library(FactoMineR)
library(Rtsne) 

library(ez)
library(dplyr)
library(ggpubr)
library(knitr)
library(tidyr)
library(car)
library(lmtest)
library(vcd)
library(arulesViz)
library("cowplot")

library(C50)
library(caret)
library(randomForest)

#Los nombres originales de las variables, una breve explicaci�n y los tipos de los datos:

#age (rango de edad): 10-19, 20-29, 30-39, 40-49,.
#menopause (momento de la menopausia): lt40, ge40, premeno.
#tumor-size (tama�o del tumor extirpado en mm): 0-4, 5-9, 10-14, .
#inv-nodes (una m�trica de presencia de c�lulas cancerosas en los nodos linf�ticos): 0-2, 3-5, 6-8, 9-11,.
#node-caps (evidencia de que c�lulas cancerosas atravesaron la c�psula de los n�dulos linf�ticos): yes, no
#deg-malig (grado histol�gico del tumor: bajo, intermedio, alto): 1, 2, 3.
#breast (mama afectada): left, right.
#breast-quad (cuadrante de la mama): left-up, left-low, right-up, right-low, central.
#irradiat (radioterapia): yes, no.
#Class (clase) Indica recurrencia, es la variable a predecir (no-recurrencia: 201 casos, recurrencia: 85 casos)


# Leemos los datos
dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)
filename <- "breast-cancer.data"
file <- file.path(dirstudio, filename)

#Se definen los nombres de las columnas, estos son los mismos de provistos por la base de datos
columns <- c("class", 
             "age", 
             "menopause", 
             "tumor.size", 
             "inv.nodes", 
             "node.caps",
             "deg.malig",
             "breast",
             "breast.quad",
             "irradiat")

tabla <- read.csv(file, col.names = columns)

#Se sacan los datos nulos del datagrama
bool.values <- tabla$node.caps=='?'
tabla <- tabla[!bool.values,]

bool.values <- tabla$breast.quad =='?'
tabla <- tabla[!bool.values,]

#Se guardan las columnas de la tabla para ser utilizadas m�s adelante.
menopause <- as.factor(tabla$menopause)
node.caps <- as.factor(tabla$node.caps)
deg.malig <- as.factor(tabla$deg.malig)
breast <- as.factor(tabla$breast)
irradiat <- as.factor(tabla$irradiat)

#Todas las variables, menos la clase, se vuelven num�ricas.
tabla$class <- as.factor(tabla$class)
tabla$age <- unclass(as.factor(tabla$age)) 
tabla$menopause <- unclass(as.factor(tabla$menopause)) 
tabla$tumor.size <- unclass(as.factor(tabla$tumor.size))
tabla$inv.nodes <- unclass(as.factor(tabla$inv.nodes)) 
tabla$node.caps <- unclass(as.factor(tabla$node.caps))
tabla$deg.malig <- unclass(as.factor(tabla$deg.malig))
tabla$breast <- unclass(as.factor(tabla$breast)) 
tabla$breast.quad <- unclass(as.factor(tabla$breast.quad)) 
tabla$irradiat <- unclass(as.factor(tabla$irradiat))

#Se reordenan los valores de tumor.size.
tabla$tumor.size[tabla$tumor.size == 10] <- 12
tabla$tumor.size[tabla$tumor.size == 9] <- 10
tabla$tumor.size[tabla$tumor.size == 8] <- 9
tabla$tumor.size[tabla$tumor.size == 7] <- 8
tabla$tumor.size[tabla$tumor.size == 6] <- 7
tabla$tumor.size[tabla$tumor.size == 5] <- 6
tabla$tumor.size[tabla$tumor.size == 4] <- 5
tabla$tumor.size[tabla$tumor.size == 3] <- 4
tabla$tumor.size[tabla$tumor.size == 2] <- 3
tabla$tumor.size[tabla$tumor.size == 12] <- 2

#Se reordenan los valores de breast.quad.
tabla$breast.quad[tabla$breast.quad == 4] <- 6
tabla$breast.quad[tabla$breast.quad == 3] <- 4
tabla$breast.quad[tabla$breast.quad == 6] <- 3

#Se reordenan los valores de inv.nodes.
tabla$inv.nodes[tabla$inv.nodes == 2] <- 8
tabla$inv.nodes[tabla$inv.nodes == 3] <- 9
tabla$inv.nodes[tabla$inv.nodes == 4] <- 10

tabla$inv.nodes[tabla$inv.nodes == 5] <- 2
tabla$inv.nodes[tabla$inv.nodes == 6] <- 3
tabla$inv.nodes[tabla$inv.nodes == 7] <- 4

tabla$inv.nodes[tabla$inv.nodes == 8] <- 5
tabla$inv.nodes[tabla$inv.nodes == 9] <- 6
tabla$inv.nodes[tabla$inv.nodes == 10] <- 7

#Se crea la tabla de entrenamiento.
set.seed(2021)
training.index <- createDataPartition(tabla$class, p=0.63)$Resample1
training.set <- tabla[training.index, ]
prueba.set <- tabla[-training.index, ]

#Se crea el �rbol de decisi�n.
arbol = C5.0(class ~ ., training.set)
arbol.reglas = C5.0(x = training.set[, -1], y = training.set$class, rules = T)
arbol.pred.class = predict(arbol, prueba.set[,-1], type = "class")
arbol.pred.prob = predict(arbol, prueba.set[,-1], type = "prob")

#Matriz de confusi�n.
conf.matriz.arbol = confusionMatrix(table(prueba.set$class, arbol.pred.class))
print(conf.matriz.arbol)

#Mejorar resultados por randomforest.
rf = randomForest(class~ ., data=training.set, ntree = 500, importance=TRUE, proximity=TRUE, ntry=10)
rf.pred.class = predict(rf, prueba.set[,-1], type = "class")

#Matriz de confusi�n de randomforest.
conf.matriz.rf = confusionMatrix(table(prueba.set$class, rf.pred.class))
print(conf.matriz.rf)