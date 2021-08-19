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

#Todas las variables, menos la clase, se vuelven num�ricas.
tabla$class <- as.factor(tabla$class)
tabla$age <- as.factor(tabla$age)
tabla$menopause <- as.factor(tabla$menopause) 
tabla$tumor.size <- as.factor(tabla$tumor.size)
tabla$inv.nodes <- as.factor(tabla$inv.nodes)
tabla$node.caps <- as.factor(tabla$node.caps)
tabla$deg.malig <- as.factor(tabla$deg.malig)
tabla$breast <- as.factor(tabla$breast)
tabla$breast.quad <- as.factor(tabla$breast.quad)
tabla$irradiat <- as.factor(tabla$irradiat)

#Se crea la tabla de entrenamiento.
training.index <- createDataPartition(tabla$class, p=0.7)$Resample1
training.set <- tabla[training.index, ]
prueba.set <- tabla[-training.index, ]

#Se crea el �rbol de decisi�n.
arbol = C5.0(class ~ ., training.set)
arbol.reglas = C5.0(x = training.set[, -8], y = training.set$class, rules = T)
arbol.pred.class = predict(arbol, prueba.set[,-1], type = "class")
arbol.pred.prob = predict(arbol, prueba.set[,-1], type = "prob")