# Esqueleto de la preparación de los datos para entrenamiento
# Falta definir en base a qué variables se va a hacer el entrenamiento


#Requiere paquete "dataPreparation"
#install.packages("dataPreparation")
library(dataPreparation)

dft <- df
##
# Dividir la base de datos en 80% train y 20% test
##
train_index <- sample(1:nrow(df),0.8*nrow(df))
test_index <- setdiff(1:nrow(df),train_index)

X_train <- dft[train_index, -15]
y_train <- dft[train_index, "Promedio"]

X_test <- dft[test_index, -15]
y_test <- dft[test_index, "Promedio"]

###
# Filtrado de variables
###
constant_cols <- whichAreConstant(dft)
double_cols <- whichAreInDouble(dft)
bijections_cols <- whichAreBijection(dft)

#Calificacion x/100 es biyección de Calificacionx/128 -> Remover
X_train$`Calificación x/100` <- NULL
X_test$`Calificación x/100` <- NULL

##
# Escalado y normalización
##
scales <- build_scales(dataSet = X_train, cols = c("Calificación x/128","Promedio"), verbose = TRUE)

X_train <- fastScale(dataSet = X_train,scales = scales, verbose = TRUE)
X_test <- fastScale(dataSet = X_test, scales = scales, verbose = TRUE)

#print(head(X_train[, c("Calificación x/128","Promedio")]))

##
# Discretizacion
##
#bins <- build_bins(dataSet = X)
X_train <- fastDiscretization(dataSet = X_train, bins = list("Calificación x/128" = c(0, 30, 60, 70, 80, 90, 100, 105, 110, +Inf)))

##
# Codificación de variables categóricas
##

encoding <- build_encoding(dataSet = X_train, cols = "auto", verbose = TRUE)

X_train <- one_hot_encoder(dataSet = X_train, encoding = encoding, drop = TRUE, verbose = TRUE)
X_test <- one_hot_encoder(dataSet = X_test, encoding = encoding, drop = TRUE, verbose = TRUE)

##
# Volver a filtrar variables, ahora que ya estás codificadas
##
bijections <- whichAreBijection(dataSet = X_train, verbose = TRUE)

#Las siguientes columnas fueron detectadas como biyecciones -> Remover
X_train$`Calificación x.128..0. 30.`<-NULL
X_train$Situacion.NA <- NULL
X_train$MotivoEstudio.NA <- NULL
X_train$Faltar.NA <- NULL
X_train$Expectativas.NA <- NULL
X_train$Agua.NA <- NULL
X_train$RedSocial.NA <- NULL
X_train$Correo.0 <- NULL
X_train$Facebook.0 <- NULL

##
# Control de la forma de Train y Test
##
X_test <- sameShape(X_test, referenceSet = X_test, verbose = TRUE)
X_train <- sameShape(X_train, referenceSet = X_train, verbose = TRUE)
