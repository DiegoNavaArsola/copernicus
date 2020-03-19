#Data frame con los datos del agua
#df <- Copernicus_Data_set
df <- data_cop_simp

# Se quitaron algunas variables por inconcistencia con el resto de datos

#Tamaño de la base de datos
n <- dim(df)[1]

#Clases a tratar / na.rm = T sirve para ignorar NAN
Nc <- sum(df$Status == 7 | df$Status == 9,na.rm = T)

#Vector con el nombre de las variables
nomvar <- names(df)

#Funcion para contar variables con celdas con multiples datos (sep es un string)
cont_mul_var <- function(column,sep) {
  column.string <- paste(column,collapse = sep)
  column.vector <- strsplit(column.string, sep)
  return(column.vector)
}

#Inicialización de matrices
Nx = matrix(NA, nrow = dim(df)[1]+1, ncol = dim(df)[2])
Ncx = matrix(NA, nrow = dim(df)[1]+1, ncol = dim(df)[2])
epsilon = matrix(NA, nrow = dim(df)[1]+1, ncol = dim(df)[2])
arg_ln = matrix(NA, nrow = dim(df)[1]+1, ncol = dim(df)[2])
score = matrix(NA, nrow = dim(df)[1]+1, ncol = dim(df)[2])

prueba <- matrix(NA,nrow = dim(df)[1]+2,ncol = 1)

#Funcion para convertir los datos de los correos y facebook a 0 y 1
#Si exite correo/facebook -> 1, de lo contrario ->0
for (k in 1:dim(df)[1]) {
  if (is.na(df$Correo[k]) == TRUE) {
    df$Correo[k] <- 0
  }
  else{
    df$Correo[k] <- 1
  }
  if (is.na(df$Facebook[k]) == TRUE) {
    df$Facebook[k] <- 0
  }
  else{
    df$Facebook[k] <- 1
  }
}

#Calculo de epsilon
for (i in 1:dim(df)[2]) {
  for (j in 0:(dim(df)[1]-1)) {
    
    #Si la variable presenta datos de tipo "char" p.ej: ","
    if (is.character(df[[i]]) == TRUE) {
      aux <- cont_mul_var(df[[i]],",")          #Separa los datos seprados con una coma
      taux <- table(aux)                        #Funcion table para hacer los conteos
      for (t_length in 1:(dim(taux)-2)) {       
        Nx[t_length+1,i] <- taux[[t_length+1]]  #Remplaza Nx con los valores de los conteos
      }
      
      #Si se trata de las clases a estudiar (Status: 7 y 9)
      if (df[[j+1,3]]==7 | df[[j+1,3]]==9) {    
        prueba[j+1,1] <- df[[j+1,i]]             #Guarda las celdas de las clases a estudiar con multiples datos
        taux2 <- table(cont_mul_var(prueba,",")) #Separa los datos seprados con una coma
        for (t2_length in 1:(dim(taux2)-2)) {
          Ncx[t2_length+1,i] <- taux2[[t2_length+1]] #Remplaza Ncx con los valores de los conteos
        }
      }
    }
    #Si la varible es numerica
    else{
      Nx[j,i] <- sum(df[[i]] == j-1,na.rm = T)   #Realiza los conteos 
      Ncx[j,i] <- sum(df[[i]] == j-1 & (df[[3]] == 7 | df[[3]] == 9),na.rm = T)  #Realiza los conteos cruzados con las clases a estudiar
    }
    
    #Convierte los valores NAN a 0
    Nx[is.nan(Nx)] <- 0     
    Ncx[is.nan(Ncx)] <- 0
    
    #Calculo de epsilon
    epsilon[j,i] <- (Nx[j,i]*((Ncx[j,i]/Nx[j,i])-(Nc/n)))/(Nx[j,i]*(Nc/n)*sqrt((1-(Nc/n))))
    
    #Calculo de los scores
    arg_ln[j,i] <- (Ncx[j,i]/Nc)/((Nx[j,i]-Ncx[j,i])/(n-Nc))
    score[j,i] <- log(arg_ln[j,i],exp(1))
  }
}   
    
#Convierte NaN en ceros
epsilon[is.nan(epsilon)] <- 0
score[is.nan(score)] <- 0

#Covierte infinitos en ceros
score[is.infinite(score)] <- 0
