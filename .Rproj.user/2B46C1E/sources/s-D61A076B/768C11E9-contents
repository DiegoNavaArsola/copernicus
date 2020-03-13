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
  column.vector <- strsplit(column.string,sep)
  return(column.vector)
}

create_temp_df <- function(matrix) {
  
}

#1:128 + 213 + 215 + 431 + 596 --> Clases:132

x <- c(0:128)
y <- c(213,215,431,596)

nclas <- c(x,y)

lnc <- length(nclas)

#Inicialización de matrices
Nx = matrix(0, nrow = lnc+1, ncol = dim(df)[2])
Ncx = matrix(0, nrow = lnc+1, ncol = dim(df)[2])
epsilon = matrix(0, nrow = lnc+1, ncol = dim(df)[2])
arg_ln = matrix(0, nrow = lnc+1, ncol = dim(df)[2])
score = matrix(0, nrow = lnc+1, ncol = dim(df)[2])

temp <- matrix(0,nrow = lnc+1,ncol = 1)

for (k in 1:lnc) {
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

for (i in 1:dim(df)[2]) {
  for (j in 0:lnc) {
    
    if (is.character(df[[i]]) == TRUE) {
      aux <- cont_mul_var(df[[i]],",")
      taux <- table(aux)
      Nx[j+1,i] <- taux[[j+1]]
      if (df[[j,3]]==7 | df[[j,3]]==9) {
        temp[j+1,1] <- df[[j,i]]
      }
    }
    
    else{
      Nx[j,i] <- sum(df[[i]] == j-1,na.rm = T)
      #Ncx[j,i] <- sum(df[[i]] == j-1 & (df[[3]] == 7 | df[[3]] == 9),na.rm = T)
    }
    Ncx[j,i] <- sum(df[[i]] == j-1 & (df[[3]] == 7 | df[[3]] == 9),na.rm = T)
    epsilon[j,i] <- (Nx[j,i]*((Ncx[j,i]/Nx[j,i])-(Nc/n)))/(Nx[j,i]*(Nc/n)*sqrt((1-(Nc/n))))
    
    arg_ln[j,i] <- (Ncx[j,i]/Nc)/((Nx[j,i]-Ncx[j,i])/(n-Nc))
    score[j,i] <- log(arg_ln[j,i],exp(1))
    
  }
}   
    
#Convierte NaN en ceros
epsilon[is.nan(epsilon)] <- 0
score[is.nan(score)] <- 0

#Covierte infinitos en ceros
score[is.infinite(score)] <- 0
