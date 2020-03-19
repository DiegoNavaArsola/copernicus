
df1 <- data_cop_simp

for (z in 1:dim(df1)[1]) {
  if (is.na(df1$Correo[z]) == TRUE) {
    df1$Correo[z] <- 0
  }
  else{
    df1$Correo[z] <- 1
  }
  if (is.na(df1$Facebook[k]) == TRUE) {
    df1$Facebook[z] <- 0
  }
  else{
    df1$Facebook[z] <- 1
  }
}

Nx1 = matrix(NA, nrow = dim(df)[1]+1, ncol = dim(df)[2])
Ncx1 = matrix(NA, nrow = dim(df)[1]+1, ncol = dim(df)[2])
epsilon1 = matrix(NA, nrow = dim(df)[1]+1, ncol = dim(df)[2])

prueba <- matrix(0,nrow = dim(df)[1]+2,ncol = 1)

for (i in 1:dim(df1)[2]) {
  for (j in 0:236) {
#    if (i == 10) {    
    if (is.character(df1[[i]]) == TRUE) {
      aux1 <- cont_mul_var(df1[[i]],",")
      taux1 <- table(aux1)
      for (t1_length in 1:(dim(taux1)-2)) {
        Nx1[t1_length+1,i] <- taux1[[t1_length+1]]
      }

      if (df1[[j+1,3]]==7 | df1[[j+1,3]]==9) {
        prueba[j+1,1] <- df1[[j+1,i]]
        taux2 <- table(cont_mul_var(prueba,","))
        for (t2_length in 1:(dim(taux2)-2)) {
          Ncx1[t2_length+1,i] <- taux2[[t2_length+1]]
        }
      }
    }
    else{
      Nx1[j,i] <- sum(df1[[i]] == j-1,na.rm = T)
      Ncx1[j,i] <- sum(df1[[i]] == j-1 & (df1[[3]] == 7 | df1[[3]] == 9),na.rm = T)
    }
  }
} 
