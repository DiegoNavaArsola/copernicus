dft <- df

#Dividir la base de datos en 80% train y 20% test
train_index <- sample(1:nrow(df),0.8*nrow(df))
test_index <- setdiff(1:nrow(df),train_index)

X_train <- dft[train_index, -15]
