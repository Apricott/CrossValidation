
train_model <- function(X_train, y_train) {
  # estymacja parametrow MNK
  a <- solve(t(X) %*% X) %*% t(X) %*% y
  return(a)
}   

train_test_split <- function(X, y, k=2, k_test=1) {
  # funkcja do dzielenia danych na k zbiorow (k-1 uczacych i jeden testowy - o numerze k_test) 
  # (do zastanowienia, czy tej funkcji nie zrobic jakos inaczej)
  
  
  X_train <- 0
  y_train <- 0
  X_test <- 0
  y_test <- 0
  
  
  return(list(X_train, y_train, X_test, y_test))
}


test_model <- function(X_test, alpha) {
  # funkcja zwraca wyestymowane wartoœci y dla podanych danych oraz wektora parametrów alpha
  y_estimated <- X_test %*% alpha
  
  return(y_estimated)
}

calc_errors <- function(y_true, y_estimated) {
  # funcja zwraca liste z bledami miedzy wartosciami rzeczywistymi y a jej oszacowanymi wartosciami
  
  s <- length(y_true)
  
  model_residuals <- y_true - y_estimated
  
  MAE <- 1/s * sum(abs(model_residuals))
  RMSE <- sqrt(1/s * sum(model_residuals^2))
  MAPE <- 1/s * sum(abs(model_residuals/y)) * 100
    
    
  errors <- list(MAE=MAE, RMSE=RMSE, MAPE=MAPE)
  return(errors)
} 


cross_val <- function(X, y, k=2) {
  # funkcja przeprowadza k-krotny sprawdzian krzy¿owy
  train_test_split()
  
}

LOOCV <- function(X, y) {
  # funkcja przeprowadza Leave-One-Out Cross Validation
  train_test_split()
}

#=============================== PRZYGOTOWANIE DANYCH ==============================
income <- read.csv("E:/IiE/IV sem 2019-2020/MNWS/projekt3/zarobki.csv", sep=";")

y_name <- "Wynagrodzenie"
X <- income[, !(names(income) %in% y_name)]
y <- income[y_name]
y <- sapply(y, as.numeric)
X <- sapply(X, as.numeric)
ones <- rep(1, nrow(X))
X <- cbind(ones, X)
# ==================================================================================



# =============================== MODEL 1 - PODSTAWOWY =============================

# (oczywiscie to trzeba bedzie zrobic za pomoca sprawdzianu krzyzowego w 3 wariantach)
alpha <- train_model(X, y)
y_estimated <- test_model(X, alpha)
errors_base_model <- calc_errors(y, y_estimated)


# to tak troche pokracznie wyszlo, ale w R niestety nie da sie tak ladnie jak Pythonie tego zrobic
train_test <- train_test_split(X, y)
X_train <- train_test[[1]]; y_train <- train_test[[2]]
X_test <- train_test[[3]]; y_test <- train_test[[4]]






# ==================================================================================



# =============================== MODEL 2 - ZLOGARYTMOWANY =========================
# (tylko zmienne objasniajace)


# ==================================================================================



# =============================== MODEL 3 - PIERWIASTKOWANY ========================
# (tylko zmienne objasniajace)


# ==================================================================================



# =============================== MODEL 4 - KWADRATOWY =============================
# (tylko zmienne objasniajace)


# ==================================================================================



# =============================== MODEL 5 - NORMALIZOWANY ==========================
# (tylko zmienne objasniajace)


# ==================================================================================

