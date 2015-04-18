#' Defines the constructor for the RotationForest module
#'
#' Requires the dependent and response values (data frames), the number 
#' of predictor variables to use in each rotation, the number of trees to 
#' train, and a logical for determining if progress should be printed
#'
#'
#' @param xdf a data frame of X dependent vectors
#' @param ydf a data frame of Y response values
#' @param npredictor the number of predictor variables that are to be used in each rotation
#' @param ntree the number of trees that are to be used to train the ensemble
#' @param verbose a logical, set true for classification output to be printed
#' @param ... extra variables to be passed on to the rpart function
#' 
#' @return an object of class RotationForest
#' @export
#' 
#' @examples
#' 
#' data <- read.table("C:/Users/Manan/Desktop/RotationForest/inst/extdata/balance-scale.data", sep = ",", header = FALSE)
#' data.dependent <- data[,-1]
#' data.response <- data[,1]
#' data.response <- as.factor(data.response)
#' total <- data.frame(data.response, data.dependent)
#' groups <- sample(rep(1:10, times = ceiling(nrow(total) / 19)), size = nrow(total), replace = TRUE)
#' data.train <- total[!groups %in% 1,]
#' data.test <- total[groups %in% 1,]
#' fit <- RotationForest(data.train[,-1], data.train[,1], 2, 10)
#'
RotationForest <- function(xdf, ydf, npredictor, ntree, verbose = F, ...) {
  RotationForestObject <- list()
  class(RotationForestObject) <- "RotationForest"
  fits <- list()
  rots <- list()
  
  for (i in 1:ntree) {
    model.current <- BuildModel(xdf, ydf, npredictor,...)
    fits[[i]] <- model.current[[1]]
    rots[[i]] <- model.current[[2]]
    if (verbose == T) {
      print(sprintf("[%i out of %i] models trained", i, ntree))
    }
  }
  
  RotationForestObject$models <- fits
  RotationForestObject$rotations <- rots
  
  return(RotationForestObject)
}

#' Provides a predict function for an object of class RotationForest
#' 
#' Predict allows for O(N) prediction based on an object of class RotationForest 
#' where N is the length of the dataframe dependent array. 
#'
#' @param RotationForestObject an object of class RotationForest 
#' (returned from the constructor RotationForest(...))
#' @param dependent a data frame of the X predictor values
#' @param prob A logical indicating whether probabilities of existing in each class are returned
#' (as opposed to the default predictions)
#'
#' @return A vector of predictions (or a table of probabilities) of the different classes
#' @export
#' 
#' @examples
#' 
#' data <- read.table("C:/Users/Manan/Desktop/RotationForest/inst/extdata/balance-scale.data", sep = ",", header = FALSE)
#' data.dependent <- data[,-1]
#' data.response <- data[,1]
#' data.response <- as.factor(data.response)
#' total <- data.frame(data.response, data.dependent)
#' groups <- sample(rep(1:10, times = ceiling(nrow(total) / 19)), size = nrow(total), replace = TRUE)
#' data.train <- total[!groups %in% 1,]
#' data.test <- total[groups %in% 1,]
#' fit <- RotationForest(data.train[,-1], data.train[,1], 2, 10)
#' predict <- predict(fit, data.dependent, prob = FALSE)
#'
predict.RotationForest <- function(RotationForestObject, dependent, prob = FALSE) {
  # Create and store predictions in a list
  prediction.probabilities <- list()
  
  for (i in 1:length(RotationForestObject[[1]])) {
    model.current <- RotationForestObject[[1]][[i]]
    
    data.current <- as.matrix(dependent) %*% RotationForestObject[[2]][[i]]
    data.current <- as.data.frame(data.current)
    
    colnames(data.current) <- paste0("X", 1:ncol(data.current))
    prediction.probabilities[[i]] <- predict(model.current, data.current)
  }
  
  # Calculates the probability of each class by averaging across the different trees
  results <- matrix(ncol = ncol(prediction.probabilities[[1]]), nrow = nrow(dependent))
  colnames(results) <- colnames(prediction.probabilities[[1]])
  
  for (i in 1:nrow(dependent)) {
    results[i, ] <- apply(do.call(rbind, 
                                  lapply(prediction.probabilities,
                                         function(x) x[i, ])
                                        ), 2, mean)
  }
  
  if (prob == TRUE) return(results)
  else return(apply(results, 1, function(x) names(which(x == max(x)))))
}


#' Builds a single decision tree using rotation forest methodology (Rodriguez et al. 2006)
#'
#' BuildModel builds one decision tree with a data frame of dependent and response vectors. 
#' It also requires the number of predictor variables to use in each rotation as well as the 
#' fraction of data points to use in each ensemble model (default = 0.75). 
#' 
#' @param dependent a data frame of x dependent vectors
#' @param response a data frame of y response values
#' @param npredictor the number of predictor variables to use in each rotation
#' @param frac the fraction of data points to use in each ensemble model
#' @param ... additional arguments to pass to the rpart function
#' 
#' @return A list containing the rpart object and rotation matrix
#'
#' @export
#'
BuildModel <- function(dependent, response, npredictor, frac = 0.75,...) {
  requireNamespace('rpart', quietly = TRUE)
  
  M <- ceiling(ncol(dependent) / npredictor)
  R <- matrix(nrow = ncol(dependent),ncol = ncol(dependent), data = 0)
  R.order <- R
  
  Order <- data.frame(1:ncol(dependent),
                      sample(sort(rep(1:npredictor, times = M))
                              [1:ncol(dependent)], 
                              size = ncol(dependent), replace = F)
                            )
  
  colnames(Order) <- c("V1","V2")
  for (i in 1:npredictor) {
    
    rows.use <- sample(1:nrow(dependent),size = round(frac * nrow(dependent)),replace = F)
    cols.use <- subset(Order,V2 == i)$V1
    
    start <- (i - 1) * M + 1
    if (i != npredictor) {
      end <- i * M
    }
    else {
      end <- ncol(dependent)
    }
    
    dependent.sub <- dependent[rows.use,cols.use]
    dependent.sub.rotation <- prcomp(dependent.sub)$rotation
    R[start:end,start:end] <- dependent.sub.rotation
    
    # Change the position of the columns to match that of dependent
    R.order[start:end,cols.use] <- R[start:end,start:end]
  }
  
  # Rotate onto matrix
  dependent.rotate <- as.matrix(dependent) %*% R.order
  Df.rotate.full <- data.frame(response,dependent.rotate)
  colnames(Df.rotate.full)[1] <- "class"
  
  fit <- rpart::rpart(class ~ ., data = Df.rotate.full,...)
  
  return.list <- list()
  return.list[[1]] <- fit
  return.list[[2]] <- R.order
  
  return(return.list)
}
