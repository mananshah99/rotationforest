# rotationforest
You can use the package `rotationForest` in R to accomplish this task. 

**1. Install**

    install.packages('devtools') # Only if needed
    require('devtools')
    devtools::install_github('mananshah99/rotationforest')
    require('rotationForest')

**2. Usage**

Sample usage is demonstrated below:

    fpath <- system.file("extdata", "balance-scale.data", package="rotationForest")
    data <- read.table(fpath, sep = ",", header = FALSE)
    data.dependent <- data[,-1]
    data.response <- data[,1]
    data.response <- as.factor(data.response)
    total <- data.frame(data.response, data.dependent)
    groups <- sample(rep(1:10, times = ceiling(nrow(total) / 19)), size = nrow(total), replace = TRUE)
    data.train <- total[!groups %in% 1,]
    data.test <- total[groups %in% 1,]
    fit <- rotationForest(data.train[,-1], data.train[,1], 2, 10)
    predict <- predict(fit, data.dependent, prob = FALSE)

The documentation describes in closer detail the arguments and functionality of `rotationForest` and the overloaded `predict`. 
