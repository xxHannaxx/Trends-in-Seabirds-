##  classification tree in R
data <- read_delim("Characteristics_islands.csv",  delim = ";", escape_double = FALSE, col_types = cols(Type = col_factor(levels = c("Island", 
                                                                                                           +         "Headland"))), trim_ws = TRUE)

# create train set

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

# each island is there only once -> so I don't know if it makes sense with the train and test data since every row is somehow unique and does not repeat expect for islands and headlands


data_train <- create_train_test(data, 0.8, train = TRUE)
data_test <- create_train_test(data, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

#install.packages("rpart.plot")


library(rpart)
library(rpart.plot)
fit <- rpart(Type~., data = data, method = 'class')
rpart.plot(fit, extra = "auto")
           

## testing by adding "scores" -> random values between 1 and 30
set.seed(123)
score<- sample(1:30)

data$score<- score


fit <- rpart(score~., data = data, method = 'class')
rpart.plot(fit, extra = "auto")
