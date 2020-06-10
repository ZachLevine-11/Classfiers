#Two layer neural network with backpropagation written from scratch in R.
feedforward <- function(input, weights_1, weights_2){
  layer1 <- sigmoid::sigmoid(pracma::dot(input, weights_1))
  layer2 <- sigmoid:: sigmoid(pracma::dot(layer1, weights_2))
  toReturn <- data.frame(layer1, layer2, stringsAsFactors = FALSE)
  names(toReturn) <- c("layer1", "layer2")
  return(toReturn)
}

backprop <- function(layer_1, input, output, weights_2, weights_1, y_){
  d_weights2 <- pracma::dot(t(layer_1), (2*(y_ - output) * sigmoid::sigmoid_output_to_derivative(output)))
  d_weights1 <- pracma::dot(t(input),  (pracma::dot(2*(y_ - output) * sigmoid::sigmoid_output_to_derivative(output), t(weights_2) * sigmoid::sigmoid_output_to_derivative(layer_1))))
  weights_1 <- weights_1 + d_weights1
  weights_2 <- weights_2 + d_weights2
  toReturn <- data.frame(weights_1, weights_2, stringsAsFactors = FALSE)
  names(toReturn) <- c("weights1", "weights2")
  return(toReturn)
}

trainNN <- function(traindf, weights1, weights2, y){
  input <- traindf$x
  output <- feedforward(input, weights1, weights2)$"layer2"
  back <- backprop(output, input, output, weights1, weights2, y)
  weights1 <- back$"weights1"
  weights2 <- back$"weights2"
  toReturn <- data.frame(weights1, weights2, stringsAsFactors = FALSE)
  names(toReturn) <- c("weights1", "weights2")
  return(toReturn)
}


runNN <- function(df){
  input <- df$x
  y <- df$y
  weights1 <- matrix(runif(4*nrow(df),
                           0,
                           1),
                     nrow = nrow(df),
                     ncol = 4)
  weights2 <- matrix(runif(4, 0, 1),
                     nrow = nrow(df),
                     1)
  i <- 0
  while (i <= 1500){
    currentNN <- trainNN(df, weights1, weights2, y)
    weights1 <- currentNN$weights1
    weights2 <- currentNN$weights2
    if (i %% 150 == 0){
      cat(paste0("for iteration # ", i,"\n"))
      cat(paste0( "Input:", df$x, "\n"))
      cat(paste0("Actual Output: ", df$y, "\n"))
      cat(paste0("Predicted Output: \n", feedforward(input, weights1, weights2), "\n"))
    }
    else{
    }
    i <- i + 1
  }
}