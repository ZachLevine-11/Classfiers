#Two layer neural network with backpropagation written from scratch in R.
feedforward <- function(input, weights_1, weights_2){
  layer1 <- sigmoid::sigmoid(pracma::dot(input, t(weights_1)))
  layer2 <- sigmoid:: sigmoid(pracma::dot(weights_2, layer1))
  toReturn <- list(layer1, layer2)
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

trainNN <- function(traindf, weights1, weights2){
  input <- traindf$x
  y <- traindf$y
  fed <- feedforward(input, weights1, weights2)
  back <- backprop(fed[["layer1"]], input, fed[["layer2"]], weights2 , weights1, y)
  weights1 <- back[["weights1"]]
  weights2 <- back[["weights2"]]
  toReturn <- data.frame(weights1, weights2, stringsAsFactors = FALSE)
  names(toReturn) <- c("weights1", "weights2")
  return(toReturn)
}


runNN <- function(df){
  input <- df$x
  y <- df$y
  weights1 <- matrix(runif(4*nrow(df)),
                     nrow = nrow(df),
                     ncol = 4)
  weights2 <- matrix(runif(4*nrow(df)),
                     nrow = 1,
                     nrow(df))
  i <- 1
  while (i <= 1500){
    if (i %% 150 == 0){
      cat(paste0("for iteration # ", i,"\n"))
      cat(paste0( "Input:", df$x, "\n"))
      cat(paste0("Actual Output: ", df$y, "\n"))
      cat(paste0("Predicted Output: \n", feedforward(input, weights1, weights2), "\n"))
    }
    else{
    }
    trained <- trainNN(df, weights1, weights2)
    weights1 <-  trained[["weights1"]]
    weights2 <- trained[["weights2"]]
    i <- i + 1
  }
}