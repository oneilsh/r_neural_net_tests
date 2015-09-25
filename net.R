



perceptron <- function(num_inputs, p) {
  perc <- list()
  perc$p <- p
  perc$weights <- rep(0.5, num_inputs + 1)
  return(perc)
}

perceptron_getoutput <- function(perc, input_vec) {
  weighted_values <- c(input_vec, -1) * perc$weights
  weighted_sum <- sum(weighted_values)
  sigmoid_out <- 1/(1 + exp(-1*weighted_sum/perc$p))
  return(sigmoid_out)
}

perceptron_error <- function(perc, input_vec, expected_out) {
  return(expected_out - perceptron_getoutput(perc, input_vec))
}

perceptron_train <- function(perc, input_vec, expected_out, learningrate) {
  out <- perceptron_getoutput(perc, input_vec)
  error <- perceptron_error(perc, input_vec, expected_out)
  for(i in 1:length(input_vec)) {
    perc$weights[i] <- perc$weights[i] + input_vec[i]*error*out*(1-out)*learningrate
  }
  return(perc)
}

perc <- perceptron(3, 1.0)
colors <- list()
colors[[1]] <- list(color = c(1,0,0), out = 1)
colors[[2]] <- list(color = c(0,0,1), out = 0)
colors[[3]] <- list(color = c(0,1,1), out = 0)
colors[[4]] <- list(color = c(1,1,0), out = 1)
colors[[5]] <- list(color = c(0.3,0.6,0), out = 1)
colors[[6]] <- list(color = c(0.1,0.6,0.8), out = 0)
colors[[7]] <- list(color = c(0.3,0.9,0.1), out = 1)
colors[[8]] <- list(color = c(0.1,0.0,0.2), out = 0)

perc <- perceptron(3, 1.0)
for(i in 1:100) {
  for(example in colors) {
    out <- perceptron_getoutput(perc, example$color)
    print(out)
    perc <- perceptron_train(perc, example$color, example$out, 0.1)
  }
}
print(perc)
perceptron_getoutput(perc, c(0.01,1,0))

# list of perceptrons
layer <- function(size, previous_layer_size) {
  l <- list()
  l$perceptrons <- list()
  for(i in 1:size) {
    l$perceptrons[[i]] <- perceptron(previous_layer_size, 1.0)
  }
  l$previous_layer_size <- previous_layer_size
  return(l)
}

## eg give it list(3, 4, 5, 1) for a 3-input, 1 output, two hidden layer
## net with 4 and 5 nodes each
neural_net <- function(layer_sizes_list) {
  nn <- list()
  nn$layers <- list()
  nn$layers$inputlayer <- list()
  for(i in 1:layer_sizes_list[[1]]) {
    nn$layers$inputlayer[[i]] <- perceptron(1, 1.0)
  }
  
  for(i in 2:length(layer_sizes_list)) {
    nn$layers[[i]] <- layer(layer_sizes_list[[i]], layer_sizes_list[[i-1]])
  }
  nn$layer_sizes_list <- layer_sizes_list
  return(nn)
}

nn <- neural_net(list(3, 4, 1))

## length of input_vec must equal number of inputs in each perceptron
## of the layer, output will be a list
layer_getoutput <- function(layer, input_vec) {
  outlist <- lapply(layer$perceptrons, perceptron_getoutput, input_vec)
  return(outlist)
}

layer_getoutput(nn$layers[[2]], c(0.2, 0.6, 0.9))



