library(rstackdeque)
library(ggplot2)
library(dplyr)

# returns a neural net as a data frame, in long form ;)
create_nn_df <- function(layer_sizes) {
  nn_df_stack <- rstack()
  
  # eh, lets not use a dedicated input layer with single input edges
  input_layer_size <- layer_sizes[1]
  for(i in 1:input_layer_size) {
    #edge <- data.frame(fromx = 0, fromy = i, tox = 1, toy = i, weight = 0.5, type = "input")
    edge2 <- data.frame(fromx = -1, fromy = -1, tox = 1, toy = i, weight = 0.5, type = "bias")
    #nn_df_stack <- insert_top(nn_df_stack, edge)
    nn_df_stack <- insert_top(nn_df_stack, edge2)
  }
  
  for(i in 2:length(layer_sizes)) {
    layer_size <- layer_sizes[i]
    last_size <- layer_sizes[i-1]
    for(k in 1:layer_size) {
      for(j in 1:last_size) {
        edge <- data.frame(fromx = i-1, fromy = j, tox = i, toy = k, weight = 0.5, type = "normal")
        nn_df_stack <- insert_top(nn_df_stack, edge)
      }
      edge2 <- data.frame(fromx = -1, fromy = -1, tox = i, toy = k, weight = 0.5, type = "bias")
      nn_df_stack <- insert_top(nn_df_stack, edge2)      
    }
  }

  
  
  normalize_ys <- function(sub_df) {
    sub_df$fromy <- sub_df$fromy - mean(sub_df$fromy)
    sub_df$toy <- sub_df$toy - mean(sub_df$toy)
    return(sub_df)
  }
  
  df <- as.data.frame(nn_df_stack)
  df_normalized <- df %>% group_by(fromx, tox) %>% do(normalize_ys(.))
  ## put the bias node somewhere below:
  df_normalized$fromx[df_normalized$type == "bias"] <- 0
  df_normalized$fromy[df_normalized$type == "bias"] <- max(df_normalized$fromy) + exp(1) - 2
  
  
  return(df_normalized)
}

## Given a vector of outputs produced by layer layer_num - 1, 
## compute the outputs of layer layer_num
## does not work for input layer!
outs_to_ins <- function(nn_df, input_vec, layer_num) {
  sub_weights <- nn_df[nn_df$tox == layer_num, ]
  sub_weights <- sub_weights[order(sub_weights$toy, sub_weights$fromy), ]
  
  single_node_output <- function(sub_df, invec) {
    invec <- c(invec, -1) # bias weight
    out <- sum(invec * sub_df$weight)
    return(data.frame(out))
  }
  
  answer <- sub_weights %>% group_by(toy) %>% do(single_node_output(., input_vec))
  return(answer$out)
}

forward_prop <- function(nn_df, input_vec) {
  num_layers <- length(unique(nn_df$tox)) # uses the bias endpoints to count
  for(i in 2:num_layers) {
    input_vec <- outs_to_ins(nn_df, input_vec, i)
  }
  return(input_vec)
}

plot_network <- function(nn_df) {
  p <- ggplot(nn_df) +
    geom_segment(aes(x = fromx, xend = tox, y = fromy, yend = toy, color = type, size = weight)) +
    geom_point(aes(x = fromx, y = fromy), size = 5) +
    geom_point(aes(x = tox, y = toy), size = 5) +
    scale_size(range = c(0, 2))
  
  plot(p)
}


layer_sizes <- c(3, 4, 3, 4, 1)
nn_df <- create_nn_df(layer_sizes)
forward_prop(nn_df, c(0, 0, 0))
plot_network(nn_df)




