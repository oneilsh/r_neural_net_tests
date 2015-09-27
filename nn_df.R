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
forward <- function(nn_df, input_vec, layer_num) {
  sub_weights <- nn_df[nn_df$tox == layer_num, ]
  sub_weights <- sub_weights[order(sub_weights$toy, sub_weights$fromy), ]
  
  single_node_output <- function(sub_df, invec) {
    invec <- c(invec, -1) # bias weight
    out <- sum(invec * sub_df$weight)
    out <- 1/(1 + exp(-out/1.0))
    return(data.frame(tox = sub_df$tox[1], toy = sub_df$toy[1], out))
  }
  
  answer <- sub_weights %>% group_by(toy) %>% do(single_node_output(., input_vec))
  return(answer)
}

forward_prop <- function(nn_df, input_vec) {
  nn_df <- data.frame(nn_df)
  nn_df <- nn_df[order(nn_df$toy, nn_df$fromy), ]
  num_layers <- length(unique(nn_df$tox)) # uses the bias endpoints to count
  out_stack <- rstack()
  
  ## we gotta put in "outs" for the first layer based on the input vec
  first_layer_df <- unique(data.frame(toy = nn_df$fromy[nn_df$fromx == 1]))
  first_layer_df$tox = 1
  first_layer_df$out <- input_vec
  first_layer_df <- first_layer_df[c("tox", "toy", "out")] # reorder cols
  out_stack <- insert_top(out_stack, first_layer_df)
  
  for(i in 2:num_layers) {
    res_df <- forward(nn_df, input_vec, i)
    out_stack <- insert_top(out_stack, res_df)
    input_vec <- res_df$out
  }
  
  return(as.data.frame(out_stack))
}


backward_errors <- function(nn_df, outs, desired) {
  num_layers <- length(unique(nn_df$tox)) # uses the bias endpoints to count
  outs <- outs[order(outs$tox, outs$toy), ]
  outs$error <- NA
  last_outs <- outs$out[outs$tox == num_layers]
  outs$error[outs$tox == num_layers] <- (desired - last_outs) * last_outs * (1 - last_outs)

  for(layer_num in seq(num_layers-1, 2)) {
    ylocs <- unique(nn_df$fromy[nn_df$fromx == layer_num])
    for(yloc in ylocs) {
      weights <- nn_df$weight[nn_df$fromy == yloc & nn_df$fromx == layer_num]
      errors <- outs$error[outs$tox == (layer_num + 1)]

      newerror <- sum(weights * errors)
      outs$error[outs$tox == layer_num &  outs$toy == yloc] <- newerror
    }
  }
  
  return(outs)
}


update_weights <- function(nn_df, outs_errors, learningconstant = 0.1) {
  ## weight = weight + error * input * learningconstant (
  ## input is the input from the previous node
  
  by_edges <- group_by(nn_df, fromx, fromy, tox, toy, type)
  edge_update <- function(sub_df, outs_errorsb, learningconstantb) {
    # because attach() sucks ;)
    fromx = sub_df$fromx
    fromy = sub_df$fromy
    tox = sub_df$tox
    toy = sub_df$toy
    weight <- sub_df$weight
    type = sub_df$type
    error = outs_errorsb$error[outs_errorsb$tox == tox & outs_errorsb$toy == toy]
    input <- NA
    if(type == "bias") {
      input <- -1
    } else {
      input <- outs_errorsb$out[outs_errorsb$tox == fromx & outs_errorsb$toy == fromy]
    }
    
    newweight <- weight + error * input * learningconstantb
    return(data.frame(weight = newweight))
  }
  
  new_nn_df <- do(by_edges, edge_update(., outs_errors, learningconstant))
  return(new_nn_df)
}



plot_network <- function(nn_df) {
  nn_df <- nn_df[!is.na(nn_df$weight), ]
  nn_df$weight <- (max(nn_df$weight) - nn_df$weight)/(max(nn_df$weight) - min(nn_df$weight) + 0.1)
  p <- ggplot(nn_df) +
    geom_segment(aes(x = fromx, xend = tox, y = fromy, yend = toy, color = type, size = weight)) +
    geom_point(aes(x = fromx, y = fromy), size = 5) +
    geom_point(aes(x = tox, y = toy), size = 5) +
    scale_size(range = c(0, 2))
  
  plot(p)
}


colors <- list()
colors[[1]] <- list(color = c(1,0,0), out = 1)
colors[[2]] <- list(color = c(0,0,1), out = 0)
colors[[3]] <- list(color = c(0,1,1), out = 0)
colors[[4]] <- list(color = c(1,1,0), out = 1)
colors[[5]] <- list(color = c(0.3,0.6,0), out = 1)
colors[[6]] <- list(color = c(0.1,0.6,0.8), out = 0)
colors[[7]] <- list(color = c(0.3,0.9,0.1), out = 1)
colors[[8]] <- list(color = c(0.4,0.4,0.1), out = 1)
colors[[9]] <- list(color = c(0.1,0.0,0.2), out = 0)


layer_sizes <- c(3, 3, 3, 1)
nn_df <- create_nn_df(layer_sizes)


for(i in 1:100) {
  for(example in colors) {
    if(i%%1 == 0) {
      plot_network(nn_df)
    }
    color <- example$color
    expected <- example$out
    outs <- forward_prop(nn_df, color)
    outs_errors <- backward_errors(nn_df, outs, expected)
    print(outs_errors$error[outs_errors$tox == max(outs_errors$tox)]) # print last layer errors

    
    
    nn_df <- update_weights(nn_df, outs_errors, 0.05) 
  }
}

forward_prop(nn_df, c(0.7, 0.0, 0.2))


