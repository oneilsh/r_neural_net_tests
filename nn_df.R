library(rstackdeque)
library(ggplot2)
library(dplyr)

# returns a neural net as a data frame, in long form ;)
create_nn_df <- function(layer_sizes) {
  nn_df_stack <- rstack()
  
  input_layer_size <- layer_sizes[1]
  for(i in 1:input_layer_size) {
    edge <- data.frame(fromx = 0, fromy = i, tox = 1, toy = i, weight = 0.5)
    nn_df_stack <- insert_top(nn_df_stack, edge)
  }
  
  
  for(i in 2:length(layer_sizes)) {
    layer_size <- layer_sizes[i]
    last_size <- layer_sizes[i-1]
    for(j in 1:last_size) {
      for(k in 1:layer_size) {
        edge <- data.frame(fromx = i-1, fromy = j, tox = i, toy = k, weight = 0.5)
        nn_df_stack <- insert_top(nn_df_stack, edge)
      }
    }
  }

  normalize_ys <- function(sub_df) {
    sub_df$fromy <- sub_df$fromy - mean(sub_df$fromy)
    sub_df$toy <- sub_df$toy - mean(sub_df$toy)
    return(sub_df)
  }
  
  df <- as.data.frame(nn_df_stack)
  df_normalized <- df %>% group_by(fromx, tox) %>% do(normalize_ys(.))
  
  return(df_normalized)
}



layer_sizes <- c(3, 4, 3, 4, 1)
nn_df <- create_nn_df(layer_sizes)


head(nn_df)

p <- ggplot(nn_df) +
  geom_point(aes(x = fromx, y = fromy), size = 5) +
  geom_point(aes(x = tox, y = toy), size = 5) +
  geom_segment(aes(x = fromx, xend = tox, y = fromy, yend = toy))

plot(p)


