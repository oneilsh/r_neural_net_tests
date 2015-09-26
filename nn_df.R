library(rstackdeque)
library(ggplot2)


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
  
  return(as.data.frame(nn_df_stack))
}

layer_sizes <- c(3, 4, 5, 3)
nn_df <- create_nn_df(layer_sizes)


head(nn_df)

p <- ggplot(nn_df) +
  geom_point(aes(x = fromx, y = fromy), size = 5) +
  geom_point(aes(x = tox, y = toy), size = 5) +
  geom_segment(aes(x = fromx, xend = tox, y = fromy, yend = toy))

plot(p)


