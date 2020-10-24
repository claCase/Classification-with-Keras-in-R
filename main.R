install.packages("tensorflow")
install.packages("keras")
install.packages("tfdatasets")
install.packages("ramify")
install.packages("ggplot2")

library(keras)
library(tfdatasets)
library(MASS)
library(ramify)
library(ggplot2)

generate_data = function(modes=6, cov=matrix(c(1,0,0,1), 2, 2), radius=15, samples=40){
  theta = seq(0, 2*pi, length.out = modes+1)
  x = cos(theta)*radius
  y = sin(theta)*radius
  mus = cbind(x,y)
  data = c()
  labels = c()
  i <- 0
  for (mu in 1:modes){
    # sample points
    sampled_data = mvrnorm(samples, mus[mu,], cov)
    data <- rbind(data, sampled_data)
    i <- i + 1
  }
  return(data)
}

generate_labels = function(modes=8, samples=30){
  labels = c()
  for (mu in 1:modes){
    # make label
    print(mu)
    labels = c(labels,replicate(samples, mu-1))
  }
  #labels = to_categorical(labels)
  return(labels)
}


modes = 8
samples = 40
data = generate_data(modes = modes, samples = samples)
labels = generate_labels(modes = modes, samples = samples)+1
plot(data, col=colors()[labels*3], pch=16)
labels = to_categorical(labels-1)

# CREATE MODEL
model <- keras_model_sequential() %>% 
  layer_dense(units = 128, activation = "softmax") %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dense(units = modes, activation = "softmax")


model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy", 
)

# UNTRAINED MODEL PREDICTIONS 
x = seq(-20,20,0.5)
data_test = as.matrix(expand.grid(x,x))

predictions.untrained = model%>%predict(data_test)
argmax_pred.untrained = argmax(predictions.untrained, rows=T)
plot(data_test, col=colors()[argmax_pred.untrained*3],  pch=15)
points(data)

# TRAIN THE MODEL
model%>%fit(data, labels, epochs=15)

# TEST PREDICTIONS
predictions.trained = model%>%predict(data_test)

# gets the index of the maximum probability class
argmax_pred.trained = argmax(predictions.trained, rows=T)
plot(data_test, col=colors()[(argmax_pred.trained)*3], pch=15)
points(data, col="red")