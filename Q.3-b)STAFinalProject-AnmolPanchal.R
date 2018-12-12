library(tidyverse)
pb2 <- read.table("C:/Users/anmol/Downloads/pb2.txt")
data<-  data.frame(pb2)
print(data)

summary(data)

summary(data[data$V1 == "1", ])

summary(data[data$V1 == "2", ])

library(h2o)
h2o.init(nthreads = -1)

# convert data to H2OFrame
data <- as.h2o(data)

splits <- h2o.splitFrame(data, 
                         ratios = c(0.25, 0.25), 
                         seed = 62)
train_unsupervised  <- splits[[1]]
train_supervised  <- splits[[2]]
test <- splits[[3]]
response <- "V1"
features <- setdiff(colnames(train_unsupervised), response)

model_nn <- h2o.deeplearning(x = features,
                             training_frame = train_unsupervised,
                             model_id = "model_nn",
                             autoencoder = TRUE,
                             reproducible = TRUE, #slow - turn off for real problems
                             ignore_const_cols = FALSE,
                             seed = 42,
                             hidden = c(10, 2, 10), 
                             epochs = 100,
                             activation = "Tanh")

#h2o.saveModel(model_nn, path="model_nn", force = TRUE)
#model_nn <- h2o.loadModel("model_nn")
model_nn

pred <- as.data.frame(h2o.predict(object = model_nn, newdata = test)) 
pred


anomaly <- h2o.anomaly(model_nn, test)
  
anomaly
mean_mse <- summary(mean(anomaly))
  
mean_mse



