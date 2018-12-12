library(caret)
library(leaps)
# library(MASS)
library("e1071")

################### PROBLEM 1 ###############################

pb2 <- read.table("C:/Users/anmol/Downloads/pb2.txt")
data<-  data.frame(pb2)
print(data)
model <- tune.svm(V1 ~ 1., data = data,  kernel='radial', cost=c(0.1,1,10,100,1000),gamma=c(0.5, 1,2,3,4))

print(model)
summary(model)
plot(model)


################### PROBLEM 2 ###############################
min.model = lm(data$V1 ~ 1)
min.model = lm(data$V1 ~ 1, data=data)
fwd.model = step(min.model, direction='forward', scope=(~ .))
summary(fwd.model)

g <- lm(V1~.,data=data)
summary(g)

g <- update(g, . ~ . - V1)
g

g <- update(g, . ~ . - V2)
g

g <- update(g, . ~ . - V3)
g

g <- update(g, . ~ . - V4)
g

g <- update(g, . ~ . - V5)
g

step(g , direction = "forward")

plot(g)


