## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message = FALSE, warning = FALSE----------------------------------------
library(ClusTorus)
library(tidyverse)

data <- toydata2
head(data)

data %>% ggplot(aes(x = phi, y = psi, color = label)) + geom_point() +
  scale_x_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))+
  scale_y_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))+
  ggtitle('Data set 2 with true labels')

## ---- warning = FALSE---------------------------------------------------------
set.seed(2021)

Jvec <- 5:30
l <- list()

for (j in Jvec){
  l[[j]] <- icp.torus.score(as.matrix(data[, 1:2]),
                            method = "kmeans",
                            kmeansfitmethod = "general",
                            init = "hierarchical",
                            param = list(J = j), 
                            verbose = FALSE)
}


## -----------------------------------------------------------------------------
output <- hyperparam.torus(data[, 1:2], icp.torus.objects = l, option = "risk")
output$IC.results
output$alpha.results
output$optim$hyperparam

## -----------------------------------------------------------------------------
icp.torus.kmeans <- output$optim$icp.torus
alphahat <- output$optim$hyperparam[2]

## -----------------------------------------------------------------------------
c_kmeans <- cluster.assign.torus(data[, 1:2], icp.torus.kmeans, level = alphahat)
c_kmeans$kmeans$plot

## -----------------------------------------------------------------------------
result.dat.kmeans <- data.frame(data[, 1:2], membership = as.factor(c_kmeans$kmeans$cluster.id.outlier)) %>%
  mutate(membership = ifelse(membership == max(c_kmeans$kmeans$cluster.id.outlier), "out", membership))

g1 <- ggplot(data = result.dat.kmeans, aes(phi,psi, color = membership)) + geom_point() +
  scale_x_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))+
  scale_y_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))

g1

