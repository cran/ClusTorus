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
l <- icp.torus(as.matrix(data[, 1:2]),
                            model = "kmeans",
                            kmeansfitmethod = "general",
                            init = "hierarchical",
                            J = Jvec,
                            verbose = FALSE)

## -----------------------------------------------------------------------------
output <- hyperparam.torus(l, option = "risk")
output
plot(output)

## -----------------------------------------------------------------------------
icp.torus.kmeans <- output$icp.torus
alphahat <- output$alphahat

## -----------------------------------------------------------------------------
c_kmeans <- cluster.assign.torus(icp.torus.kmeans, level = alphahat)
plot(icp.torus.kmeans, level = alphahat)

## -----------------------------------------------------------------------------
c_kmeans
plot(c_kmeans)

