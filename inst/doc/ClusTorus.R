## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message = FALSE, warning = FALSE----------------------------------------
library(ClusTorus)
library(tidyverse)

data <- SARS_CoV_2$tbl
data <- data.frame(data, id = rownames(data)) %>%
  separate(id,into = c(',','position','type','rest'))
data <- data %>% select(-",")

data <- data[(data$type == "B"), ]
data <- na.omit(data[, 1:2])
head(data)

## -----------------------------------------------------------------------------
data <- data / 180 * pi
data <- on.torus(data)
ggplot(data = as.data.frame(data), mapping = aes(x = phi, y = psi)) + geom_point()

## ---- warning = FALSE---------------------------------------------------------
set.seed(2021)

Jvec <- 3:35
l <- list()

for (j in Jvec){
  l[[j]] <- icp.torus.score(as.matrix(data),
                            method = "all",
                            mixturefitmethod = "axis-aligned",
                            kmeansfitmethod = "general",
                            param = list(J = j, concentration = 25), 
                            verbose = FALSE)
}


## -----------------------------------------------------------------------------
alpha <- 0.1
out <- data.frame()

for (j in Jvec){
  a<-icp.torus.eval(l[[j]], level = alpha, eval.point = grid.torus())
  mu_kde <- sum(a$Chat_kde[,1])
  mu_e <- sum(a$Chat_e[,1])
  mu_kmeans <- sum(a$Chat_kmeans[,1])
  out <- rbind(out, data.frame(J = j, mu_kde = mu_kde, mu_e = mu_e, mu_kmeans = mu_kmeans))
}

head(out)

## -----------------------------------------------------------------------------
J_kde <- out$J[which.min(out$mu_kde)]
J_e <- out$J[which.min(out$mu_e)]
J_kmeans <- out$J[which.min(out$mu_kmeans)]

icp.torus.kde <- l[[J_kde]]
icp.torus.mix <- l[[J_e]]
icp.torus.kmeans <- l[[J_kmeans]]

c_mix <- cluster.assign.torus(data, icp.torus.mix, level = alpha)
c_kmeans <- cluster.assign.torus(data, icp.torus.kmeans, level = alpha)

## -----------------------------------------------------------------------------
output <- hyperparam.torus(data, icp.torus.objects = l, option = "risk")
output$IC.results
output$alpha.results
output$optim$hyperparam

## -----------------------------------------------------------------------------
c_mix$mixture$plot
c_kmeans$kmeans$plot

## -----------------------------------------------------------------------------
library(cowplot)
result.dat.mix <- data.frame(data, membership = as.factor(c_mix$mixture$cluster.id.outlier)) %>%
  mutate(membership = ifelse(membership == max(c_mix$mixture$cluster.id.outlier), "out", membership))
result.dat.kmeans <- data.frame(data, membership = as.factor(c_kmeans$kmeans$cluster.id.outlier)) %>%
  mutate(membership = ifelse(membership == max(c_kmeans$kmeans$cluster.id.outlier), "out", membership))

g1 <- ggplot(data = result.dat.mix, aes(phi,psi, color = membership)) + geom_point() +
  scale_x_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))+
  scale_y_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))
g2 <- ggplot(data = result.dat.kmeans, aes(phi,psi, color = membership)) + geom_point() +
  scale_x_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))+
  scale_y_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))

plot_grid(g1, g2)

## -----------------------------------------------------------------------------
ia <- icp.torus.eval(icp.torus.kde, level = alpha, eval.point = grid.torus())
b <- data.frame(ia$eval.point, ia$Chat_kde == 1)
colnames(b) <- c("phi","psi","C_kde")

g0 <- ggplot() + geom_contour(aes(phi, psi, z = ifelse(C_kde,1,0)), data = b, size = 1,lineend = "round" ) +
  geom_point(mapping = aes(x,y), data = data.frame(x = data[,1],y =data[,2])) +
  scale_x_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))+
  scale_y_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))
g0

