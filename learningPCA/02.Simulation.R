#
#Here I wil try to create a 2D data set and conduct a PCA on it
#

#-------create the simulated data set-------

n <- 200 #sample size
mean.t <- 0 #mean
sd.t <- 0.2 #standard deviation
set.seed(1)#forreproducibility
random.n <- rnorm(n, mean.t, sd.t) #generate random points around 0

#from those random points, generate a sequence of 6 points as obesity values
obesity <- c(1:(n/2), (n+1):(1.5*n)) + random.n #values are between 0 and 1 with noise 

#corresponing y values are x* (1+ noise); so coeficients are not always the same
#nowlet us create a rather tightly correlated variable depicting diabetes rates:
diabetes <- obesity * (.95 + random.n)

#lets say, the density of fast food restaurants is less tightly correlated to 
#obesity, so, we lower correlation coefficient and amplify the noise
ff.dens <-    obesity * (.5 + random.n)

#the amount of physically active people weakly negatively correlated to obesity
active <-   obesity * (-.25 + random.n) 

#and lastly avariable that is directly  negatively correlated to fast food 
#restaurant density
# farmers market density 
fm.dens <-    obesity * (-.9 + 2* random.n)

#let's store these values in a dataframe sim
sim <- data.frame(obesity = obesity, diabetes = diabetes, ff.dens = ff.dens, 
                  active = active, fm.dens = fm.dens)

#Let' store this simulated data
write.csv(sim, 
          paste(path.data.clean,"Simulation1.csv", sep = ""), 
          row.names = FALSE)
#note that for now, we don't care about wehter or not negtive values in the data 
#actually make sense or not.


#----Visualizing with the ordinarymethods------------------

#subtract column means tocenter the data
for (i in 1:ncol(sim)){
  sim[,i] <- sim[,i] - c(rep( mean(sim[,i]), nrow(sim)))
}

#-----------Analyze the data------------
#let's decompose the data into singular values:
sim.svd <- svd(sim)

#show how much variation every eigenvector captures:
PCs <- c('PC1', 'PC2', 'PC3', 'PC4', 'PC5')
info.retained <-  100* sim.svd$d^2/sum(sim.svd$d^2)

                           
#let's visualize how much variance each Principal component explains
pdf(paste(path.fig, 'SimVarExplained.pdf', sep = ""))
barplot(info.retained, names.arg = PCs, ylab = 'variance explained (%)',
        ylim = c(0,100))
dev.off()

#let's illustrate that we can recover the data:
#to do that weneed to convert the vector ofeigen vectors into a diagonal matrix:
D <- matrix( c(rep(0, length(sim.svd$d)^2)), ncol = length(sim.svd$d),
                     nrow = length(sim.svd$d))
for (i in 1:length(sim.svd$d)){
  D[i,i] <- sim.svd$d[i]
}
#if we recover the data using all collumns the difference tothe original data is 
# just floating point error
sim.recov <- sim.svd$u %*% D %*% t(sim.svd$v)
sim - sim.recov

#the difference stay extremely small even if we just use the first two PCs
sim.recov <- sim.svd$u[,c(1,2)] %*% D[c(1,2),c(1,2)] %*% t(sim.svd$v[,c(1,2)])
sim - sim.recov

#So let's find out what the Principle components are for that we call on the
#u-matrix of our svd. The first two column vectors from the left  is the 
#eigenvector that is the first two principle Component.
PC1 <- sim.svd$v[,1]
PC2 <- sim.svd$v[,2]
# normalize the principal components to see their composition of differnt 
# variables
norm.PC1 <- PC1^2 / sum(PC1^2)
norm.PC2 <- PC2^2 / sum(PC2^2)
norm.PC <- round(data.frame(norm.PC1 = norm.PC1, norm.PC2 = norm.PC2),4)
rownames(norm.PC) <- c('obesity', 'diabetes', 'fastfood restaurants ', 
              'physically active', "farmer's markets")
colnames(norm.PC) <- c('PC1', 'PC2')

#check if the PCs are orthogonal by checking if the dot product is 0
0 == PC1 %*% PC2 #are Pc1 & PC2 orthogonal?

#safe this as a result
write.csv(norm.PC, paste(path.results, 'SimCompositionPC12.csv'))

#--------------Clustering---------------
#cluster the data points with a machine learning method called kmeans
# we will look for two clusters, to possibly catch differences due to  
km <- kmeans(t(sim), centers= 2)
km$cluster
write.csv(km$cluster, paste(path.results, 'SimClusters.csv'))

#-----------------Visualizing-------------------------
#------------------PC plot-------------
#  Principal Component Plot
#Let's visualihe our data:
# the rescaled  "user input"is stored in the u matrix
km <- kmeans(sim, centers= 2)
pdf(paste(path.fig, 'SimPrincipalComponentPlot.pdf', sep = ""))
  plot(sim.svd$u[,1], sim.svd$u[,2], xlab = 'PC1', ylab = 'PC2', 
       col = km$cluster)
dev.off()

#-----------------------HEATMAP---------------------------------------
#Another visualization method is to create a heatmap of correlations
# ggplot method

#calculate the correlations
cormat <- abs(round(cor(sim),2))
rownames(cormat) <- c( 'obesity',  'diabetes', 'fast food', 
                       'physically active', "farmer's markets")
colnames(cormat) <- c( 'obesity',  'diabetes', 'fast food', 
                       'physically active', "farmer's markets")

head(cormat)

#melt the correlation matrix
melted_cormat <- melt(cormat)
head(melted_cormat)

#plot the matrix as heat map
pdf(paste(path.fig, 'SimHeatmap.pdf', sep = ""))
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  theme(axis.text = element_text(size = 7))
dev.off()


