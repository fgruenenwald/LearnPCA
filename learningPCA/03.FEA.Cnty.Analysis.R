#===============================================================================
# FOOD ENVIRONMENT COUNTY DATA 
#===============================================================================


#load the data------------------------------------------------------------------
ct <- read.csv(paste(path.data.clean, 'CountyDataClean.csv', sep = ""))
vrb <- read.csv(paste(path.data.clean, 'VariableListClean.csv', sep = ""))
my.data <- read.csv(paste(path.data.raw, 'StateandCountyData.csv', sep = ""))


#------------------------ANALYSIS-----------------------------------------------

#decompose County data into singular values
s <- svd(ct)

#see how much information we retained :
info.ret <- 100 * s$d^2/sum(s$d^2)
sum(info.ret[c(1,2)])
pdf(paste(path.fig, 'CntyPcPower.pdf'))
barplot(info.ret[1:10], names.arg = c(1:10), xlab = 'Principle Component #', 
        ylab = 'variation explained (%)', ylim = c(0,100))
dev.off()


#------------------Loading Scores------------------------

#determine loading scores of PC1
ld.sc.PC1.value <- s$v[,1]^2/sum(s$v[,1]^2)
ld.sc.PC1 <- data.frame(vrb.name = vrb$Variable_Name,
                        cat.name = vrb$Category_Name,
                        Value = ld.sc.PC1.value)
ld.sc.PC1 <- ld.sc.PC1[order(ld.sc.PC1$Value, decreasing = TRUE),]

#store in results
write.csv(ld.sc.PC1, paste(path.results, 'FEA.LoadingScoresPc1.csv', sep = ""))

#determine loading scores of PC1
ld.sc.PC2.value <- s$v[,2]^2/sum(s$v[,2]^2)
ld.sc.PC2 <- data.frame(vrb.name = vrb$Variable_Name,
                        cat.name = vrb$Category_Name,
                        Value = ld.sc.PC2.value)

#order so the strongest variables are on top
ld.sc.PC2 <- ld.sc.PC1[order(ld.sc.PC1$Value, decreasing = TRUE),]

#store in results
write.csv(ld.sc.PC1, paste(path.results, 'FEA.LoadingScoresPc2.csv', sep = ""))

#---------------Clustering----------------

#lets try to divide the data set into clusters 
#we would expect 51 clusters by states
set.seed(10)
km <- kmeans(ct, centers = 51, iter.max = 100)

#----------------PC plot-------------------

#Let's look at the actual data coloredaccording to states
pdf(paste(path.fig, '2D_pca_plot.pdf'))
plot(s$u[,1], s$u[,2], xlab = 'PC1', ylab = 'PC2', col = km$cluster )
dev.off()

#------compare against clustering by state
#create the list of States for counties in the data
States <- c()
County <- c(sort(unique(my.data$County)))
my.data1 <- my.data[which(my.data$Variable_Code %in% vrb$Variable_Code),]

#list States in the right order
for (c in 1 : length (County)){
  States <- append( States, 
                    my.data1$State[c(which(my.data$County ==  County[c]))])
}

pdf(paste(path.fig, '2D_pca_plot_states.pdf'))
plot(s$u[,1], s$u[,2], xlab = 'PC1', ylab = 'PC2', col = as.fumeric(States))
dev.off()

#let's find out which variables explain most of the variability in the data
#look at the composition (cmp) of PC 1 & 2 
cmp1 <- s$v[,1]^2 / sum(s$v[,1]^2)
cmp2 <- s$v[,2]^2 / sum(s$v[,2]^2)
vrb[.1 < cmp1,]  # get the varaible names of most relevant variables in PC1
vrb[.1 < cmp2,]  # and PC2

#---------------------------------HEATMAP---------------------------------------
#Visualize correlations among variables with a correlation heatmap

#calculate the correlations
cormat1 <- abs(round(cor(ct),2))
rownames(cormat1) <- vrb$Variable_Name
colnames(cormat1) <- vrb$Variable_Name
head(cormat1)

#melt the correlation matrix
melted_cormat1 <- melt(cormat1)
head(melted_cormat1)

#plot the matrix as heat map
pdf(paste(path.fig, 'CntyHeatmap.pdf', sep = ""))
ggplot(data = melted_cormat1, aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  theme(axis.text = element_text(size = 1, color = as.fumeric(vrb$Subcategory_Name)))
dev.off()


