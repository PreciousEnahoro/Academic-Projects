# Tutorial from r for marketing research & analytics
install.packages('nFactors')
library(nFactors)
install.packages('corrplot')
library(corrplot)
install.packages('gplots')
library(gplots)
install.packages('RColorBrewer')
library(RColorBrewer)
install.packages('GPArotation')
library(GPArotation)
install.packages('semPlot')
library(semPlot)

# get dataset
brand.ratings <- read.csv("http://goo.gl/IQl8nc")
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean)
rownames(brand.mean) <- brand.mean[, 1]
brand.mean <- brand.mean[, -1]

# OK now for the tutorial
nScree(brand.sc[, 1:9])
eigen(cor(brand.sc[, 1:9]))
factanal(brand.sc[, 1:9], factors=2)
factanal(brand.sc[, 1:9], factors=3)

# section 3.3
(brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation="oblimin"))
heatmap.2(brand.fa.ob$loadings, col=brewer.pal(9, "Greens"), trace="none", key=FALSE, dend="none", Colv=FALSE, cexCol = 1.2, main="\n\n\n\n\nFactor loadings for brand adjectives")

semPaths(brand.fa.ob, what="est", residuals=FALSE, cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"), edge.label.cex=0.75, nCharNodes=7)

# section 3.4
brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation="oblimin", scores="Bartlett")
brand.scores <- data.frame(brand.fa.ob$scores)
brand.scores$brand <- brand.sc$brand
head(brand.scores)
brand.fa.mean <- aggregate(. ~ brand, data=brand.scores, mean)
rownames(brand.fa.mean) <- brand.fa.mean[, 1]
brand.fa.mean <- brand.fa.mean[, -1]
names(brand.fa.mean) <- c("Leader", "Value", "Latest")
brand.fa.mean

# concluse with a graph
heatmap.2(as.matrix(brand.fa.mean), col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none", cexCol=1.2, main="\n\n\n\n\n\nMean factor score by brand")