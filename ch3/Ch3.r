# 3.6.2.  Head Length in brothers

# Read data (head.dat) from your directory and attach in R for use.
head = read.table("./mva/head.dat",header=T)
attach(head)

# Conduct principal component analysis.
head.pca = prcomp(head)    
head.pca$rotation
head.pca$sdev^2

# Draw a figure (p. 25 - Ch 3 lecture note).
# Calculate intercepts.
a  = mean(head$Second) - head.pca$rotation[2,1]/head.pca$rotation[1,1]* mean(head$First)
b  = mean(head$Second) - head.pca$rotation[2,2]/head.pca$rotation[1,2]* mean(head$First)

# Draw the scatterplot between first and second;
plot(head$First,head$Second,xlim=c(130,210),ylim=c(130,210), xlab="Head length of first son", ylab="Head length of second son")
#Draw lines with intercepts a and b and slopes PC1 and PC2, respectively;
abline(a,head.pca$rotation[2,1]/head.pca$rotation[1,1])
abline(b,head.pca$rotation[2,2]/head.pca$rotation[1,2],lty=2)  
                                                              


##############################################################################################

#3.7.1. Olympic Decathlon 

# Read data (olympic.dat) from your directory and attach in R for use.
olympic = read.table("./mva/olympic.dat")
# Put the label of each variable.
colnames(olympic) = c("100m","longjump","shotput","highjump","400m","110mhurdles","discus",
                             "polevault","javelin","1500m","TS")

# Exclude an outlier (observation #34)
olympic = olympic[-34,]

# Calculate correlation matrix.
cor(olympic[,1:10])

# Transform four running events by taking negative values.
olympic[,c(1,5,6,10)] = -olympic[,c(1,5,6,10)] 

# Calculate correlation matrix.
cor(olympic[,1:10])

# Conduct principal component analysis.
olympic.pca = prcomp(olympic[,1:10],center=T,scale=T)   
olympic.pca
summary(olympic.pca)
olympic.pca$sdev^2

# Draw a scree plot.
screeplot(olympic.pca, type="l")

# Scatterplot of principal component scores
plot(olympic.pca$x[,1], olympic.pca$x[,2], xlab="PC1",ylab="PC2",type="n",lwd=2)
text(olympic.pca$x[,1], olympic.pca$x[,2],labels=row.names(olympic),col="blue",cex=.8,lwd=2)

# Plot of principal component scores vs. TS 
plot(olympic$TS,olympic.pca$x[,1], xlab="Score in decathlon",ylab="First PC score")

# Calculate the correlation between the first principal component and the total score)
cor(olympic$TS,olympic.pca$x[,1]) 





##############################################################################################

#3.7.2. Drug usage by American college students

drug =
 matrix(c(1.000, 0.447, 0.422, 0.435, 0.114, 0.203, 0.091, 0.082, 0.513, 0.304, 0.245, 0.101, 0.245, 
	 0.447, 1.000, 0.619, 0.604, 0.068, 0.146, 0.103, 0.063, 0.445, 0.318, 0.203, 0.088, 0.199, 
         0.422, 0.619, 1.000, 0.583, 0.053, 0.139, 0.110, 0.066, 0.365, 0.240, 0.183, 0.074, 0.184,
         0.435, 0.604, 0.583, 1.000, 0.115, 0.258, 0.122, 0.097, 0.482, 0.368, 0.255, 0.139, 0.293,               0.114, 0.068, 0.053, 0.115, 1.000, 0.349, 0.209, 0.321, 0.186, 0.303, 0.272, 0.279, 0.278,
         0.203, 0.146, 0.139, 0.258, 0.349, 1.000, 0.221, 0.355, 0.315, 0.377, 0.323, 0.367, 0.545, 
	 0.091, 0.103, 0.110, 0.122, 0.209, 0.221, 1.000, 0.201, 0.150, 0.163, 0.310, 0.232, 0.232, 
         0.082, 0.063, 0.066, 0.097, 0.321, 0.355, 0.201, 1.000, 0.154, 0.219, 0.288, 0.320, 0.314,
	 0.513, 0.445, 0.365, 0.482, 0.186, 0.315, 0.150, 0.154, 1.000, 0.534, 0.301, 0.204, 0.394, 
         0.304, 0.318, 0.240, 0.368, 0.303, 0.377, 0.163, 0.219, 0.534, 1.000, 0.302, 0.368, 0.467,
         0.245, 0.203, 0.183, 0.255, 0.272, 0.323, 0.310, 0.288, 0.301, 0.302, 1.000, 0.304, 0.392, 
         0.101, 0.088, 0.074, 0.139, 0.279, 0.367, 0.232, 0.320, 0.204, 0.368, 0.340, 1.000, 0.511,
         0.245, 0.199, 0.184, 0.293, 0.278, 0.545, 0.232, 0.314, 0.394, 0.467, 0.392, 0.511, 1.000),
       nrow = 13, ncol = 13,
       dimnames = list(c("cigarettes", "beer", "wine", "liquor", "cocaine", "tranquillizers", 
                         "drug store medication", "heroin", "marijuana", "hashish", "inhalants", 
                         "haluucinogenics", "amphetamine"),
                       c("cigarettes", "beer", "wine", "liquor", "cocaine", "tranquillizers",
                         "drug store medication", "heroin", "marijuana", "hashish", "inhalants", 
                         "haluucinogenics", "amphetamine")))

# Conduct principal component analysis.
# To conduct principal component analysis for a covariance (or correlation) matrix, use the princomp function.
drug.pca = princomp(covmat=drug,cor=T)
drug.pca$loadings
drug.pca$sdev^2
drug.pca$sdev^2/13

# Draw a Scree plot.
screeplot(drug.pca, npcs=13,type="l")






