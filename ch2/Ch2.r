# Read data (husb.txt) from your directory and attach in R for use.

husb = read.table("c://husb.dat", col.names = c("hage", "hheight", "wage", "wheight", "hage_marriage"))

# Univeriate plots
hist(husb$wage)
stem(husb$wage)
boxplot(husb$wage)

# Scatterplot
plot(husb$hage,husb$wage)
 
# Scatterplot matrix.
pairs(husb)

# Draw a 3D Scatterplot
library(rgl)
plot3d(husb$hage,husb$wage,husb$hage_marriage) 
# You can right click to identify the observation ID
identify3d(husb$hage,husb$wage,husb$hage_marriage)


########################################################################

# Generate multivariate standard normal random numbers with n=50 and rho=0.5
n=50; p=5
mu=rep(0,p)
vmat=matrix(0.5,p,p)+0.5*diag(p)
library(MASS)
mvdata=mvrnorm(n,mu,vmat)

# Draw normal probabiltiy plot for individual variables
par(mfrow=c(3,2))
for(i in 1:p){qqnorm(mvdata[,i],xlab="Quantiles of Standard Normal",ylab=paste("Var",i))}

# Calculate the distance
Sx = cov(mvdata)
di2 = mahalanobis(mvdata, colMeans(mvdata), Sx)

# Draw the chi-square probability plot
library(lattice) 
qqmath(di2,distribution = function(p) qchisq(p,df=5))


########################################################################

# Draw normal probabiltiy plot for individual variables of husb data  
par(mfrow=c(3,2))
p=ncol(husb)
for(i in 1:p){qqnorm(husb[,i],xlab="Quantiles of Standard Normal",ylab=colnames(husb)[i])}

# Calculate the distance (formula 2.2);
Sx = cov(husb)
di2 = mahalanobis(husb, colMeans(husb), Sx)

# Draw the chi-square probability plot 
qqmath(di2,distribution = function(p) qchisq(p,df=5))


