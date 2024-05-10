# Use the library "psych".
library(psych)

# Example: Ability and Aspiration
cor = matrix(c(1.00,0.73,0.70,0.58,0.46,0.56,
               0.73,1.00,0.68,0.61,0.43,0.52,
               0.70,0.68,1.00,0.57,0.40,0.48,
               0.58,0.61,0.57,1.00,0.37,0.41,
               0.46,0.43,0.40,0.37,1.00,0.72,
               0.56,0.52,0.48,0.41,0.72,1.00),
               nrow = 6, ncol = 6,
               dimnames = list(c("x1", "x2", "x3", "x4", "x5", "x6"),
                               c("x1", "x2", "x3", "x4", "x5", "x6")))

# Conduct principal component analysis for choosing the number of factors in the principal factor method.
student.pca=princomp(covmat=cor,cor=T)
summary(student.pca)
student.pca$sdev^2
screeplot(student.pca, npcs=6,type="l")

# Conduct factor analysis using the principal factor method without rotations.
fac.pf1 = fa(cor, nfactors=2, fm="pa", rotate="none")
fac.pf1

# Plot factor loadings 
fa.plot(fac.pf1,xlim=c(-1,1), ylim=c(-1,1),)


# Conduct factor analysis using the principal factor method with the Varimax rotation.
fac.pf2 = fa(cor, nfactors=2, fm="pa", rotate="varimax")
fac.pf2


# Conduct factor analysis using the maximum likelihood method without rotations.
fac.ml1 = fa(cor, nfactors=2, n.obs=556, fm="ml", rotate="none")
fac.ml1

# Conduct factor analysis using the maximum likelihood method with the Varimax rotation.
fac.ml2 = fa(cor, nfactors=2, n.obs=556, fm="ml", rotate="varimax")
fac.ml2
 


#####################################################################

# Example. AIDS patients¡¯ reaction to their physicians

cor = matrix(c(1.00,0.56,0.63,0.64,0.52,0.70,0.45,0.61,0.79,0.57,0.32,0.55,0.69,0.62,
               0.56,1.00,0.58,0.46,0.44,0.51,0.48,0.68,0.58,0.63,0.27,0.72,0.51,0.42,
               0.63,0.58,1.00,0.35,0.50,0.49,0.28,0.44,0.66,0.40,0.33,0.51,0.60,0.33,
               0.64,0.46,0.35,1.00,0.52,0.52,0.34,0.43,0.55,0.55,0.21,0.49,0.54,0.47,
               0.52,0.44,0.50,0.52,1.00,0.54,0.38,0.56,0.66,0.54,0.13,0.63,0.51,0.38,
               0.70,0.51,0.49,0.52,0.54,1.00,0.63,0.64,0.64,0.58,0.26,0.62,0.73,0.58,
               0.45,0.48,0.28,0.34,0.38,0.63,1.00,0.49,0.34,0.65,0.22,0.47,0.44,0.51,
               0.61,0.68,0.44,0.43,0.56,0.64,0.49,1.00,0.70,0.62,0.24,0.75,0.50,0.49,
               0.79,0.58,0.66,0.55,0.66,0.64,0.34,0.70,1.00,0.62,0.17,0.70,0.66,0.53,
               0.57,0.63,0.40,0.55,0.54,0.58,0.65,0.62,0.62,1.00,0.25,0.67,0.53,0.56,
               0.32,0.27,0.33,0.21,0.13,0.26,0.22,0.24,0.17,0.25,1.00,0.31,0.24,0.23,
               0.55,0.72,0.51,0.49,0.63,0.62,0.47,0.75,0.70,0.67,0.31,1.00,0.65,0.51,
               0.69,0.51,0.60,0.54,0.51,0.73,0.44,0.50,0.66,0.53,0.24,0.65,1.00,0.56,
               0.62,0.42,0.33,0.47,0.38,0.58,0.51,0.49,0.53,0.56,0.23,0.51,0.56,1.00),
               nrow = 14, ncol = 14,
               dimnames = list(c("x1", "x2", "x3", "x4", "x5", "x6","x7", "x8", "x9", "x10", "x11", "x12","x13", "x14"),
                               c("x1", "x2", "x3", "x4", "x5", "x6","x7", "x8", "x9", "x10", "x11", "x12","x13", "x14")))

# Conduct PCA for choosing the number of factors in the principal factor method.
student.pca=princomp(covmat=cor,cor=T)
summary(student.pca)
student.pca$sdev^2
screeplot(student.pca, npcs=14,type="l")

# Conduct factor analysis using the maximum likelihood method without rotations.
fac1 = fa(cor, nfactors=3, n.obs=64, fm="ml", rotate="none")
fac1

# Conduct factor analysis using the maximum likelihood method with the Varimax rotation.
fac2 = fa(cor, nfactors=3, n.obs=64, fm="ml", rotate="varimax")
fac2
 
