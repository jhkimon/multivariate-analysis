library(psych)
library(dplyr)
library(ggplot2)

data_origin <- read.csv('project/datasetsatisfaction.csv')
head(data_origin)
str(data_origin)

## ---- EDA -----------------------------------------------------------------

# Check null data
dim(data_origin)
colSums(is.na(data_origin))

# Check overall Satisfaction column - consider only unsatisfied case
table(data_origin$satisfaction.in.RM) 

data_origin$satisfaction.in.RM <- as.factor(data_origin$satisfaction.in.RM)

ggplot(data_origin, aes(x = satisfaction.in.RM)) +
    geom_bar() +
    xlab("Satisfaction in RM") +
    ylab("Frequency") +
    ggtitle("Histogram of Satisfaction in RM")

data <- data_origin %>%
    filter(satisfaction.in.RM == 2) %>%
    select(-satisfaction.in.RM) %>%
    na.omit()

dim(data)

## ---- Principal Factor Method --------------------------------------------

# Conduct principal component analysis for choosing the number of factors in the principal factor method.
cormat <- cor(data)
dimnames(cormat) <- list(c("x1", "x2", "x3", "x4", "x5", "x6","x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16"),
                         c("x1", "x2", "x3", "x4", "x5", "x6","x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16"))
round(cormat,3) # No negative correlation here.


dat.pca <- princomp(covmat=cormat,cor=T)
summary(dat.pca)
dat.pca$sdev^2
screeplot(dat.pca, npcs=16,type="l")

# Rule 1. Scree Plot - Choose p = 4
# Rule 2. Kaiser's Rule - Choose p = 2
# Rule 3. Choose cumulative proportion over 70% - choose p = 4

fac.pf1 = fa(data, nfactors=4, fm="pa", rotate="none", max.iter = 1000, tol = 1e-6)
fac.pf1
fa.plot(fac.pf1,xlim=c(-1,1), ylim=c(-1,1),)

fac.pf2 = fa(data, nfactors=4, fm="pa", rotate="varimax", max.iter = 1000, tol = 1e-6)
fac.pf2
fac.pf2$loadings

# Assume Simple Structure
loadings_matrix <- as.matrix(fac.pf2$loadings)
max_loadings <- apply(loadings_matrix, 1, function(x) which.max(x))

loadings_df <- data.frame(
    Variable = names(max_loadings),
    Max_Factor = max_loadings
)

grouped_variables.fa <- loadings_df %>%
    group_by(Max_Factor) %>%
    summarize(Variables = paste(Variable, collapse = ", "))

grouped_variables.fa

## By using simple structure, 
## f1 (Doctor) - x1 x2 x3 x4 x5
## f2 (Time) - x10 x14 x15 x16
## f3 (Rooms) - x6 x7 x8 x9
## f4 (Services) - x11 x12 x13

# Check s > 0 Situation
p <- 204
k <- 4
cat("Number of Free Parameters:", ((p-k)^2 - (p+k))/2)


## ---- Maximum Likelihood Factor Method ----------------------------------

# Consider 4 factor Model (same with Principal Facotr Method)
fac.ml.result <- fa(data, nfactors = 4, fm = "ml", rotate = "varimax", max.iter = 10000, tol = 1e-6)
summary(fac.ml.result)
fac.ml.result$loadings

loadings_matrix <- as.matrix(fac.ml.result$loadings)
max_loadings <- apply(loadings_matrix, 1, function(x) which.max(x))

loadings_df <- data.frame(
    Variable = names(max_loadings),
    Max_Factor = max_loadings
)

grouped_variables.ml <- loadings_df %>%
    group_by(Max_Factor) %>%
    summarize(Variables = paste(Variable, collapse = ", "))

grouped_variables.fa
grouped_variables.ml

fac.ml1 <- fa(data, nfactors = 1, fm = "ml", rotate = "none", max.iter = 10000, tol = 1e-6)
summary(fac.ml1)

fac.ml2 <- fa(data, nfactors = 2, fm = "ml", rotate = "none", max.iter = 10000, tol = 1e-6)
summary(fac.ml2)

fac.ml3 <- fa(data, nfactors = 3, fm = "ml", rotate = "none", max.iter = 10000, tol = 1e-6)
summary(fac.ml3)

fac.ml4 <- fa(data, nfactors = 4, fm = "ml", rotate = "none", max.iter = 10000, tol = 1e-6)
summary(fac.ml4)

fac.ml5 <- fa(data, nfactors = 5, fm = "ml", rotate = "none", max.iter = 10000, tol = 1e-6)
summary(fac.ml5)

fac.ml6 <- fa(data, nfactors = 6, fm = "ml", rotate = "none", max.iter = 10000, tol = 1e-6)
summary(fac.ml6)

fac.ml7 <- fa(data, nfactors = 7, fm = "ml", rotate = "none", max.iter = 10000, tol = 1e-6)
summary(fac.ml7)


## ---- FA Scores ----------------------------------

head(fac.ml.result$scores)
apply(fac.ml.result$scores, 2, mean)
apply(fac.ml.result$scores, 2, var)

head(fac.pf2$scores)
apply(fac.pf2$scores, 2, mean)
apply(fac.pf2$scores, 2, var)
