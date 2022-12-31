# Loading Data
data = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt", header=FALSE)

names(data) <- c('variance ','skewness ','curtosis', 'entropy', 'class' )

#data$class <- factor(data$class, levels = 0:1, labels = c("fake", "real"))

# Apply PCA using prcomp function
# Need to scale / Normalize as
# PCA depends on distance measure
my_pca <- prcomp(data, scale = TRUE,
                 center = TRUE, retx = T)
names(my_pca)

# Summary
summary(my_pca)
my_pca

# View the principal component loading
# my_pca$rotation[1:5, 1:4]
my_pca$rotation

# See the principal components
dim(my_pca$x)
my_pca$x

# Plotting the resultant principal components
# The parameter scale = 0 ensures that arrows
# are scaled to represent the loadings
biplot(my_pca, main = "Biplot", scale = 0)

# Compute standard deviation
my_pca$sdev

# Compute variance
my_pca.var <- my_pca$sdev ^ 2
my_pca.var

# Proportion of variance for a scree plot
propve <- my_pca.var / sum(my_pca.var)
propve

# Plot variance explained for each principal component
plot(propve, xlab = "principal component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b",
     main = "Scree Plot")

# Plot the cumulative proportion of variance explained
plot(cumsum(propve),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Find Top n principal component
# which will atleast cover 90 % variance of dimension
which(cumsum(propve) >= 0.9)[1]

# Predict mpg using first 4 new Principal Components
# Add a training set with principal components
new_data <- data.frame(disp = data$class, my_pca$x[, 1:4])

# Running a Decision tree algporithm
## Installing and loading packages
# install.packages("rpart")
# install.packages("rpart.plot")
# library(rpart)
# library(rpart.plot)
# 
# rpart.model <- rpart(disp ~ .,
#                      data = train.data, method = "anova")
# 
# rpart.plot(rpart.model)

new_data$disp <- factor(new_data$disp, levels = 0:1, labels = c("fake", "real"))
ratios = list(c(0.6,0.4), c(0.75,0.25),c(0.9,0.1))

means = c()
sds = c()

for (ratio in ratios)
{
  values = c()
  for (i in 1:100)
  {
    idx = sample(2,nrow(new_data), replace=T, prob=ratio)
    train = new_data[idx==1,]
    test = new_data[idx==2,]
    model = ctree(disp ~., data=train)
    #plot(model)
    
    p=predict(model, test)
    cf = table(p,  test$disp)
    cf
    
    
    pcf=cf/sum(cf)
    pcf
    values = append(values, sum(diag(cf))/sum(cf))
  }
  sds=append(sds, sd(values))
  means=append(means, mean(values))
}
sds
means

