library(class)
data = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt", header=FALSE)

names(data) <- c('variance ','skewness ','curtosis', 'entropy', 'class' )

data$class <- factor(data$class, levels = 0:1, labels = c("fake", "real"))

ratios = list(c(0.6,0.4), c(0.75,0.25),c(0.9,0.1))

k_values = list(3,5,7)

means = c()
sds = c()
for(k in k_values) 
{
  for (ratio in ratios)
  {
    values = c()
    for (i in 1:100)
    {
      idx = sample(2,nrow(data), replace=T, prob=ratio)
      trainBanknotes = data[idx==1,]
      testBanknotes = data[idx==2,]
      
      train=trainBanknotes[1:4]
      test=testBanknotes[1:4]
      
      
      cl_test=testBanknotes$class;
      cl_train=trainBanknotes[,5]  #as factor :)
      
      ## A 3-nearest neighbours model with no normalization
      #species vs reszta
      model <- knn(train,test, cl=cl_train,k=k)
      
      ## The resulting confusion matrix
      cf=table(testBanknotes[,'class'],model)
     
      pcf=cf/sum(cf)
      pcf
      values = append(values, sum(diag(cf))/sum(cf))
    }
    sds=append(sds, sd(values))
    means=append(means, mean(values))
  }
}

sds
means

cf



