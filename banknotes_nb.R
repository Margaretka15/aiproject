library(naivebayes)
data = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt", header=FALSE)

names(data) <- c('variance ','skewness ','curtosis', 'entropy', 'class' )

data$class <- factor(data$class, levels = 0:1, labels = c("fake", "real"))

ratios = list(c(0.6,0.4), c(0.75,0.25),c(0.9,0.1))

means = c()
sds = c()

for (ratio in ratios)
{
  values = c()
  for (i in 1:100)
  {
    idx = sample(2,nrow(data), replace=T, prob=ratio)
    train = data[idx==1,]
    test = data[idx==2,]
    model = naive_bayes(class ~ ., data=train, usekernel = T)
    #plot(model)
    
    p=predict(model, test)
    cf = table(p,  test$class)
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


cf


