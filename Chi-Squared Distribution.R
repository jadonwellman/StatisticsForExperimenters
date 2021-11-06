# Understanding Chi-Squared Distribution

set.seed(123)

intV <- 10
vPopulation <- rnorm(1000, mean=0, sd=1)
nMean <- mean(vPopulation)
nSD <- sd(vPopulation)

set_info <- function (vInput,strTitle) {
  hist(vInput,main = paste('Histogram of' , strTitle))
  print(paste('Mean:',mean(vInput)))
  print(paste('Variance:',var(vInput)))
  print(paste('SD:',sd(vInput)))
}

set_info(vPopulation,'Population')

vSumZSquared <- c()
for (i in 1:1000) {
  vSample <- sample(vPopulation, size=intV, replace = T)
  vZ <- sapply(vSample,function(x) (x-nMean)/nSD)
  vZSquared <- sapply(vZ, function(x) x^2)
  vSumZSquared[i] <- sum(vZSquared)
}

# mean should be "v"(intV), and variance "2v" (2*intV)
set_info(vSumZSquared,'Sum Z Squared') 
