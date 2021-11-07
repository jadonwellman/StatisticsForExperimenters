# Understanding Chi-Squared Distribution

set.seed(123)

# The v (Nu), degrees of freedom. Compare 5 and 50. 5 should be skewed to the
# right. 50+ should be ~normal
intV <- 50

vPopulation <- rnorm(1000, mean=0, sd=1)
nMean <- mean(vPopulation)
nSD <- sd(vPopulation)

set_info <- function (vInput,strTitle) {
  hist(vInput,main = paste('Histogram of' , strTitle),
       xlab=paste(strTitle,'Values'))
  print(paste('Mean:',mean(vInput)))
  print(paste('Variance:',var(vInput)))
  print(paste('SD:',sd(vInput)))
}

set_info(vPopulation,'Population')

vSumZSquared <- c()
for (i in 1:1000) {
  vSample <- sample(vPopulation, size=intV, replace = T)
  
  # Since the mean ~0 and the sd ~1, vZ should be pretty much a copy of 
  # the sample
  vZ <- sapply(vSample,function(x) (x-nMean)/nSD)
  
  # Squaring the values will widen the spread and make everything >=0
  vZSquared <- sapply(vZ, function(x) x^2)
  
  # Summing them up should make the spread even greater, and has a 
  # relationship to the size of the sample, the intV. As intV increases
  # there is room to breathe on the left side of the mean, so it gets
  # more normally distributed
  vSumZSquared[i] <- sum(vZSquared)
}

# Mean should be "v"(intV), and variance "2v" (2*intV)
# I'm thinking the squared part of X^2 comes from the Z "squared"
set_info(vSumZSquared,'Sum Z Squared') 
