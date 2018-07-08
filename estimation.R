n = 10000
running_pi = c(rep(0,n))
distance = c(rep(0,n))

for(i in 1:n){
  x = runif(1, min=0, max=.5)
  y = runif(1, min=0, max=.5)
  distance[i] = sqrt( (x-0.25)^2+(y-0.25)^2 )
  running_pi[i] = 4*(length(which(distance[1:i] < 0.25))/i)
}

plot(running_pi,type = 'l', main = "Estimate over iterations")
abline(a=3.14, b=0, col="red")


pi = data.frame(iter = c(1:n), pi_iter= running_pi)
library(ggplot2)

population<-rpois(100000,lambda = 3)
hist(population)


n = 10000
total = 0
for (i in 1:n) {
  x = runif(1)
  total = total + exp(x)
}
average = total/n
average

