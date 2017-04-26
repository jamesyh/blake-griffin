# Has he gotten better at threes??

library(R2jags)

blake <- read.csv("C:/Users/James/Documents/My R Research/blake-griffen/blake-griffin/blake griffin.txt")

threeMade <- blake$X3P[-8]
threeAttempted <- blake$X3PA[-8]
year <- 1:7


model_string <- "model{
  
  for(i in 1:7){
    
    threeMade[i] ~ dbinom(theta[i],threeAttempted[i])
    
    theta[i] ~ dbeta(alpha, beta)
    
  }
    
  alpha ~ dgamma(1,1)
  beta ~ dgamma(1,1)

}"

model <- jags.model(textConnection(model_string), 
                    data = list(threeMade = threeMade,threeAttempted = threeAttempted))

update(model, 10000, progress.bar="none"); # Burnin for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("theta"), 
                     n.iter=100000)

summary(samp)

# plot(samp)



draws <- as.matrix(samp)




season1 <- draws[,1]
season2 <- draws[,2]
season3 <- draws[,3]
season4 <- draws[,4]
season5 <- draws[,5]
season6 <- draws[,6]
season7 <- draws[,7]



mean(season7 > season6)
mean(season7 > season5)
mean(season7 > season4)
mean(season7 > season3)
mean(season7 > season2)
mean(season7 > season1)



plot(density(season1),col = 2,lwd = 3,ylim = c(0,9),
     main = "Probability of Making a Three Pointer by Season")

for(i in 2:7){
  
 lines(density(draws[,i]),col = i + 1,lwd = 3)
  
}

legend(.47,8.5,c("Season 1","Season 2","Season 3","Season 4","Season 5","Season 6","Season 7"),
       lty=c(1,1),lwd=c(2.5,2.5),col=c(2,3,4,5,6,7,8))


# prob <- c(mean(season1),mean(season2),mean(season3),mean(season4),mean(season5),mean(season6),mean(season7))
# year <- 1:7
# 
# 
# plot(log(prob) ~ year)
# 
# model <- lm(log(prob) ~ year)
# summary(model)
# 
# abline(model)

