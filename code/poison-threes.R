# Has he started making more threes???

# Data scrapped from basketball reference
blake <- read.csv("C:/Users/James/Documents/My R Research/blake-griffen/blake-griffin/blake griffin game.txt")

threeMade <- as.numeric(as.character(blake$X3P))
threeAttempted <- as.numeric(as.character(blake$X3PA))
year <- as.numeric(as.character(blake$Rk))

k <- 1
y <- NA

for(i in 1:length(year)){

    if(i != 1 & year[i] == 1) k <- k + 1
      
     y[i] <- k
  
}
year <- y

year <- year[!is.na(threeMade)]
threeAttempted <- threeAttempted[!is.na(threeMade)]
threeMade <- threeMade[!is.na(threeMade)]

#threeAttempted <- threeMade

# For Made

model_string <- "model{
  
  for(i in 1:466){
    
    threeMade[i] ~ dpois(lambda[year[i]])
    
  }
  
  for(i in 1:7){
    
    lambda[i] ~ dgamma(alpha,beta)
    
  }
  
  alpha ~ dgamma(1,1)
  beta ~ dgamma(1,1)
  
}"




model <- jags.model(textConnection(model_string), 
                    data = list(year = year,threeMade = threeMade))

update(model, 10000, progress.bar="none"); # Burnin for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("lambda"), 
                     n.iter=100000)


draws <- as.matrix(samp)




season1 <- draws[,1]
season2 <- draws[,2]
season3 <- draws[,3]
season4 <- draws[,4]
season5 <- draws[,5]
season6 <- draws[,6]
season7 <- draws[,7]



mean(sample(season7) > sample(season6))
mean(sample(season7) > sample(season5))
mean(sample(season7) > sample(season4))
mean(sample(season7) > sample(season3))
mean(sample(season7) > sample(season2))
mean(sample(season7) > sample(season1))



plot(density(season1),col = 2,lwd = 3,ylim = c(0,20),xlim=c(0,1),
     main = "Number of Threes Made by Season")

for(i in 2:7){
  
  lines(density(draws[,i]),col = i + 1,lwd = 3)
  
}

legend(.65,17.5,c("Season 1","Season 2","Season 3","Season 4","Season 5","Season 6","Season 7"),
       lty=c(1,1),lwd=c(2.5,2.5),col=c(2,3,4,5,6,7,8))

# prob <- c(mean(season1),mean(season2),mean(season3),mean(season4),mean(season5),mean(season6),mean(season7))
# year <- 1:7
# 
# 
# plot(prob ~ year)
# 
# model <- lm(prob ~ year)
# summary(model)
# 
# abline(model)
# 
# tapply(threeAttempted,year,table)
#
# 
# setwd("C:/Users/James/Documents/My R Research/Blake G/")
# 
# blake <- blake[!is.na(threeMade),]
# 
# write.csv(blake,"blake.csv")



# For Attempted

model_string <- "model{
  
for(i in 1:466){

threeAttempted[i] ~ dpois(lambda[year[i]])

}

for(i in 1:7){

lambda[i] ~ dgamma(alpha,beta)

}

alpha ~ dgamma(1,1)
beta ~ dgamma(1,1)

}"




model <- jags.model(textConnection(model_string), 
                    data = list(year = year,threeAttempted = threeAttempted))

update(model, 10000, progress.bar="none"); # Burnin for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("lambda"), 
                     n.iter=100000)


draws <- as.matrix(samp)




season1 <- draws[,1]
season2 <- draws[,2]
season3 <- draws[,3]
season4 <- draws[,4]
season5 <- draws[,5]
season6 <- draws[,6]
season7 <- draws[,7]



mean(sample(season7) > sample(season6))
mean(sample(season7) > sample(season5))
mean(sample(season7) > sample(season4))
mean(sample(season7) > sample(season3))
mean(sample(season7) > sample(season2))
mean(sample(season7) > sample(season1))



plot(density(season1),col = 2,lwd = 3,ylim = c(0,8),xlim=c(0,2.5),
     main = "Number of Threes Taken by Season")

for(i in 2:7){
  
  lines(density(draws[,i]),col = i + 1,lwd = 3)
  
}

legend(1.7,7.5,c("Season 1","Season 2","Season 3","Season 4","Season 5","Season 6","Season 7"),
       lty=c(1,1),lwd=c(2.5,2.5),col=c(2,3,4,5,6,7,8))

