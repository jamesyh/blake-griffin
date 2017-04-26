# Has he started making more threes?

blake <- read.csv("C:/Users/James/Documents/My R Research/Blake G/blake griffin game.txt")

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

threeAttempted <- threeMade

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



plot(density(season1),col = 2,lwd = 3,ylim = c(0,15),xlim=c(0,1))

for(i in 2:7){
  
  lines(density(draws[,i]),col = i + 1,lwd = 3)
  
}



for(i in 1:7){
  hist(draws[,i])
}





tapply(threeAttempted,year,table)

# 
# setwd("C:/Users/James/Documents/My R Research/Blake G/")
# 
# blake <- blake[!is.na(threeMade),]
# 
# write.csv(blake,"blake.csv")
