library(extraDistr)   #For picking random number
library(mgcv)   #For loading model

horseList <- read.csv('gtaHorseNamesAndOdds.csv')



v1 <- subset(horseList,1 <= Odds & Odds <=  5, select = c(Odds))
v2 <- subset(horseList,6 <= Odds & Odds <=  15, select = c(Odds))
v3 <- subset(horseList,16 <= Odds & Odds <=  30, select = c(Odds))

n1 <- nrow(v1)
n2 <- nrow(v2)
n3 <- nrow(v3)

totalNumberHorseCombos <- n1*(n1-1)*(n2*(n2-1))*(n3*(n3-1))


getSample <- function(v1,v2,v3){
  S <- c()
  S[1:2] <- sample(v1$Odds,2,replace = FALSE)
  S[3:4] <- sample(v2$Odds,2,replace = FALSE)
  S[5:6] <- sample(v3$Odds,2,replace = FALSE)
  return(S)
}


runAlphaSim <- function(trials, startCash){
  model <- readRDS("mymodel.rds")
  data <- read.csv('validateData.csv', header = TRUE)
    
  changeA1 <- c()      #low bet, best horse
  changeA2 <- c()      #high bet, best horse
  changeA3 <- c()      #med bet, random horse
  changeA4 <- c()      #random amount, random horse
  changeA5 <- c()      #random amount, best horse
  changeA6 <- c()      #Predicted Success, if yes then high, if no then low
  changeA7 <- c()      #Predict Prob of success, use kelly criterion
  
  
  
  for(i in 1:nrow(data)){
    if(i==1){
      changeA1[i] <- startCash      
      changeA2[i] <- startCash      
      changeA3[i] <- startCash      
      changeA4[i] <- startCash      
      changeA5[i] <- startCash      
      changeA6[i] <- startCash      
      changeA7[i] <- startCash    
    }
    else{
      
      #Place trivial Bets
      lowBet = 100
      medBet = 5000
      highBet = 10000
      randomBet <- rdunif(1,100,10000)
      
      #Generate Horses
      S <- getSample(v1,v2,v3)   #Get sample from horse list
      
      
      bestBetIndex <- which.min(S)   #returns bet index
      bestOdds <- min(S)
      randomHorse_Index <- sample(1:6, 1)   #returns random horse index 
      
      P <- (S+1)^-1  #transform S from odds to probabilities 
      winner_index <- sample(1:6, 1,prob=P)    #returns a winner index
      
      
      tem <-sort(S, decreasing = FALSE)
      NextBest <- tem[2]
      
      ratio1 <- (NextBest+1)/(min(S)+1)   #min/min(2)  
      ratio2 <- (tem[4] +1)/(tem[3]+1)
      ratio3 <- (tem[6] +1)/(tem[5]+1)
      ratioStat <- ratio1*ratio2*ratio3
      
      
      
      
      #alphas 3 and 4 run independent of the others
      if(winner_index == randomHorse_Index){ 
        changeA3[i] <- medBet*S[randomHorse_Index]
        changeA4[i] <- randomBet*S[randomHorse_Index]
      } else {
        changeA3[i] <- -1*medBet
        changeA4[i] <- -1*randomBet
      }
      
      #Get Pre trained model
      newData1 <- data.frame(ratioStat=ratioStat)
      prob <- predict(model, newdata = newData1, type = "response")
      
      kelly <- (prob + (prob-1)/bestOdds)
      
      recBet <- abs(sum(changeA7)*kelly)
      #recBet <- 10000*kelly           #Note we are treating 10000 as the bankroll and not the total cash available
      
      betAmountA7 <- min(c(recBet,10000))  #recommended betting amount based on kelly criterion
      
      #Set the bet amount for alpha 6
      if(round(prob)==1){
        betAmountA6 = highBet
      }else betAmountA6 = lowBet
      
      
      
      
      if(winner_index == bestBetIndex){ #checks if winner was horse bet on
        changeA1[i] <- lowBet*bestOdds
        changeA2[i] <- highBet*bestOdds
        changeA5[i] <- randomBet*bestOdds
        changeA6[i] <- betAmountA6*bestOdds
        changeA7[i] <- betAmountA7*bestOdds
      } else {
        changeA1[i] <- -1*lowBet
        changeA2[i] <- -1*highBet
        changeA5[i] <- -1*randomBet
        changeA6[i] <- -1*betAmountA6
        changeA7[i] <- -1*betAmountA7
      }
      
      
    } #closes else 
  }#closes loop
  return(data.frame(changeA1,changeA2,changeA3,changeA4,changeA5,changeA6,changeA7))
}#closes function



monteCarlo <- function(games,trials){
  result <- runAlphaSim(games,50000)
  
  for( i in 1:trials){
    if( i %% 20 == 0 ){print(i)}
    result <- result + runAlphaSim(games,50000)
  }
  return(result/trials)
}

trials <- 1000
games <- 20

resultData <- monteCarlo(games,trials)

write.csv(resultData, "alphaSimMCResult.csv")





library(ggplot2)
data <- read.csv('alphaSimMCResult.csv', header = TRUE)

data <- data[,-1]  #eliminate index column


data <- data  %>% mutate(GrandSum = cumsum(.))

Totals <- data[,-c(1:7)]


P <- ggplot(Totals, aes(x=1:nrow(Totals))) + 
  geom_line(aes(y = Totals[,1]), col = 'orange') +
  geom_line(aes(y = Totals[,2]), col = 'green') +
  geom_line(aes(y = Totals[,3])) +
  geom_line(aes(y = Totals[,4])) +
  geom_line(aes(y = Totals[,5]), col = 'purple') +
  geom_line(aes(y = Totals[,6])) +
  geom_line(aes(y = Totals[,7]), col = 'blue') 


P


data2 <- data[-1,]
data2$X <- 1:152



Totals[games,]
plot(data2$X,data2$changeA1)
plot(data2$X,data2$changeA2)
plot(data2$X,data2$changeA3)
plot(data2$X,data2$changeA4)
plot(data2$X,data2$changeA5)
plot(data2$X,data2$changeA6)
plot(data2$X,data2$changeA7)


mean(data2$changeA1)     #Expected change is 20.68744 per game
mean(data2$changeA2)     #Expected change is 2068.744 per game
mean(data2$changeA3)     #Expected change is 988.9698 per game
mean(data2$changeA4)     #Expected change is 1044.106 per game
mean(data2$changeA5)     #Expected change is 1035.564 per game
mean(data2$changeA6)     #Expected change is 62.37688 per game
mean(data2$changeA7)     #Expected change is 1600.358 per game

var(data2$changeA1)     #mean Var change is 21.87161 per 200 games
var(data2$changeA2)     #mean Var change is 218,716.1 per 200 games
var(data2$changeA3)     #mean Var change is 372,081.1 per 200 games
var(data2$changeA4)     #mean Var change is 469,880.6 per 200 games
var(data2$changeA5)     #mean Var change is 74,287.34 per 200 games
var(data2$changeA6)     #mean Var change is 6706.787 per 200 games
var(data2$changeA7)     #mean Var change is 161,562.8 per 200 games






lmA1 <- lm(changeA1~X, data=data)
summary(lmA1)

lmA2 <- lm(changeA2~X, data=data)
summary(lmA2)

lmA3 <- lm(changeA3~X, data=data)
summary(lmA3)

lmA4 <- lm(changeA4~X, data=data)
summary(lmA4)

lmA5 <- lm(changeA5~X, data=data)
summary(lmA5)

lmA6 <- lm(changeA6~X, data=data)
summary(lmA6)

lmA7 <- lm(changeA7~X, data=data)
summary(lmA7)


plot(data$X,data$changeA2)



# Expand the scale of the upper and lower values so that the difference
# is visible in the plot

test <- cbind(rep("A", 100), rnorm(100, 0, 1))
test <- rbind(data, cbind(rep("B", 100), rnorm(100, 5, 1)))
test <- rbind(data, cbind(rep("C", 100), rnorm(100, 10, 1)))
test <- rbind(data, cbind(rep("D", 100), rnorm(100, 15, 1)))
test <- cbind(rep(1:100, 4), data)
test <- data.frame(data)

#################
library(dplyr)
data2 <- read.csv('alphaSimMCResult.csv', header = TRUE)

data2 <- data2[-1,]
#data2$X <- 1:19
data2 <- data2  %>% mutate(GrandSum = cumsum(.))

data2 <- data2[,-c(1:7)]


data2 <- data2[,-1]  #eliminate index column




data2 <- data.frame(Cumalative_A1=cumsum(data2$changeA1),Cumalative_A2=cumsum(data2$changeA2),Cumalative_A3=cumsum(data2$changeA3),
           Cumalative_A4=cumsum(data2$changeA4),Cumalative_A5=cumsum(data2$changeA5),Cumalative_A6=cumsum(data2$changeA6),
           Cumalative_A7=cumsum(data2$changeA7))



L <- length(data2$Cumalative_A1)

bundle <- c()
bundle_tags <- c()

for (topic in colnames(data2)){
  bundle <- append(bundle,data2[,topic])
  bundle_tags <- append(bundle_tags,rep.int(topic, L))
}

test <- data.frame(Game = rep.int(1:L,7),Change = bundle, 
                   Topic = bundle_tags) #bundle for ggplot 

ggplot(test,aes(Game,Change,col=Topic))+geom_line()


#################
var(data2$changeA1)     #mean Var change is 21.87161 per 200 games
var(data2$changeA2)     #mean Var change is 218,716.1 per 200 games
var(data2$changeA3)     #mean Var change is 372,081.1 per 200 games
var(data2$changeA4)     #mean Var change is 469,880.6 per 200 games
var(data2$changeA5)     #mean Var change is 74,287.34 per 200 games
var(data2$changeA6)     #mean Var change is 6706.787 per 200 games
var(data2$changeA7)     #mean Var change is 161,562.8 per 200 games



d <- test

varList <- c()
for(i in colnames(data2)){
  varList[i] <- var(d$Change[which(d$Topic==i)])
}


#Note here replace 10 with the confidence interval
d$upper <- d$Change + 10

d$lower <- d$Change - 10

# Order data by category and num
d = d[order(d$Topic, d$Game),]

# Create LOESS predictions for the values of upper and lower 
# and add them to the data frame. I'm sure there's a better way to do this,
# but my attempts with dplyr and tapply both failed, so I've resorted to the clunky 
# method below.
d$upperLoess = unlist(lapply(colnames(data2), 
                                function(x) predict(loess(d$upper[d$Topic==x] ~ 
                                                            d$Game[d$Topic==x]))))
d$lowerLoess = unlist(lapply(colnames(data2), 
                                function(x) predict(loess(d$lower[d$Topic==x] ~ 
                                                            d$Game[d$Topic==x]))))

# Use geom_ribbon to add a prediction band bounded by the LOESS predictions for 
# upper and lower
ggplot(d, aes(Game, Change, colour=Topic, fill=Topic)) +
  geom_smooth(method="loess", se=FALSE) +
  geom_ribbon(aes(x=Game, y=Change, ymax=upperLoess, ymin=lowerLoess), 
              alpha=0.2)



data <- cbind(rep("A", 100), rnorm(100, 0, 1))
data <- rbind(data, cbind(rep("B", 100), rnorm(100, 5, 1)))
data <- rbind(data, cbind(rep("C", 100), rnorm(100, 10, 1)))
data <- rbind(data, cbind(rep("D", 100), rnorm(100, 15, 1)))
data <- cbind(rep(1:100, 4), data)
data <- data.frame(data)
names(data) <- c("num", "category", "value")
