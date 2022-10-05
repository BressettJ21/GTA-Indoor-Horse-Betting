library(extraDistr)   #For picking random number
library(mgcv)   #For loading model


horseList <- read.csv('gtaHorseNamesAndOdds.csv')



v1 <- subset(horseList,1 <= Odds & Odds <=  5, select = c(Odds))
v2 <- subset(horseList,6 <= Odds & Odds <=  15, select = c(Odds))
v3 <- subset(horseList,16 <= Odds & Odds <=  30, select = c(Odds))

getSample <- function(v1,v2,v3){
  S <- c()
  S[1:2] <- sample(v1$Odds,2,replace = FALSE)
  S[3:4] <- sample(v2$Odds,2,replace = FALSE)
  S[5:6] <- sample(v3$Odds,2,replace = FALSE)
  return(S)
}



#Initializations 

trials <- 10000
games <- 200

m_cumSum_A1 <- matrix(nrow = trials,ncol = games)
m_cumSum_A2 <- matrix(nrow = trials,ncol = games)
m_cumSum_A3 <- matrix(nrow = trials,ncol = games)
m_cumSum_A4 <- matrix(nrow = trials,ncol = games)
m_cumSum_A5 <- matrix(nrow = trials,ncol = games)
m_cumSum_A6 <- matrix(nrow = trials,ncol = games)
m_cumSum_A7 <- matrix(nrow = trials,ncol = games)

var_m <- matrix(nrow = games, ncol = 7)



startCash <- 50000



model <- readRDS("mymodel.rds")

for( j in 1:trials){
    
  
  changeA1 <- c()      #low bet, best horse
  changeA2 <- c()      #high bet, best horse
  changeA3 <- c()      #med bet, random horse
  changeA4 <- c()      #random amount, random horse
  changeA5 <- c()      #random amount, best horse
  changeA6 <- c()      #Predicted Success, if yes then high, if no then low
  changeA7 <- c()      #Predict Prob of success, use kelly criterion
  
  
  
  for(i in 1:games){
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
      
      ratio1 <- (tem[2]+1)/(min(S)+1)   #min/min(2)  
      ratio2 <- (tem[4] +1)/(tem[3]+1)
      ratio3 <- (tem[6] +1)/(tem[5]+1)
      ratioStat <- ratio1*ratio2*ratio3
      
      #Get Pre trained model
      newData1 <- data.frame(ratioStat=ratioStat)
      prob <- predict(model, newdata = newData1, type = "response")
      
      kelly <- (prob + (prob-1)/bestOdds)
      
      recBet <- abs(sum(changeA7)*kelly)
      betAmountA7 <- min(c(recBet,10000))  #recommended betting amount based on kelly criterion
      
      #Set the bet amount for alpha 6
      if(round(prob)==1){
        betAmountA6 = highBet
      }else betAmountA6 = lowBet
      
      
      #alphas 3 and 4 run independent of the others
      if(winner_index == randomHorse_Index){ 
        changeA3[i] <- medBet*S[randomHorse_Index]
        changeA4[i] <- randomBet*S[randomHorse_Index]
      } else {
        changeA3[i] <- -1*medBet
        changeA4[i] <- -1*randomBet
      }
      
      
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
      
    } #closes first else 
    m_cumSum_A1[j,i] <- cumsum(changeA1)[length(changeA1)]
    m_cumSum_A2[j,i] <- cumsum(changeA2)[length(changeA2)]
    m_cumSum_A3[j,i] <- cumsum(changeA3)[length(changeA3)]
    m_cumSum_A4[j,i] <- cumsum(changeA4)[length(changeA4)]
    m_cumSum_A5[j,i] <- cumsum(changeA5)[length(changeA5)]
    m_cumSum_A6[j,i] <- cumsum(changeA6)[length(changeA6)]
    m_cumSum_A7[j,i] <- cumsum(changeA7)[length(changeA7)]
    if(j==1){
      result <- data.frame(changeA1,changeA2,changeA3,changeA4,changeA5,changeA6,changeA7)
    }#end if
  }#closes game loop
  result <- result + data.frame(changeA1,changeA2,changeA3,changeA4,changeA5,changeA6,changeA7)
  if(j%%50==0){
    print(j)
  }
}#Closes carlo loop

result <- result/(trials+1) #scale to get mean by game and alpha
result <- result[-1,]

data <- result  %>% mutate(GrandSum = cumsum(.))

result <- data[,-c(1:7)]

write.csv(result,'monteCarloData.csv')


for(i in 1:games){
  var_m[i,1] <- var(m_cumSum_A1[,i])
  var_m[i,2] <- var(m_cumSum_A2[,i])
  var_m[i,3] <- var(m_cumSum_A3[,i])
  var_m[i,4] <- var(m_cumSum_A4[,i])
  var_m[i,5] <- var(m_cumSum_A5[,i])
  var_m[i,6] <- var(m_cumSum_A6[,i])
  var_m[i,7] <- var(m_cumSum_A7[,i])
}

var_m #This tells variance of cumulative earnings for each alpha at every game

st_dev <- sqrt(var_m)


clean_st_dev <- st_dev[-1,]


upperBounds <- result + 1.96*clean_st_dev/sqrt(trials) 


lowerBounds <- result - 1.96*clean_st_dev/sqrt(trials) 



L <- length(result$changeA1)

bund <- function(L,data){
  bundle <- c()
  bundle_tags <- c()
  
  for (topic in colnames(data)){
    bundle <- append(bundle,data[,topic])
    bundle_tags <- append(bundle_tags,rep.int(topic, L))
  }
  
  
  test <- data.frame(Game = rep.int(1:L,7),Change = bundle, 
                     Topic = bundle_tags) #bundle for ggplot 
  return(test)
}#end function

bundled_means <- bund(L,result)
bundled_Upper <- bund(L,upperBounds)
bundled_Lower <- bund(L,lowerBounds)

bundled_means$Upper <- bundled_Upper$Change
bundled_means$Lower <- bundled_Lower$Change


ggplot(bundled_means, aes(Game, Change, colour=Topic, fill=Topic)) +
  geom_smooth(method="loess", se=FALSE) +
  geom_ribbon(aes(x=Game, y=Change, ymax=Upper, ymin=Lower), 
              alpha=0.1)


