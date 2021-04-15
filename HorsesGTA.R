HorsesGTA2 <- read.csv("HorsesUpdated.csv")

data <- HorsesGTA2
data <- subset(data, select = -c(X) )

hist(data$BookPercent)

hist(data$Fstat)

Fs <- data$Fstat
FLosses <- data$Fstat[data$WinLoss == 0]
FWins <- data$Fstat[data$WinLoss == 1]
hist(FLosses)
hist(FWins)
hist(data$Change)
ks.test(FLosses,FWins)  #If p-value small then data from different distributions


#Check pairings
for (n in ns){
  hist(data$Change[data$BestHorse ==n & data$RatioLeaders == 1])
  print(n)
  print(sum(data$Change[data$BestHorse == n & data$RatioLeaders == 1], na.rm = TRUE))
}

ns <- c(1,2,3,4,5)
for (n in ns){
  print(n)
  print(mean(data$WinLoss[data$BestHorse == n]))
  boxplot(data$change[data$BestHorse == n])
}


boxplot(Change~BestHorse,
        data=data,
        main="Change Distribution by Horse",
        xlab="Best Horse",
        ylab="change ($)",
        col="steelblue",
        border="black"
)


mean(Fs, na.rm = TRUE)
poissonValues <- rpois(201,60.95)
ks.test(poissonValues,data$Fstat)

B6 <- .10
B5 <- .15
B4 <- .20
B3 <- .50
B2 <- .60
B1 <- .70
B0 <- .88
Bs <- c(B0, B1, B2, B3, B4, B5, B6)

quantile(data$Fstat, Bs, na.rm = TRUE)


calcBookPercent <- function(odds){
  percents <- c()
  for (odd in odds){
  percents <- append(percents, 100/(odd + 1), after = length(percents))
  }
  return(sum(percents))
}

OddsReport <- function(odds){
  bet <- 0
  BookPercent <- calcBookPercent(odds)
  BestHorse <- min(odds)
  NextBest <- sort(odds, decreasing = FALSE)[2]
  
  
  ratio <- (100/(BestHorse+1))/(100/(NextBest+1))
  #Create scale to scale down bet in difficult scenerio
  scale = 1
  if (ratio == 1){
    scale = .5
  }
  
  Fstat <- BookPercent/ratio
  print(Fstat)
  
  position <- ecdf(data$Fstat)(Fstat)
  print(position)
  print(quantile(data$Fstat, Bs, na.rm = TRUE))
  if (position > B0){
    bet = 100
  } else {
    if(position > B1){
      bet = 1000
    } else {
      if(position > B2){
        bet = 2500
      } else {
        if(position > B3){
          bet = 4000
        } else {
          if(position > B4){
            bet = 5500
          } else {
            if(position > B5){
              bet = 7000
            } else {
              if(position > B6){
                bet = 8500
              } else {
                bet = 10000
              }
            }
          }
        }
      }
    }
  }
print(bet*scale)
}



addrow <- function(data, odds,BetHorse,winLoss,winningHorse,change){
  BookPercent <- calcBookPercent(odds)
  BestHorse <- min(odds)
  
  NextBest <- sort(odds, decreasing = FALSE)[2]
  
  ratio <- (100/(BestHorse+1))/(100/(NextBest+1))
  
  total <- data$Total[length(data$Total)] + change
  
  Fstat <- BookPercent/ratio
  #Append new row
  data[nrow(data) + 1,] <- c(BookPercent, BetHorse, BestHorse, NextBest, ratio, 
                             winLoss, winningHorse, change, total, Fstat)
  write.csv(data,'HorsesUpdated.csv')
  print("updated")
  print(data$Total[length(data$Total)])
  print(paste0("Net Gain: ", sum(data$Change)))
  return(data)
}

plot(data$Change,Fs, pch = as.integer(data$WinLoss))
plot(data$BetHorse, Fs, pch = as.integer(data$WinLoss))

#Remove Row if needed
data <- data[-nrow(data),]


#Enter Odds Here:
odds <- c(20,15,3,25,9,2)  #Be sure to press ctrl + enter
OddsReport(odds)  #Run report with ctrl + enter



#Input for add row: (data, odds, BetHorse, winLoss, winningHorse, change)
data <- addrow(data, odds, 1, 1, 1, 10000)

