## Loading the data
baseball <- read.csv("baseball.csv", header = T)

## View the structure of the data
str(baseball)

## View the summary of the data
summary(baseball)

## Visualizing the variables having continuous values
qplot(baseball$RS, geom = "histogram")
qplot(baseball$RA, geom = "histogram")
qplot(baseball$W, geom = "histogram")
qplot(baseball$OBP, geom = "histogram")
qplot(baseball$SLG, geom = "histogram")
qplot(baseball$BA, geom = "histogram")
qplot(baseball$OOBP, geom = "histogram")
qplot(baseball$OSLG, geom = "histogram")

## Changing the modes of some variables from integer to factor 
baseball$Playoffs <- as.factor(baseball$Playoffs)
baseball$RankSeason <- as.factor(baseball$RankSeason)
baseball$RankPlayoffs <- as.factor(baseball$RankPlayoffs)

## Subsetting the data between 1996 and 2001
data <- subset(baseball, Year > 1995 & Year < 2002)

## Looking at the number of wins by playoffs
table(data$Playoffs, data$W)

## Loading the package ggplot2
library(ggplot2)

## Visualizing the number of wins required to make it to the playoffs
ggplot(data, aes(W, Team, color = factor(Playoffs))) + geom_point(shape = 16, size = 3) +
  theme(legend.position = c(0.90, 0.90), 
        legend.background = element_rect(colour = "black"), 
        panel.background = element_rect(fill = "white")) +
  labs(x = "Number of Wins",  y = "Team") +
  scale_colour_manual(values = c("#E8E8E8", "#5B5B5B"),
                                  name = "Playoffs",
                                  labels = c("0", "1")) + 
                                  scale_x_continuous(breaks = c(60,65,70,75,80,85,90,95,100,105))

## Subsetting data before 2002
moneyball <- subset(baseball, Year < 2002)

## Taking a look at the structure of moneyball
str(moneyball)

## Creating a new variable Run Difference 
moneyball$RD <- moneyball$RS - moneyball$RA

## Taking a look at the structure of moneyball again
str(moneyball)

## Checking the relationship between Run Difference and Wins
cor(moneyball$RD, moneyball$W)

## Visualizing the relationship between Run Difference and Wins
plot(moneyball$RD, moneyball$W, xlab = "Run Difference", ylab = "No. of Wins")

## Building a linear regression model for Wins
WinsReg <- lm(W ~ RD, data = moneyball)

## Viewing the summary of the model
summary(WinsReg)

## Visualizing the model
plot(WinsReg)

## Taking a look at the structure of moneyball
str(moneyball)

## Building a linear regression equation for Runs scored
RunsReg <- lm(RS ~ OBP + SLG + BA, data = moneyball)

## Looking at the summary of the model
summary(RunsReg)

## Building a linear regression equation for Runs scored by removing batting average
RunsReg <- lm(RS ~ OBP + SLG, data = moneyball)

## Looking at the summary of the model
summary(RunsReg)

## Model by removing On-base percentage
RunsReg <- lm(RS ~ SLG + BA, data = moneyball)

## Looking at the summary of the model
summary(RunsReg)

## Model by removing Slugging percentage
RunsReg <- lm(RS ~ OBP + BA, data = moneyball)

## Looking at the summary of the model
summary(RunsReg)

## Model for Runs allowed
RunsAllowedReg <- lm(RA ~ OOBP + OSLG, data = moneyball)

## Looking at the summary of the model
summary(RunsAllowedReg)


