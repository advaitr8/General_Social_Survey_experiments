rm(list = ls())
setwd("/Users/Advait/Desktop/New_School/Spring17/GSS_experiments/race_income")
getwd()
#Load packages
library(foreign)
library(dplyr)

#data <- read.dta("GSS7216_R1a.dta")
#df_race_income <- data.frame(data$race, data$income)
#df_race_income <- df_race_income[complete.cases(df_race_income),]

#str(df_race_income)
#levels(df_race_income$data.race)
#levels(df_race_income$data.income)
#writing to csv
#write.table(df_race_income, file = "race_income.csv", sep = ",")
# get the data
test <- read.csv("race_income.csv", header = T,sep = ",", row.names = 1)
test[50:70,]
#######
#Reorder factor levels to match original data
#######
print(levels(test$data.race))
print(levels(test$data.income))
#reordering here
test$data.income <- factor(test$data.income,
                           levels(test$data.income)[c(12,1,6:11,2:5)])
print(levels(test$data.income))
df_race_income <- test

#total whites = 44109
dim(df_race_income[df_race_income$data.race == "white",])

#total blacks = 7497
dim(df_race_income[df_race_income$data.race == "black",])



#Part 1
#filter white people
#calculate proportion of white earners of each bracket
df_white <- df_race_income %>%
  filter(data.race == "white") %>%
   group_by(data.income) %>%
  summarize(prop_white = (n()/44109))

#Part 2
#filter black people
#calculate proportion of black earners of each bracket
df_black <- df_race_income %>%
  filter(data.race == "black") %>%
  group_by(data.income) %>%
  summarize(prop_black = (n()/7497))

#incomebracket vector to aid plots
income <- c("<1k","1k - 2.9k","3k - 3.9k",
            "4k - 4.9k", "5k - 5.9k", "6k - 6.9k",
            "7k - 7.9k", "8k - 9.9k", "10k - 14.9k",
            "15k - 19.9k", "20k - 24.9k",">=25k")

#creating dataframe for plots
df_plot_raceincome <- data.frame(inc_brack = df_black$data.income,
                                 prop_white = df_white$prop_white,
                                 prop_black = df_black$prop_black)

#Plot 1, plot white and black separately
par(mar = c(3,4.3,5,2.2) + 0.1,
    col = "gray",
    mfcol = c(1,2))

#White people Plots
plot(df_plot_raceincome$prop_white,1:12,
     xlim = c(0,0.6),
     pch = 16,
     cex = 0.7,
     yaxt = "n",  
     ylab = "",
     xlab = "",
     col = "black",
     xaxt = "n",
     main = "Proportion of White Earners",
     cex.main = 0.7)
abline(h = c(1:12), lty = 2)
axis(2, 
     at = pretty(1:12,n = 12),
     labels = paste(income),
     las = 2, 
     lwd.tick = 0.5,
     cex.axis = 0.7, 
     col = "gray",
     tick = T)
axis(1, 
     at = (seq(0,0.6,length.out = 9)),
     labels = (seq(0,0.6,length.out = 9)), 
     col.ticks = "gray", 
     lwd.tick = 0.5, 
     cex.axis = 0.5,
     col = "gray")
text(x = df_plot_raceincome$prop_white,
     y = 1:12,
     labels = paste(round(df_plot_raceincome$prop_white, 3)),
     pos = 3,
     cex = 0.5,
     col = "black")

#####


#Black people Plots
plot(df_plot_raceincome$prop_black,1:12,
     xlim = c(0,0.6),
     pch = 16,
     cex = 0.7,
     yaxt = "n",  
     ylab = "",
     xlab = "",
     col = "black",
     xaxt = "n",
     main = "Proportion of Black Earners",
     cex.main = 0.7)
abline(h = c(1:12), lty = 2)
axis(2, 
     at = c(1:12),
     labels = paste(income),
     las = 2, 
     lwd.tick = 0.5,
     cex.axis = 0.7, 
     col = "gray")
axis(1, 
     at = (seq(0,0.6,length.out = 9)),
     labels = (seq(0,0.6,length.out = 9)), 
     col.ticks = "gray", 
     lwd.tick = 0.5, 
     cex.axis = 0.5,
     col = "gray")
text(x = df_plot_raceincome$prop_black,
     y = 1:12,
     labels = paste(round(df_plot_raceincome$prop_black, 3)),
     pos = 3,
     cex = 0.5,
     col = "black")
dev.off()
#Plot 2
#Plot white and black on the same plot
par(mar = c(2.5,4.5,2.5,3),
    col = "gray")
plot(df_plot_raceincome$prop_white,1:12,
     xlim = c(0,0.6),
     pch = 1,
     cex = 1,
     yaxt = "n",  
     ylab = "",
     xlab = "",
     col = "black",
     xaxt = "n",
     main = "Black and White Earners",
     cex.main = 0.7)
abline(h = c(1:12), lty = 2)
axis(2, 
     at = pretty(1:12,n = 12),
     labels = paste(income),
     las = 2, 
     lwd.tick = 0.5,
     cex.axis = 0.7, 
     col = "gray",
     tick = T)
axis(1, 
     at = (seq(0,0.55,length.out = 11)),
     labels = (seq(0,0.55,length.out = 11)), 
     col.ticks = "gray", 
     lwd.tick = 0.5, 
     cex.axis = 0.5,
     col = "gray")
text(x = df_plot_raceincome$prop_white,
     y = 1:12,
     labels = paste(round(df_plot_raceincome$prop_white, 3)),
     pos = 3,
     cex = 0.5,
     col = "black")
points(df_plot_raceincome$prop_black,1:12,
       pch = 16,
       cex = 1,
       col = "black")
text(x = df_plot_raceincome$prop_black,
     y = 1:12,
     labels = paste(round(df_plot_raceincome$prop_black, 3)),
     pos = 1,
     cex = 0.5,
     col = "black")
legend('bottomright', 
       legend = c("Black", "White"), 
       col = "black",
       pch = c(16,1),
       cex = 0.7,
       bty = 'n',
       text.col = "black")

