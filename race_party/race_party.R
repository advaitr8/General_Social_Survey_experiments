#rm(list = ls())
getwd()
setwd("/Users/Advait/Desktop/New_School/Spring17/GSS_experiments/General_Social_Survey_experiments/race_party_exp")


#Load packages
library(foreign)
library(dplyr)

#Import csv which was written inorder to avoid importing the giant GSS dataset every time
test <- read.csv("race_party.csv", header = T,sep = ",", row.names = 1)

#Check the original order of levels is in alphabetic order
#this does not make sense for plots
print(levels(test$data.partyid))
test$data.partyid[50:70]
#Reorder factor levels to match original data
test$data.partyid <- factor(test$data.partyid, 
                            levels(test$data.partyid)[c(7,4,1,3,2,5,8,6)])
print(levels(test$data.partyid))
test$data.partyid[50:70]
#factor levels fixed.

#create the dataframe that I'm going to be working with
df_race_party <- test

#Part 1
#filter white people
#calculate proportion of white voters of each type
df_white <- df_race_party %>%
  filter(data.race == "white") %>%
  group_by(data.partyid) %>%
  summarize(prop_white = (n()/50085))

#Part 2
#filter black people
#calculate proportion of white voters of each type
df_black <- df_race_party %>%
  filter(data.race == "black") %>%
  group_by(data.partyid) %>%
  summarize(prop_black = (n()/8711))

df_plot_raceparty <- data.frame(party = df_black$data.partyid,
                                prop_white = df_white$prop_white,
                                lower_white = df_white$prop_white - 
                                  (2*sqrt((df_white$prop_white)*(1 - df_white$prop_white)))/50085,
                                upper_white = df_white$prop_white + 
                                  (2*sqrt((df_white$prop_white)*(1 - df_white$prop_white)))/50085,
                                prop_black = df_black$prop_black,
                                lower_black = df_black$prop_black - 
                                  (2*sqrt((df_black$prop_black)*(1 - df_black$prop_black)))/8711,
                                upper_black = df_black$prop_black + 
                                  (2*sqrt((df_black$prop_black)*(1 - df_black$prop_black)))/8711)
##
####
#df_plot_raceparty has all the information needed for the plots in one place

#Create vector to aid plots
party <- c("StrongDem", "WeakDem", "Indep/Dem", 
           "Independent",
           "Indep/Rep","WeakRep","StrongRep", "Other")

#Plot 1, plot white and black separately
par(mar = c(3,4.3,5,2.2) + 0.1,
    col = "gray",
    mfcol = c(1,2))

#White people Plots
plot(df_plot_raceparty$prop_white,1:8,
     xlim = c(0,0.4),
     pch = 16,
     cex = 0.7,
     yaxt = "n",  
     ylab = "",
     xlab = "",
     col = "black",
     xaxt = "n",
     main = "Proportion of White Voters",
     cex.main = 0.7)
grid(nx = NA, ny = NULL)
axis(2, 
     at = c(1:8),
     labels = paste(party),
     las = 2, 
     lwd.tick = 0.5,
     cex.axis = 0.7, 
     col = "gray")
axis(1, 
     at = (seq(0,0.4,length.out = 9)),
     labels = (seq(0,0.4,length.out = 9)), 
     col.ticks = "gray", 
     lwd.tick = 0.5, 
     cex.axis = 0.5,
     col = "gray")
text(x = df_plot_raceparty$prop_white,
     y = 1:8,
     labels = paste(round(df_plot_raceparty$prop_white, 3)),
     pos = 3,
     cex = 0.5,
     col = "black")

#####
######
######
#Black people plots
plot(df_plot_raceparty$prop_black,1:8,
     xlim = c(0,0.4),
     pch = 16,
     cex = 0.7,
     yaxt = "n",  
     ylab = "",
     xlab = "",
     col = "black",
     xaxt = "n",
     main = "Proportion of Black Voters",
     cex.main = 0.7)
grid(nx = NA, ny = NULL)
axis(2, 
     at = c(1:8),
     labels = paste(party),
     las = 2, 
     lwd.tick = 0.5,
     cex.axis = 0.7, 
     col = "gray")
axis(1, 
     at = (seq(0,0.4,length.out = 9)),
     labels = (seq(0,0.4,length.out = 9)), 
     col.ticks = "gray", 
     lwd.tick = 0.5, 
     cex.axis = 0.5,
     col = "gray")
text(x = df_plot_raceparty$prop_black,
     y = 1:8,
     labels = paste(round(df_plot_raceparty$prop_black, 3)),
     pos = 3,
     cex = 0.5,
     col = "black")

dev.off()

#Plot 2
#Plot white and black on the same plot
par(mar = c(2.5,4.5,2.5,3),
    col = "gray")
plot(df_plot_raceparty$prop_white,1:8,
     xlim = c(0,0.4),
     pch = 1,
     cex = 1,
     yaxt = "n",  
     ylab = "",
     xlab = "",
     col = "black",
     xaxt = "n",
     main = "Blacks and Whites Political Affiliation",
     cex.main = 0.7)
grid(nx = NA, ny = NULL)
axis(2, 
     at = c(1:8),
     labels = paste(party),
     las = 2, 
     lwd.tick = 0.5,
     cex.axis = 0.7, 
     col = "gray")
axis(1, 
     at = (seq(0,0.4,length.out = 9)),
     labels = (seq(0,0.4,length.out = 9)), 
     col.ticks = "gray", 
     lwd.tick = 0.5, 
     cex.axis = 0.5,
     col = "gray")
text(x = df_plot_raceparty$prop_white,
     y = 1:8,
     labels = paste(round(df_plot_raceparty$prop_white, 3)),
     pos = 3,
     cex = 0.5,
     col = "black")
points(df_plot_raceparty$prop_black,1:8,
       pch = 16,
       cex = 1,
       col = "black")
text(x = df_plot_raceparty$prop_black,
     y = 1:8,
     labels = paste(round(df_plot_raceparty$prop_black, 3)),
     pos = 1,
     cex = 0.5,
     col = "black")
legend('topright', 
       legend = c("Black", "White"), 
       col = "black",
       pch = c(16,1),
       cex = 0.7,
       bty = 'n',
       text.col = "black")

