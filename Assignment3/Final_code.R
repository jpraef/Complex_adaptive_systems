
  
#This is a notebook to document my attempts to develop a zero determinant strategy in R, play it against other strategies, and examine the transfer entropy over time.
#Press & Dyson provide the mathematical underpinnings for this code, 
#and python script at https://github.com/CamDavidsonPilon/ipd/blob/master/ipd.py 
#has a simple script for ZD strategies. I extensively modified the above script to work in this environment

install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")
install.packages("rJava")
library(ggplot2)
library(dplyr)
library(reshape2)
library(rJava)

opposite_move <- function(move){
  if (move == "DEFECT"){
    return("COOPERATE")
  }
  if (move == "COOPERATE"){
    return("DEFECT")
  }
}

score <- function(my_move, opponents_move){
  if(my_move == "DEFECT" & opponents_move == "DEFECT" & !is.na(my_move == "DEFECT" & opponents_move == "DEFECT" )){
    return(1)}
  if(my_move == "COOPERATE" & opponents_move == "DEFECT" & !is.na(my_move == "COOPERATE" & opponents_move == "DEFECT")){
    return(0)}
  if(my_move == "DEFECT" & opponents_move == "COOPERATE" & !is.na(my_move == "DEFECT" & opponents_move == "COOPERATE" )){
    return(5)}
  if(my_move == "COOPERATE" & opponents_move == "COOPERATE" & !is.na(my_move == "COOPERATE" & opponents_move == "COOPERATE")){
    return(3)}
}




IPD <- function(p1, p2, n_matches = 10, extort_factor){
  
  zd <- data.frame("t" = 0:n_matches, "move1" = NA, "move2" = NA, "score1" = NA, "score2" = NA, "meanscore1" = NA, "meanscore2" = NA, "scoresum1" = NA,"scoresum2" = NA)
  zd[zd$t == 0, "score1"] <- 0
  zd[zd$t == 0, "score2"] <- 0
  zd[zd$t == 0, "move2"] <- "COOPERATE"
  zd[zd$t == 0, "move1"] <- "COOPERATE"
  
  for (i in 1:n_matches){
    ### Pavlov
    if (p1 == "Pavlov"){
      
      
      
      zd[zd$t == i, "move1"] <-ifelse(zd[zd$t == i - 1, "score1"] <= 1,
                                      opposite_move(zd[zd$t == i - 1, "move1"]),
                                      zd[zd$t == i - 1, "move1"])
      
      
      
    }
    if (p2 == "Pavlov"){
      
      
      
      zd[zd$t == i, "move2"] <-ifelse(zd[zd$t == i -1, "score2"] <= 1,
                                      opposite_move(zd[zd$t == i - 1, "move2"]),
                                      zd[zd$t == i - 1, "move2"])
      
    }
    
    
    ###TFT    
    if (p1 == "TFT"){
      
      zd[zd$t == 0, "move1"] <- "COOPERATE"
      
      zd[zd$t == i, "move1"] <-  zd[zd$t == i - 1, "move2"]
      
    }
    
    if (p2 == "TFT"){
      
      zd[zd$t == 0, "move2"] <- "COOPERATE"
      
      zd[zd$t == i, "move2"] <- zd[zd$t == i - 1, "move1"]
      
    }
    
    ####Random
    
    if (p2 == "Random"){
      
      zd[zd$t == 0, "move2"] <- sample(c("COOPERATE","DEFECT"), 1)
      
      zd[zd$t == i, "move2"] <- sample(c("COOPERATE","DEFECT"), 1)
      
    }
    
    if (p1 == "Random"){
      
      zd[zd$t == 0, "move1"] <- sample(c("COOPERATE","DEFECT"), 1)
      
      zd[zd$t == i, "move1"] <- sample(c("COOPERATE","DEFECT"), 1)
      
    }
    
    ####Always Defect
    if (p1 == "AD"){
      zd[zd$t == 0, "move1"] <- "DEFECT" 
      
      zd[zd$t == i, "move1"] <- "DEFECT"
      
    }
    
    if (p2 == "AD"){
      zd[zd$t == 0, "move2"] <- "DEFECT"
      
      zd[zd$t == i, "move2"] <- "DEFECT"
      
    }
    
    ####Always Cooperate
    if (p1 == "AC"){
      zd[zd$t == 0, "move1"] <- "COOPERATE" 
      
      zd[zd$t == i, "move1"] <- "COOPERATE"
      
      
    }
    if (p2 == "AC"){
      zd[zd$t == 0, "move2"] <- "COOPERATE"      
      
      zd[zd$t == i, "move2"] <- "COOPERATE"
      
      
    }
    
    
    #### Extortion
    if (p1 == "Extortion"){
      zd[zd$t == 0, "move1"] <- "COOPERATE"
      
      if(zd[zd$t == (i - 1) , "move1"] == "COOPERATE" & zd[zd$t == (i - 1) , "move2"] == "COOPERATE" & !is.na(zd[zd$t == i - 1 , "move1"] == "COOPERATE" & zd[zd$t == i - 1 , "move2"] == "COOPERATE")){
        zd[zd$t == i , "move1"] <- ifelse(runif(1) < (1 - (2 * extort_factor - 2.) / (4. * extort_factor + 1.)), "COOPERATE", "DEFECT")
      }
      
      if(zd[zd$t == (i - 1) , "move1"] == "DEFECT" & zd[zd$t == (i - 1) , "move2"] == "DEFECT" & !is.na(zd[zd$t == i - 1 , "move1"] == "DEFECT" & zd[zd$t == i - 1 , "move2"] == "DEFECT")){
        zd[zd$t == i , "move1"] <-  "DEFECT"
      }
      
      if(zd[zd$t == i - 1 , "move1"] == "COOPERATE" & zd[zd$t == i - 1 , "move2"] == "DEFECT" & !is.na(zd[zd$t == i - 1 , "move1"] == "COOPERATE" & zd[zd$t == i - 1 , "move2"] == "DEFECT")){
        zd[zd$t == i , "move1"] <-  "DEFECT"
      }
      
      if(zd[zd$t == i - 1 , "move1"] == "DEFECT" & zd[zd$t == i - 1 , "move2"] == "COOPERATE" & !is.na(zd[zd$t == i - 1 , "move1"] == "DEFECT" & zd[zd$t == i - 1 , "move2"] == "COOPERATE")){
        zd[zd$t == i , "move1"] <- ifelse(runif(1) < ((extort_factor + 4) / (4. * extort_factor + 1)), "COOPERATE", "DEFECT")
      }	
      
    }
    
    if (p2 == "Extortion"){   
      zd[zd$t == 0, "move2"] <- "COOPERATE"
      
      if(zd[zd$t == i - 1 , "move2"] == "COOPERATE" & zd[zd$t == i - 1 , "move1"] == "COOPERATE" & !is.na(zd[zd$t == i - 1 , "move2"] == "COOPERATE" & zd[zd$t == i - 1 , "move1"] == "COOPERATE")){
        zd[zd$t == i , "move2"] <- ifelse(runif(1) < (1 - (2 * extort_factor - 2.) / (4. * extort_factor + 1.)), "COOPERATE", "DEFECT")
      }
      
      if(zd[zd$t == i - 1 , "move2"] == "DEFECT" & zd[zd$t == i - 1 , "move1"] == "DEFECT" & !is.na(zd[zd$t == i - 1 , "move2"] == "DEFECT" & zd[zd$t == i - 1 , "move1"] == "DEFECT")){
        zd[zd$t == i , "move2"] <-  "DEFECT"
      }
      if(zd[zd$t == i - 1 , "move2"] == "COOPERATE" & zd[zd$t == i - 1 , "move1"] == "DEFECT" & !is.na(zd[zd$t == i - 1 , "move2"] == "COOPERATE" & zd[zd$t == i - 1 , "move1"] == "DEFECT")){
        zd[zd$t == i , "move2"] <-  "DEFECT"
      }
      
      if(zd[zd$t == i - 1 , "move2"] == "DEFECT" & zd[zd$t == i - 1 , "move1"] == "COOPERATE" & !is.na(zd[zd$t == i - 1 , "move2"] == "DEFECT" & zd[zd$t == i - 1 , "move1"] == "COOPERATE")){
        zd[zd$t == i , "move2"] <- ifelse(runif(1) < ((extort_factor + 4) / (4. * extort_factor + 1)), "COOPERATE", "DEFECT")
      }	
      
    }
    
    ##### go by majority, memory == 20
    
    if (p1 == "gobymajority"){
      zd[zd$t == 0, "move1"] <- "COOPERATE"      
      zd[zd$t == i, "move1"] <- ifelse(sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move2"] == "COOPERATE", na.rm = T) >= sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move2"] == "DEFECT", na.rm = T), "COOPERATE", "DEFECT")
    }
    
    if (p2 == "gobymajority"){
      zd[zd$t == 0, "move2"] <- "COOPERATE"      
      zd[zd$t == i, "move2"] <- ifelse(sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move1"] == "COOPERATE", na.rm = T) >= sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move1"] == "DEFECT", na.rm = T), "COOPERATE", "DEFECT")
    }
    
    #### my majority vs their majority
    if (p1 == "majorityvsmajority"){
      zd[zd$t == 0, "move1"] <- "COOPERATE"      
      zd[zd$t == i, "move1"] <- ifelse(sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move1"] == "COOPERATE", na.rm = T) > sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move2"] == "COOPERATE", na.rm = T), "DEFECT", "COOPERATE")
    }
    
    if (p2 == "majorityvsmajority"){
      zd[zd$t == 0, "move2"] <- "COOPERATE"      
      zd[zd$t == i, "move2"] <- ifelse(sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move2"] == "COOPERATE", na.rm = T) > sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move1"] == "COOPERATE", na.rm = T), "DEFECT", "COOPERATE")
    }
    
    #### my majority vs their majority, random
    if (p1 == "majorityvsmajorityrandom"){
      zd[zd$t == 0, "move1"] <- "COOPERATE"      
      zd[zd$t == i, "move1"] <- ifelse(sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move1"] == "COOPERATE", na.rm = T) > sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move2"] == "COOPERATE", na.rm = T), sample(c("DEFECT", "COOPERATE"), 1), "COOPERATE")
    }
    
    if (p2 == "majorityvsmajorityrandom"){
      zd[zd$t == 0, "move2"] <- "COOPERATE"      
      zd[zd$t == i, "move2"] <- ifelse(sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move2"] == "COOPERATE", na.rm = T) > sum(zd[c(ifelse(i-20 < 0, 0, (i-20)):i),"move1"] == "COOPERATE", na.rm = T), sample(c("DEFECT", "COOPERATE"), 1), "COOPERATE")
    }
    
    ##### Grudger
    
    if (p1 == "grudger"){
      zd[zd$t == 0,"move1"] <- "COOPERATE"
      zd[zd$t == i,"move1"] <- ifelse(sum(zd[c(ifelse(i-10 < 0, 0, (i-10)):i),"move2"] == "DEFECT", na.rm = T) > 1, "DEFECT", "COOPERATE")
    }
    
    if (p2 == "grudger"){
      zd[zd$t == 0,"move2"] <- "COOPERATE"
      zd[zd$t == i,"move2"] <- ifelse(sum(zd[c(ifelse(i-10 < 0, 0, (i-10)):i),"move1"] == "DEFECT", na.rm = T) > 1, "DEFECT", "COOPERATE")
    }
    
    
    #### Mindreader
    if (p2 == "mindreader"){
      zd1 <- subset(zd, t < i & t > i - 20)
      zd1$move1 <- ifelse(zd1$move1 == "COOPERATE", 1, 0)
      zd1$move2 <- ifelse(zd1$move2 == "COOPERATE", 1, 0)
      
      zd.glm <- glm(move1 ~ move2, data = zd1, family = binomial())
      
      z.d2 <- data.frame("move2" = sample(c(0,1), 100, replace = T), "punish" = NA)
      z.d2$move1 <- predict.glm(zd.glm, newdata = z.d2, "response")
      z.d2$move1 <- ifelse(z.d2$move1 > 0.3, 1, 0)
      
      #does player 2 punish player 1 for defection?
      
      for (nn in 1:(nrow(z.d2)-1)){
        z.d2[nn, "punish"] <- ifelse(z.d2[nn, "move2"] == 0 & z.d2[nn + 1,"move1"] != 0, 1, 0)
      }
      zd[zd$t == i, "move2"] <- ifelse(mean(z.d2$punish, na.rm = T) > 0.3, "DEFECT","COOPERATE")
      
    }
    
    #### scoring
    zd[zd$t == i, "score1"] <- score(zd[zd$t == i, "move1"], zd[zd$t == i, "move2"])
    zd[zd$t == i, "score2"] <- score(zd[zd$t == i, "move2"], zd[zd$t == i, "move1"])
    
    zd[zd$t == i, "meanscore1"] <- mean(zd[,"score1"], na.rm = T)
    zd[zd$t == i, "meanscore2"] <- mean(zd[,"score2"], na.rm = T)
    
    zd[zd$t == i, "scoresum1"] <- sum(zd[,"score1"], na.rm = T)
    zd[zd$t == i, "scoresum2"] <- sum(zd[,"score2"], na.rm = T)
  }
  return(zd)
}

###########
## The below function runs every iteration of extortion factor and strategy 
options(warn=-1)
strategies <- c("Pavlov","TFT","AC","AD","Random","Extortion","gobymajority","majorityvsmajority","majorityvsmajorityrandom","grudger", "mindreader")
s.list <- list()
for (ss in strategies){
  zd.list <- list()
  for (ee in seq(from = 1, to = 13, by = 0.1)){
    df <- IPD(p1 ="Extortion", p2 = ss, extort_factor = ee, n_matches = 1000)
    df$extort_factor <- ee
    zd.list[[as.character(ee)]] <- df
  }
  df.s <- do.call(rbind, zd.list)
  df.s$strategy <- ss
  s.list[[ss]] <- df.s
}

df.strategies <- do.call(rbind, s.list)

df.strategies$p1statenum <- ifelse(df.strategies$move1 == "COOPERATE", 1, 0)
df.strategies$p2statenum <- ifelse(df.strategies$move2 == "COOPERATE", 1, 0)


##### 
#Calculate transfer entropy using the JIDT demo R script.  Note that setting up rJava is difficult and time consuming.
#certain parts of the below code is useful only for Mac setup.  (dyn.load, etc.)


##
##  Java Information Dynamics Toolkit (JIDT)
##  Copyright (C) 2012, Joseph T. Lizier
##  
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/lib/server/libjvm.dylib'))
library(rJava)


setwd('/Users/Home1/Desktop/Classes/Spring_2018/Complex_adaptive_systems/infodynamics-dist-1.4')
.jinit(".")
.jaddClassPath("/Users/Home1/Desktop/Classes/Spring_2018/Complex_adaptive_systems/infodynamics-dist-1.4/infodynamics.jar")
strat.list <- list()
system.time(
  for (ii in unique(df.strategies$strategy)){
    
    zd2 <- subset(df.strategies, strategy == ii)
    extort.list <- list()
    for (i in seq(from = 1, to = 13, by = 0.1)){
      # Create a TE calculator and run it:
      teCalc<-.jnew("infodynamics/measures/discrete/TransferEntropyCalculatorDiscrete",
                    2L,
                    1L)
      .jcall(teCalc,
             "V",
             "initialise") # V for void return value
      .jcall(teCalc,
             "V",
             "addObservations",
             as.integer(subset(zd2, extort_factor == i)$p1statenum),
             as.integer(subset(zd2, extort_factor == i)$p2statenum))
      
      result1 <- .jcall(teCalc,
                        "D",
                        "computeAverageLocalOfObservations")
      
      teCalc<-.jnew("infodynamics/measures/discrete/TransferEntropyCalculatorDiscrete",
                    2L,
                    1L)
      .jcall(teCalc,
             "V",
             "initialise") # V for void return value
      .jcall(teCalc,
             "V",
             "addObservations",
             as.integer(subset(zd2, extort_factor == i)$p2statenum),
             as.integer(subset(zd2, extort_factor == i)$p1statenum))
      
      result2 <- .jcall(teCalc,"D","computeAverageLocalOfObservations")
      extort.list[[as.character(i)]] <- data.frame(result1, result2, "Extortion" = i)
    }
    
    df1 <- do.call(rbind, extort.list)
    df1 <- data.frame(df1, "Strategy" = ii)
    strat.list[[ii]] <- df1
  }
)
strat.df <- do.call(rbind, strat.list)

save(strat.df, file = "~/Desktop/strat_df3.rda")
save(df.strategies, file = "~/Desktop/df_strategies3.rda")

options(warn=0)


#### Plotting results

load("~/Desktop/strat_df3.rda")
load("~/Desktop/df_strategies3.rda")
ggplot(df.strategies) + geom_boxplot(aes(as.character(strategy),meanscore1),fill = "blue", outlier.shape = NA) + geom_boxplot(aes(as.character(strategy),meanscore2),fill = "red", outlier.shape = NA) + theme_bw() + scale_fill_manual(labels = c("Extortion", "Opponent"), values = c("blue","red")) + labs(x = "Strategy", y = "Mean Score (1000 steps)")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("~/Desktop/fig1CAS.jpg", h = 4, w = 3)

ggplot(strat.df) + geom_line(aes(Extortion, result1), color = "blue") + geom_line(aes(Extortion, result2), color = "red") + facet_wrap(~Strategy) + theme_bw() + labs(x = "Extortion Factor", y = "Transfer Entropy")

ggsave("~/Desktop/fig2CAS.jpg", h = 4, w = 3)

ggplot(subset(df.strategies, t <= 100 & extort_factor == c(1, 3, 7, 10))) + geom_line(aes(t,score1, color = as.character(extort_factor))) + facet_wrap(~strategy) + theme_bw() + labs(x = "Time", y = "Score") + scale_color_brewer(palette = "Spectral", name = "Extort Factor") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("~/Desktop/fig3CAS.jpg", h = 4, w = 3)
shannonsH <- function(mystring){
  freqs <- mystring / sum(mystring, na.rm = T)
  H <- -sum(freqs * log(freqs) / log(2), na.rm = T)
  return(H)
}
df.strat.entrop <- df.strategies %>%
  group_by(extort_factor, strategy) %>%
  summarize("ShannonH1" = shannonsH(score1),
            "ShannonH2" = shannonsH(score2))

ggplot(df.strat.entrop) + geom_line(aes(extort_factor, ShannonH1), color = "blue") + geom_line(aes(extort_factor, ShannonH2), color = "red") + facet_wrap(~strategy) + theme_bw() +
  labs(x = "Extortion Factor", y = "Shannon's H")
ggsave("~/Desktop/fig35CAS.jpg", h = 4, w = 3)

### how many possible values per strat as extortion factor increases?

df.strat.len <- subset(df.strategies, t > 100) %>%
  group_by(extort_factor, strategy) %>%
  summarize("r1" = length(unique(score1)),
            "r2" = length(unique(score2)))


ggplot(df.strat.len) + geom_line(aes(extort_factor, r1), color = "blue") + geom_line(aes(extort_factor, r2), color = "red") + facet_wrap(~strategy) + labs(x = "Extortion Factor", y = "Possible Scores") + theme_bw()

ggsave("~/Desktop/fig4CAS.jpg", h = 4, w = 3)
