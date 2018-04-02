#code for CAS, due Thursday 1/25

####################################################################################################
####################################################################################################
#1. Plot population vs time for 50 time steps. Demonstrate trajectories with fixed points, periodic cycles and chaotic dynamics. 
####################################################################################################
####################################################################################################


#General logistic growth function:
#R = 4*r
#xt = 1<=xt<=0


log_growth <- function(xt,R){
	xt_1 <- R * xt *(1 - xt)   #R is between 0-4
	print(xt_1)
}

#50 timesteps, r = 0.5, initial fractional carrying capacity = 0.4
df<-data.frame("time" = 0:50, "xt" = NA, "R" = 3)
df[1,"xt"] <- 0.2


for (i in 2:51){

df[i,"xt"] <- log_growth(xt = df[i-1,"xt"], R = df[i-1,"R"])

}

#plotting using ggplot2
library(ggplot2)
ggplot(df) +
 geom_line(aes(time, xt)) +
 labs(x = "Time", y = "Population") +
 ylim(0,1) +
 ggtitle("Population vs Time, Logistic growth") +
 theme_bw()





####################################################################################################
####################################################################################################
#2. Plot population vs time and demonstrate sensitive dependence on initial conditions. Demonstrate how far into the future you can expect two populations to be correlated for a chaos-generating and a non chaos- generating value of R. 
####################################################################################################
####################################################################################################
df.2<-data.frame("time" = rep(0:50, 2), "xt" = NA, "R" = 3.76, "sensitivity" = rep(1:2, each = 51))
df.2[1,"xt"] <- 0.2
df.2[52,"xt"] <- 0.200001

for (i in 1:length(df.3$time)){

a <- ifelse(df.2[i, "time"] == 0, df.2[i,"xt"], log_growth(xt = df.2[i-1,"xt"], R = df.2[i-1,"R"]))
df.2[i,"xt"] <- a;

}

#plotting using ggplot2
library(ggplot2)
ggplot(df.2) +
 geom_line(aes(time, xt, color = as.character(sensitivity))) +
 labs(x = "Time", y = "Population") +
 ylim(0,1) +
 ggtitle("Population vs Time, Sensitivity to Initial Conditions") +
 theme_bw()


####################################################################################################
####################################################################################################
#3. Create a bifurcation diagram. You must write code for this yourself (you cannot download it).
#Label the diagram to show Fiegenbaum’s constant. 
####################################################################################################
####################################################################################################
df.3<-data.frame("time" = rep(0:50, 400), "xt" = NA, "R" = rep(seq(0.001,4, 0.001), each = 51))

df.3[df.3$time == 0,"xt"] <- 0.2

for (i in 1:length(df.3$time)){

a <- ifelse(df.3[i, "time"] != 0, log_growth(xt = df.3[i-1,"xt"], R = df.3[i-1,"R"]), df.3[i,"xt"]);

df.3[i,"xt"] <- a;

}




library(ggplot2)
ggplot(subset(df.3, time >= 25 )) +
 geom_point(aes(R, xt), size = .001) +
 labs(x = "R", y = "x(t)") +
 ylim(0,1) +
 ggtitle("Intrinsic Growth rate (R) vs Output, Logistic growth") +
 theme_bw()
####################################################################################################
####################################################################################################
#4. Create a return map showing xt+1 vs xt for R = 2.75 and for R = 3.85.
#Create a return map for the population mean of 100 populations with R randomly sampled between 2.5 and 3,
#and for R between 3.8 and 3.9. 
####################################################################################################
####################################################################################################
df.4<-data.frame("time" = 0:500, "xt" = NA,"xt_1" = NA, "R" = 2.75)

df.4[df.4$time == 0,"xt"] <- 0.002

df.42<-data.frame("time" = 0:500, "xt" = NA,"xt_1" = NA, "R" = 3.85)

df.42[df.42$time == 0,"xt"] <- 0.002

for (i in 1:length(df.4$time)){
  
  df.4[i,"xt_1"] <- log_growth(xt = df.4[i,"xt"], R = df.4[i,"R"])
  df.4[i+1,"xt"] <- df.4[i,"xt_1"]
  
  df.42[i,"xt_1"] <- log_growth(xt = df.42[i,"xt"], R = df.42[i,"R"])
  df.42[i+1,"xt"] <- df.42[i,"xt_1"]
  
  
}

df.4 <- rbind(df.4, df.42)
df.4$xt_line <- df.4$xt_1

#plotting using ggplot2
library(ggplot2)
ggplot(subset(df.4, !is.na(xt_1))) +
  stat_smooth(aes(xt, xt_1, color = as.character(R)), method = "lm", formula = y~x + I(x^2))+
  geom_segment(aes(x = xt, xend = xt_1, y = xt_1, yend = xt_1, color = as.character(R)), size = 0.15) +
  geom_segment(aes(x = xt, xend = xt, y = xt, yend = xt_1, color = as.character(R)), size = 0.15) +
  geom_point(aes(xt, xt_1, shape = as.character(R))) +
  labs(x = "x(t)", y = "x(t + 1)") +
  ggtitle("Return Map of x(t) vs x(t+1)") +
  theme_bw() + scale_linetype_discrete(na.value = "blank") +
  geom_abline(intercept = 0, slope = 1)  +
  scale_color_manual(values = c("red", "blue"), labels = c("R = 2.75", "R = 3.85"), name = NULL)+
scale_shape_manual(values = c(4, 16), labels = c("R = 2.75", "R = 3.85"), name = NULL)

ggsave("~/Desktop/fig_4.jpg", h = 3, w = 4)
ggplot(subset(df.4, !is.na(xt_1))) +
  geom_line(aes(time, xt, linetype = as.character(R)))


#100 population means with 100 random samples between 2.5 and 3.0
#100 population means with 100 random samples between 3.8 and 3.9

df.5 <- data.frame("time" = rep(0:100, 100),
                   "population" = rep(1:100, each = 101),
                   "xt" = NA,
                   "xt_1" = NA,
                   "R" = rep(sample(seq(2.5,3.0, by = 0.0001), 100, replace = T), each = 101),
                   "samp" = 1)
df.5[df.5$time == 0,"xt"] <- 0.2
df.52 <- data.frame("time" = rep(0:100, 100),
                   "population" = rep(1:100, each = 101),
                   "xt" = NA,
                   "xt_1" = NA,
                   "R" = rep(sample(seq(3.8,3.9, by = 0.0001), 100, replace = T), each = 101),
                   "samp" = 2)
df.52[df.52$time == 0,"xt"] <- 0.2

df.5 <- rbind(df.5, df.52)

for (i in 1:length(df.5$time)){
  
  df.5[i,"xt_1"] <- log_growth(xt = df.5[i,"xt"], R = df.5[i,"R"])
  df.5[i+1,"xt"] <- df.5[i,"xt_1"]
  
}

ggplot(subset(df.5, !is.na(xt_1))) +
  geom_point(aes(xt, xt_1, color = R)) +
  labs(x = "x(t)", y = "x(t + 1)") +
  ggtitle("Return Map of x(t) vs x(t+1)") +
  theme_bw() + scale_linetype_discrete(na.value = "blank") +
  geom_abline(intercept = 0, slope = 1) + scale_color_distiller(palette = "Spectral", direction = 1) +
  geom_point(dataaes(xt, xt_1, color = as.character(samp))) +
  geom_abline(intercept = 0, slope  = 1)+
  geom_segment(aes(x = xt, xend = xt_1, y = xt_1, yend = xt_1, color = as.character(samp)), size = 0.15) +
  geom_segment(aes(x = xt, xend = xt, y = xt, yend = xt_1, color = as.character(samp)), size = 0.15) +
  labs(x = "x(t)", y = "x(t + 1)") + theme_bw()
ggplot(subset(df.4, !is.na(xt_1))) +
  geom_line(aes(time, xt, linetype = as.character(R)))


library(dplyr)
df.52 <- df.5 %>%
  group_by(time, samp) %>%
  summarize(xt = mean(xt, na.rm = T),xt_1 = mean(xt_1, na.rm = T))

ggplot(subset(df.52, !is.na(xt) & !is.na(xt_1))) +
  geom_point(aes(xt, xt_1, color = as.character(samp))) +
  geom_abline(intercept = 0, slope  = 1)+
  geom_segment(aes(x = xt, xend = xt_1, y = xt_1, yend = xt_1, color = as.character(samp)), size = 0.15) +
  geom_segment(aes(x = xt, xend = xt, y = xt, yend = xt_1, color = as.character(samp)), size = 0.15) +
  labs(x = "x(t)", y = "x(t + 1)") + theme_bw() +
  ggtitle("Return Plot, Population means of \nsampled R between 2.5 and 3.0 vs \n3.8 and 3.9") +
  scale_color_manual(values = c("red", "blue"), labels = c("2.5 to 3.0", "3.8 to 3.9"), name = "Range of R")
  
ggsave("~/Desktop/fig_42.jpg", h = 3, w = 4)
