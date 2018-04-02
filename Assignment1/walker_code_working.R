### Code to generate four (five) figures for project 1 part 2
### Author:  Joe Crockett
### R.Version()
#platform       x86_64-apple-darwin15.6.0   
#arch           x86_64                      
#os             darwin15.6.0                
#system         x86_64, darwin15.6.0        
#status                                     
#major          3                           
#minor          4.3                         
#year           2017                        
#month          11                          
#day            30                          
#svn rev        73796                       
#language       R                           
#version.string R version 3.4.3 (2017-11-30)
#nickname       Kite-Eating Tree  

####
# This code is separated into four  sections that detail the four figures.
# Prior to running code, it is important to identify folder locations using OS-specific terminology.
# As this was written and compiled on using Apple OS, it is essential to check locations if a non-Mac is used


#### Load Dependencies (Packages)
# The following function ndpkg identifies if packages are present on the host machine, downloads and loads if not, and loads if they are.
# The user must designate a CRAN source from the popup menu when it is present.
# The sapply function applies ndpkg to all package names identified as terms.
library(ggplot2)
library(reshape2)
library(dplyr)
library(rJava)
### Figure 1: populations over time, using 2 values of R for 2 initial conditions each
### Figure 1 demonstrates sensitivity to initial conditions by examining 1) the effect of slightly different starting populations and 2) the effect of slightly different inherent growth values (R)
#Equation for logistic Map from Complexity, a guided Tour
log_growth_1 <- function(xt, R){
  R * xt * (1-xt)
}


fig1.vect_x <- vector()
fig1.vect_x_t <- vector()

df1.list <- list()
df2.list <- list()

for (rr in c(2.6, 3.9)){  #building dataframe that uses two values of R and two starting populations.  This loops through each iteration and provides output as vectors

  r = rr
  for (xx in c(0.2, 0.7654, 0.201)){
    x = xx
for (i in 1:50){
   xt_1 <- log_growth_1(xt = x, R = r)
   fig1.vect_x[[i]] <- x
   x <- xt_1
   fig1.vect_x_t[[i]] <- x

}
  df1 <- data.frame("xt" = fig1.vect_x, "xt_1" = fig1.vect_x_t, "Start_x" = xx, "timestep" = 1:50) #concatnating dataframe from vectors with ID of starting population
  df1.list[[as.character(xx)]] <- df1
}
  df1_l <- do.call(rbind, df1.list)
  df1_l$R <- as.character(rr)
  df2.list[[as.character(rr)]] <- df1_l
}

fig1_df <- do.call(rbind,df2.list) #concatnating final dataframe from previous dataframes with ID of R

#Determine divergence
fig1_df1 <- subset(fig1_df, Start_x == 0.2 & R == 2.6)$xt
fig1_df2 <- subset(fig1_df, Start_x == 0.7654 & R == 2.6)$xt

fig1_df1.1 <- subset(fig1_df, Start_x == 0.2 & R == 3.9)$xt
fig1_df2.1 <- subset(fig1_df, Start_x == 0.201 & R == 3.9)$xt

fig1_df_div <- melt(data.frame("pop3.85" = fig1_df1 - fig1_df2,
                          "pop3.9" = fig1_df1.1 - fig1_df2.1,
                          "timestep" = 1:50), id.vars = "timestep")


#determine mutual information
fig1_df_mut <- data.frame("R3.85P.4" = round(fig1_df1 * 100),
                               "R3.9P.4" = round(fig1_df1.1 * 100),
                               "R3.85P.43" = round(fig1_df2 * 100),
                               "R3.9P.43" = round(fig1_df2.1 * 100))
write.csv(fig1_df_mut, file = "~/Desktop/fig1.csv")

# Pull out the columns from the data set which correspond to each of variable 1 and 2:
variable1 <- fig1_df_mut[, 1:2]
variable2 <- fig1_df_mut[, 3:4]
# Extra step to extract the raw values from these data.frame objects:
variable1 <- apply(variable1, 2, function(x) as.numeric(x))
variable2 <- apply(variable2, 2, function(x) as.numeric(x))

#Calculating Mutual information using JIDT 
# Load the rJava library and start the JVM
# Generate some random binary data:
sourceArray<-sample(0:100, 100, replace=TRUE)
destArray<-c(0L, sourceArray[1:99]); # Need 0L to keep as integer array 
sourceArray2<-sample(0:100, 100, replace=TRUE)
# Create a TE calculator and run it:
teCalc<-.jnew("infodynamics/measures/discrete/TransferEntropyCalculatorDiscrete", 2L, 1L) 
.jcall(teCalc,"V","initialise") # V for void return value 
.jcall(teCalc,"V","addObservations",sourceArray, destArray)
result1 <- .jcall(teCalc,"D","computeAverageLocalOfObservations")
cat("For copied source, result should be close to 1 bit : ", result1, "\n")
# Now look at the unrelated source:
.jcall(teCalc,"V","initialise") # V for void return value .jcall(teCalc,"V","addObservations",sourceArray2, destArray)
result2 <- .jcall(teCalc,"D","computeAverageLocalOfObservations")
cat("For random source, result should be close to 0 bits: ", result2, "\n")



library("rJava")
.jinit()
setwd("~/Desktop/Classes/Spring 2018/Complex_adaptive_systems/infodynamics-dist-1.4")
.jaddClassPath("./infodynamics.jar")
#---------------------

data = apply(fig1_df_mut, 2, function(x) as.numeric(x))
#1. Construct the calculator:
  micalc <- .jnew('infodynamics/measures/discrete/MutualInformationCalculatorDiscrete', 100L, 1L);
# 2. No other properties to set for discrete calculators.

# Compute for all pairs:
  for (s in 1:4){
    for (d in 1:4){
# For each source-dest pair:
  if (s != d){
    sourceArray = data[, s]
    destArray = data[, d]
# 3. Initialise the calculator for (re-)use:
  .jcall(micalc,"V","initialise")
# 4. Supply the sample data:
  .jcall(micalc,"V","setObservations", 
         .jarray(sourceArray),
         .jarray(destArray))
# 5. Compute the estimate:
  result <- .jcall(miCalc,"V","computeAverageLocalOfObservations")

print( s, d, result);
}}}
---------------
# 4. Start using the MI calculator, paying attention to only
#  call common methods defined in the interface type

#  infodynamics.measures.continuous.MutualInfoCalculatorMultiVariate
#  not methods only defined in a given implementation class.
# a. Initialise the calculator to use the required number of
#   dimensions for each variable:
.jcall(miCalc,"V","initialise", length(variable1Columns), length(variable2Columns))
# b. Supply the observations to compute the PDFs from:
.jcall(miCalc,"V","setObservations",
       .jarray(variable1, "[D", dispatch=TRUE),
       .jarray(variable2, "[D", dispatch=TRUE))
# c. Make the MI calculation:
miValue <- .jcall(miCalc,"D","computeAverageLocalOfObservations")

cat("MI calculator", implementingClass, "\n computed the joint MI as ",
    miValue, "\n")



#plotting using ggplot2. f1.1 shows different values of R for different starting values, f1.2 shows different starting values for the same R
f1.1 <- ggplot(subset(fig1_df, Start_x == 0.2)) +
  geom_line(aes(timestep, xt, color = as.character(R), linetype = as.character(R))) +
  theme_bw() +
  scale_color_manual(values = c("red","black"),
                     name = "Inherent Growth Rate") +
  scale_linetype_manual(values = c("solid","dashed"),
                        name = "Inherent Growth Rate") +
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text=element_text(size=15),
        legend.background = element_rect(fill="white",
                                          size=0.5,
                                          linetype="solid", 
                                          colour ="black")) +
  labs(x = "Generation", y = "Population Size")

f1.2 <- ggplot(subset(fig1_df, R == 3.85)) +
  geom_line(aes(timestep, xt, color = as.character(Start_x), linetype = as.character(Start_x))) +
  theme_bw() +
  scale_color_manual(values = c("red","black"),
                     name = "Starting Population") +
  scale_linetype_manual(values = c("solid","dashed"),
                        name = "Starting Population") +
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text=element_text(size=15),
        legend.background = element_rect(fill="white",
                                         size=0.5,
                                         linetype="solid", 
                                         colour ="black")) +
  labs(x = "Generation", y = "Population Size")

f1.3 <- ggplot(fig1_df_div) +
  geom_line(aes(timestep, value, color = as.character(variable))) +
  theme_bw() +
  scale_color_manual(values = c("orange", "blue"),
                     name = "Diverging Populations",
                     labels = c("Differing Populations for R = 2.6",
                                "Differing Populations for R = 3.9")) +
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.text=element_text(size=15),
        legend.background = element_rect(fill="white",
                                         size=0.5,
                                         linetype="solid", 
                                         colour ="black")) +
  labs(x = "t", y = "Population Difference")
ggsave("~/Desktop/fig1c.jpg", h = 3, w = 7)



#---------------------
# 2. Load in the data
#data <- read.csv(datafile, header=FALSE, sep="")
# Pull out the columns from the data set which correspond to each of variable 1 and 2:
variable1 <- round(as.matrix(data.frame("v1" = as.numeric(fig1_df1),
                        "v2" = as.numeric(fig1_df1.1))) * 100)

variable2 <- round(as.matrix(data.frame("v1" = as.numeric(fig1_df2),
                        "v2" = as.numeric(fig1_df2.1))) *100)

#variable2 <- as.list(fig1_df[fig1_df$R == 3.5 & fig1_df$Start_x == 0.4, "xt"])
#variable2.2 <- as.list(fig1_df[fig1_df$R == 3.5 & fig1_df$Start_x == 0.400003, "xt"])


#variable4 <- as.list(fig1_df[fig1_df$R == 3.9 & fig1_df$Start_x == 0.4, "xt"])
#variable4.2 <- as.list(fig1_df[fig1_df$R == 3.9 & fig1_df$Start_x == 0.400003, "xt"])






### Figure 2:  Recreate Fig 3 from Walker et al. 2005


#mn is a function of all the populations at timestep i. how is it a function? mean, var, what?  Hmm. Maybe if I assume that the entire population follows a logistic growth map as well
#K here is the number of populations = 1000
log_growth_1 <- function(xt, R){
  (R * xt * (1 - (xt/100)))
}
log_growth_2 <- function(fxt, e, mn){
  (1- e) * fxt + e * mn #R is between 3.9-4.0
}



set.seed(123)
rr <- runif(1000, min = 3.9, max = 4.0)
xv <- sample(1:1000, 3)
#get r values
e.vect <- seq(from = 0, to = 1, by = 0.005)
#e.vect <- c(0, 0.075, 0.1, 0.2, 0.225,0.25,0.3,0.4)
e.list <- list()
for (ee in e.vect){


mn.list <- vector()
mn_1.list <- vector()
xtt <- rep(1, 1000)
x.list <- list()
for (i in 1:10000){
xti <- log_growth_1(xtt, rr)
mnn <- mean(xti)
xt_M <- mean(xtt)
xt_1 <- log_growth_2(xti, ee, mnn)
xt_M_1 <- mean(xt_1)
x.list[[i]] <- xtt
xtt <- xt_1
mn.list[[i]] <- xt_M
mn_1.list[[i]] <- xt_M_1

}

mn.df <- data.frame("Mn" = mn.list, "Mn_1" = mn_1.list, "e" = ee, "timestep" = 1:10000)
x.df <- as.data.frame(do.call(rbind, x.list))
x.df <- x.df[,paste0("V",xv)]
mn.df <- cbind(mn.df, x.df)
e.list[[as.character(ee)]] <- mn.df
}

e.df <- do.call(rbind, e.list)
#e.df.f2 <- subset(e.df, e ==  c(0, 0.075, 0.1, 0.2, 0.225,0.25,0.3,0.4))

#plot for F2
ggplot(subset(e.df, e ==  c(0, 0.075, 0.1, 0.2, 0.225,0.25,0.3,0.4))) + geom_point(aes(Mn, Mn_1, color = as.factor(e)), size = 0.01) + theme_bw() + 
  labs(x = "Mn", y = "Mn + 1") + scale_color_brewer(palette = "Spectral", name = "Epsilon")
ggsave("~/Desktop/walker_fig3.jpg")



### Figure 3: Transfer entropy from Walker et al. 2005

#JIDT
install.packages("rJava")
if (Sys.info()['sysname'] == 'Darwin') {
  libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
  message (paste0('Load libjvm.dylib from: ',libjvm))
  dyn.load(libjvm)
}  #Solution code from https://github.com/MTFA/CohortEx/wiki/Run-rJava-with-RStudio-under-OSX-10.10,-10.11-(El-Capitan)-or-10.12-(Sierra)

library(rJava)
.jinit()
setwd("~/Desktop/Classes/Spring 2018/Complex_adaptive_systems/infodynamics-dist-1.4")
.jaddClassPath("./infodynamics.jar")


#### Walker Transfer entropy for M = X and X = M for each e
ei.list <- list()
vv.list <- list()
set.seed(323)
for (vv in c("V274","V594","V160")){
  ve.df <- e.df[,c("Mn","e",vv)]
for (ei in unique(e.df$e)){

#For walker code, set source array 1 as Mn and destarray as V(x) (M -> X)
sourceArray <- round(subset(ve.df, e == ei)[,"Mn"])
destArray <- round(subset(ve.df, e == ei)[,vv])

#and for X -> M
sourceArray2 <- round(subset(ve.df, e == ei)[,vv])
destArray2 <- round(subset(ve.df, e == ei)[,"Mn"])


# Create a TE calculator and run it:
teCalc<-.jnew("infodynamics/measures/continuous/kernel/TransferEntropyCalculatorKernel")
.jcall(teCalc,"V","setProperty", "NORMALISE", "true") # Normalise the individual variables
.jcall(teCalc,"V","initialise", 1L, 1) # Use history length 1 (Schreiber k=1), kernel width of 1 normalised units

# result 1 is M -> X
.jcall(teCalc,"V","setObservations", sourceArray, destArray)
result1 <- .jcall(teCalc,"D","computeAverageLocalOfObservations")


#for X -> M
.jcall(teCalc,"V","setObservations", sourceArray2, destArray2)
result2 <- .jcall(teCalc,"D","computeAverageLocalOfObservations")

ei.list[[as.character(ei)]] <- data.frame("MX" = result1, "XM" = result2, "e" = ei)
}

vv.list[[as.character(vv)]] <- do.call(rbind, ei.list)
}

vv.df <- bind_rows(vv.list, .id = "id")
vv.df.m <- melt(vv.df, id.vars= c("id", "e"))
#Figure 3 plot:
ggplot(vv.df.m) +
  geom_line(aes(as.numeric(e), value, color = variable, linetype = id)) +
  theme_bw() 
ggsave("~/Desktop/f3.jpg", h = 6, w = 9)
############################################################################################
############################################################################################
####Figure 4

set.seed(344)
e.df.m <- melt(subset(e.df, e == sample(e.df$e, 2))[,-2], id.vars = c("e","timestep"))
ggplot(e.df.m) +
  geom_point(aes(timestep, value, color = variable), size = 0.1) +
  facet_wrap(~e, ncol = 1) +
  theme_bw()
ggsave("~/Desktop/f4.jpg", h = 4, w = 5, limitsize = F)



###########################################################################################
###########################################################################################
###Figure 5
#new coupled relationship

#Global atmopheric to local soil moisture
