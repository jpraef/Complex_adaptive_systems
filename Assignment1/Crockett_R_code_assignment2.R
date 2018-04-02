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
# This code details generation of figure 3.
# Prior to running code, it is important to identify folder locations using OS-specific terminology.
# As this was written and compiled on using Apple OS, it is essential to check locations if a non-Mac is used


#### Load Dependencies (Packages)

# The user must designate a CRAN source from the popup menu when it is present.
# The sapply function applies ndpkg to all package names identified as terms.
install.package("ggplot2")
install.package("reshape2")
install.package("dplyr")
install.package("rJava")

library(ggplot2)
library(reshape2)
library(dplyr)
library(rJava)

#Set working directory
setwd("~/Desktop/Project 1")

#output locations
save1 <- "./Data/fig24data.rda"
save2 <- "./Data/f3.jpg"
save3 <- "./Data/fig3data.rda"
save4 <- "./Data/f4.jpg"
### Figure 2:  Recreate Fig 3 from Walker et al. 2005

# Logistic Growth function derived from Walker et al. 2005
#mn is a function of all the populations at timestep i.  
#K here is the number of populations = 1000
#
log_growth_1 <- function(xt, R){
  (R * xt * (1 - (xt/100)))
}
log_growth_2 <- function(fxt, e, mn){
  (1- e) * fxt + e * mn #R is between 3.9-4.0
}



set.seed(123) #set seed to reproduce randomness
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
save(e.df, file = save1)


### Figure 3: Transfer entropy from Walker et al. 2005
#Note: Code is developed from JIDT R demos included here.  Also included is JIDT for R if troubleshooting is necessary.
#Prior to running this section, it is essential to download the JIDT tool
#JIDT
install.packages("rJava")
if (Sys.info()['sysname'] == 'Darwin') {
  libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
  message (paste0('Load libjvm.dylib from: ',libjvm))
  dyn.load(libjvm)
}  #Solution code from https://github.com/MTFA/CohortEx/wiki/Run-rJava-with-RStudio-under-OSX-10.10,-10.11-(El-Capitan)-or-10.12-(Sierra)

library(rJava)
.jinit()
#Set working directory to JIDT tool location
setwd("./Data/infodynamics-dist-1.4")
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
save(vv.df, file = save3)  #saving


#Figure 3 plot:
ggplot(vv.df.m) +
  geom_line(aes(as.numeric(e),
                value,
                color = variable,
                linetype = id)) +
  theme_bw() +
  scale_color_manual(values = c("red","black"),
                     name = "Transfer Entropy",
                     labels = c("M -> X", "X -> M")) +
  scale_linetype_manual(values = c(1,4,6),
                        name = "Individual Populations") +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15),
        legend.background = element_rect(fill="white",
                                         size=0.5,
                                         linetype="solid", 
                                         colour ="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15)) +
  labs(x = "Epsilon", y = "Bits")
ggsave(save2, h = 5, w = 5)  #indicate location to save here


####Figure 4

set.seed(344)
e.df.m <- melt(subset(e.df, e == c(0.005, 0.18, 0.385, 0.42))[,-2], id.vars = c("e","timestep"))
ggplot(e.df.m) +
  geom_point(aes(timestep, value, color = variable), size = 0.1) +
  facet_wrap(~e, ncol = 2) +
  theme_bw() +
  scale_color_manual(values = c("red","black","blue","green"),
                     name = "Selected Populations",
                     labels = c("Mean Population","V274","V594","V160")) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15),
        legend.background = element_rect(fill="white",
                                         size=0.5,
                                         linetype="solid", 
                                         colour ="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15)) +
  labs(x = "T", y = "Population Size") + 
  guides(color = guide_legend(override.aes = list(size = 15)))

ggsave(save4, h = 5, w = 9, limitsize = F)

#Export to csv to calculate mutual information
e1 <- as.matrix(subset(e.df, e == 0.005)[,c("Mn","V274")])
e2 <- as.matrix(subset(e.df, e == 0.18)[,c("Mn","V274")])
e3 <- as.matrix(subset(e.df, e == 0.385)[,c("Mn","V274")])
e4 <- as.matrix(subset(e.df, e == 0.42)[,c("Mn","V274")])
e5 <- as.matrix(subset(e.df, e == 0.075)[,c("V594","V274","V160", "Mn")])

setwd("~/Desktop/Project 1")
write.csv(e1, "./Data/e1.csv")
write.csv(e2, "./Data/e2.csv")
write.csv(e3, "./Data/e3.csv")
write.csv(e4, "./Data/e4.csv")
write.csv(e5,"./Data/e5.csv")

#Note:  Prior to calculating Mutual Information, it is essential that the .csv files be cleaned up.  Row 1 and column 1 must be removed.
#from JIDT calculator: e1 = 0.0091 bits
#e2 0.3219 bits
#e3 1.7667 bits
#e4 2.3013 bits
#e5: 0.0297 bits, 0.1908 bits, 0.1012 bits