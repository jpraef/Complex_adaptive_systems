#Part 2: Define what constitutes a neutral phenotype for your CA. You could define “neutral” based on binned fitness values (ie., all CA’s that classify the same input correctly between 80 and 81% of the time have the same fitness). Alternatively, you could consider all rulesets that misclassify a certain fraction of 0’s in the input below some threshold as having the same phenotype. Or you could classify any ruleset that produces a glider as having the same phenotype. Think carefully about what a meaningful phenotype is for your CA, and consider whether it is feasible to measure it sufficiently quickly for the remaining tasks. Explain your choice to define “neutral” in whatever way you have chosen. Use your CA and your neutral network to test the hypothesis proposed by Wagner that evolution over a neutral network increases robustness. Your explanation should measure the fraction of 1 bit mutations from your initial ruleset are neutral. You may repeat for 2-mutations, 3-mutations, and up to N-mutations that you specify. You may want to explore how many mutations away from the original ruleset have at least one neutral solution. Limit yourself to a reasonable run time, and turn in code that produces partial results in a short (less than 1 minute) run time. You should define a new fitness function (f’) and compare f’ for rulesets one mutation away from the neutral network to f’ for rulesets one mutation away from the original ruleset. Explain whether the neutral network helped your CA to find a better solution to f’, and whether your results support Wagner’s hypothesis. Extend your analysis in any way that you think is interesting for extra credit.

#neutral Phenotype: top 10% of speeds
#Test: evolution over a neutral network increases robustness
#Measure: fraction of 1 bit mutations from initial ruleset - initial ruleset == the first rule identified - that are neutral
#new fitness function: average classification rate of the rule




#neutral map

#This code takes the genotypes from the previously run genetic algorithm and cellular automata.
#column one: rule
#column two: generation
#column three: speed
#column four: fitness
#genotypes are connected by the number of flips from one to another


#simulating some data
library(dplyr)
g.list <- list()
for (i in 1:10000){
rule <- paste0(sample(c(0,1),1),sample(c(0,1),1),sample(c(0,1),1),sample(c(0,1),1),sample(c(0,1),1),sample(c(0,1),1),sample(c(0,1),1))
class_rate <- runif(1)
speed <- sample(1:150, 1)
generation <- sample(c(0:1000),1)
g.list[[as.character(i)]] <- data.frame(rule, class_rate, speed, generation)

}

g <- bind_rows(g.list)

g$speedbin <- cut(g$speed, breaks = seq(0, 150, by = 30), labels = F) * 10
g2 <- g[!duplicated(g$rule),]

g2$f1 <- ifelse(g2$speedbin == 10, 1, 0)



#from
#"https://www.r-bloggers.com/count-different-positions-between-two-strings-of-equal-length/"

string.diff.ex<-function(a="ATTCGAN",b="attTGTT",exclude=c("n","N","?"),ignore.case=TRUE)
{
if(nchar(a)!=nchar(b)) stop("Lengths of input strings differ. Please check your input.")
if(ignore.case==TRUE)
{
a<-toupper(a)
b<-toupper(b)
}
diff.a<-unlist(strsplit(a,split=""))
diff.b<-unlist(strsplit(b,split=""))
diff.d<-rbind(diff.a,diff.b)
for(ex.loop in 1:length(exclude))
{
diff.d<-diff.d[,!(diff.d[1,]==exclude[ex.loop]|diff.d[2,]==exclude[ex.loop])]
}
differences<-sum(diff.d[1,]!=diff.d[2,])
return(differences)
}

g.m <- matrix(NA, nrow = 128, ncol = 128)
colnames(g.m) <- rownames(g.m) <- as.character(g2$rule)

for (i in colnames(g.m)){
	for (j in rownames(g.m)){
		g.m[i,j] <- string.diff.ex(i,j)
		}
		}
colnames(g.m) <- rownames(g.m) <- strtoi(as.character(g2$rule), base = 2)
g2$id <- as.character(as.numeric(strtoi(as.character(g2$rule), base = 2)))






g.m <- ifelse(g.m != 1, 0, g.m)




##############################################
###############################################
#############################################
###############################################

g4 <- g2[,c("id","speed","speedbin","class_rate")]
n <- network(g.m, directed = F)
n %v% "speedbin" <- as.character(g4$speedbin)
n %v% "speed" <- as.character(g4$speed)
n %v% "class_rate" <- as.character(g4$class_rate)

ggplot(n, layout = "kamadakawai" , cell.jitter = 0.75, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes(aes(color = as.numeric(class_rate))) +
  theme_blank() + facet_wrap(~speedbin)

s1 <- subset(g2, speedbin == 10)$id
s2 <-subset(g2, speedbin == 20)$id
s3 <- subset(g2, speedbin == 30)$id
s4 <- subset(g2, speedbin == 40)$id
s5 <- subset(g2, speedbin == 50)$id

g.m2 <- subset(melt(g.m), value == 1)
g.m21 <-length(s1)/ dim(subset(g.m2, Var1 %in% s1 & !(Var2 %in% s1)))[1] # ratio of neutral net nodes to non-neutral net nodes 1 bit away from neutral net
g.m22 <-length(s2)/dim(subset(g.m2, Var1 %in% s2 & !(Var2 %in% s2)))[1]   #ratio of neutral net nodes to non-neutral net nodes 1 bit away from neutral net
g.m23 <-length(s3)/dim(subset(g.m2, Var1 %in% s3 & !(Var2 %in% s3)))[1] # ratio of neutral net nodes to non-neutral net nodes 1 bit away from neutral net
g.m24 <-length(s4)/dim(subset(g.m2, Var1 %in% s4 & !(Var2 %in% s4)))[1]  # ratio of neutral net nodes to non-neutral net nodes 1 bit away from neutral net
g.m25 <-length(s5)/dim(subset(g.m2, Var1 %in% s5 & !(Var2 %in% s5)))[1]  #ratio of neutral net nodes to non-neutral net nodes 1 bit away from neutral net

g.m31 <- subset(g.m2,  !(Var2 %in% s1)) %>%
  group_by(Var1) %>%
  summarize("tot" = sum(value))

g.m31 <- 1/ mean(g.m31$tot)

g.m32 <- subset(g.m2,  !(Var2 %in% s2)) %>%
  group_by(Var1) %>%
  summarize("tot" = sum(value))

g.m32 <- 1/ mean(g.m32$tot)
g.m33 <- subset(g.m2,  !(Var2 %in% s3)) %>%
  group_by(Var1) %>%
  summarize("tot" = sum(value))

g.m33 <- 1/ mean(g.m33$tot)
g.m34 <- subset(g.m2,  !(Var2 %in% s4)) %>%
  group_by(Var1) %>%
  summarize("tot" = sum(value))

g.m34 <- 1! mean(g.m34$tot)
g.m35 <- subset(g.m2,  !(Var2 %in% s5)) %>%
  group_by(Var1) %>%
  summarize("tot" = sum(value))

g.m35 <- 1/ mean(g.m35$tot)

node.comp <- data.frame("Neutral_net" = c(g.m21,g.m22,g.m23,g.m24,g.m25), "Single_node" =  c(g.m31,g.m32,g.m33,g.m34,g.m35))





