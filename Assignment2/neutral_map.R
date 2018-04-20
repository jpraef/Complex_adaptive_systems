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
g.list[[as.character(i)]] <- data.frame(rule, fitness, speed, generation)

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
g.m.m2 <- melt(g.m)

g.m.m2$distance <- g.m.m2$value
g.m.m2$value <- NULL
g2$X1 <- strtoi(g2$rule, base = 2)
g.m.m <- merge(g.m.m2, g2, by = "X1")
g.m.m <- arrange(g.m.m, X1,X2)

ggplot(subset(g.m.m, distance == 1)) + geom_point(aes(X1, X2))


head(g.m)		
		
g.m <- ifelse(g.m != 1, 0, g.m)
g.mn <- as.network(g.m)


install.package("GGally")
library(GGally)
ggnet2(g.mn, mode = "hall", size = 2, edge.size = 0.3, color = "mode")
?ggnet2

g2$speedbin <- as.factor(g2$speedbin)
x <- data.frame(rule = network.vertex.names(g.mn))
x <- merge(x, g2[,c("rule","speedbin")], by = "rule", sort = F)$speedbin
g.mn %v% "speed" = as.character(x)
y <- palette(rainbow(5))
names(y) <- levels(x)
ggnet2(g.mn, mode = "circrand", color = "speed",edge.color = c("color", "grey50"), palette = y, alpha = 0.75, size = 1, edge.alpha = 0.5)

