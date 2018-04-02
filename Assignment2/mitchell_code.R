#Genetic Algorithm

#Step 1:  Randomly generate P rules
#k = 2 (0/1) , r = 3 (000_000, 3 neighbors per side)
#Step 2:  Apply each rule to a randomly generated initial condition
# Keep track of:  
# 1) fraction of black and white initial pixels
# 2) Timesteps
#Step 3: End simulation when condition at time T = condition at time T - 1 OR if timestep > 200 (exact number?)

#Step 4: Compare classification rates of final condition to inital condition and keep all rules that generate > 80% accuracy averaged across all their samples
#Step 5: Determine average time to 80% accurracy for each rule and rank
#Step 6: Clone fastest 1% of rules
#Step 7: Random swap between 99% of rules

	#This defines one generation of the GA; it is repeated G times for one run of the GA. An experiment consists of a set of runs with identical parameters but different random number seeds.
	#Our experiments used single-point crossover, which takes two strings, selects a position at random, and forms two offspring by exchanging parts of the strings before and after that position. Mutation consists of flipping a randomly chosen bit in a string.

#Step 8: Random mutate 2 points in rules with < 80% accuracy
#Step 9: Coda Step 2

#Step 1:
IC.length <- 25  #length of Initial condition string
rule.size <- 5  #number of rules
time.steps <- 150  #number of timesteps
IC.size <- 40 #number of initial conditions
iter.size <- 200 #number of generations
k <- 2
neighborhood <- 2
bit.length <- 2^(neighborhood * 2 +1)


#initial rule table
rule.list <- list()
for(i in 0:(bit.length - 1)){
ID <- substr(paste(rev(as.integer(intToBits(i))), collapse=""), start = (32 - (neighborhood * 2 +1) + 1), stop = 32)
rule.list[[as.character(i)]] <- ID
#rule <- sample(c(0,1), size = 1, replace = T)
#rule.list[[as.character(i)]] <- data.frame(ID, rule, rr)
}
#Matrix of 50 rules, with neighborhood string as rowname
rule.mat <- matrix(data = NA, nrow = bit.length, ncol = rule.size)
IDS <- do.call(rbind, rule.list)
rownames(rule.mat) <- IDS[,1]


#Initial rules
for (r in 1:rule.size){
	rule <- sample(c(0,1), bit.length, replace = T)
	rule.mat[,r] <- rule
	}

#each column is a chromosome




iter.list <- list()
#The following applies the current CA rules, assesses fitness, ranks chromosomes, splits/combines chromosomes, then updates ruleset
for (iter in 1:iter.size){
fit.chrom <- list()

for (rr in 1:rule.size){  #number of rules

rule1 <- list()
for (ic in 1:IC.size){   # number of initial conditions to test
IC <- sample(c(0,1), size = IC.size, replace = T)  #initial condition
IC <- paste(IC, collapse = "")
t.list <- list()  
t.list[[as.character(1)]] <- IC
for (t in 1:time.steps){  # number of timesteps
i.list <- list()
str <- t.list[[as.character(t)]]
for (i in 1:IC.length){ #length of initial condition
	sta <- ifelse(i - neighborhood < 0, 0, i - neighborhood)
	sto <- ifelse(i + neighborhood > IC.length, IC.length, i + neighborhood)
	
	if(i + 3 < IC.size & i - 3 > 0){  #wrapping around for full string
				                     string = substr(str, start = sta, stop = sto)
				                     }
				                     
	if(i + 3 > IC.size){
				        string = paste0(substr(str, start = sta, stop = sto),
	                                      substr(str, start = 1, stop = i + neighborhood - IC.length))
	                     }
	                     
	if(i - 3 < 0){
	              string = paste0(substr(str, start = sta, stop = sto),
	                              substr(str, start = i - neighborhood + IC.size, stop = IC.length))
	             }
	             
	             	
	i.list[[as.character(i)]] <- rule.mat[grep(string, rownames(rule.mat)),rr]  #output
	
	
	}
t.list[[as.character(t + 1)]] <- paste(do.call(rbind, i.list), collapse = "")
	
}
	
tlist <- as.data.frame(do.call(rbind, t.list))

#does the last timestep have all 1 if pr(IC) > .5, all 0 if pr(IC) < .5 or 0/1 if pr(IC) = .5?
freq.IC <- sum(as.numeric(strsplit(as.vector(tlist[1,]), "")[[1]]))/100 
last <- ifelse(dim(unique(tlist))[1] == time.steps+1, time.steps+1, as.numeric(which(duplicated(tlist))[1]))
freq.last <- sum(as.numeric(strsplit(as.vector(tlist[last,]), "")[[1]]))/100

fitness <- ifelse(freq.IC > 0.5 & freq.last == 1 | freq.IC < 0.5 & freq.last == 0 | freq.IC == 0.5 & freq.last == 0.5, 1, 0)


rule1[[as.character(ic)]] <- data.frame(fitness, last)
}
avg.fit <- sum(do.call(rbind, rule1)[,"fitness"])/ic  #average fitness
avg.speed <- sum(do.call(rbind, rule1)[,"last"])/ic
chromosome <- paste(rule.mat[,rr], collapse = "")  #rule chromosome
fit.chrom[[rr]] <- data.frame(avg.fit, avg.speed, "chromosome" = as.character(chromosome))
}

speed.fit.chrom <- do.call(rbind, fit.chrom)
speed.fit.chrom$rank <- ifelse(speed.fit.chrom$avg.fit > 0.3, rank(speed.fit.chrom$avg.speed), NA)  #has to have accuracy > 0.6 & fastest

#new rule table:

#split, combine
split <- sample(1:bit.length, 1)  #random split location
speed.fit.chrom$chromosome <- as.character(speed.fit.chrom$chromosome)
speed.fit.chrom$newchrom1 <- ifelse(speed.fit.chrom$rank > 7 | is.na(speed.fit.chrom$rank), substr(speed.fit.chrom$chromosome, start = 0, stop = split), NA)
speed.fit.chrom$newchrom2 <- ifelse(speed.fit.chrom$rank > 7 | is.na(speed.fit.chrom$rank), substr(speed.fit.chrom$chromosome, start = split+1, stop = bit.length), NA)
speed.fit.chrom$newchrom <- paste0(speed.fit.chrom$newchrom1,ifelse(!is.na(speed.fit.chrom$newchrom2), speed.fit.chrom[sample(rownames(speed.fit.chrom[!is.na(speed.fit.chrom$newchrom2),])),"newchrom2"], NA))
speed.fit.chrom$newchrom <- ifelse(speed.fit.chrom$newchrom == "NANA", speed.fit.chrom$chromosome, speed.fit.chrom$newchrom)


#new ruleset
for (r in 1:rule.size){
	rule.mat[,r] <- as.numeric(strsplit(as.vector(speed.fit.chrom[r,"newchrom"]), "")[[1]])
	}
	
iter.list[[as.character(iter)]] <- data.frame("speed" = speed.fit.chrom[speed.fit.chrom$rank == 1,"avg.speed"],"accuracy" = speed.fit.chrom[speed.fit.chrom$rank == 1,"avg.fit"])
}

