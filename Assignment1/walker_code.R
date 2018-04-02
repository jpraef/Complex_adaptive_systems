#Transfer entropy of logistic growth models from Walker et al.
#model system defined as function of local dynamics, total number of elements, current time step, and global coupling strength to the instaneous dynamics of the mean field
# mn = 1/n * sum( fj(xj,n)) which directly affects all local elements 
# initialized with xi,0 for all elements i, representing initial pop size of one for each poopulation
# ri values randomly drawn from [3.9, 4.0]
#N = 1000 coupled logistic maps
#generaled time series of local elements (xi,n) and mean field Mn
#determined causal directionality and flow of information determined.
#carrying capacity = 100

#so I need to do some number of timesteps for 1000 popultations, with each timestep the whole population
log_growth_walker <- function(xt,R, e, mn){
   (1- e) * (R * xt *(1 - (xt/100))) + e * mn #R is between 3.9-4.0
}

#mn is a function of all the populations at timestep i. how is it a function? mean, var, what?  Hmm. Maybe if I assume that the entire population follows a logistic growth map as well
#K here is the number of populations = 1000
log_growth_mn <- function(xti, R){
  (R * xti * (1 - (xti/100)))
}
#need to recursively update with the mean field dynamic mn
df.walker1 <- data.frame("timestep" = rep(0:1000, each = 1000),
                        "n" = rep(1:1000, 1001),
                        "R" = rep(
                          sample(
                            seq(3.9,4.0,
                                by = 0.0001),
                            1000,
                            replace = T),
                          1001),
                        "xt" = NA,
                        "xt_1" = NA,
                        "e" = 0,
                        "mn" = NA,
                        "mn_1" = NA)

df.walker2 <- data.frame("timestep" = rep(0:1000, each = 1000),
                         "n" = rep(1:1000, 1001),
                         "R" = rep(
                           sample(
                             seq(3.9,4.0,
                                 by = 0.0001),
                             1000,
                             replace = T),
                           1001),
                         "xt" = NA,
                         "xt_1" = NA,
                         "e" = 0.075,
                         "mn" = NA,
                         "mn_1" = NA)

df.walker3 <- data.frame("timestep" = rep(0:1000, each = 1000),
                         "n" = rep(1:1000, 1001),
                         "R" = rep(
                           sample(
                             seq(3.9,4.0,
                                 by = 0.0001),
                             1000,
                             replace = T),
                           1001),
                         "xt" = NA,
                         "xt_1" = NA,
                         "e" = 0.1,
                         "mn" = NA,
                         "mn_1" = NA)

df.walker4 <- data.frame("timestep" = rep(0:1000, each = 1000),
                         "n" = rep(1:1000, 1001),
                         "R" = rep(
                           sample(
                             seq(3.9,4.0,
                                 by = 0.0001),
                             1000,
                             replace = T),
                           1001),
                         "xt" = NA,
                         "xt_1" = NA,
                         "e" = 0.2,
                         "mn" = NA,
                         "mn_1" = NA)

df.walker5 <- data.frame("timestep" = rep(0:1000, each = 1000),
                         "n" = rep(1:1000, 1001),
                         "R" = rep(
                           sample(
                             seq(3.9,4.0,
                                 by = 0.0001),
                             1000,
                             replace = T),
                           1001),
                         "xt" = NA,
                         "xt_1" = NA,
                         "e" = 0.225,
                         "mn" = NA,
                         "mn_1" = NA)

df.walker7 <- data.frame("timestep" = rep(0:1000, each = 1000),
                         "n" = rep(1:1000, 1001),
                         "R" = rep(
                           sample(
                             seq(3.9,4.0,
                                 by = 0.0001),
                             1000,
                             replace = T),
                           1001),
                         "xt" = NA,
                         "xt_1" = NA,
                         "e" = 0.25,
                         "mn" = NA,
                         "mn_1" = NA)

df.walker8 <- data.frame("timestep" = rep(0:1000, each = 1000),
                         "n" = rep(1:1000, 1001),
                         "R" = rep(
                           sample(
                             seq(3.9,4.0,
                                 by = 0.0001),
                             1000,
                             replace = T),
                           1001),
                         "xt" = NA,
                         "xt_1" = NA,
                         "e" = 0.3,
                         "mn" = NA,
                         "mn_1" = NA)

df.walker9 <- data.frame("timestep" = rep(0:1000, each = 1000),
                         "n" = rep(1:1000, 1001),
                         "R" = rep(
                           sample(
                             seq(3.9,4.0,
                                 by = 0.0001),
                             1000,
                             replace = T),
                           1001),
                         "xt" = NA,
                         "xt_1" = NA,
                         "e" = 0.4,
                         "mn" = NA,
                         "mn_1" = NA)



df.walker <- bind_rows(df.walker1,df.walker2,df.walker3,df.walker4,df.walker5,df.walker7,df.walker8,df.walker9)
df.walker[df.walker$timestep == 0, "xt"] <- 1

#for each timestep, find xt + 1 of each n in the population by first finding the population xti
walker.list <- list()
for (e in c(0.0, 0.1, 0.2, 0.225, 0.25, 0.3, 0.4)){

for (i in 0:1000){
df.walkeri <- subset(df.walker, timestep == i)
df.walkeri$mn <- mean(log_growth_mn(xti = df.walkeri$xt, R = mean(df.walkeri$R)))

  
  for (j in 1:1000){
  df.walker_j <- df.walkeri[df.walkeri$n == j,]
  r = df.walker_j$R
  xt = df.walker_j$xt
  e = df.walker_j$e
  mn = df.walker_j$mn
  df.walker[df.walker$timestep == i  & df.walker$n == j, "xt_1" ] <- log_growth_walker(xt = xt, R = r, e = e, mn = mn)
df.walker[df.walker$timestep == i +1  & df.walker$n == j, "xt" ] <- df.walker[df.walker$timestep == i  & df.walker$n == j, "xt_1" ]


  }
}

df.walker.mean <- df.walker %>%
  group_by(timestep, e)%>%
  summarize("mn" = as.numeric(mean(xt, na.rm = T)),
            "mn_1" = as.numeric(mean(xt_1, na.rm = T)))%>%
  as.data.frame()
save(df.walker.mean, df.walker, file = "~/Desktop/df.walker.mean.rda")
ggplot(df.walker.mean[1:153,]) + geom_point(aes(mn, mn_1, color = as.factor(e)))

df.walker.mean <- as