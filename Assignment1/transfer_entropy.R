#JIDT
install.packages("rJava")
if (Sys.info()['sysname'] == 'Darwin') {
  libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
  message (paste0('Load libjvm.dylib from: ',libjvm))
  dyn.load(libjvm)
}

library(rJava)
.jinit()
setwd("~/Desktop/Classes/Spring 2018/Complex_adaptive_systems/infodynamics-dist-1.4")
.jaddClassPath("./infodynamics.jar")
teCalc<-.jnew("infodynamics/measures/discrete/TransferEntropyCalculatorDiscrete", 2L, 1L)

#from examples.r from jlizier github
sourceArray <- sample(0:1, 100, replace = T)
destArray <- c(0L, sourceArray[1:99]);
sourceArray2 <- sample(0:1, 100, replace = T)

teCalc<-.jnew("infodynamics/measures/discrete/TransferEntropyCalculatorDiscrete", 2L, 1L)
.jcall(teCalc, "V","initialise")
.jcall(teCalc,"V","addObservations",sourceArray,destArray)

result1 <- .jcall(teCalc,"D","computeAverageLocalOfObservations")
cat("For copied source, result should be close to 0 bits: ", result1, "\n")

.jcall(teCalc, "V", "initialise")
.jcall(teCalc,"V","addObservations",sourceArray2,destArray)
result2 <- .jcall(teCalc,"D","computeAverageLocalOfObservations")
cat("For random source, result should be close to 0 bits: ", result2, "\n")
