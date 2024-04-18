


#### SETUP ####
rm(list = ls())
library(tidyverse)
library(ggpubr)
set.seed(1)


#### VARIABLES ####

#e
e <- 2.7182818284590452353602874713527

#number of repeats for permutation
repeats <- 100000

#standard deviation
sd <- 1

#standard mean
sm <- 2

#of effect sizes generated from the exponential distribution
m <- 100

#number of samples in both control and effected groups
n <- 10

#the multiple n values for testing
multiple.n <- c(2, 3, 4, 6, 8, 10, 15, 20, 25, 30)

#the multiple group sizes
multiple.m <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200)
#effect size
#es

#observed effect size
#observedes

#repeated observed effect size
#robservedes

#generated effect size
#multiple.es

#smallest possible num
IOTA <- .Machine$double.xmin

#### MAKING STORAGE ####

nalist <- list(rep(NA, length(multiple.m) * length(multiple.n)))

actualmean <- matrix(unlist(nalist), ncol = length(multiple.m))
colnames(actualmean) <- multiple.m
rownames(actualmean) <- multiple.n

actualsd <- matrix(unlist(nalist), ncol = length(multiple.m))
colnames(actualsd) <- multiple.m
rownames(actualsd) <- multiple.n

compmean <- matrix(unlist(nalist), ncol = length(multiple.m))
colnames(compmean) <- multiple.m
rownames(compmean) <- multiple.n

compsd <- matrix(unlist(nalist), ncol = length(multiple.m))
colnames(compsd) <- multiple.m
rownames(compsd) <- multiple.n

actuales <- rep(NA, m * repeats)
actuales <- matrix(actuales, ncol=repeats)
colnames(actuales) <- 1:repeats
rownames(actuales) <- c(1:m)

compes <- rep(NA, m * repeats)
dim(compes) <- c(m, repeats)
colnames(compes) <- 1:repeats
rownames(compes) <- c(1:m)

diffes <- rep(NA, m * repeats)
dim(diffes) <- c(m, repeats)
colnames(diffes) <- c(1:m)
rownames(diffes) <- c(1:m)

diffes.log <- rep(NA, m * repeats)
dim(diffes.log) <- c(m, repeats)
colnames(diffes.log) <- c(1:m)
rownames(diffes.log) <- c(1:m)

diffes.sort <- rep(NA, m * repeats)
dim(diffes.sort) <- c(m, repeats)
colnames(diffes.sort) <- c(1:m)
rownames(diffes.sort) <- c(1:m)

diffes.orig <- rep(NA, m * repeats)
dim(diffes.orig) <- c(m, repeats)
colnames(diffes.orig) <- c(1:m)
rownames(diffes.orig) <- c(1:m)

meanlist <- list(Actual=as.list(rep(NA, repeats)),
					Computed=as.list(rep(NA, repeats)),
					Difference=as.list(rep(NA, repeats))
				)
sdlist <- list(Actual=as.list(rep(NA, repeats)),
					Computed=as.list(rep(NA, repeats)),
					Difference=as.list(rep(NA, repeats))
				)


prettylinedata <- data.frame(x = rep(NA, 100))

thrownx <- list(Actual=as.list(rep(NA, repeats)),
					Computed=as.list(rep(NA, repeats)),
					Difference=as.list(rep(NA, repeats))
				)


#### DEFINING FUNCTIONS ####


#single case - monte carlo
scase <- function(es = 2, n, sm, sd){
	#making random groups from normal dist
	g1 <- rnorm(n, sm, sd)
	g2 <- rnorm(n, sm + es, sd)
	sdg1 <- sd(g1)
	sdg2 <- sd(g2)
	observedes <- ((mean(g2) - mean(g1)) / (mean(sdg1, sdg2)))
	}	


#final product maker
finalfunc <- function(fn = n, fsm = sm, fsd = sd){
	for(i in 1:repeats){
		multiple.es <- rnorm(m, 0, fsd)

		for(var in 1:m){
			actuales[var, i] <<- multiple.es[var]
			compes[var, i] <<- scase(es = multiple.es[var],  n = fn, sm = fsm, sd = fsd)
		}

	}

	diffes.orig <<- compes / actuales
	#	print(diffes.orig[1,1])
	
	diffes.log <<- diffes.orig #apply(diffes.orig, 2, log)
	#	print(diffes.log[1,1])
	
	diffes.sort <<- apply(diffes.log, 2, sort)
	#	print(diffes.sort[1,1])
	
	diffes <<- diffes.sort
	rownames(diffes) <<- 1:nrow(diffes)

}


#[(diffes.dfmod$x >= 0) && (diffes.dfmod$y >= IOTA)]


#exponential line of best fit maker
#1 is actual, 2 is computed, 3 is difference
sdmeanfuncgen <- function(data, input = 1){

	for(var in colnames(data)){
		diffes.df <- matrix(data[, var], ncol = 1)
		meanlist[[input]][[as.numeric(as.character(var))]] <<- mean(diffes.df)
		sdlist[[input]][[as.numeric(as.character(var))]] <<-  sd(diffes.df)
		}
	}	


#### GRAPHS ####


#[diffes.dfmod$x > 0]

#outputs graph of lambda distribution
outputdistgraph <- function(data){
	slopecurvedata <<- data.frame(x = unlist(as.data.frame(data, ncol = 1)))
	ggplot(slopecurvedata, aes(x)) + geom_density(fill = 'red', alpha = .5) + geom_vline(xintercept = 0, color = 'green', size = .5)
	#summary(slopecurvedata)

	}



# # #single graph of actual lambda and 
# prettylines <- function(permutation, psd = sd){

#  	prettylinedata <- data.frame(x = actuales[, permutation])
# 	prettylinedata$y <- prettylinedata$x * - lambda)))
# 	theoreticalnormal <- function(x){
#  		(1 / ((psd) * ((2 * pi) ^ .5))) * (e ^ (- (x ^ 2) / 2 * (psd ^ 2)))
#  		}
#  	prettylinedata$x2 <- compes[, permutation]
# 	prettylinedata$y2 <- meanlist[[2]][[permutation]] * ( e ^ ( - meanlist[[2]][[permutation]] * prettylinedata$x2))
# 	graph <- ggplot(prettylinedata) + geom_jitter(alpha = 1, color = 'blue', shape = 1, size = 2, aes(x = x, y = y)) + stat_function(fun = theoreticalnormal, color = 'blue') + 
#  			geom_jitter(alpha = 1, color = 'red', shape = 16, size = 2, aes(x = x2, y = y2)) + ylim(0, .75) + 
# 			xlim(0, 30)
# 	return(graph)

#  	}


#quad prettylines using slightly diffrent permutations
finalplines <- function(permutation){
	ggarrange(prettylines(permutation = permutation), prettylines(permutation = permutation - 1), prettylines(permutation = permutation - 2), prettylines(permutation = permutation - 3), nrow = 2, ncol = 2)

}


 #theoretical, and density of actual and comp
triplegraph <- function(psd = sd){
 	actuales.combine <-  as.vector(actuales)
 	compes.combine 	 <-  as.vector(compes)
 	tripledata <<- data.frame(actual = actuales.combine, comp = compes.combine)
	theoreticalnormal <- function(x){
 		(1 / ((psd) * ((2 * pi) ^ .5))) * (e ^ (- (x ^ 2) / 2 * (psd ^ 2)))
 		}
 	ggplot(tripledata) + geom_density( aes(x = comp), fill = 'red', alpha = .5) +
 						 geom_density( aes(x = actual), fill = 'blue', alpha = .5) + stat_function(fun = theoreticalnormal, color = 'green') 
 	}

#graph of means
meangraph <- function() {
meandata <<- data.frame( xdata = as.vector(unlist(meanlist[[2]])))
ggplot(meandata) + geom_density( aes(x = xdata), fill = 'red', alpha = .9) +
		 geom_vline(xintercept = 0, color = 'green', size = .5) + annotate("text", x = .6 * max(meandata), y = 3, label = paste0('The sd is : ', sd(meandata[, 1]))) + 
		 annotate("text", x = .6 * min(meandata), y = 3, label = paste0('The mean is : ', mean(meandata[, 1])))
	}

#graph of sds
sdgraph <- function() {
sddata <<- data.frame( xdata = as.vector(unlist(sdlist[[2]])))
ggplot(sddata) + geom_density( aes(x = xdata), fill = 'red', alpha = .9) +
		 geom_vline(xintercept = 1, color = 'green', size = .5) + annotate("text", x = .6 * max(sddata), y = 3, label = paste0('The sd is : ', sd(sddata[, 1]))) + 
		 annotate("text", x = .6 * min(sddata), y = 3, label = paste0('The mean is : ', mean(sddata[, 1])))
	}


#### DOIN STUFF ####


#finalfunc()
# sdmeanfuncgen(actuales)
# sdmeanfuncgen(compes, input = 2)
# sdmeanfuncgen(diffes, input = 3)
# triplegraph()
# meangraph()
# sdgraph()
#loggedlines(50)


#### FINAL FOR LOOP ####
for(m.mult in multiple.m){

	for(n.mult in multiple.n){

		actuales           <- rep(NA, m.mult * repeats)
		actuales           <- matrix(actuales, ncol=repeats)
		colnames(actuales) <- 1:repeats
		rownames(actuales) <- c(1:m.mult)

		compes             <- rep(NA, m.mult * repeats)
		compes             <- matrix(compes, ncol=repeats)
		colnames(compes)   <- 1:repeats
		rownames(compes)   <- c(1:m.mult)

		actuales.sd   <- rep(NA, repeats)
		compes.sd     <- rep(NA, repeats)
		actuales.mean <- rep(NA, repeats)
		compes.mean   <- rep(NA, repeats)

		for(i in 1:repeats){
			multiple.es <- rnorm(m.mult, 0, 1)

			for(var in 1:length(multiple.es)){
				actuales[var, i] <- multiple.es[[var]]
				compes[var, i] <- scase(es = multiple.es[var],  n = n.mult, sm = 2, sd = 1)
				}
			actuales.sd <- sd(actuales[, i])
			compes.sd   <- sd(compes[, i])
			actuales.mean <- mean(actuales[, i])
			compes.mean   <- mean(compes[, i])
			}
		actualsd[as.character(n.mult), as.character(m.mult)]   <- mean(actuales.sd)
		actualmean[as.character(n.mult), as.character(m.mult)] <- mean(actuales.mean)
		compsd[as.character(n.mult), as.character(m.mult)]     <- mean(compes.sd)
		compmean[as.character(n.mult), as.character(m.mult)]   <- mean(compes.mean)

		}
	}

# save(actualsd, file = 'actualsd100kperm')
# save(compsd, file = 'compsd100kperm')
# save(actualmean, file = 'actualmean100kperm')
# save(compmean, file = 'compmean100kperm')