library(R.basic)
library(Rmpfr)

#############################################################################
# Experiment to look at how the replication factor should scale in a HDFS
# file system in order to maintain probability of data loss below a given
# threshold.
# 
# See Blog Post:
#
#
# John Hawkins 
# 30 October2013
#############################################################################


# Library version bugs out for large values
# This version makes use of the Rmpfr library to
# calculate the answer for larger values of N
getNChooseK <- function(numN, numK) {
	if(numN<171) {
		return(nChooseK(numN, numK))
	} else {
		# You will need the Rmpfr Library for this
		numerator = factorial(as(numN,"mpfr"))
		denominator = factorial(as(numK,"mpfr")) * factorial(as(numN-numK,"mpfr"))
		return(as.numeric(numerator/denominator))
	}
}

getProbabilityOfFailure <- function(numN, numK, numX) {
	return( getNChooseK(numN, numK) * (numX^numK) * ( (1-numX)^(numN-numK) ) )
}

calculateExact <- function(numN, numK, numX) {
	prob = 0
	for(i in numK:numN) {
		prob = prob + getProbabilityOfFailure(numN, i, numX)
	}
	return(prob)
}

calculateApprox <- function(numN, numK, numX) {
	return( getNChooseK(numN, numK) * (numX^numK) )
}

# Experiment Start Parameters
bigN = 10
bigK = 3
maxN = 1000

# Run three experiments and plot them
par(mfrow=c(3,1))

for (probabilityX in c(0.01, 0.001, 0.0001) ) {
	littleK = c(1:maxN)
	baselineProbLoss = calculateApprox(bigN, bigK, probabilityX)
	currentLittleK = 0
	for (n in 1:maxN) {
		while(1) {
			tempProb = calculateApprox(bigN+n, bigK+currentLittleK, probabilityX)
			if (tempProb<=baselineProbLoss)
				break
			currentLittleK = currentLittleK + 1
		}
		littleK[n]=currentLittleK
	}

	plot( bigN+1:maxN, littleK+bigK, pch=19, xlab="n (Nodes)",ylab="k (Replication Factor)", main=paste(" Probability of Machine Failure X=",probabilityX), ylim=c(0,littleK[maxN]+bigK+1) )
}

