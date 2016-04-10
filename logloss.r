
logloss <- function(truth,probResponse) {
	logloss <- 0
	for (i in 1:length(truth)) {
		probResponse[i] <- max(min(probResponse[i],1-1e-16),1e-16)
		logloss <- logloss + (truth[i]*log(probResponse[i])+(1-truth[i])*log(1-probResponse[i]))
	}
	return(-logloss/length(truth))
}
