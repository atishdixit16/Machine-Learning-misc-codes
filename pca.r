pca <- function(data,varFract) {
	output <- as.matrix(data) %*% (eigen(cov(data))$vectors)
	eigenValues <- sort(eigen(cov(data))$values)
	for (i in 1:length(eigenValues))
		if ((sum(eigenValues[1:i]) / sum(eigenValues)) > varFract)
			break;
	return(output[,1:i])
}
