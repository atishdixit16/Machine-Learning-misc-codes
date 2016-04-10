R_square <- function(y,y_pred) {
	return(1 - (sum((y-y_pred)^2)/sum((y-mean(y))^2)) )
}
