linearRegression <- function(x_train,y_train,x_test) {
	x_train <- cbind(1,x_train)
	beta <- solve(t(x_train)%*%x_train)%*%(t(x_train)%*%y_train)
	return (x_test%*%beta)
}
