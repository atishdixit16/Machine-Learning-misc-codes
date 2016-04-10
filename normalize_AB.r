#function to normalize the given vector
normalize_AB <- function(vect,a=0,b=1) {
        vect_max <- max(vect)
        vect_min <- min(vect)
        norm_vect <- NULL
        for (i in 1:length(vect) )
                norm_vect <- c(norm_vect,  ( (b-a)*( (vect[i] - vect_min) / (vect_max - vect_min) ) + a)  )
        return(norm_vect)
}

