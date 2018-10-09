
add2 <- function(x,y){
    x + y
}

above10 <- function(x){
    use <- x > 10
    x[use]
}

aboveThan <- function(x, n = 10){
    use <- x > n
    x[use]
}

column_mean <- function(m, remove_na = TRUE){
    nc <- ncol(m)
    means <- numeric(nc)
    for(i in 1:nc){
        means[i] <- mean(m[,i],na.rm = remove_na)
    }
    means
}

power <- function(n){
    pow <- function(x){
        x^n
    }
    pow
}

cube <- function(x, n) {
    x^3
}
f <- function(x) {
    g <- function(y) {
        y + z
    }
    z <- 4
    x + g(x)
}
