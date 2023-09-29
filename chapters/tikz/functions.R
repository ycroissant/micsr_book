dg <- function(x) x / pi * 180

Pmat <- function(x){
    if (! is.matrix(x)) x <- matrix(x, ncol = 1)
    x %*% solve(crossprod(x)) %*% t(x)
}
    
pastevect <- function(x) paste("(", paste(x, collapse = ","), ")", sep = "")

draw3D <- function(x, name = NULL, vect = TRUE, linetype = NULL, lab = 1, pos = "right", xo = NULL, color = NULL){
    if (is.null(xo)) xo <- c(0, 0, 0)
    if (is.null(linetype)) linetype <- "solid"
    if (is.null(color)) color <- "black"
    labpos <- (1 - lab) * xo + lab * x
    vect <- ifelse(vect, "->, >=stealth", "")
    result <- paste("\\draw [", linetype, ",", color, ",", vect, "]", pastevect(xo), " -- ", pastevect(x), ";", sep = "")
    ## if (! is.null(name)){
    ##     name <- paste("node[", pos, "]{$", name, "$}", sep = "")
    ##     result <- paste(result, name)
    ## }
    if (! is.null(name)){
        name <- paste("\\draw ", pastevect(labpos), "node[", pos, "]{$", name, "$};", sep = "")
        result <- paste(result, name)
    }
    result
}

drawvect <- function(x, name = NULL, coord = FALSE, pos = "right", label = FALSE, xo = NULL, vect = TRUE, linetype = NULL){
    if (is.null(xo)) xo <- c(0, 0)
    if (is.null(linetype)) linetype = "solid"
    result <- paste("\\draw [", linetype, ",", ifelse(vect, "->, >=stealth", ""), "] (", xo[1], ",", xo[2], ")--(",
                    x[1], ",", x[2], ")")
    if (! is.null(name)){
        node <- paste("node[", pos, "]{$", name, "$};", sep = "")
        result <- paste(result, node)
    }
    else result <- paste(result, ";")
    if (coord){
        coord <- paste("\\draw[dotted] (", x[1], ",0)--(", x[1], ",", x[2], ");")
        coord <- paste(coord,
                       "\\draw[dotted] (0,",x[2], ")--(", x[1], ",", x[2], ");")
        result <- paste(result, coord)
    }
    if (label){
        posx <- ifelse(x[2] > 0, "below", "above")
        posy <- ifelse(x[1] > 0, "left", "right")
        label <- paste("\\draw (", x[1], ",0) node[", posx, "] {$", x[1], "$};")
        label <- paste(label,
                       paste("\\draw (0,", x[2],") node[", posy, "] {$", x[2], "$};"))
        result <- paste(result, label)
    }
    paste(result, "un", "\n")
}
