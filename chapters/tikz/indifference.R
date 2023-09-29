# Courbes d'indifférence

pastevect <- function(x) paste("(", paste(x, collapse = ","), ")", sep = "")

yxCES <- function(x, u = 10, alpha = 0.5, rho = 0.5){
    # équation y=f(x) de la courbe d'indifférence
    ( (u ^ rho - alpha  * x ^ rho) / (1 - alpha) ) ^ (1 / rho)
}

slopePtCES <- function(x, u = 10, alpha = 0.5, rho = 0.5){
    # pente de la courbe d'indifférence en un point
    alpha / (1 - alpha) * x[1] ^ (rho - 1) * ( (u ^ rho - alpha * x[1] ^ rho) / (1 - alpha) ) ^ ( (1 - rho) / rho)
}

ptSlopeCES <- function(a = 4, b = 1, alpha = 0.5, rho = 0.5){
    # point de la courbe d'indifférence pour une pente donnée
    Denom <- b + ( (1 - alpha) / alpha * b) ^ (1 / (1 - rho))
    xopt <- a / Denom
    yopt <- a * ( (1 - alpha) / alpha * b) ^ (1 / (1 - rho)) / Denom
    Uopt <- (alpha * xopt ^ rho + (1 - alpha) * yopt ^ rho) ^ (1 / rho)
    list(x = xopt, y = yopt, u = Uopt)
}

intDteCES <- function(a = 100, b = 1.1, alpha = 0.5, rho = 0.5, u = 40, xmin = 1, xmax = 90){
    # points d'intersection de la courbe d'indifférence avec une droite
    ff <- function(x) (alpha * x ^ rho + (1 - alpha) * (a - b * x) ^ rho - u ^ rho) ^ 2
    x1 <- nlm(ff, xmin)$estimate
    x2 <- nlm(ff, xmax)$estimate
    y1 <- a - b * x1
    y2 <- a - b * x2
    list(c(x1, y1), c(x2, y2))
}    

indCurveCES <- function(step = 1E-01, u = 10, alpha = 0.5, rho = 0.5, xmax = 20, ymax = 20){
    # points permettant de tracer la courbe d'indifférence
    yCES <- function(x)  yxCES(x, u = u, alpha = alpha, rho = rho)
    xCES <- function(y) ( (u ^ rho - (1 - alpha) * y ^ rho) /      alpha  ) ^ (1 / rho)
    xmin <- xCES(ymax)
    thexs <- seq(0, xmax, step)
    thexs <- thexs[thexs >= xmin]
    theys <- yCES(thexs)
    cbind(c(xmin, thexs), c(ymax, theys))
}

tangente <- function(point, slope, length){
    thex <- point[1]
    they <- point[2]
    l <- length / 2
    dx <- l / (2 * sqrt(1 + slope ^ 2))
    dy <- slope * dx
    list(c(thex - dx, they + dy), c(thex + dx, they - dy))
}

TIKcoords <- function(point, text = NULL, labx = NULL, laby = NULL, line = TRUE, slides = NULL, pos = "left", dot = TRUE){
    x <- point[1]
    y <- point[2]
    thepoint <- paste("(", x, ", ", y, ")")
    abs <- paste("(", x, ", 0)", sep = "")
    ord <- paste("(0, ", y, ")", sep = "")
    myslides <- if(is.null(slides)) "" else paste("<", slides, ">", sep = "")
    thestring <- c()
    if (dot) thestring <- c(thestring, TIKpoint(point, slides))
    if (! is.null(text)) thestring <- c(thestring, TIKnode(point, text, pos, slides))
    if (line) thestring <- c(thestring, TIKline(list(c(x, 0), c(x, y), c(0, y)), slides, "dotted"))
    if (! is.null(labx)) thestring <- c(thestring, TIKnode(c(x, 0), labx, "below", slides))
    if (! is.null(laby)) thestring <- c(thestring, TIKnode(c(0, y), laby, "left", slides))
    cat(paste(thestring))
}

TIKaxes <- function(x, y, labx = "", laby = ""){
    tikzCoord(0, 0, "zero")
    tikzCoord(0, y, "ymax")
    tikzCoord(x, 0, "xmax")
    tikzAnnotate("\\draw [ultra thick, <->] (ymax) -- (zero) -- (xmax);")
    tikzAnnotate(paste("\\node [right] at (xmax) {", labx, "};", sep = ""))
    tikzAnnotate(paste("\\node [above] at (ymax) {", laby, "};", sep = ""))
}

TIKline <- function(x, slides = NULL, options = NULL){
    if (is.list(x)) x <- Reduce("rbind", x)
    z <- apply(x, 1, function(v) paste("(", v[1], " ,", v[2], ")", sep = ""))
    z <- paste(z, collapse = " -- ")
    preamb <- "\\draw"
    if (! is.null(slides))  preamb <- paste(preamb, "<", slides, ">", sep = "")
    if (! is.null(options)) preamb <- paste(preamb, "[", options, "]", sep = "")
    cat(paste(preamb, z, "; \n ", sep = ""))
}

TIKmat2path <- function(x){
    z <- apply(x, 1, function(v) paste("(", v[1], " ,", v[2], ")", sep = ""))
    paste(z, collapse = " -- ")
}
    


TIKpoint <- function(x, slides = NULL, options = NULL, size = 1){
    z <- paste("(", x[1], " ,", x[2], ")", sep = "")
    if (is.null(slides)) preamb <- paste("\\draw[fill, ", options, "]", sep = "")
    else preamb <- paste("\\draw<", slides, ">[fill]", sep = "")
    cat(paste(preamb, z, " circle [radius = ", paste(size ,"pt]", sep = ""), ";\n ", sep = ""))
}

TIKnode <- function(coord, text, pos = "left", slides = NULL, options = NULL){
    preamb <- "\\node"
    if (! is.null(slides)) preamb <- paste(preamb, "<", slides, ">", sep = "")
    preamb <- paste(preamb, "[", pos, ", ", options, "] at ", sep = "")
    z <- paste(preamb, "(", paste(coord, collapse = ","), ")", sep = "")
    z <- paste(z, "{\\tiny ", text, "};\n ", sep = "")
    cat(z)
}

TIKticks <- function(coord, height){
    x <- coord[1]
    y <- if (length(coord) == 1) 0 else coord[2]
    pts1 <- c(x, - height)
    pts2 <- c(x, height)
    TIKline(list(pts1, pts2))
}
