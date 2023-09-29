files <- c("OLS2D", "vectors2D", "OLS3D", "frishWaugh")
for (i in files){
    Sweave(paste("./Rnw/", i, ".Rnw", sep = "")) ;
    system(paste("pdflatex ", i, ".tex", sep = ""))
    system(paste("convert -density 600 ", i, ".pdf ", i, ".png", sep = ""))
    system(paste("rm ", paste(i, c("aux", "log", "tex"), sep = ".", collapse = " ")))
    system(paste("mv ", i, ".pdf ", i, ".png ./fig", sep = ""))
}
