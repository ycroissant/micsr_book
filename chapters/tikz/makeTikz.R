args <- commandArgs(trailingOnly = TRUE)
thefile <- args[1]
xcm <- as.numeric(args[2])
ratio <- as.numeric(args[3])
convert <- as.numeric(args[4])
if (is.na(args[2])) xcm <- 10
if (is.na(args[3])) ratio <- 10
if (is.na(args[4])) convert <- FALSE else convert <- TRUE

ycm <- ratio * xcm
Sweave(paste("./Rnw/", thefile, ".Rnw", sep = ""))
system(paste("pdflatex ", thefile, sep = ""))
rmfiles <- paste(thefile, c(".aux", ".log", ".tex"), sep = "", collapse = " ")
if (convert){
#    system(paste("convert -density 800 ", thefile, ".pdf -resize 500 ", thefile, ".png", sep = ""))
    system(paste("pdftoppm -png ", thefile, ".pdf ", "zz", sep = ""))
    system(paste("mv zz-1.png ", thefile, ".png", sep = ""))
    system(paste("rm ", rmfiles, sep = ""))
    system(paste("mv ", thefile, ".png ./fig", sep = ""))
    system(paste("mv ", thefile, ".pdf ./fig", sep = ""))
}


