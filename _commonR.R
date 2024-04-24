knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      out.width = "60%",
                      fig.align = "center",
                      cache = FALSE)
options(
    htmltools.dir.version = FALSE,
    formatR.indent = 2,
    width = 70,#65,
    digits = 4,
    tibble.print_max = 5,
    tibble.print_min = 5
)

library(tidyverse)
theme_set(theme_classic())
library(micsr)
library(micsr.data)
