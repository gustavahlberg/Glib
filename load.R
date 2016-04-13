library(devtools)
library(roxygen2)

setwd("./Glib")
document()

setwd("..")
install("Glib")



