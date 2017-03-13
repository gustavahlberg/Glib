library(devtools)
# devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("./Glib")
document()

setwd("..")
install("Glib")



