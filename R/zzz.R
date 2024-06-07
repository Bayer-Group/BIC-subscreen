.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the Subgroup Explorer!")
  shiny::addResourcePath('www', system.file("www", package = "subscreen"))
}
