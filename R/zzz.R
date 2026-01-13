.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Thank you for using the causalplot package. Have fun theorizing!"
  )
}

utils::globalVariables(c("x", "y", "id", "fill", "xend", "yend", "label_display"))
