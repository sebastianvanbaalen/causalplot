#' Launch the causalplot Shiny app
#'
#' Opens an interactive Shiny application for building causal diagrams
#' and typology plots using the causalplot package.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @return This function does not return a value; it launches a Shiny app.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the app. Install it with install.packages('shiny').",
         call. = FALSE)
  }
  app_dir <- system.file("shinyApp", package = "causalplot")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Try reinstalling the causalplot package.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
