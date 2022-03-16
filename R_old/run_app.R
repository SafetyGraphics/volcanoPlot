#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @import shiny
#' @import shinyjs
#' @import data.table
#' @import DT
#' @import ggplot2
#' @import plotly
#' @import scales
#' @import shinycssloaders
#' @import dplyr
#' @import tidyr
#' @import survival
#' @import survminer
#' @import fmsb
#' @import cowplot
#' @import shinyWidgets



run_app <- function(onStart = NULL, options = list(), enableBookmarking = NULL) {
  print("running app")
  app = shinyApp(
    ui = app_ui,
    server = app_server,
    onStart = onStart,
    options = options,
    enableBookmarking = enableBookmarking
  )
  runApp(app, launch.browser = TRUE)
}
