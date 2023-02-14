#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  settings <- app_settings()
  data("initial_data")
  export_data(conn = settings$myDB,
              my_table = settings$myTable,
              data = initData,
              initial = TRUE)



  modMain  <- mod_main(settings=settings,
                       mod_1 = "mod_1_update",
                       mod_2 = "mod_2_retrieve")

  with_golem_options(
    app = shinyApp(
      ui = fluidPage(
        span(p(modMain$ui(id = "main")))),
      server =   server <-function(input, output, session) {
        modMain$server("main")},
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
