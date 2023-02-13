run_mw_app <- function() {

  ### DATA
  settings <- app_settings()
  data("initial_data")
  export_data(conn = settings$myDB,
              my_table = settings$myTable,
              data = initData,
              initial = TRUE)



  modMain  <- mod_main(settings=settings,
                       mod_1 = "mod_1_update",
                       mod_2 = "mod_2_retrieve")

  ui <- fluidPage(
    span(p(modMain$ui(id = "main"))))

  server <-function(input, output, session) {
    modMain$server("main")}

  shiny::shinyApp(ui, server)
}
