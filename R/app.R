source('musband.R', encoding = 'UTF-8')

ui <- fluidPage(
  span(p(musband_ui(id = "musband"))))

server <-function(input, output, session) {
                 musband_server("musband")}

shinyApp(ui, server)