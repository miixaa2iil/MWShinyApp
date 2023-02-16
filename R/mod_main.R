mod_main <- function(settings, mod_1, mod_2){

mod1 <- do.call(mod_1, list(settings=settings))

mod2 <- do.call(mod_2, list(settings=settings))
### UI
ui <- function(id){
  ns <- NS(id)
  list(navbarPage(title = "Musicians' and Bands' relations",
                  fluid = TRUE,
                  theme = shinythemes::shinytheme("cerulean"),
                  collapsible = TRUE,
                  header = tagList(
                    shinyWidgets::useShinydashboard()
                  ),
                  tabPanel(title = "Data update",
                           mod1$ui(id=ns("mod_1"))
                  ),
                  tabPanel(title = "Retrieve relations",
                           mod2$ui(id=ns("mod_2"))
                  )

  ))}
### Server
server <- function(id){
  moduleServer(id,
               function(input, output, session) {
                 mod1$server(id="mod_1", parent_session = session)
                 mod2$server(id="mod_2")

               })}
list(ui=ui, server = server)
}
