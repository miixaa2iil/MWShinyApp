source("adds/app_settings.R")
source("adds/app_libraries.R")
source("adds/app_functions.R")
source("apps/01_update_app.R", encoding = "UTF-8")
source("apps/02_retrieve_app.R", encoding = "UTF-8")


###

# select all performers name 
### Initial database
initData <- readRDS("C:/Users/miixaa2iil/Desktop/MiB/adds/initial_data.rds")
myDB <- dbConnect(RSQLite::SQLite(), "")
export_data(myDB, initData, initial = TRUE)
# musicians <- initData %>% filter(performerType == "Musicians") %>% "[["("performerName")
# bands <- initData %>% filter(performerType == "Bands") %>% "[["("performerName")


### UI
musband_ui <- function(id){ 
  ns <- NS(id)
  list(navbarPage(title = "Musicians' and Bands' relations",
                 fluid = TRUE,
                 theme = shinythemes::shinytheme("cerulean"),
                 collapsible = TRUE, 
                 header = tagList(
                   useShinydashboard()
                 ),
               tabPanel(title = "Data update",
                        update_ui(id=ns("update_1"))
                        ),
               tabPanel(title = "Retrieve relations", 
                        retrieve_ui(id=ns("retrieve_2"))
                        )
               
    ))}
###

### Server 
musband_server <- function(id){
  moduleServer(id,
               function(input, output, session) {
  update_server(id="update_1")
  retrieve_server(id="retrieve_2")

})}
####
