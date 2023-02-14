mod_1_update <- function(settings){

#### VARIABLES

cooperationForm <- c("None",
                     "Played in band",
                     "Played with another musician",
                     "Played with another band")

coopCond <- cooperationForm[3:4]

#### UI

ui <- function(id, session){
  ns <- NS(id)

  tags$table(style = "width: 95%;",
             tags$tr(style = "height: 85%;",
                     tags$td(id = "left_1", ### input
                             h2(icon("gear"), "Settings"),
                             shinyWidgets::radioGroupButtons(inputId = ns('performer_type_1'),
                                               label = 'Select performer type',
                                               choices = settings$performerType,
                                               status = "primary",
                                               checkIcon = list(
                                                 yes = icon("ok",
                                                            lib = "glyphicon"),
                                                 no = icon("remove",
                                                           lib = "glyphicon"))),
                             shiny::textInput(inputId = ns("performer_name_1"),
                                       label = "Enter performer's name",
                                       value = ""),
                             shinyBS::bsTooltip(ns("performer_name_1"),
                                       HTML(paste0("<b>It is strongly recommended to introduce first a family name,",
                                                   br(),
                                                   "then a given name with a comma as separator.</b>")),
                                       "right",
                                       options = list(container = "body")),
                             shiny::conditionalPanel(paste0("input['",
                                                     ns("performer_type_1"),
                                                     "'] == 'Musician'"),
                                              shinyWidgets::radioGroupButtons(inputId = ns('cooperation_form_1_1'),
                                                                label = "Select cooperation form",
                                                                choices = cooperationForm[1:3],
                                                                selected = "None",
                                                                status = "primary",
                                                                checkIcon = list(
                                                                  yes = icon("ok",
                                                                             lib = "glyphicon"),
                                                                  no = icon("remove",
                                                                            lib = "glyphicon"))),
                                              shiny::conditionalPanel(paste0("input['",
                                                                      ns("cooperation_form_1_1"),
                                                                      "'] != 'None'"),
                                                               shiny::textInput(inputId = ns("cooperator_name_1_1"),
                                                                         label = "Enter cooperator name",
                                                                         value =""),
                                                               shinyBS::bsTooltip(ns("cooperator_name_1_1"),
                                                                         HTML(paste0("<b>It is strongly recommended to introduce first a family name,",
                                                                                     br(),
                                                                                     "then a given name with a comma as separator.</b>")),
                                                                         "right",
                                                                         options = list(container = "body")))),
                             shiny::conditionalPanel(paste0("input['",
                                                     ns("performer_type_1"),
                                                     "'] =='Band'"),
                                              shinyWidgets::radioGroupButtons(inputId = ns('cooperation_form_1_2'),
                                                                label = "Select cooperation form",
                                                                choices = cooperationForm[c(1,4)],
                                                                selected = "None",
                                                                status = "primary",
                                                                checkIcon = list(
                                                                  yes = icon("ok",
                                                                             lib = "glyphicon"),
                                                                  no = icon("remove",
                                                                            lib = "glyphicon"))),
                                              shiny::conditionalPanel(paste0("input['",
                                                                      ns("cooperation_form_1_2"),
                                                                      "'] != 'None'"),
                                                               shiny::textInput(inputId = ns("cooperator_name_1_2"),
                                                                         label = "Enter cooperator name",
                                                                         value = "")
                                              )
                             ),
                             shinyWidgets::actionBttn(inputId=ns("insertion_1"),
                                        label = "Insert introduced data into table",
                                        style = "gradient",
                                        color = "primary",
                                        icon = icon("table")),
                             shinyBS::bsTooltip(ns("insertion_1"), HTML(paste0("<b>You have to click to save the introduced data.",
                                                                      br(),
                                                                      "Otherwise you will lose it :(</b>")),
                                       "right", options = list(container = "body")),
                             br(),
                             br(),
                             shinyWidgets::actionBttn(inputId=ns("transmission_1"),
                                        label = "Transfer table",
                                        style = "gradient",
                                        color = "primary",
                                        icon = icon("paper-plane")),
                             shinyBS::bsTooltip(ns("transmission_1"), HTML(paste0("<b>You have to click to send the introduced data.",
                                                                         br(),
                                                                         "Otherwise there will be no action :)</b>")),
                                       "right", options = list(container = "body"))),
                     tags$td(id = "right_1", ### output
                             DT::dataTableOutput(ns("insertion_table_1")),
                             shinyBS::bsTooltip(ns("insertion_table_1"),  HTML(paste0("<b>You can sort the table by columns, both increasingly and decreasingly.",
                                                                             br(),
                                                                             "Row filtering is also possible in the window ",
                                                                             "&#39;",
                                                                             "Search",
                                                                             "&#39;",
                                                                             "</b>")),
                                       "top", options = list(container = "body")),
                             h1(shiny::htmlOutput(ns("text_info_1")))
                     )
             )
  )




}

### SERVER

server <- function(id){
  moduleServer(id,
               function(input, output, session){

                 #### Data frame of new data
                 insDf <- reactiveValues(df = NULL)

                 ### warning info or transfer status
                 text_Info <- NULL

                 #### memory reservetion of output
                 output$insertion_table_1 <- NULL
                 output$text_info_1 <- NULL

                 #### Data upload
                 observeEvent(input$insertion_1, {
                   if(input$performer_name_1 == ""){ # first warning
                     output$text_info_1 <- renderText({HTML(paste0("<b>Please, enter performer's name first</b>"))})
                   }else if(switch(input$performer_type_1,
                                   "Musician" = switch(input$cooperation_form_1_1,
                                                       "None" = FALSE,
                                                       {"" %in% input$cooperator_name_1_1}),
                                   "Band" =  switch(input$cooperation_form_1_2,
                                                    "None" = FALSE,
                                                    {"" %in% input$cooperator_name_1_2}) # second warning
                   )){
                     output$text_info_1 <- shiny::renderText({HTML(paste0("<b>Please, enter cooperator's name first</b>"))})
                   }else{ # main work
                     df <- data.frame(performerType = input$performer_type_1,
                                      performerName = input$performer_name_1,
                                      cooperationForm = switch(input$performer_type_1,
                                                               "Musician" = input$cooperation_form_1_1,
                                                               "Band" = input$cooperation_form_1_2),
                                      cooperatorName = switch(input$performer_type_1,
                                                              "Musician" = switch(input$cooperation_form_1_1,
                                                                                  "None" = "None",
                                                                                  input$cooperator_name_1_1),
                                                              "Band" = switch(input$cooperation_form_1_2,
                                                                              "None" = "None",
                                                                              input$cooperator_name_1_2),
                                                              "None"),
                                      createdAt = as.character(Sys.time())
                     )
                     # mutual relation
                     if (input$performer_type_1 == "Musician" & input$cooperation_form_1_1 == coopCond[1]){
                       df <- dplyr::bind_rows(df,
                                       data.frame(performerType = "Musician",
                                                  performerName = input$cooperator_name_1_1,
                                                  cooperationForm = coopCond[1],
                                                  cooperatorName = input$performer_name_1,
                                                  createdAt = as.character(Sys.time())))

                     }
                     # mutual relation
                     if (input$performer_type_1 == "Band" & input$cooperation_form_1_2 == coopCond[2]){
                       df <- dplyr::bind_rows(df,
                                       data.frame(performerType = "Band",
                                                  performerName = input$cooperator_name_1_2,
                                                  cooperationForm = coopCond[2],
                                                  cooperatorName = input$performer_name_1,
                                                  createdAt = as.character(Sys.time())))

                     }    # new row(s) biding

                     insDf$df <- "if"(is.null(insDf$df),
                                      df,
                                      dplyr::bind_rows(insDf$df, df)
                     )

                     output$text_info_1 <- NULL

                     output$insertion_table_1 <- DT::renderDataTable({DT::datatable(insDf$df %>% rename_with(~settings$DTColumns) %>% dplyr::mutate(rowGroup = 1:dplyr::n()%% 2),
                                                                                    rownames = TRUE,
                                                                                    options = list(
                                                                                      pageLength = 20,
                                                                                      columnDefs = list(list(visible=FALSE,
                                                                                                             targets=6)
                                                                                      ))) %>% formatStyle(6,
                                                                                                          target='row',
                                                                                                          backgroundColor = styleEqual(c(1,0), settings$alternateColors))
                     }
                     )

                   }})
                 #### Data transfer
                 observeEvent(input$transmission_1, {
                   if(is.null(insDf$df)){
                     transmissionInfo <- HTML(paste0('<b>Please, insert data into table first</b>'))
                   }else{
                     export_data(conn=settings$myDB, data = insDf$df %>% select(-createdAt), my_table = settings$myTable, initial=FALSE)
                     output$insertion_table_1 <- NULL
                     insDf$df <- NULL
                     transmissionInfo <- HTML(paste0('<b>Data sent :)</b>'))}
                   output$text_info_1 <- shiny::renderText({transmissionInfo})

                 })
               })}

return(list(ui=ui, server=server))

}
