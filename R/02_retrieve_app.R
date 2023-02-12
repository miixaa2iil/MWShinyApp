### UI
retrieve_ui <- function(id){
  
  ns <- NS(id)
  tags$table(style = "width: 95%;",
             tags$tr(style = "height: 85%;",
                     tags$td(id = "left_2", ### input
                             h2(icon("gear"), "Settings"),
                             radioGroupButtons(ns('performer_type_2'), 
                                               label = "Select performers' type",
                                               choices = performersType,
                                               status = "primary",
                                               checkIcon = list(
                                                 yes = icon("ok", 
                                                            lib = "glyphicon"),
                                                 no = icon("remove",
                                                           lib = "glyphicon"))),
                             conditionalPanel(paste0("input['",
                                                     ns("performer_type_2"),
                                                     "'] == 'Musicians'"),
                                              pickerInput(inputId = ns("musicians_names_2"),
                                                          label = "Select or enter musicians' names",
                                                          choices =   unlist(get_performers(myDB, "Musician"), use.names = FALSE),
                                                          multiple = TRUE,
                                                          options = list(`actions-box` = TRUE)
                                              )
                             ),
                             conditionalPanel(paste0("input['",
                                                     ns("performer_type_2"),
                                                     "'] == 'Bands'"),
                                              pickerInput(inputId = ns("bands_names_2"),
                                                          label = "Select or enter bands' names",
                                                          choices =   unlist(get_performers(myDB, "Band"), use.names = FALSE),
                                                          multiple = TRUE,
                                                          options = list(`actions-box` = TRUE))
                             ),
                             actionBttn(inputId=ns("relations_2"), 
                                        label = "Retrieve relations",
                                        style = "gradient",
                                        color = "primary",
                                        icon = icon("link")
                             ),
                             bsTooltip(ns("relations_2"), HTML(paste0("<b>After the selection click me",
                                                                      br(),
                                                                      "to retrieve performers", "&#39;", " relations :)</b>")),
                                       "right", options = list(container = "body")),
                             br(),
                             br(),
                             actionBttn(inputId=ns("choice_upload_2"), 
                                        label = "upload lisf of performers",
                                        style = "gradient",
                                        color = "primary",
                                        icon = icon("upload")
                             ),
                             bsTooltip(ns("choice_upload_2"), HTML(paste0("<b>Click me if you want to update the list of performers.</b>",
                                                                          br(),
                                                                          "to retrieve performers", "&#39;", " relations :)</b>")))),
                     tags$td(id="right_2", ### output
                             DT::dataTableOutput(ns("relation_table_2")),
                             bsTooltip(ns("relation_table_2"),  HTML(paste0("<b>You can sort the table by columns, both increasingly and decreasingly.",
                                                                            br(),
                                                                            "Row filtering is also possible in the window ",  
                                                                            "&#39;",
                                                                            "Search",
                                                                            "&#39;",
                                                                            "</b>")),
                                       "top", options = list(container = "body")),
                             h1(htmlOutput(ns("text_info_2")))
                     )))
}

### SERVER

retrieve_server <- function(id){
  moduleServer(id,
               function(input, output, session){
                 
                 ### Choices upload
                 reacTimer <- reactiveTimer(myInterval)


                 observeEvent(input$choice_upload_2, {
                   updatePickerInput(
                     session,
                     "musicians_names_2",
                     choices = unlist(get_performers(myDB, "Musician"), use.names = FALSE),
                     clearOptions = TRUE
                   )

                   updatePickerInput(
                     session = session,
                     "bands_names_2",
                     choices = unlist(get_performers(myDB, "Band"), use.names = FALSE),
                     clearOptions = TRUE
                   )
                 })
                 
                 ### memory reservation of outpu
                 output$relation_table_2 <- NULL
                 output$text_info_2 <- NULL
                 
                 
                 ### Table creation
                 observeEvent(input$relations_2, {
                   performers <- switch(input$performer_type_2,
                                        "Musicians" = input$musicians_names_2,
                                        "Bands" = input$bands_names_2)
                   if(is.null(performers)){
                     output$relation_table_2 <- NULL
                     output$text_info_2 <- renderText({HTML(paste0("<b>Please, select at least one performer</b>"))})
                   }else{
                     output$text_info_2 <- NULL
                     output$relation_table_2 <- renderDataTable({datatable(import_data(myDB,
                                                                                       input$performer_type_2,
                                                                                       performers) %>% group_by(`Selected performer`) %>% mutate(groupColor = (cur_group_id()-1) %% grColLen) %>% ungroup(),
                                                                           rownames = TRUE,
                                                                           options = list(
                                                                             pageLength = 20,
                                                                             columnDefs = list(list(visible=FALSE, 
                                                                                                    targets=4
                                                                                                    )
                                                                                               )
                                                                             )
                                                                           )  %>% formatStyle(4, 
                                                                                                  target='row',
                                                                                                  fontWeight = 'bold',
                                                                                                  backgroundColor = styleEqual(0:(grColLen-1), 
                                                                                                                               groupColors)
                                                                                                  )
                     })
                   }
                   
                 })
                 ###
                 
               }   )}        