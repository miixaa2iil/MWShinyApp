mod_2_retrieve <- function(settings){
  ### UI
ui <- function(id){

    ns <- NS(id)
    tags$table(style = "width: 95%;",
               tags$tr(style = "height: 85%;",
                       tags$td(id = "left_2", ### input
                               h2(icon("gear"), "Settings"),
                               shinyWidgets::radioGroupButtons(ns('performer_type_2'),
                                                 label = "Select performers' type",
                                                 choices = settings$performersType,
                                                 status = "primary",
                                                 checkIcon = list(
                                                   yes = icon("ok",
                                                              lib = "glyphicon"),
                                                   no = icon("remove",
                                                             lib = "glyphicon"))),
                               shiny::conditionalPanel(paste0("input['",
                                                       ns("performer_type_2"),
                                                       "'] == 'Musicians'"),
                                                shinyWidgets::pickerInput(inputId = ns("musicians_names_2"),
                                                            label = "Select or enter musicians' names",
                                                            choices =   unlist(get_performers(conn = settings$myDB,
                                                                                              my_table = settings$myTable,
                                                                                              performer_type = "Musician"),
                                                                               use.names = FALSE),
                                                            multiple = TRUE,
                                                            options = list(`actions-box` = TRUE)
                                                )
                               ),
                               shiny::conditionalPanel(paste0("input['",
                                                       ns("performer_type_2"),
                                                       "'] == 'Bands'"),
                                                shinyWidgets::pickerInput(inputId = ns("bands_names_2"),
                                                            label = "Select or enter bands' names",
                                                            choices =   unlist(get_performers(conn=settings$myDB,
                                                                                              my_table = settings$myTable,
                                                                                              performer_type = "Band"
                                                                                              ),
                                                                               use.names = FALSE),
                                                            multiple = TRUE,
                                                            options = list(`actions-box` = TRUE))
                               ),
                               shinyWidgets::actionBttn(inputId=ns("relations_2"),
                                          label = "Retrieve relations",
                                          style = "gradient",
                                          color = "primary",
                                          icon = icon("link")
                               ),
                               shinyBS::bsTooltip(ns("relations_2"), HTML(paste0("<b>After the selection click me",
                                                                        br(),
                                                                        "to retrieve performers", "&#39;", " relations :)</b>")),
                                         "right", options = list(container = "body")),
                               br(),
                               br(),
                               shinyWidgets::actionBttn(inputId=ns("choice_upload_2"),
                                          label = "upload lisf of performers",
                                          style = "gradient",
                                          color = "primary",
                                          icon = icon("upload")
                               ),
                               shinyBS::bsTooltip(ns("choice_upload_2"), HTML(paste0("<b>Click me if you want to update the list of performers.</b>",
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
                               h1(shiny::htmlOutput(ns("text_info_2")))
                       )))
  }

  ### SERVER

  server <- function(id){
    moduleServer(id,
                 function(input, output, session){

                   ### Choices upload
                   observeEvent(input$choice_upload_2, {
                     shinyWidgets::updatePickerInput(
                       session,
                       "musicians_names_2",
                       choices = unlist(get_performers(conn=settings$myDB,
                                                       my_table = settings$myTable,
                                                       performer_type = "Musician"),
                                        use.names = FALSE),
                       clearOptions = TRUE
                     )

                     shinyWidgets::updatePickerInput(
                       session,
                       "bands_names_2",
                       choices = unlist(get_performers(conn=settings$myDB,
                                                       my_table = settings$myTable,
                                                       performer_type = "Band"),
                                        use.names = FALSE),
                       clearOptions = TRUE
                     )
                   })

                   output$relation_table_2 <- NULL
                   output$text_info_2 <- NULL


                   ### Table creation
                   observeEvent(input$relations_2, {
                     performers <- switch(isolate(input$performer_type_2),
                                          "Musicians" = input$musicians_names_2,
                                          "Bands" = input$bands_names_2)
                     if(is.null(performers)){
                       output$relation_table_2 <- NULL
                       output$text_info_2 <- shiny::renderText({HTML(paste0("<b>Please, select at least one performer</b>"))})
                     }else{
                       output$text_info_2 <- NULL
                       output$relation_table_2 <- DT::renderDataTable({DT::datatable(import_data(conn = settings$myDB,
                                                                                             my_table = settings$myTable,
                                                                                             performer_type = isolate(input$performer_type_2),
                                                                                             performer_names = isolate(performers)) %>% dplyr::group_by(`Selected performer`) %>% dplyr::mutate(groupColor = (dplyr::cur_group_id()-1) %% settings$grColLen) %>% dplyr::ungroup(),
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
                                          backgroundColor = styleEqual(0:(settings$grColLen-1),
                                                                       settings$groupColors)
                       )
                       })
                     }

                   })
                   ###

                 }   )}


  return(list(ui=ui, server=server))

}
