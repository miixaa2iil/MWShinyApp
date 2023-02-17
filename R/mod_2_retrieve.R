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
                                                            options = list(`actions-box` = TRUE,
                                                                           `multiple-separator` = " * ",
                                                                           `none-selected-text` = "No Musician selected",
                                                                           `live-search` = TRUE,
                                                                           `live-search-normalize` = TRUE)
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
                                                            options = list(`actions-box` = TRUE,
                                                                           `multiple-separator` = " * ",
                                                                           `none-selected-text` = "No band selected",
                                                                           `live-search` = TRUE,
                                                                           `live-search-normalize` = TRUE))
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
                                         "right", options = list(container = "body"))),
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
