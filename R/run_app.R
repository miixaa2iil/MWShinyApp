run_app <- function() {

####################### UTILS ###################################################################

  #### PACKAGES

  require(dplyr)
  require(DT)
  require(RSQLite)
  require(shiny)
  require(shinyBS)
  require(shinydashboard)
  require(shinyjs)
  require(shinythemes)
  require(shinyWidgets)
  require(tibble)


  #### PARAMETERS

  performerType <- c("Musician",
                     "Band"
  )

  performersType <- paste0(performerType, "s")
  primaryColor <-  "#0073b7"
  secondaryColor <- "#00c0ef"
  alternateColors <-c(secondaryColor, primaryColor)
  groupColors <- c("aqua", "green", "orange", "blue", "olive", "fuchsia", "lightblue", "lime", "yellow", "teal", "chartreuse", "blueviolet")
  grColLen <- length(groupColors)

  DTColumns <- c("Performer's type", "Performer's Name", "Cooperation type", "Cooperator's Name", "Created at")

  #### Function

  export_data <- function(conn, data, initial = FALSE){
    myTable <- "musicians_bands"
    tmpTable <- "temp"
    if(initial){
      dbWriteTable(conn, myTable, data) # to create a table in a database
    }else{
      dbWriteTable(conn, "temp", data) # temp table
      dbExecute(conn,
                paste0("INSERT INTO ",
                       myTable,
                       " ",
                       "SELECT * ",
                       "FROM ",
                       tmpTable
                )
      ) #
      dbExecute(conn,
                paste0(
                  "ALTER TABLE ",
                  myTable,
                  " ",
                  "ADD COLUMN isDuplicate INT"))

      dbExecute(conn,
                paste0("UPDATE ",
                       myTable,
                       " ",
                       "SET isDuplicate =(SELECT ROW_NUMBER() OVER(",
                       "PARTITION BY performerType, performerName, cooperationForm, cooperatorName ",
                       "ORDER BY performerType, performerName, cooperationForm, cooperatorName) mb ",
                       "FROM ",
                       myTable,
                       ")"
                ))

      dbExecute(conn,
                paste(
                  "DELETE FROM ",
                  myTable,
                  " ",
                  "WHERE isDuplicate > 1"
                ))

      dbExecute(conn,
                paste0("ALTER TABLE ",
                       myTable,
                       " ",
                       "DROP COLUMN isDuplicate")
      )
      # dbGetQuery(conn,
      #            paste(
      #            "WITH cte AS (",
      #            "SELECT *, ",
      #            "ROW_NUMBER() OVER (",
      #            "PARTITION BY performerType, performerName, cooperationForm, cooperatorName ",
      #            "ORDER BY performerType, performerName, cooperationForm, cooperatorName) mb ",
      #            "FROM musicians_bands ",
      #            "DELETE FROM cte",
      #            "WHERE mb > 1")) # duplicates dropping

      dbExecute(conn,
                paste0("DROP TABLE ",
                       tmpTable
                )) # temp table deletion

    }
  }

  get_performers <- function(conn, performer_type = NULL){
    myTable <- "musicians_bands"
    res <- "if"(is.null(performer_type),
                dbGetQuery(conn,
                           paste0("SELECT DISTINCT performerType, performerName ",
                                  "FROM ",
                                  myTable,
                                  " ",
                                  "ORDER BY performerType, performerName"
                           )),
                dbGetQuery(conn,
                           paste0("SELECT DISTINCT performerName ",
                                  "FROM ",
                                  myTable,
                                  " ",
                                  "WHERE performerType = '",
                                  performer_type,
                                  "' ",
                                  "ORDER BY performerName"
                           ))
    )

    return(res)
  }


  import_data <- function(conn, performer_type, performer_names){
    myTable <- "musicians_bands"
    switch(performer_type,
           "Musicians" = {
             bandMusicians <- dbGetQuery(conn,
                                         paste0("SELECT performerName, ",
                                                " cooperatorName ",
                                                "FROM ",
                                                myTable,
                                                " ",
                                                "WHERE performerName IN ('",
                                                paste(performer_names, collapse = "', '"),
                                                "') AND cooperationForm = 'Played in band'"
                                         ))

             musicians <- bind_rows(dbGetQuery(conn,
                                               paste0(
                                                 "SELECT ",
                                                 "performerName AS 'sp', ",
                                                 "(CASE ",
                                                 "WHEN cooperationForm = 'None' THEN 'No relation' ",
                                                 "WHEN cooperationForm = 'Played with another musician' THEN 'Joint performance' ",
                                                 "ELSE 'Band member'",
                                                 "END) AS 'r', ",
                                                 "cooperatorName AS 'rp' ",
                                                 "FROM musicians_bands ",
                                                 "WHERE performerName IN ('",
                                                 paste0(performer_names,
                                                        collapse = "', '"),
                                                 "')")),
                                    "if"(length(bandMusicians) == 0, NULL,
                                         {do.call(bind_rows, apply(bandMusicians, 1, function(df){
                                           bind_cols(data.frame(sp = df[1]),
                                                     dbGetQuery(conn,
                                                                paste0("SELECT 'Same band' AS 'r', ",
                                                                       "PerformerName as 'rp' ",
                                                                       "FROM ",
                                                                       myTable,
                                                                       " ",
                                                                       "WHERE cooperatorName = '",
                                                                       df[2],
                                                                       "' and performerName <> '",
                                                                       df[1],
                                                                       "'"))
                                           )}, simplify = FALSE))})
             ) %>% arrange(sp, r, rp) %>% tibble::remove_rownames() %>% rename(`Selected performer` = sp,
                                                                               Relation = r,
                                                                               `Related performer` = rp)
             musicians},
           "Bands" = dbGetQuery(conn,
                                paste0(
                                  "SELECT performerName AS 'Selected performer', ",
                                  "(CASE ",
                                  "WHEN cooperationForm = 'None' THEN 'No relation' ",
                                  "ELSE 'Joint performance' ",
                                  "END) AS 'Relation', ",
                                  "cooperatorName AS 'Related performer' ",
                                  "FROM ",
                                  myTable,
                                  " ",
                                  "WHERE PerformerType = 'Band' ",
                                  "AND performerName IN ('",
                                  paste(performer_names,
                                        collapse = "', '"),
                                  "')"
                                )
           ) %>% arrange(`Selected performer`, `Related performer`),
           NULL)
  }


  ### DATA
  data("initial_data")
  myDB <- RSQLite::dbConnect(RSQLite::SQLite(), "")
  export_data(myDB, initData, initial = TRUE)


############################################## MODULE 1 #############################################

  #### VARIABLES

  cooperationForm <- c("None",
                       "Played in band",
                       "Played with another musician",
                       "Played with another band")

  coopCond <- cooperationForm[3:4]

  #### UI

  update_ui <- function(id, session){
    ns <- NS(id)

    tags$table(style = "width: 95%;",
               tags$tr(style = "height: 85%;",
                       tags$td(id = "left_1", ### input
                               h2(icon("gear"), "Settings"),
                               radioGroupButtons(inputId = ns('performer_type_1'),
                                                 label = 'Select performer type',
                                                 choices = performerType,
                                                 status = "primary",
                                                 checkIcon = list(
                                                   yes = icon("ok",
                                                              lib = "glyphicon"),
                                                   no = icon("remove",
                                                             lib = "glyphicon"))),
                               textInput(inputId = ns("performer_name_1"),
                                         label = "Enter performer's name",
                                         value = ""),
                               bsTooltip(ns("performer_name_1"),
                                         HTML(paste0("<b>It is strongly recommended to introduce first a family name,",
                                                     br(),
                                                     "then a given name with a comma as separator.</b>")),
                                         "right",
                                         options = list(container = "body")),
                               conditionalPanel(paste0("input['",
                                                       ns("performer_type_1"),
                                                       "'] == 'Musician'"),
                                                radioGroupButtons(inputId = ns('cooperation_form_1_1'),
                                                                  label = "Select cooperation form",
                                                                  choices = cooperationForm[1:3],
                                                                  selected = "None",
                                                                  status = "primary",
                                                                  checkIcon = list(
                                                                    yes = icon("ok",
                                                                               lib = "glyphicon"),
                                                                    no = icon("remove",
                                                                              lib = "glyphicon"))),
                                                conditionalPanel(paste0("input['",
                                                                        ns("cooperation_form_1_1"),
                                                                        "'] != 'None'"),
                                                                 textInput(inputId = ns("cooperator_name_1_1"),
                                                                           label = "Enter cooperator name",
                                                                           value =""),
                                                                 bsTooltip(ns("cooperator_name_1_1"),
                                                                           HTML(paste0("<b>It is strongly recommended to introduce first a family name,",
                                                                                       br(),
                                                                                       "then a given name with a comma as separator.</b>")),
                                                                           "right",
                                                                           options = list(container = "body")))),
                               conditionalPanel(paste0("input['",
                                                       ns("performer_type_1"),
                                                       "'] =='Band'"),
                                                radioGroupButtons(inputId = ns('cooperation_form_1_2'),
                                                                  label = "Select cooperation form",
                                                                  choices = cooperationForm[c(1,4)],
                                                                  selected = "None",
                                                                  status = "primary",
                                                                  checkIcon = list(
                                                                    yes = icon("ok",
                                                                               lib = "glyphicon"),
                                                                    no = icon("remove",
                                                                              lib = "glyphicon"))),
                                                conditionalPanel(paste0("input['",
                                                                        ns("cooperation_form_1_2"),
                                                                        "'] != 'None'"),
                                                                 textInput(inputId = ns("cooperator_name_1_2"),
                                                                           label = "Enter cooperator name",
                                                                           value = "")
                                                )
                               ),
                               actionBttn(inputId=ns("insertion_1"),
                                          label = "Insert introduced data into table",
                                          style = "gradient",
                                          color = "primary",
                                          icon = icon("table")),
                               bsTooltip(ns("insertion_1"), HTML(paste0("<b>You have to click to save the introduced data.",
                                                                        br(),
                                                                        "Otherwise you will lose it :(</b>")),
                                         "right", options = list(container = "body")),
                               br(),
                               br(),
                               actionBttn(inputId=ns("transmission_1"),
                                          label = "Transfer table",
                                          style = "gradient",
                                          color = "primary",
                                          icon = icon("paper-plane")),
                               bsTooltip(ns("transmission_1"), HTML(paste0("<b>You have to click to send the introduced data.",
                                                                           br(),
                                                                           "Otherwise there will be no action :)</b>")),
                                         "right", options = list(container = "body"))),
                       tags$td(id = "right_1", ### output
                               DT::dataTableOutput(ns("insertion_table_1")),
                               bsTooltip(ns("insertion_table_1"),  HTML(paste0("<b>You can sort the table by columns, both increasingly and decreasingly.",
                                                                               br(),
                                                                               "Row filtering is also possible in the window ",
                                                                               "&#39;",
                                                                               "Search",
                                                                               "&#39;",
                                                                               "</b>")),
                                         "top", options = list(container = "body")),
                               h1(htmlOutput(ns("text_info_1")))
                       )
               )
    )




  }

  ### SERVER

  update_server <- function(id){
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
                       output$text_info_1 <- renderText({HTML(paste0("<b>Please, enter cooperator's name first</b>"))})
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
                         df <- bind_rows(df,
                                         data.frame(performerType = "Musician",
                                                    performerName = input$cooperator_name_1_1,
                                                    cooperationForm = coopCond[1],
                                                    cooperatorName = input$performer_name_1,
                                                    createdAt = as.character(Sys.time())))

                       }
                       # mutual relation
                       if (input$performer_type_1 == "Band" & input$cooperation_form_1_2 == coopCond[2]){
                         df <- bind_rows(df,
                                         data.frame(performerType = "Band",
                                                    performerName = input$cooperator_name_1_2,
                                                    cooperationForm = coopCond[2],
                                                    cooperatorName = input$performer_name_1,
                                                    createdAt = as.character(Sys.time())))

                       }    # new row(s) biding

                       insDf$df <- "if"(is.null(insDf$df),
                                        df,
                                        bind_rows(insDf$df, df)
                       )

                       output$text_info_1 <- NULL

                       output$insertion_table_1 <- DT::renderDataTable({DT::datatable(insDf$df %>% rename_with(~DTColumns) %>% mutate(rowGroup = 1:n()%% 2),
                                                                                      rownames = TRUE,
                                                                                      options = list(
                                                                                        pageLength = 20,
                                                                                        columnDefs = list(list(visible=FALSE,
                                                                                                               targets=6)
                                                                                        ))) %>% formatStyle(6,
                                                                                                            target='row',
                                                                                                            backgroundColor = styleEqual(c(1,0), alternateColors))
                       }
                       )

                     }})
                   #### Data transfer
                   observeEvent(input$transmission_1, {
                     if(is.null(insDf$df)){
                       transmissionInfo <- HTML(paste0('<b>Please, insert data into table first</b>'))
                     }else{
                       export_data(myDB, insDf$df %>% select(-createdAt), initial=FALSE)
                       output$insertion_table_1 <- NULL
                       insDf$df <- NULL
                       transmissionInfo <- HTML(paste0('<b>Data sent :)</b>'))}
                     output$text_info_1 <- renderText({transmissionInfo})

                   })
                 })}


  ################################### MODULE 2 #################################################################################

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
                       output$relation_table_2 <- DT::renderDataTable({datatable(import_data(myDB,
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


  ############################# MAIN #################################

  ### UI
  main_ui <- function(id){
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
  main_server <- function(id){
    moduleServer(id,
                 function(input, output, session) {
                   update_server(id="update_1")
                   retrieve_server(id="retrieve_2")

                 })}
  ####

  ######################## LAST BUT NOT LEAST ################

  ui <- fluidPage(
    span(p(main_ui(id = "main"))))

  server <-function(input, output, session) {
    main_server("main")}


  shinyApp(ui, server)
}
