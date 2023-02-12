#myDB <- dbConnect(RSQLite::SQLite(), "")

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
