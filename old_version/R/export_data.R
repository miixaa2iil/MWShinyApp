export_data <- function(conn, data, my_table, tmp_table = "temp", initial = FALSE){
  if(initial){
    RSQLite::dbWriteTable(conn, my_table, data) # to create a table in a database
  }else{
    RSQLite::dbWriteTable(conn, "temp", data) # temp table
    RSQLite::dbExecute(conn,
              paste0("INSERT INTO ",
                     my_table,
                     " ",
                     "SELECT * ",
                     "FROM ",
                     tmp_table
              )
    ) #
    RSQLite::dbExecute(conn,
              paste0(
                "ALTER TABLE ",
                my_table,
                " ",
                "ADD COLUMN isDuplicate INT"))
    
    RSQLite::dbExecute(conn,
              paste0("UPDATE ",
                     my_table,
                     " ",
                     "SET isDuplicate =(SELECT ROW_NUMBER() OVER(",
                     "PARTITION BY performerType, performerName, cooperationForm, cooperatorName ",
                     "ORDER BY performerType, performerName, cooperationForm, cooperatorName) mb ",
                     "FROM ",
                     my_table,
                     ")"
              ))
    
    RSQLite::dbExecute(conn,
              paste(
                "DELETE FROM ",
                my_table,
                " ",
                "WHERE isDuplicate > 1"
              ))
    
    RSQLite::dbExecute(conn,
              paste0("ALTER TABLE ",
                     my_table,
                     " ",
                     "DROP COLUMN isDuplicate")
    )

    RSQLite::dbExecute(conn,
              paste0("DROP TABLE ",
                     tmp_table
              )) # temp table deletion
    
  }
}