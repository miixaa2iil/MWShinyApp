get_performers <- function(conn, my_table, performer_type = NULL){
  res <- "if"(is.null(performer_type),
              RSQLite::dbGetQuery(conn,
                         paste0("SELECT DISTINCT performerType, performerName ",
                                "FROM ",
                                my_table,
                                " ",
                                "ORDER BY performerType, performerName"
                         )),
              RSQLite::dbGetQuery(conn,
                         paste0("SELECT DISTINCT performerName ",
                                "FROM ",
                                my_table,
                                " ",
                                "WHERE performerType = '",
                                performer_type,
                                "' ",
                                "ORDER BY performerName"
                         ))
  )
  
  return(res)
}
