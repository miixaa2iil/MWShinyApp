import_data <- function(conn, my_table, performer_type, performer_names){
  switch(performer_type,
         "Musicians" = {
           bandMusicians <- RSQLite::dbGetQuery(conn,
                                       paste0("SELECT performerName, ",
                                              " cooperatorName ",
                                              "FROM ",
                                              my_table,
                                              " ",
                                              "WHERE performerName IN ('",
                                              paste(performer_names, collapse = "', '"),
                                              "') AND cooperationForm = 'Played in band'"
                                       ))
           
           musicians <- bind_rows(RSQLite::dbGetQuery(conn,
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
                                       {do.call(dplyr::bind_rows, apply(bandMusicians, 1, function(df){
                                         dplyr::bind_cols(data.frame(sp = df[1]),
                                                   RSQLite::dbGetQuery(conn,
                                                              paste0("SELECT 'Same band' AS 'r', ",
                                                                     "PerformerName as 'rp' ",
                                                                     "FROM ",
                                                                     my_table,
                                                                     " ",
                                                                     "WHERE cooperatorName = '",
                                                                     df[2],
                                                                     "' and performerName <> '",
                                                                     df[1],
                                                                     "'"))
                                         )}, simplify = FALSE))})
           ) %>% dplyr::arrange(sp, r, rp) %>% tibble::remove_rownames() %>% dplyr::rename(`Selected performer` = sp,
                                                                             Relation = r,
                                                                             `Related performer` = rp)
           musicians},
         "Bands" = RSQLite::dbGetQuery(conn,
                              paste0(
                                "SELECT performerName AS 'Selected performer', ",
                                "(CASE ",
                                "WHEN cooperationForm = 'None' THEN 'No relation' ",
                                "ELSE 'Joint performance' ",
                                "END) AS 'Relation', ",
                                "cooperatorName AS 'Related performer' ",
                                "FROM ",
                                my_table,
                                " ",
                                "WHERE PerformerType = 'Band' ",
                                "AND performerName IN ('",
                                paste(performer_names,
                                      collapse = "', '"),
                                "')"
                              )
         ) %>% dplyr::arrange(`Selected performer`, `Related performer`),
         NULL)
}