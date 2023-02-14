app_settings <- function() {

resList <- list(myTable = "musicians_bands",
                myDB = RSQLite::dbConnect(RSQLite::SQLite(), ""),
                performerType = c("Musician",
                   "Band"
                   ),
                performersType = NULL,
                primaryColor = "#0073b7",
                secondaryColor = "#00c0ef",
                alternateColors = NULL,
                groupColors = c("aqua", "green", "orange",
                                "blue", "olive", "fuchsia",
                                "lightblue", "lime", "yellow",
                                "teal", "chartreuse", "blueviolet"),
                DTColumns =  c("Performer's type", "Performer's Name", "Cooperation type", "Cooperator's Name", "Created at"),
                grColLen = NULL)

resList$performersType <- paste0(resList$performerType , "s")

resList$alternateColors <- unlist(resList[paste0(c("primary", "secondary"), "Color")], use.names = FALSE)

resList$grColLen <- length(resList$groupColors)

return(resList)
}
