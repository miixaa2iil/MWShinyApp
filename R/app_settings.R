performerType <- c("Musician",
                   "Band"
)

performersType <- paste0(performerType, "s")
#dtColumns <- c("performerType", "performerName", "cooperationType", "cooperatorName")
spinnerColor <- "#00B7EB"
spinnerType <- 5
primaryColor <-  "#0073b7"
secondaryColor <- "#00c0ef"
alternateColors <-c(secondaryColor, primaryColor) 
groupColors <- c("aqua", "green", "orange", "blue", "olive", "fuchsia", "lightblue", "lime", "yellow", "teal", "chartreuse", "blueviolet")
grColLen <- length(groupColors)

DTColumns <- c("Performer's type", "Performer's Name", "Cooperation type", "Cooperator's Name", "Created at")
myInterval <- 6e4#3e5
delayTime <- 5e3