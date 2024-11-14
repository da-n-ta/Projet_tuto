library(dplyr)
quot <- read.csv("~/Downloads/Q_38_previous-1950-2022_RR-T-Vent.csv/Q_38_previous-1950-2022_RR-T-Vent.csv", header=FALSE, sep=";")

#faire de petite bases de donnees pour chacune des stations et de la stat descriptives

colnames(quot) <- as.character(unlist(quot[1, ]))
quot <- quot[-1, ]
typeof(quot$RR)

quot$RR <- as.numeric(quot$RR)
quot$NUM_POSTE <- as.numeric(quot$NUM_POSTE)
quot$AAAAMMJJ <- as.numeric(quot$AAAAMMJJ)



data_per_station <- split(quot,quot$NOM_USUEL)

data_per_station$`AIGLETON-NIVOSE`$AAAAMMJJ