data <- read.csv(file = file.choose(), sep = ";")
attach(data)

# Traitement de la variable année
annee <- substr(AAAAMMJJ, 1,4)
data <- cbind(annee,data)

# split 
data_station <- split(data, NOM_USUEL)
# Liste traitée comme un dataframe
data_station$`AIGLETON-NIVOSE`

station <- unique(NOM_USUEL)
periode <- unique(annee)

min(periode)
