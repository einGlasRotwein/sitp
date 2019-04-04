# Kurzes Validierungsskript, dass den Datensatz auf Fehler
# überprüft. Wenn alles läuft, kann die .Rmd laufen gelassen
# werden, die die Daten auswertet und in eine Präsi packt

library(tidyverse)

daten <- xlsx::read.xlsx2("./live daten/daten_190402.xlsx", 1)

# read.xlsx2 lässt leere Felder auch wirklich leer, anstatt NAs reinzusetzen
# Daher: leere Zellen mit NA ersetzen

daten[daten == ""] <- NA


## TIPPFEHLER ÜBERPRÜFEN
# Funktionen reinladen
source("./live daten/daten_validierung.R")

unplausible_NAs(daten)
ungueltige_werte(daten)
