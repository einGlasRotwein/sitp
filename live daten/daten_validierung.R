## Funktionen zur Validierung der eingetippten Livedaten

## ÜBERPRÜFUNG AUF TIPPFEHLER
# NAs können nur pro Kategorie auftreten:
# a) Wie aufmerksam geworden?
# b) Wie oft besucht?

names_aufmerksam <- c("fb_gwup", "twitter_gwup", "web_sitp", "web_gwup",
                      "web_mail", "flyer", "fb_juli", "twitter_juli", "web_juli",
                      "web_koeln", "web_meetup", "freunde", "sonstiges")

# Dann wird die jeweilige Kategorie gar nicht erst eingegeben
# Bsp.: Jemand gibt die Zahl der Besuche ein, füllt aber gar
# nichts bei "aufmerksam geworden" aus; dann würden wir trotzdem 
# die Daten für die Besuche eingeben

# Heißt auch: fehlende Werte innerhalb einer Kategorie wenn nicht
# alle anderen Werte der Kategorie ebenfalls NAs sind, sind Fehler
# beim Eintippen.
# Hier also die Daten auf Vollständigkeit prüfen und warnen!

unplausible_NAs <- function(x) {
  # Anfangs: kein Fehlerstatus, keine problematischen IDs
  error_aufmerksam <- FALSE
  IDs_aufmerksam <- NULL
  
  # Kategorie "aufmerksam" als Ganzes überprüfen
  subset_aufmerksam <- subset(x, select = fb_gwup:sonstiges)
  
  # zeige alle NAs
  NA_index_aufmerksam <- is.na(subset_aufmerksam)
  
  # identifiziere alle Fälle, wo nicht alle Items einer Kategorie 
  # entweder alle NA oder alle nicht NA sind
  problematisch_aufmerksam <- 
    rowMeans(NA_index_aufmerksam) != 0 & rowMeans(NA_index_aufmerksam) != 1
  
  # Gibt es ein Problem?
  if(sum(problematisch_aufmerksam) != 0) {
    error_aufmerksam <- TRUE
    IDs_aufmerksam <- x$ID[problematisch_aufmerksam]
  }
  
  if (error_aufmerksam == TRUE) {
    stop("unplausible NAs gefunden in IDs: ", 
         paste(IDs_aufmerksam, collapse = ", "))
  }
  
    return("typo tyrant approves")
}

# ungültige Werte finden
ungueltige_werte <- function(x) {
  # Anfangs: kein Fehlerstatus, keine problematischen IDs
  error <- FALSE
  fehlerlocation <- list()
  IDs_aufmerksam <- NULL
  
  # Finde ungültige Werte und speichere Variablenname und ID in fehlerlocation
  # Jede Kategorie hat andere gültige Werte: eigene Schleife für jede
  # Kategorie
  
  # KATEGORIE AUFMERKSAM
  for (i in names(x)[names(x) %in% names_aufmerksam]) {
    # Index aller ungültigen Werte
    ungueltig_index <-  x[[i]] %in% c(0, 1, NA)
    
    # Wenn es ungültige Werte in der aktuellen Variable gibt ...
    if(FALSE %in% ungueltig_index) {
      # füge an der Listenposition der ID, wo der Fehler liegt ...
      for (j in as.character(x$ID[!ungueltig_index])) {
        # ... die fehlerhafte Variable hinzu
        fehlerlocation[[j]] <- c(fehlerlocation[[j]], i)
      }
   }
  }
  
  # KATEGORIE BESUCH
  ungueltig_index <- x[["besuche"]] %in% c(1:5, NA)
  
  if(FALSE %in% ungueltig_index) {
    for (j in as.character(x$ID[!ungueltig_index])) {
      # ... die fehlerhafte Variable hinzu
      fehlerlocation[[j]] <- c(fehlerlocation[[j]], "besuche")
    }
  }
  
  # Wenn es absolut keinen Fehler gibt, ist fehlerlocation leer
  # Wenn der Aufruf unten also TRUE ergibt, ist unsere Liste mit
  # den Fehlerlocations nicht leer
  error <- !is_empty(fehlerlocation)
  
  # in dem Fall wollen wir uns die IDs und die dazugehörigen Variablen,
  # die in fehlerlocation gespeichert sind, ausgeben lassen
  if (error) {
    for (i in names(fehlerlocation)) {
      warning("Typos in ID ", i, " in Variable ", 
              paste(fehlerlocation[[i]], collapse = " und "))
    }
  }
  
  if (error == FALSE) {
    return("typo tyrant approves")
  } else {
    return("woops, typos")
  }
}
