## FUNKTIONEN LADEN

# ENCHANT DIE FUNCTION
# generiert Zufallszahlen aus einem frei wählbaren Datenset (default: 
# normaler Würfel, 1:6) "verzaubert" den Würfel immer dann, wenn eine 
# bestimmte Zahl geworfen wurde: Erstellt eine Spalte die kodiert, ob 
# nach dem vorherigen Wurf "Magie" stattgefunden hat. Set der Elemente 
# aus dem Zufallssample, die verzaubert werden sollen, wird in 
# magic_numbers übergeben.

enchant.die <- function(nrolls, magic_numbers = c(1, 2), die_outcomes = 1:6) {
  
  if (sum(!magic_numbers %in% die_outcomes) > 0) {
    stop("Woah there! You want to enchant at least one die outcome that your die cannot possibly roll!")
  }
  
  dd <- data.frame(rolls = sample(die_outcomes, nrolls, replace = TRUE), 
                   magic_before = rep(2, nrolls))
  
  dd$magic_before[1] <- 0
  
  for (i in 1:(nrolls - 1)) {
    ifelse(dd$rolls[i] %in% magic_numbers == TRUE, 
           dd$magic_before[i + 1] <- 1, dd$magic_before[i + 1] <- 0)
  }
  
  invisible(dd)
}

#####

## DATEN EINLESEN UND AUFBEREITEN
# SIMPSON'S PARADOX
simpson <- read.csv("./data/simpson.txt")


## DATEN FÜR KREISPLOT
# von: https://github.com/jrauser/writing/blob/master/how_humans_see_data/hhsd_notes.Rmd
# siehe: https://www.youtube.com/watch?v=fSgEeI2Xpdc&feature=youtu.be

# Daten einlesen
circle <- read.csv("./data/circle.txt", sep = "\t")

# ID hinzufügen
circle <- circle %>% 
  mutate(ID = 1:nrow(circle))

# für Darstellung in Tabellenform
# Damit die Tabelle auf die Folie passt: in zwei Hälften aufbrechen 
# (und dann untereinander packen)

circle1 <- circle[1:12,] %>%
  # dreckiger Hack für Abstand
  mutate(dummy = "") %>% 
  select(dummy, x, y)

names(circle1) <- c("", "x", "y")

circle2 <- circle[13:24,] %>%
  mutate(dummy = "") %>% 
  select(x, y, dummy)

names(circle2) <- c("x", "y", "")

# delete rownames
row.names(circle2) <- NULL

# long format
circle_lf <- circle %>% 
  gather(variable, value, x, y)

# um geschlossenen Kreis im Plot darstellen zu können:
# circle neu ordnen
# untere Hälfte des Kreises: y <= 1
circle_whole <- 
  circle %>% 
  mutate(part = ifelse(y <= 1, "lower", "upper")) %>% 
  # dummy: upper mit negativem Vorzeichen, um "gegen den Uhrzeigersinn" 
  # ordnen zu können
  mutate(dummy_x = ifelse(part == "upper", x * -1, x)) %>% 
  arrange(part, dummy_x)

circle_whole <- 
  circle_whole %>% 
  # ersten Datenpunkt von x und y als letzten hinzufügen, um Schließen 
  # des Kreises zu ermöglichen
  rbind(
    data.frame(
      x = circle_whole$x[1],
      y = circle_whole$y[1],
      ID = nrow(circle_whole) + 1,
      part = "lower",
      dummy_x = circle_whole$x[1] * -1
    )
  )


## WÜRFEL
# Veranschaulichung, wann gezaubert wird (dataframe um "Würfel" zu 
# plotten und die mit Magie  zu highlighten)
wuerfel <- data.frame(wuerfel = c(4, 2, 6, 1, 5, 3), 
                      x = rep(seq(1, 6, 2), 2), 
                      y = rep(c(1, 3), 3)) %>% 
  mutate(highlight = ifelse(wuerfel %in% c(1, 2), "magie", "nope"))

augen <- data.frame(Augenzahl = rep(1:6, 1:6), 
                    x = c(1, 
                          2.75, 3.25, 
                          4.75, 5, 5.25, 
                          rep(c(0.75, 1.25), 2), 
                          rep(c(2.75, 3.25), 2), 3,
                          rep(c(4.75, 5.25), 3)),
                    y = c(3, 
                          3.25, 2.75, 
                          3.25, 3, 2.75, 
                          rep(1.25, 2), rep(0.75, 2), 
                          rep(1.25, 2),rep(0.75, 2), 1,
                          rep(1.25, 2), rep(1, 2), rep(0.75, 2))) %>% 
  mutate(highlight = ifelse(Augenzahl %in% c(1, 2), "magie", "nope"))

# Daten der Würfelwürfe aus der "echten Welt"
rolls_real <- read.csv("./data/enchanted_die.csv", sep = ";")

# Differenz zum vorherigen Wurf mit lag column berechnen, die rolls 
# column um 1 verschiebt
rolls_real$rolls_lag <- lag(rolls_real$rolls)
rolls_real$diff <- rolls_real$rolls - rolls_real$rolls_lag

# verzauberte Würfelwürfe mit enchanted die function generieren
rolls_sim <- enchant.die(1000)
rolls_sim$rolls_lag <- lag(rolls_sim$rolls)
rolls_sim$diff <- rolls_sim$rolls - rolls_sim$rolls_lag


## DATEN FÜR REGRESSION ZUR MITTE NACH HOMÖOPATHIE
erkaeltung <- 
  data.frame(Tag = 1:14,
             hom = jitter(sort(seq(0, 6.5, 0.5) *
                                 sort(seq(2, 4.6, 0.2), decreasing = TRUE),
                               decreasing = TRUE), amount = 1),
             placebo = jitter(sort(seq(0, 6.5, 0.5) *
                                     sort(seq(2, 4.6, 0.2), decreasing = TRUE),
                                   decreasing = TRUE), amount = 1))

erkaeltung <- erkaeltung %>% 
  gather(Behandlung, Symptomstaerke, hom, placebo)


## EINSCHÄTZUNG VON WAHRSCHEINLICHKEITEN
# Über-/Untergewichtung von sehr niedrigen/hohen Wahrscheinlichkeiten?
# Kahneman & Tversky: probability weighting function

p <- seq(0, 1, 0.01)

# sollten irgendwo um die .35 kreuzen
# delta so getüftelt, dass das der Fall ist (siehe auskommentierten Kram unten)
delta <- 0.68
wahrgenommen <- (p^delta) / ((p^delta) + ((1 - p)^delta))^(1/delta)

# intercept_index <- min(abs(p[-c(1, 101)] - wahrgenommen[-c(1, 101)])) == 
#   abs(p[-c(1, 101)] - wahrgenommen[-c(1, 101)])
# 
# p[intercept_index]
# # [1] 0.36

# in einen dataframe
prob_weight_plot <- data.frame(p = p, rational = p, menschlich = wahrgenommen)

# long format für plot
prob_weight_plot <- prob_weight_plot %>% 
  gather(perspektive, pw, rational, menschlich)

## KREBSATLAS
uk <- readRDS("./data/gadm36_GBR_2_sp.rds")
