# SIMPSON'S PARADOX
simpsonplot <- simpson %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(shape = "dummy"), colour = "white") +
  geom_point(colour = "#0c2c76", alpha = .6, size = 3) +
  scale_y_continuous(breaks = seq(10, 150, 20), limits = c(10, 150)) +
  scale_x_continuous(breaks = seq(480, 660, 40), limits = c(480, 660))  +
  theme(legend.position = "top", legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white"), legend.key = element_blank())


# KREISPLOTS
# als time series
circle_timeseries <- circle_lf %>% 
  # Leerschritt als Hack einfügen, damit der Abstand in der Legende größer ist
  mutate(variable = ifelse(variable == "y", "y   ", "x   ")) %>% 
  ggplot(aes(x = ID, y = value)) +
  geom_line(aes(colour = variable), size = 1) +
  scale_colour_manual(values = c("#0c2c76", "#ff4600")) +
  labs(y = "Wert") +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 20), legend.title = element_blank(),
        legend.position = "top")

# kreisförmiger Scatterplot
circle_scatter <- circle %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 21, fill = "#0c2c76", alpha = .8, size = 3.5) +
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 18))

# kreisförmiger Scatterplot mit geschlossenem Kreis drübergelegt
circle_whole_plot <- circle_whole %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 21, fill = "#0c2c76", alpha = .8, size = 3.5) +
  geom_path(colour = "#ff4600", size = 1.2, alpha = .4) +
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 18))


## WÜRFEL
# Plot mit "Würfeln" ohne Highlights
wuerfel_erklaerung <-  ggplot() +
  geom_point(data = wuerfel, aes(x = x, y = y), shape = 22, size = 50, 
             fill = "#ff4600", stroke = 2) +
  geom_point(data = augen, aes(x = x, y = y), shape = 21, size = 5, 
             fill = "#0c2c76", stroke = 1.5) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_x_continuous(limits = c(0, 6)) +
  theme(panel.background = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank())

# Plot mit Würfeln und Highlights, welche Augenzahlen verzaubert werden
wuerfel_erklaerung2 <- ggplot() +
  geom_point(data = wuerfel, aes(x = x, y = y, alpha = highlight), 
             shape = 22, size = 50, fill = "#ff4600", stroke = 2) +
  geom_point(data = augen, aes(x = x, y = y, alpha = highlight), 
             shape = 21, size = 5, fill = "#0c2c76", stroke = 1.5) +
  scale_alpha_discrete(range = c(1, .3)) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_x_continuous(limits = c(0, 6)) +
  theme(panel.background = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.position = "none")

# Differenz mit vs. ohne Magie reale Daten
wuerfel_differenz_real <- rolls_real %>% 
  na.omit() %>% 
  ggplot(aes(y = diff, x = magic_before, fill = factor(magic_before))) +
  stat_summary(fun.y = mean, geom = "col", 
               colour = "black", alpha = .6) +
  geom_hline(yintercept = 0, size = 1.5, linetype = "dashed") +
  scale_fill_manual(values = c("#0c2c76", "#ff4600"), 
                    labels = c(" ohne Magie   ", " mit Magie")) +
  theme(plot.title = element_text(size = 20, hjust = .5),
        axis.title.y = element_text(size = 20), axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 18), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top", legend.text = element_text(size = 16), 
        legend.title = element_blank())

# Erklärung Regression zur Mitte

# Es wurde eine 1 geworfen
one_rolled <- rolls_real %>% 
  group_by(rolls) %>% 
  count(rolls) %>% 
  # Dummyvector, um nur Balken 1 eine andere Farbe zu geben
  mutate(faerben = ifelse(rolls == 1, "ja", "nein")) %>%
  ggplot(aes(x = rolls, y = (n/sum(n)) * 100)) +
  geom_col(aes(fill = faerben), colour = "black", alpha = .7) +
  scale_fill_manual(values = c("#ff4600", "#0c2c76")) +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  labs(x = "Wurf", y = "Anteil in %") +
  theme(legend.position = "none",
        axis.title = element_text(size = 20), axis.text = element_text(size = 18))

# Wo kann der Würfel nun "hin" und was wäre die Differenz zum vorherigen Wurf?
# Labels für Balken: Differenz zum vorherigen Wurf (der eine 1 war)
bar_labels1 <- 1:6 - 1

# Muss Daten hier von Hand zusammenfassen, weil geom_text jedem Datenpunkt ein Label 
# zuweisen möchte. Und das sind hier 1000 anstatt 6.

one_rolled2 <- rolls_real %>% 
  group_by(rolls) %>% 
  count(rolls) %>% 
  mutate(faerben = ifelse(rolls == 1, "ja", "nein")) %>% 
  ggplot(aes(x = rolls, y = (n/sum(n)) * 100)) +
  geom_col(aes(fill = faerben), colour = "black", alpha = .7) +
  # nudge regelt hier den vertikalen Abstand zu den Balken
  geom_text(aes(label = bar_labels1), vjust = 0, nudge_y = 3, size = 10) +
  scale_fill_manual(values = c("#ff4600", "#0c2c76")) +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  labs(x = "Wurf", y = "Anteil in %") +
  theme(legend.position = "none",
        axis.title = element_text(size = 20), axis.text = element_text(size = 18))

# selbes Spiel für eine 6
bar_labels2 <- 1:6 - 6

six_rolled <- rolls_real %>% 
  group_by(rolls) %>% 
  count(rolls) %>% 
  # Dummyvector, um nur Balken 1 eine andere Farbe zu geben
  mutate(faerben = ifelse(rolls == 6, "ja", "nein")) %>%
  ggplot(aes(x = rolls, y = (n/sum(n)) * 100)) +
  geom_col(aes(fill = faerben), colour = "black", alpha = .7) +
  scale_fill_manual(values = c("#ff4600", "#0c2c76")) +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  labs(x = "Wurf", y = "Anteil in %") +
  theme(legend.position = "none",
        axis.title = element_text(size = 20), axis.text = element_text(size = 18))

six_rolled2 <- rolls_real %>% 
  group_by(rolls) %>% 
  count(rolls) %>% 
  mutate(faerben = ifelse(rolls == 6, "ja", "nein")) %>% 
  ggplot(aes(x = rolls, y = (n/sum(n)) * 100)) +
  geom_col(aes(fill = faerben), colour = "black", alpha = .7) +
  # nudge regelt hier den vertikalen Abstand zu den Balken
  geom_text(aes(label = bar_labels2), vjust = 0, nudge_y = 3, size = 10) +
  scale_fill_manual(values = c("#ff4600", "#0c2c76")) +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  labs(x = "Wurf", y = "Anteil in %") +
  theme(legend.position = "none",
        axis.title = element_text(size = 20), axis.text = element_text(size = 18))

# simulierte Würfeldaten
# Differenzplot
plot_diff_sim <- rolls_sim %>% 
  na.omit() %>% 
  ggplot(aes(y = diff, x = magic_before, fill = factor(magic_before))) +
  stat_summary(fun.y = mean, geom = "col", 
               colour = "black", alpha = .6) +
  geom_hline(yintercept = 0, size = 1.5, linetype = "dashed") +
  scale_fill_manual(values = c("#0c2c76", "#ff4600"), 
                    labels = c(" ohne Magie   ", " mit Magie")) +
  labs(title = "Unterschied", y = "Differenz") +
  theme(plot.title = element_text(size = 20, hjust = .5),
        axis.title.y = element_text(size = 20), axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 18), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top", legend.text = element_text(size = 14), 
        legend.title = element_blank())

# Plot Verteilung mit vs. ohne Magie
plot_dist_sim <- rolls_sim %>% 
  ggplot(aes(x = rolls)) +
  geom_bar(aes(y = (..prop..) * 100, fill = factor(magic_before)), 
           colour = "black", alpha = .6, position = "dodge") +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  scale_fill_manual(values = c("#0c2c76", "#ff4600"), 
                    labels = c(" ohne Magie   ", " mit Magie")) +
  labs(title = "Wurf-Verteilung", y = "Anteil in %", x = "geworfene Zahl") +
  theme(plot.title = element_text(size = 20, hjust = .5),
        axis.title = element_text(size = 20), axis.text = element_text(size = 18),
        legend.position = "top", legend.text = element_text(size = 16), 
        legend.title = element_blank())

# BEISPIEL REGRESSION ZUR MITTE: Erkältung mit Homöopathie
erkaeltung_hom <- erkaeltung %>% 
  filter(Behandlung == "hom") %>% 
  ggplot(aes(y = Symptomstaerke, x = Tag, colour = Behandlung)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = c("#ff4600", "#0c2c76"), 
                      labels = c(" Globuli   ", " Placebo")) +
  scale_x_continuous(breaks = seq(0, 14, 2)) +
  theme(legend.position = "top", legend.title = element_blank(), 
        legend.text = element_text(size = 20),
        axis.title = element_text(size = 20), 
        axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

erkaeltung_both <- erkaeltung %>% 
  ggplot(aes(y = Symptomstaerke, x = Tag, colour = Behandlung)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = c("#ff4600", "#0c2c76"), 
                      labels = c(" Globuli   ", " Placebo")) +
  scale_x_continuous(breaks = seq(0, 14, 2)) +
  theme(legend.position = "top", legend.title = element_blank(), 
        legend.text = element_text(size = 20),
        axis.title = element_text(size = 20), 
        axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
  

# SIMPSON'S PARADOX
cor_subgroups <- round(mean(simpson$sub_cor), 2)

# (semi-)fiktives konkretes Beispiel Simpson's Paradox: mehr Todesfälle in 
# besser ausgestatteten Krankenhäusern
simpson_konkret_plot <- simpsonplot +
  geom_smooth(method = "lm", colour = "#ff4600", se = FALSE) +
  scale_x_continuous(breaks = seq(480, 660, 40),
                     labels = c("unterirdisch", "schlecht", "mittel", 
                                       "gut", "exzellent")) +
  scale_y_continuous(breaks = seq(10, 150, 20), limits = c(10, 150)) +
  labs(title = "Todesfalle Spezialklinik?", x = "Klinikausstattung",
       y = "Todesrate") +
  theme(plot.title = element_text(size = 20, hjust = .5), 
        axis.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 20))
  

simpson_konkret <- simpson %>% 
  mutate(group = recode(group, `1` = "Notfall", `2` = "gravierend", 
                        `3` = "ernst", `4` = "mittel", `5` = "leicht")) %>% 
  mutate(group = factor(group, 
                        levels = c("leicht", "mittel", "ernst",
                                   "gravierend", "Notfall"), ordered = TRUE))

simpson_konkret_groups_plot <- simpson_konkret %>%
  mutate(Erkrankung = group) %>% 
  ggplot(aes(x = x, y = y, colour = Erkrankung, shape = Erkrankung)) +
  geom_point(alpha = .6, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_manual(values = c("#7C0036", "#0C2C76", "#FF4600", "#74AF0E",
                                 "#FFCC00")) +
  scale_y_continuous(breaks = seq(10, 150, 20), limits = c(10, 150)) +
  scale_shape_manual(values = c(3, 15, 17, 18, 19, 15)) +
  scale_x_continuous(breaks = seq(480, 660, 40),
                     labels = c("unterirdisch", "schlecht", "mittel", 
                                "gut", "exzellent")) +
  labs(title = "Todesfalle Spezialklinik?", x = "Klinikausstattung", 
       y = "Todesrate") +
  theme(plot.title = element_text(size = 20, hjust = .5), 
        axis.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "top")


## PROBABILITY WEIGHT PLOT
prob_plot <- prob_weight_plot %>% 
  ggplot(aes(x = p, y = pw, colour = perspektive)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = c("#ff4600", "#0c2c76"), 
                      labels = c(" menschlich   ", " rational")) +
  theme(legend.position = "top", legend.title = element_blank(), 
        legend.text = element_text(size = 20),
        axis.title = element_text(size = 20), 
        axis.text = element_text(size = 18))
