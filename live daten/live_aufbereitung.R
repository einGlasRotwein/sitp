# alles zur Aufbereitung der live Daten in ein Skript gezogen
# Beschreibung siehe datenanalyse.R

## AUFBEREITUNG
names_items <- c("Sonstiges", "Freunde/Bekannte", "Meetup", "koeln.de",
                 "Blog Rotwein", "Twitter Rotwein", "Facebook Rotwein",
                 "Flyer", "Newsletter", "Website GWUP", "Website SitP",
                 "Twitter Skeptiker", "Facebook Skeptiker")

# Umkodieren
daten <- daten %>% 
  mutate_at(names(daten)[-1], as.character) %>% 
  mutate_at(names(daten)[-1], as.integer)

# Items ins long format
daten_lf <- daten %>% 
  select(ID, fb_gwup:sonstiges) %>% 
  filter(!is.na(fb_gwup)) %>% 
  gather(Item, Wert, fb_gwup:sonstiges)

# schickere Label
daten_lf$Item <- factor(daten_lf$Item, 
                        levels = c("sonstiges", "freunde", "web_meetup", 
                                   "web_koeln", "web_juli", "twitter_juli", 
                                   "fb_juli", "flyer", "web_mail", "web_gwup", 
                                   "web_sitp", "twitter_gwup", "fb_gwup"),
                        labels = names_items)

# Gruppierung: Skeptiker, Rotwein, Sonstiges
Gruppe_Skeptiker <- c("Facebook Skeptiker", "Twitter Skeptiker", "Website SitP", 
                      "Website GWUP", "Newsletter", "Flyer", "koeln.de", "Meetup")
Gruppe_Rotwein <- c("Facebook Rotwein", "Twitter Rotwein", "Blog Rotwein")

daten_lf <- daten_lf %>% 
  mutate(Gruppe_skep_rw = case_when(Item %in% Gruppe_Skeptiker ~ "Skeptiker",
                                    Item %in% Gruppe_Rotwein ~ "Rotwein",
                                    Item %in% c("Freunde/Bekannte", "Sonstiges")
                                    ~ "Sonstiges"))

# Gruppierung: social media vs. Website vs. offline
Gruppe_social_media <- c("Facebook Skeptiker", "Twitter Skeptiker", 
                         "Facebook Rotwein", "Twitter Rotwein")
Gruppe_web <- c("Website SitP", "Website GWUP", "Newsletter", "koeln.de", "Meetup",
                "Blog Rotwein")
Gruppe_offline <- c("Freunde/Bekannte", "Flyer")

daten_lf <- daten_lf %>% 
  mutate(Gruppe_on_off = case_when(Item %in% Gruppe_social_media ~ "Social Media",
                                   Item %in% Gruppe_web ~ "Web",
                                   Item %in% Gruppe_offline ~ "offline",
                                   Item == "Sonstiges" ~ "Sonstiges"))


## 1) JEDES ITEM
# Tabellenform - Reihenfolge Anzahl absteigend
items_tabelle_sortiert <- daten_lf %>% 
  group_by(Item, Gruppe_skep_rw, .drop = FALSE) %>% 
  count(Wert, name = "Anzahl") %>%
  ungroup() %>% 
  filter(Wert == 1 | (Wert == 0 & Anzahl == 39)) %>% 
  mutate(Anzahl = ifelse(Wert == 0, 0, Anzahl))

items_tabelle_sortiert$Item <- 
  factor(items_tabelle_sortiert$Item,
         levels = items_tabelle_sortiert$Item[order(items_tabelle_sortiert$Anzahl)])

# Plot - Reihenfolge Anzahl absteigend
plot_items_sortiert <- items_tabelle_sortiert %>%
  ggplot(aes(x = Item, y = Anzahl, fill = Gruppe_skep_rw)) + 
  geom_col(alpha = .8, colour = "black") +
  scale_fill_manual(values = c("#ff4600", "#0c2c76", "grey"),
                    labels = c(" Rotwein   ", " Skeptiker   ", " Sonstiges")) +
  labs(title = "Aufmerksam geworden auf SitP durch ...",
       y = "Anzahl an Menschen") +
  theme(plot.title = element_text(size = 18, hjust = .5),
        legend.position = "top", legend.title = element_blank(), 
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 16), axis.title.y = element_blank(),
        axis.text = element_text(size = 14)) +
  coord_flip()


## 2) SKEPTIKER VS. ROTWEIN
# Tabelle für Plot
Gruppe_skep_rw_tabelle <- daten_lf %>% 
  group_by(Gruppe_skep_rw, .drop = FALSE) %>% 
  count(Wert, name = "Anzahl") %>% 
  ungroup() %>% 
  filter(Wert == 1) %>% 
  mutate(Gruppe_skep_rw = factor(Gruppe_skep_rw, 
                                 levels = Gruppe_skep_rw[order(Anzahl)])) %>%
  arrange(Gruppe_skep_rw)

# Plot
plot_Gruppe_skep_rw <- Gruppe_skep_rw_tabelle %>%
  ggplot(aes(x = Gruppe_skep_rw, y = Anzahl, fill = Gruppe_skep_rw)) + 
  geom_col(alpha = .8, colour = "black") +
  scale_fill_manual(values = c("grey", "#ff4600", "#0c2c76")) +
  labs(title = "Aufmerksam geworden auf SitP durch ...",
       y = "Anzahl an Menschen") +
  theme(plot.title = element_text(size = 18, hjust = .5),
        legend.position = "none",
        axis.title.x = element_text(size = 16), axis.title.y = element_blank(),
        axis.text = element_text(size = 14)) +
  coord_flip()

## 2) ONLINE VS. OFFLINE
# Tabelle für Plot
Gruppe_on_off_tabelle <- daten_lf %>% 
  group_by(Gruppe_on_off, .drop = FALSE) %>% 
  count(Wert, name = "Anzahl") %>%
  ungroup() %>% 
  filter(Wert == 1) %>% 
  mutate(Gruppe_on_off = factor(Gruppe_on_off, 
                                levels = Gruppe_on_off[order(Anzahl)])) %>%
  arrange(Gruppe_on_off)

# Plot
plot_Gruppe_on_off <- Gruppe_on_off_tabelle %>%
  ggplot(aes(x = Gruppe_on_off, y = Anzahl, fill = Gruppe_on_off)) + 
  geom_col(alpha = .8, colour = "black") +
  scale_fill_manual(values = c("grey", "#ff4600", "#0c2c76", "#74AF0E")) +
  labs(title = "Aufmerksam geworden auf SitP durch ...",
       y = "Anzahl an Menschen") +
  theme(plot.title = element_text(size = 18, hjust = .5),
        legend.position = "none",
        axis.title.x = element_text(size = 16), axis.title.y = element_blank(),
        axis.text = element_text(size = 14)) +
  coord_flip()

# 4) ANZAHL DER BESUCHE
besuche_labels <- c("mehr als 10 Mal", "6 - 10 Mal", "3 - 5 Mal", 
                    "2 Mal", "erstes Mal")
daten$besuche <- factor(daten$besuche, levels = 5:1, labels = besuche_labels,
                        ordered = TRUE)

# Plot
besuche_plot <- daten %>% 
  filter(!is.na(besuche)) %>% 
  ggplot(aes(x = besuche)) +
  geom_bar(alpha = .8, colour = "black", fill = "#ff4600") +
  labs(x = "Besuche",
       y = "Anzahl an Menschen") +
  theme(plot.title = element_text(size = 18, hjust = .5),
        axis.title.x = element_text(size = 16), axis.title.y = element_blank(),
        axis.text = element_text(size = 14)) +
  coord_flip()
