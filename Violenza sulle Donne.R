#**#**#**#**#**# OPERAZIONI PRELIMINARI #**#**#**#**#**#

# Settaggio della working directory:
setwd("C://Users//Andrea//Desktop//Università//Fondamenti di Scienza dei Dati e Laboratorio//Progetto")

# Settaggio della lingua:
Sys.setenv(language = "en")

# Librerie necessarie:

library(extrafont)
extrafont::font_import (path="C:\\Users\\Andrea\\AppData\\Local\\Microsoft\\Windows\\Fonts", pattern = "fontawesome-webfont.ttf", prompt =  FALSE)
loadfonts(device = "win")

library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(waffle)
library(scales)

# La popolazione femminile censita nel 2020 divisa in classi di età (16-70):
popolazione_femminile_anni_16_70 <- c(2515341, 3146742, 3877476, 4865525, 4342911, 2172085)
names(popolazione_femminile_anni_16_70) <- c("16-24", "25-34", "35-44", "45-54", "55-64", "65-70")

# Popolazione utilizzata nelle ricerche:
popolazione_femminile_ricerche <- c(4086, 4540, 4540, 4540, 4540, 2724)
names(popolazione_femminile_ricerche) <- c("16-24", "25-34", "35-44", "45-54", "55-64", "65-70")
totale_ricerche <- sum(popolazione_femminile_ricerche)
popolazione_femminile_ricerche["16-24"]

# Totale delle violenze nel corso ella vita ricavate dall'indagine del 2006:
totale_fisica_2006 <- 7316
totale_sessuale_2006 <- 7682
totale_stupro_tentato_2006 <- 1754

# Totale delle violenze nel corso della vita ricavate dall'indagine del 2014:
totale_fisica_2014 <- 7686
totale_sessuale_2014 <- 6666
totale_stupro_tentato_2014 <- 1870

# Colori per i grafici:
un_colore <- "#F6BD60FF"
due_colori <- c("#F7EDE2FF", "#F5CAC3FF")
tre_colori <- c("#F6BD60FF", "#F5CAC3FF", "#F28482FF")
cinque_colori <- c("#F6BD60FF", "#F7EDE2FF", "#F5CAC3FF", "#84A59DFF", "#F28482FF")
sei_colori <- c("#F6BD60FF", "#F7EDE2FF", "#F5CAC3FF", "#BDB8B0FF", "#84A59DFF", "#F28482FF")
undici_colori <- c("#EEE5E9FF", "#F3DFA2FF", "#92DCE5FF", "#FAC05EFF", "#F79D84FF", "#59CD90FF", "#3FA7D6FF", "#7C7C7CFF", "#E17666FF", "#496E73FF", "#6F3228FF")

# Posizione dei barplot in caso non fossero ordinati:
posizione <- c("Violenza Fisica", "Violenza Sessuale", "Stupro/Tentato")

# --- #

#**#**#**#**#**# IL FENOMENO #**#**#**#**#**#

# --- DONNE VITTIME DI VIOLENZA: PLOT --- #

donne_vittime_di_violenza <- read.csv("File CSV//Violenza sulle Donne//donne_vittime_di_violenza.csv", sep = ";")
head(donne_vittime_di_violenza)

# - WAFFLE PLOT 1: DONNE VITTIME DI VIOLENZA - #

totali_vita <- c("Vittime Violenza Fisica" = 315, "Donne" = 685)

vita_plot <- waffle(
  round(totali_vita / 100, 0), rows = 1, size = 0.5,
  colors = c("#F5CAC3FF", "#BDB8B0FF"),
  use_glyph = "female", glyph_size = 16) +
  theme_void() + # Empty theme without axis lines and texts
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  )

# - BAR PLOT 2: TIPOLGIA DI VIOLENZA - #

tipologia_di_violenza_vita_plot <- donne_vittime_di_violenza %>%
  filter(Anno == 2014 & Autore == "Totale", Fascia == "16-70") %>%
  mutate(Percentuale = Totale / 24970) %>%
  ggplot(., mapping = aes(x = reorder(Violenza, -Totale), y = Percentuale * 100, fill = Violenza, label = scales::percent(Percentuale, accuracy = 0.1))) +
  geom_bar(stat = "identity", position = "dodge", color = "#000000FF") +
  theme_minimal() +
  scale_fill_manual(values = tre_colori) +
  scale_x_discrete(limits = posizione) +
  ylab(NULL) +
  xlab(NULL) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "none",
  ) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 4)

# - BAR PLOT 3: ETA' DIFFUSIONE DEL FENOMENO - #

fasce_eta_plot <- donne_vittime_di_violenza %>%
  filter(Anno == 2014 & Autore == "Totale" & Fascia != "16-70") %>%
  mutate(Percentuale = ifelse(Fascia == "16-24", Totale / popolazione_femminile_ricerche["16-24"],
                              ifelse(Violenza == "65-70", Totale / popolazione_femminile_ricerche["65-70"],
                                      Totale / popolazione_femminile_ricerche["25-34"]))) %>%
  ggplot(., mapping = aes(x = reorder(Violenza, -Totale), y = Percentuale, fill=Fascia, label = format(Percentuale * 100, digit = 1, nsmall = 1))) +
  geom_bar(stat = "identity", position = "dodge", color = "#000000FF") +
  theme_minimal() +
  scale_fill_manual(values = sei_colori) +
  scale_x_discrete(limits = posizione) +
  ylab("%") +
  xlab(NULL) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
  ) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3)

# - BAR PLOT 4: PERCENTUALE DEI PARTNER - #

partner_sistemato <- donne_vittime_di_violenza %>%
  filter(Anno == 2014 & Autore != "Totale" & Tipologia == "Donne di 16-70 anni che hanno subito violenza nel corso della vita" & Fascia == "16-70") %>%
  group_by(Anno, Tipologia, Autore, Violenza) %>%
  arrange(Violenza)

totali_violenze <- partner_sistemato %>%
  group_by(Violenza) %>%
  summarise(Totale.Violenza = sum(Totale))

totali_violenze$Violenza <- NULL
totali_violenze <- totali_violenze[rep(seq_len(nrow(totali_violenze)), each = 3), ]
totali_violenze <- cbind(as.data.frame(partner_sistemato), as.data.frame(totali_violenze))

legame_plot <- totali_violenze %>%
  mutate(Percentuale = Totale / Totale.Violenza) %>%
  ggplot(., mapping = aes(x = reorder(Violenza, -Totale), y = Percentuale, fill = factor(Autore), label = scales::percent(Percentuale, accuracy = 0.1))) +
  geom_bar(stat = "identity", position = "dodge", color = "#000000FF") +
  theme_minimal() +
  scale_fill_manual(values = tre_colori) +
  ylab(NULL) +
  xlab(NULL) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
  ) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3)

# --- PRESENZA DI ALTRI: PLOT --- #

presenza_di_altri <- read.csv("File CSV//Violenza sulle Donne//presenza_di_altri.csv", sep = ";")
head(presenza_di_altri)

# - DONUT CHART 5: PRESENTE QUALCUN ALTRO - #

presenza_di_altri_sistemato <- presenza_di_altri %>%
  group_by(Violenza, Presenza.Altri) %>%
  summarise(Valore = sum(Valore))

totali_presenza <- presenza_di_altri %>%
  group_by(Violenza) %>%
  summarise(Valore.Totale = sum(Valore))

totali_presenza$Violenza <- NULL
totali_presenza <- totali_presenza[rep(seq_len(nrow(totali_presenza)), each = 2), ]
totali_presenza <- cbind(as.data.frame(presenza_di_altri_sistemato), as.data.frame(totali_presenza))

presenza_di_altri_plot <- totali_presenza %>%
  mutate(Percentuale = round(Valore / Valore.Totale *100, 1),) %>%
  mutate(Posizione.Label = ifelse(Presenza.Altri == "Sì", 100 - 0.5 * Percentuale,
                                  ifelse(Presenza.Altri != "Sì", 0.5 * Percentuale, NA))) %>%
  ggplot(., aes(x = 2, y = Percentuale, fill = reorder(Presenza.Altri, -Posizione.Label))) +
  geom_bar(stat = "identity", color = "#000000FF") +
  coord_polar("y", start = 0, ) +
  geom_text(aes(y = Posizione.Label, label = paste0(Percentuale, "%")), color = "#444444", size = 5)+
  scale_fill_manual(values = due_colori, name = "Presenza Altri:") +
  theme_void() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 11),
    strip.text.x = element_text(size = 11),
    strip.text.y = element_text(size = 11)
  ) +
  xlim(0.5, 2.5) +
  facet_wrap(Violenza~., nrow = 1, strip.position = "bottom")

# - BAR PLOT 6: AZIONI - #

posizione_chart <- c("Nessuno è intervenuto", "Qualcuno è intervenuto ma la situazione è peggiorata", "Qualcuno è intervenuto e la situazione è migliorata", "Nessuno se ne è accorto", "Non sa/Non risponde")

Valori.Totali <- c(rep(1377, 5), rep(784, 5), rep(143, 5))
presenza_di_altri_sistemato <- presenza_di_altri %>% filter(Presenza.Altri == "Sì")
presenza_di_altri_sistemato <- cbind(as.data.frame(presenza_di_altri_sistemato), as.data.frame(Valori.Totali))

presenza_altri <- presenza_di_altri_sistemato %>%
  mutate(Percentuale = Valore / Valori.Totali) %>%
  arrange(desc(Percentuale)) %>%
  ggplot(., mapping = aes(x = Risposta, y = Percentuale, fill = Risposta, label = scales::percent(Percentuale, accuracy = 0.1))) +
  geom_bar(stat = "identity", position = "dodge", color = "#000000FF") +
  coord_flip(ylim = c(0, 0.75)) +
  scale_x_discrete(limits = rev(posizione_chart)) +
  theme_minimal() +
  scale_fill_manual(values = cinque_colori) +
  ylab(NULL) +
  xlab(NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "none",
  ) +
  geom_text(position = position_dodge(width = .9), hjust = -0.1, vjust = 0.3, size = 3.5) +
  facet_wrap(.~Violenza, strip.position = "bottom")

#**#**#**#**#**# RICHIESTA D'AIUTO #**#**#**#**#**#

# --- CHIAMATE AL GIORNO DA VITTIME: PLOT --- #

chiamate_al_giorno_da_vittime <- read.csv("File CSV//Richiesta d'Aiuto//chiamate_al_giorno_da_vittime.csv", sep = ";")
head(chiamate_al_giorno_da_vittime)

# - LINE PLOT 1: CHIAMATE AL GIORNO DA VITTIME - #

chiamate_al_giorno_da_vittime <- separate(chiamate_al_giorno_da_vittime, Giorno, into = c("Giorno", "Mese"), sep = "/", convert = TRUE)

chiamate_al_giorno_da_vittime <- gather(chiamate_al_giorno_da_vittime, "X2013", "X2014", "X2015", "X2016", "X2017", "X2018", "X2019", "X2020", key = "Anno", value = "Chiamate")

chiamate_al_giorno_da_vittime$Mese <- factor(chiamate_al_giorno_da_vittime$Mese, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                             labels = c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre"))

chiamate_al_giorno_da_vittime$Anno <- factor(chiamate_al_giorno_da_vittime$Anno, levels = c("X2013", "X2014", "X2015", "X2016", "X2017", "X2018", "X2019", "X2020"),
                                              labels = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"))
chiamate_al_giorno_da_vittime$Anno <- as.numeric(as.character(chiamate_al_giorno_da_vittime$Anno))

chiamate_al_giorno_da_vittime_plot <- chiamate_al_giorno_da_vittime %>%
  group_by(Anno, Mese) %>%
  summarise(Totale.Chiamate = sum(Chiamate)) %>%
  filter(Anno >= 2016) %>%
  ggplot(., mapping = aes(x = Mese, y = Totale.Chiamate, color = factor(Anno), group = Anno)) +
  geom_line(size = 1.5) +
  geom_point(size = 4, pch = 21, color = "black", aes(fill = factor(Anno))) +
  theme_minimal() +
  scale_colour_manual(values = cinque_colori) +
  scale_fill_manual(values = cinque_colori) +
  xlab("Mese") +
  ylab("Totale Chiamate") +
  theme(
    axis.text.x = element_text(size = 10, angle = 45),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.title = element_blank(),
  )

# - LINE PLOT 2: CONFRONTO MARZO-MAGGIO 2019/2020 - #

chiamate_al_giorno_da_vittime_marzo_maggio <- read.csv("File CSV//Richiesta d'Aiuto//chiamate_al_giorno_da_vittime.csv", sep = ";")

chiamate_al_giorno_da_vittime_marzo_maggio$Giorno <- as.Date(chiamate_al_giorno_da_vittime_marzo_maggio$Giorno, format = "%d/%m")
chiamate_al_giorno_da_vittime_marzo_maggio <- chiamate_al_giorno_da_vittime_marzo_maggio %>%
  select(Giorno, X2019, X2020) %>%
  filter(Giorno >= "2021-03-09" & Giorno <= "2021-05-17")

chiamate_al_giorno_da_vittime_marzo_maggio$Giorno <- format(chiamate_al_giorno_da_vittime_marzo_maggio$Giorno, "%d/%m")

chiamate_al_giorno_da_vittime_marzo_maggio <- separate(chiamate_al_giorno_da_vittime_marzo_maggio, Giorno, into = c("Giorno", "Mese"), sep = "/", convert = TRUE)

chiamate_al_giorno_da_vittime_marzo_maggio <- gather(chiamate_al_giorno_da_vittime_marzo_maggio, "X2019", "X2020", key = "Anno", value = "Chiamate")
chiamate_al_giorno_da_vittime_marzo_maggio$Anno <- factor(chiamate_al_giorno_da_vittime_marzo_maggio$Anno, levels = c("X2019", "X2020"), labels = c("2019", "2020"))

chiamate_al_giorno_da_vittime_marzo_maggio$Giorno <- NULL
chiamate_al_giorno_da_vittime_marzo_maggio$Mese <- NULL
chiamate_al_giorno_da_vittime_marzo_maggio <- cbind(chiamate_al_giorno_da_vittime_marzo_maggio, Settimana = rep(rep(c(9, 16, 23, 30, 6, 13, 20, 27, 4, 11), each = 7), 2))
chiamate_al_giorno_da_vittime_marzo_maggio <- cbind(chiamate_al_giorno_da_vittime_marzo_maggio, Mese = rep(rep(c(3, 3, 3, 3, 4, 4, 4, 4, 5, 5), each = 7), 2))

chiamate_al_giorno_da_vittime_marzo_maggio <- unite(chiamate_al_giorno_da_vittime_marzo_maggio, Settimana, Settimana, Mese, sep = "/")

chiamate_al_giorno_da_vittime_marzo_maggio$Settimana <- as.Date(chiamate_al_giorno_da_vittime_marzo_maggio$Settimana, format = "%d/%m")
chiamate_al_giorno_da_vittime_marzo_maggio <- chiamate_al_giorno_da_vittime_marzo_maggio %>%
  select(Giorno, X2019, X2020) %>%
  filter(Giorno >= "2021-03-09" & Giorno <= "2021-05-17")

chiamate_al_giorno_da_vittime_marzo_maggio_plot <- chiamate_al_giorno_da_vittime_marzo_maggio %>%
  group_by(Anno, Settimana) %>%
  summarise(Chiamate = sum(Chiamate)) %>%
  ggplot(., mapping = aes(x = Settimana, y = Chiamate, color = factor(Anno), group = Anno)) +
  geom_line(size = 2) +
  geom_point(size = 4, pch = 21, color = "#000000FF", aes(fill = factor(Anno))) +
  theme_minimal() +
  scale_colour_manual(values = due_colori) +
  scale_fill_manual(values = due_colori) +
  scale_x_date(labels = date_format("%d/%m")) +
  xlab("Periodo") +
  ylab("Totale Chiamate") +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.title = element_blank(),
  )

# --- TIPOLOGIA DI VIOLENZA DELLE VITTIME: PLOT --- #

tipologia_di_violenza <- read.csv("File CSV//Richiesta d'Aiuto//tipologia_di_violenza.csv", sep = ";")
head(tipologia_di_violenza)

# - BAR PLOT 3: TIPOLOGIA DI VIOLENZA - #

tipologia_di_violenza_plot <- tipologia_di_violenza %>%
  filter(Anno >= 2016 & Tipologia != "Missing Values") %>%
  group_by(Anno, Tipologia) %>%
  summarise(Totale = sum(Totale)) %>%
  ggplot(., mapping = aes(x = reorder(Tipologia, -Totale), y = Totale, fill = factor(Anno))) +
  geom_bar(stat = "identity", color = "#000000FF", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = cinque_colori) +
  ylab(NULL) +
  xlab(NULL) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
  )

# --- DENUNCIA: PLOT --- #

anno <- c(rep("2006", 12), rep("2014", 12))
legame_uomo <- rep(c(rep("Partner", 6), rep("Non Partner", 6)), 2)
violenza <- rep(c(rep("Fisica", 2), rep("Sessuale", 2), rep("Stupro/Tentato", 2)), 4)
ha_denunciato <- rep(c("Sì", "No/Non risponde"), 12)
totale_percentuale <- c(7.5, 92.5, 4.8, 95.2, 5.3, 94.7, 11, 89, 1.8, 98.2, 7.1, 92.9,
                        12.5, 87.5, 16, 84, 17.5, 82.5, 12.7, 87.3, 1.6, 98.4, 4.3, 95.7)

denunce <- data.frame(anno, legame_uomo, violenza, ha_denunciato, totale_percentuale)
colnames(denunce) <- c("Anno", "Legame.Uomo", "Violenza", "Ha.Denunciato", "Totale.Percentuale")

# - DONUT CHART 4: PERCENTUALE DI CHI NON DENUNCIA 2014 - #

denunce_plot <- denunce %>%
  mutate(Posizione.Label = ifelse(Ha.Denunciato == "Sì", 100 - 0.5 * Totale.Percentuale,
                                  ifelse(Ha.Denunciato != "Sì", 0.5 * Totale.Percentuale, NA)))

denunce_plot <- denunce_plot %>%
  filter(Anno == 2014) %>%
  ggplot(., aes(x = 2, y = Totale.Percentuale, fill = reorder(Ha.Denunciato, -Posizione.Label))) +
  geom_bar(stat = "identity", color = "#000000FF") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = Posizione.Label, label = paste0(Totale.Percentuale, "%")), color = "#333333FF", size = 4.5)+
  scale_fill_manual(values = due_colori, name = "Ha Denunciato:") +
  theme_void() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    strip.text.x = element_text(size = 11),
    strip.text.y = element_text(size = 11, angle = 270)
  ) +
  xlim(0.5, 2.5) +
  facet_grid(Legame.Uomo~Violenza, switch = "both")

# - DONUT CHART 5: PERCENTUALE DI CHI NON DENUNCIA 2006-2014 - #

denunce_2006_2014_plot <- denunce %>%
  group_by(Anno, Ha.Denunciato) %>%
  summarise(Totale.Percentuale = sum(Totale.Percentuale)) %>%
  mutate(Totale.Percentuale = round(Totale.Percentuale / 6, digit = 1)) %>%
  mutate(Posizione.Label = ifelse(Ha.Denunciato == "Sì", 100 - 0.5 * Totale.Percentuale,
                                  ifelse(Ha.Denunciato != "Sì", 0.5 * Totale.Percentuale, NA))) %>%
  ggplot(., aes(x = 2, y = Totale.Percentuale, fill = reorder(Ha.Denunciato, -Posizione.Label))) +
  geom_bar(stat = "identity", color = "#000000FF") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = Posizione.Label, label = paste0(Totale.Percentuale, "%")), color = "#333333FF", size = 6)+
  scale_fill_manual(values = due_colori, name = "Ha Denunciato:") +
  theme_void() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      strip.text.x = element_text(size = 12)
    ) +
  xlim(0.5, 2.5) +
  facet_wrap(Anno~., nrow = 1, strip.position = "bottom")

# --- DENUNCIA E RITIRO DELLA DENUNCIA: PLOT --- #
  
denuncia_e_ritiro_della_stessa <- read.csv("File CSV//Richiesta d'Aiuto//denuncia_e_ritiro_della_denuncia.csv", sep = ";")
head(denuncia_e_ritiro_della_stessa)

# - BAR PLOT 6: MOTIVI DELLA NON DENUNCIA - #

denuncia_e_ritiro_della_stessa_plot <- denuncia_e_ritiro_della_stessa %>%
  filter(Anno >= 2016 & Denuncia == "Non Denuncia") %>%
  group_by(Motivo) %>%
  summarise(Totale.Denunce = sum(Totale)) %>%
  mutate(Percentuale = Totale.Denunce / sum(Totale.Denunce)) %>%
  arrange(desc(Percentuale)) %>%
  ggplot(., mapping = aes(x = reorder(Motivo, Percentuale), y = Percentuale, fill = Motivo, label = scales::percent(Percentuale, accuracy = 0.1))) +
  geom_bar(stat = "identity", position = "dodge", color = "#000000FF", fill = un_colore) +
  coord_flip(ylim = c(0, 0.24)) +
  theme_minimal() +
  scale_fill_manual(values = undici_colori) +
  ylab(NULL) +
  xlab(NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "none",
  ) +
  geom_text(position = position_dodge(width = .9), hjust = -0.2, vjust = 0.3, size = 4.5)

# --- #

#**#**#**#**#**# PERCORSO GIUDIZIARIO #**#**#**#**#**#

# --- CONDANNATI 2000-2017: PLOT --- #

condannati_2000_2017 <- read.csv("File CSV//Percorso Giudiziario//condannati_2000_2017.csv", sep = ";")
head(condannati_2000_2017)

# - LINE PLOT 1: CONDANNATI PER MOTIVO E ANNO - #

condannati_2000_2017_plot <- condannati_2000_2017 %>%
  filter(Sesso == "M") %>%
  group_by(Anno, Reato) %>%
  summarise(Totale = sum(Totale)) %>%
  ggplot(., mapping = aes(x = Anno, y = Totale, color = Reato, group = Reato)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  geom_line(size = 2) +
  theme_minimal() +
  scale_color_manual(values = cinque_colori) +
  scale_x_continuous(breaks = seq(2000, 2017, by = 4)) +
  xlab("Anno") +
  ylab("Numero Condannati") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position="top",
    legend.title = element_blank(),
  )

# - BAR PLOT 2: PERCENTUALI PROVENIENZA - #

condannati_2000_2017_sistemato <- condannati_2000_2017 %>%
  filter(Sesso == "M" & Anno >= 2013) %>%
  group_by(Provenienza, Reato) %>%
  summarise(Totale = sum(Totale)) %>%
  arrange(Reato)

totali_condannati <- condannati_2000_2017 %>%
  filter(Sesso == "M" & Anno >= 2013) %>%
  group_by(Reato) %>%
  summarise(Totale.Reato = sum(Totale))

totali_condannati$Reato <- NULL

totali_condannati <- totali_condannati[rep(seq_len(nrow(totali_condannati)), each = 2), ]

condannati_2000_2017_sistemato <- cbind(as.data.frame(condannati_2000_2017_sistemato), as.data.frame(totali_condannati))

condannati_2000_2017_sistemato_plot <- condannati_2000_2017_sistemato %>%
  mutate(Percentuale = Totale / Totale.Reato) %>%
  ggplot(., mapping = aes(x = Provenienza, y = Percentuale, fill = Provenienza, label = scales::percent(Percentuale, accuracy = 0.1))) +
  geom_bar(stat = "identity", position = "dodge", color = "#000000FF") +
  facet_wrap(Reato~., nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  scale_fill_manual(values = due_colori) +
  coord_cartesian(ylim = c(0, 0.85)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    strip.text.x = element_text(size = 10),
    legend.position = "top",
  ) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 4)

# --- PERIODO RECLUSIONE 2000-2017: PLOT --- #

periodo_reclusione_2000_2017 <- read.csv("File CSV//Percorso Giudiziario//periodo_reclusione_2000_2017.csv", sep = ";")
head(periodo_reclusione_2000_2017)
colnames(periodo_reclusione_2000_2017) <- c("Anno",	"Sesso", "Provenienza", "Reato", "<=1 Mese",	"1-3 Mesi",	"3-6 Mesi",	"6-12 Mesi",	"1-2 Anni", "2-3 Anni",	"3-5 Anni", "5-10 Anni", ">10 Anni")

# - HISTOGRAM PLOT 3: DURATA DELLA CONDANNA - #

periodo_reclusione_2000_2017 <- gather(periodo_reclusione_2000_2017, "<=1 Mese",	"1-3 Mesi",	"3-6 Mesi",	"6-12 Mesi",	"1-2 Anni", "2-3 Anni",	"3-5 Anni", "5-10 Anni", ">10 Anni", key = "Periodo", value = "Totale", factor_key = TRUE)

periodo_reclusione_2000_2017_sistemato <- periodo_reclusione_2000_2017 %>%
  filter(Sesso == "M" & Anno >= 2013) %>%
  group_by(Reato, Periodo) %>%
  summarise(Totale = sum(Totale)) %>%
  arrange(Reato)

periodo_reclusione <- periodo_reclusione_2000_2017 %>%
  filter(Sesso == "M" & Anno >= 2013) %>%
  group_by(Reato) %>%
  summarise(Totale.Reato = sum(Totale))

periodo_reclusione$Reato <- NULL

periodo_reclusione <- periodo_reclusione[rep(seq_len(nrow(periodo_reclusione)), each = 9), ]

periodo_reclusione_2000_2017_sistemato <- cbind(as.data.frame(periodo_reclusione_2000_2017_sistemato), as.data.frame(periodo_reclusione))

periodo_reclusione_2000_2017_sistemato <- periodo_reclusione_2000_2017_sistemato %>%
  mutate(Percentuale = Totale / Totale.Reato)

periodo_reclusione_2000_2017_sistemato_plot <- periodo_reclusione_2000_2017_sistemato %>%
  ggplot(., mapping = aes(x = Periodo, y = Percentuale, fill = Reato, label = scales::percent(Percentuale, accuracy = 0.1))) +
  geom_histogram(stat = "identity", color = "#000000FF") +
  facet_wrap(Reato~., ncol = 1, strip.position = "top") +
  theme_minimal() +
  scale_fill_manual(values = cinque_colori) +
  coord_cartesian(ylim = c(0, 1.08)) +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    strip.text.x = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    legend.position = "none",
  ) +
  geom_text(data = subset(periodo_reclusione_2000_2017_sistemato, Percentuale > 0.01), position = position_dodge(width = .9), vjust = -0.5, size = 2.5)
