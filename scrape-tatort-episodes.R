#install.packages("dplyr")
#install.packages("rvest")
#install.packages("xlsx")
library(dplyr)
library(rvest)
library(xlsx)
setwd("~/GitHub/scrape-tatort-episodes/")
monate <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
dienstgrade <- c("Hauptkommissare", "Kommissare", "Hauptkommissarin", "Hauptkommissar", "Kommissarin", 
                 "Kommissar", "Polizeipsychologin", "Oberinspektor", "Inspektor", "Zollfahnder", 
                 "Verdeckter Ermittler", "MAD-Oberstleutnant", "Polizeihauptmeister", "Wachtmeister", 
                 "Fallanalytikerin", "Polizeioberkommissar", "Oberkommissar", "a.D.")
einsatzorte <- c("Hamburg", "Saarbrücken", "Köln", "Stuttgart", "Frankfurt", "Kiel", "Wien", "Berlin", 
                 "München", "Baden-Baden", "Bremen", "Essen", "Hannover", "Mainz", "Braunschweig", 
                 "nordd. Kleinstadt", "Duisburg", "Lübeck", "Bremerhaven", "Stade", "Heppenheim", 
                 "Ludwigshafen", "Bern", "Leipzig", "Düsseldorf", "Konstanz", "Münster", "Wiesbaden", 
                 "Luzern", "Dortmund", "Erfurt", "Weimar", "Franken", "Dresden", "Freiburg", "Schwarzwald", 
                 "Leipzig und Dresden")
einsatzbundesländer <- c("Hamburg", "Saarland", "Nordrhein-Westfalen", "Baden-Württemberg", "Hessen", 
                         "Schleswig-Holstein", "Wien", "Berlin", "Bayern", "Baden-Württemberg", "Bremen", 
                         "Nordrhein-Westfalen", "Niedersachsen", "Rheinland-Pfalz", "Niedersachsen", 
                         "Schleswig-Holstein", "Nordrhein-Westfalen", "Schleswig-Holstein", "Bremen", 
                         "Niedersachsen", "Hessen", "Rheinland-Pfalz", "Bern", "Sachsen", "Nordrhein-Westfalen", 
                         "Baden-Württemberg", "Niedersachsen", "Hessen", "Luzern", "Nordrhein-Westfalen", 
                         "Thüringen", "Thüringen", "Bayern", "Sachsen", "Baden-Württemberg", "Baden-Württemberg", 
                         "Sachsen")
einsatzländer <- c("Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", 
                   "Österreich", "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", 
                   "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", 
                   "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Schweiz", "Deutschland", 
                   "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Schweiz", "Deutschland", 
                   "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", 
                   "Deutschland")
Geodaten <- data.frame(cbind(iconv(einsatzorte, "LATIN1", "UTF-8"), 
                             einsatzbundesländer, 
                             einsatzländer), 
                       stringsAsFactors = FALSE)


# Tatorte
## Daten von der Wikipedia-Seite parsen
url <- "https://de.wikipedia.org/wiki/Liste_der_Tatort-Folgen"
tatorte <- url %>%
  read_html() %>%
  html_node(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table() %>% 
  ## Daten mittels RegEx verarbeiten
  ### Folge
  mutate(unechter.Tatort = grepl("\\d(?:a|b)\\*", Folge)) %>% 
  filter(!unechter.Tatort) %>% 
  ### Titel
  mutate(Titel = gsub("\\(Folge .+? trägt den(?: gleichen|selben) Titel\\)", 
                      "", 
                      Titel)) %>% 
  ### Sender
  mutate(Sender = gsub("\\n", 
                       "", 
                       Sender)) %>% 
  ### Besonderheiten
  mutate(Besonderheiten = gsub("\\[.+?\\]", 
                               "", 
                               Besonderheiten)) %>% 
  ### Fall
  mutate(Fall = gsub("\\s.*", 
                     "", 
                     Fall))
### Erstausstrahlung
Erstausstrahlung.Datum <- sub("(\\d{1,2})\\.\\s?(\\w+?)\\.?\\s?(\\d{4})(.*)", 
                               "\\1@\\2@\\3@", 
                               tatorte$Erstausstrahlung, 
                               perl = TRUE) %>% 
  strsplit("@")
Erstausstrahlung.Monat <- sapply(Erstausstrahlung.Datum, 
                                 "[",
                                 2) %>% 
  substr(1, 3) %>% 
  match(., monate)
tatorte$Erstausstrahlung <- paste(sapply(Erstausstrahlung.Datum, 
                                         "[",
                                         1), 
                                  Erstausstrahlung.Monat, 
                                  sapply(Erstausstrahlung.Datum, 
                                         "[",
                                         3), 
                                  sep = ".")
### Ermittler (komplett)
tatorte$Ermittler.komplett <- gsub("(\\n.*|\\(.*\\))", 
                                   "", 
                                   tatorte$Ermittler)
### Ermittler
tatorte$Ermittler <- gsub("\\s/.*", 
                          "", 
                          tatorte$Ermittler.komplett)
tatorte <- tatorte %>% 
  mutate(Ermittler = gsub("\\s/.*", 
                          "", 
                          Ermittler)) %>% 
  mutate(Ermittler = gsub(" +$", 
                          "", 
                          Ermittler)) %>% 

## Daten für Export vorbereiten
  select(Folge, Titel, Sender, Erstausstrahlung, Ermittler, Fall, Autor, Regie, Besonderheiten) %>% 
  transform(Folge = as.numeric(Folge), 
            Fall = as.numeric(Fall)) %>% 
  arrange(Folge)

## Daten exportieren
write.csv(tatorte,
          file = "Tatorte.csv",
          row.names = FALSE)


# ehemalige Kommissare
## Daten von der ARD-Seite parsen
url <- "http://www.daserste.de/unterhaltung/krimi/tatort/kommissare/kommissare-ausser-dienst-100.html"
kommissare.ehemalig <- url %>%
  read_html() %>%
  html_node(xpath='//*[@id="content"]/div/div[2]/div[1]/div/div/div/div/div[3]/table') %>%
  html_table() %>% 
  ## Spalten umbenennen
  rename(Kommissare = `Die Kommissare`) %>% 
  ## unnötige Spalte 2 entfernen
  select(-2) %>% 
  ## leere Zeilen entfernen
  filter(Kommissare != "") %>% 
  ## Daten mittels RegEx verarbeiten
  ### Kommissare
  mutate(Kommissare = gsub(paste(dienstgrade, 
                                 collapse = "|"), 
                           "", 
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("\\s{2,}", 
                           " ", 
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^ +", 
                           "", 
                           Kommissare)) %>% 
  ### fehlenden Kommissar hinzufügen
  rbind(c("Felber", "Frankfurt", "Heinz Schubert", "HR", 1))


# aktuelle Kommissare
## Daten von der ARD-Seite parsen
url <- "http://www.daserste.de/unterhaltung/krimi/tatort/kommissare/tatort-filter-aktuelle-kommissare-100.html"
kommissare.aktuell <- url %>%
  read_html() %>%
  html_nodes(css = '.headline') %>% 
  sapply(function(x){html_text(html_children(x))}) %>% 
  unlist() %>% 
  matrix(ncol = 2, 
         byrow = TRUE) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  ## Spalten umbenennen
  rename(Kommissare = X1, 
         Einsatzort = X2) %>% 
  ## Daten mittels RegEx verarbeiten
  mutate(Einsatzort = gsub("\\(|\\)", "", Einsatzort))


# Zusammenführen ehemalige & aktuelle Kommissare
kommissare <- kommissare.ehemalig %>% 
  select(Kommissare, Einsatzort) %>% 
  union(kommissare.aktuell)
  

# Zusammenführen Kommissare und Geodaten
kommissare <- kommissare %>% 
  left_join(Geodaten, 
            by = c("Einsatzort" = "V1")) %>% 
  rename(Stadt = Einsatzort, 
         Bundesland = einsatzbundesländer, 
         Land = einsatzländer)

## Daten exportieren
write.csv(kommissare,
          file = "Kommissare.csv",
          row.names = FALSE)