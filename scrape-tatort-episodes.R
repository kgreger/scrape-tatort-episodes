#install.packages("dplyr")
#install.packages("rvest")
#install.packages("xlsx")
library(dplyr)
library(rvest)
library(xlsx)
setwd("~/GitHub/scrape-tatort-episodes/")
monate <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
dienstgrade <- c("Hauptkommissare", "Kommissare", "Hauptkommissar", "Kommissar", "Polizeipsychologin", 
                 "Oberinspektor", "Inspektor", "Kommissarin", "Zollfahnder", "Verdeckter Ermittler", 
                 "MAD-Oberstleutnant", "Polizeihauptmeister", "Wachtmeister")
einsatzorte <- c("Hamburg", "Saarbrücken", "Köln", "Stuttgart", "Frankfurt", "Kiel", "Wien", "Berlin", 
                 "München", "Baden-Baden", "Bremen", "Essen", "Hannover", "Mainz", "Braunschweig", 
                 "Endwarden", "Duisburg", "Lübeck", "Bremerhaven", "Stade", "Heppenheim (Bergstraße)", 
                 "Ludwigshafen", "Berne", "Leipzig", "Düsseldorf", "Konstanz", "Münster", "Wiesbaden", 
                 "Luzern", "Dortmund", "Erfurt", "Weimar", "Nürnberg", "Dresden", "Freiburg")
einsatzbundesländer <- c("Hamburg", "Saarland", "Nordrhein-Westfalen", "Baden-Württemberg", "Hessen", 
                         "Schleswig-Holstein", "Wien", "Berlin", "Bayern", "Baden-Württemberg", "Bremen", 
                         "Nordrhein-Westfalen", "Niedersachsen", "Rheinland-Pfalz", "Niedersachsen", 
                         "Schleswig-Holstein", "Nordrhein-Westfalen", "Schleswig-Holstein", "Bremen", 
                         "Niedersachsen", "Hessen", "Rheinland-Pfalz", "Bern", "Sachsen", "Nordrhein-Westfalen", 
                         "Baden-Württemberg", "Niedersachsen", "Hessen", "Luzern", "Nordrhein-Westfalen", 
                         "Thüringen", "Thüringen", "Bayern", "Sachsen", "Baden-Württemberg")
einsatzländer <- c("Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", 
                   "Österreich", "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", 
                   "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland", 
                   "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Schweiz", "Deutschland", 
                   "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Schweiz", "Deutschland", 
                   "Deutschland", "Deutschland", "Deutschland", "Deutschland", "Deutschland")
Geodaten <- data.frame(cbind(iconv(einsatzorte, "LATIN1", "UTF-8"), 
                             einsatzbundesländer, 
                             einsatzländer), 
                       stringsAsFactors = FALSE)


# Tatorte
## Daten von der Wikipedia-Seite parsen
url <- "https://de.wikipedia.org/wiki/Liste_der_Tatort-Folgen"
tatorte <- url %>%
  read_html() %>%
  html_node(xpath='//*[@id="mw-content-text"]/table[1]') %>%
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
  mutate(Ermittler = gsub("^Bienzle und Gächter$",
                          "Bienzle",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Blum$",
                          "Blum und Perlmann",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Blum und Perlmann, Matteo Lüthi$",
                          "Blum und Perlmann",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Blum und Perlmann, Reto Flückiger$",
                          "Blum und Perlmann",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Borowski und Brandt$",
                          "Borowski",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Borowski und Jung$",
                          "Borowski",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Carlucci und Gertsch$",
                          "Carlucci",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Delius a.D.$",
                          "Delius",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Dellwo$",
                          "Dellwo und Sänger",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Sänger und Dellwo$",
                          "Dellwo und Sänger",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Eisner$",
                          "Eisner und Fellner",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Falke und Grosz$",
                          "Falke",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Falke und Lorenz$",
                          "Falke",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Flemming und Koch$",
                          "Flemming",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Flemming, Koch und Ballauf$",
                          "Flemming",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Flückiger und Lanning$",
                          "Flückiger und Ritschard",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Hellmann und Ritter$",
                          "Hellmann",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Lürsen$",
                          "Lürsen und Stedefreund",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Lutz und Schreitle$",
                          "Lutz",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Marek a. D.$",
                          "Marek",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Odenthal$",
                          "Odenthal und Kopper",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Ritter und Stark$",
                          "Ritter",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Sänger$",
                          "Dellwo und Sänger",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Sieland, Gorniak, Mohr und Schnabel$",
                          "Sieland, Gorniak und Schnabel",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Stark$",
                          "Ritter",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Steier und Mey$",
                          "Steier",
                          Ermittler)) %>% 
  mutate(Ermittler = gsub("^Stoever$",
                          "Stoever und Brockmöller",
                          Ermittler))

## Daten exportieren
# write.csv(tatorte, 
#           file = "Tatorte.csv", 
#           row.names = FALSE)


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
  ### Einsatzort
  mutate(Einsatzort = gsub("^nordd. Kleinstadt$", 
                           "Endwarden", 
                           Einsatzort)) %>% 
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
  union(kommissare.aktuell) %>% 
  ## Daten mittels RegEx verarbeiten
  ### Einsatzort
  mutate(Einsatzort = gsub("Franken",
                           "Nürnberg",
                           Einsatzort)) %>%
  mutate(Einsatzort = gsub("Heppenheim",
                           "Heppenheim (Bergstraße)",
                           Einsatzort)) %>%
  mutate(Einsatzort = gsub("Bern",
                           "Berne",
                           Einsatzort)) %>%
  mutate(Einsatzort = gsub("Leipzig und Dresden",
                           "Leipzig",
                           Einsatzort)) %>%
  ### Kommissare
## add Felber (ep.303)
  mutate(Kommissare = gsub("^Batic und Leitmayr mit Team$",
                           "Batic und Leitmayr",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Borowski und Brandt$",
                          "Borowski",
                          Kommissare)) %>% 
  mutate(Kommissare = gsub("^Borowski und Jung$",
                          "Borowski",
                          Kommissare)) %>% 
  mutate(Kommissare = gsub("^Cenk Batu$",
                           "Batu",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Heinz Brammer$",
                           "Brammer",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Marianne Buchmüller$",
                           "Buchmüller",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Casstorff und Oberkommissar Holicek$",
                           "Casstorff und Holicek",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Sänger und Dellwo$",
                           "Dellwo und Sänger",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Falke und Grosz$",
                           "Falke",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Haferkamp$",
                           "Haferkamp und Kreutzer",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Robert Hellmann$",
                           "Hellmann",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Paul Kant und Jakob Varasani$",
                           "Kant und Varanasi",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Ludwig Lenz$",
                           "Lenz",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Dorn und Lessing$",
                           "Lessing und Dorn",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Liersdahl$",
                           "Liersdahl und Schäfermann",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Franz Markowitz$",
                           "Markowitz",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Pfeiffer$",
                           "Pfeifer",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Eva Saalfeld und Andreas Keppler$",
                           "Saalfeld und Keppler",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Schimanski$",
                           "Schimanski und Thanner",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Martin Schmidt$",
                           "Schmidt",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Lea Sommer$",
                           "Sommer",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Frank Steier$",
                           "Steier",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Philipp von Burg und Gertsch$",
                           "von Burg und Gertsch",
                           Kommissare)) %>% 
  mutate(Kommissare = gsub("^Till Ritter$",
                           "Ritter",
                           Kommissare)) 
  

## Daten exportieren
# write.csv(kommissare, 
#           file = "Kommissare.csv", 
#           row.names = FALSE)


# Zusammenführen Tatorte, Kommissare und Geodaten
komplett <- tatorte %>% 
  full_join(kommissare, 
            by = c("Ermittler" = "Kommissare")) %>% 
  filter(!is.na(Folge)) %>% 
  select(Folge, Titel, Ermittler, Fall, Einsatzort) %>% 
  left_join(Geodaten, 
            by = c("Einsatzort" = "V1")) %>% 
  rename(Stadt = Einsatzort, 
         Bundesland = einsatzbundesländer, 
         Land = einsatzländer) %>% 
  transform(Folge = as.numeric(Folge), 
            Fall = as.numeric(Fall)) %>% 
  arrange(Folge)


# write data to xslx file
write.xlsx(komplett, 
           file = "Folgen.xlsx",
           sheetName = "Tatort", 
           row.names = FALSE)