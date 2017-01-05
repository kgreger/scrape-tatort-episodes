#install.packages("rvest")
library(rvest)
library(xlsx)
setwd("~/GitHub/scrape-tatort-episodes/")
monate <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
dienstgrade <- c("Hauptkommissare", "Kommissare", "Hauptkommissar", "Kommissar", "Polizeipsychologin", 
                 "Oberinspektor", "Inspektor", "Kommissarin", "Zollfahnder", "Verdeckter Ermittler", 
                 "MAD-Oberstleutnant", "Polizeihauptmeister", "Wachtmeister")


# Folgen
## scrape data from Wikipedia page
url <- "https://de.wikipedia.org/wiki/Liste_der_Tatort-Folgen"
tatort <- url %>%
  read_html() %>%
  html_node(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table()

## fix data using regex
### Titel
tatort$Titel <- gsub("\\(Folge .+? trägt den(?: gleichen|selben) Titel\\)", 
                     "", 
                     tatort$Titel)
### Sender
tatort$Sender <- gsub("\\n", 
                      "", 
                      tatort$Sender)
### Erstausstrahlung
Erstausstrahlung.Datum <- sub("(\\d{1,2})\\.\\s?(\\w+?)\\.?\\s?(\\d{4})(.*)", 
                               "\\1@\\2@\\3@", 
                               tatort$Erstausstrahlung, 
                               perl = TRUE) %>% 
  strsplit("@")
Erstausstrahlung.Monat <- sapply(Erstausstrahlung.Datum, 
                                 "[",
                                 2) %>% 
  substr(1, 3) %>% 
  match(., monate)
tatort$Erstausstrahlung <- paste(sapply(Erstausstrahlung.Datum, 
                                        "[",
                                        1), 
                                 Erstausstrahlung.Monat, 
                                 sapply(Erstausstrahlung.Datum, 
                                        "[",
                                        3), 
                                 sep = ".")
### Ermittler (komplett)
tatort$Ermittler.komplett <- gsub("(\\n.*|\\(.*\\))", 
                                  "", 
                                  tatort$Ermittler)
### Ermittler
tatort$Ermittler <- gsub("\\s/.*", 
                         "", 
                         tatort$Ermittler.komplett)
### Besonderheiten
tatort$Besonderheiten <- gsub("\\[.+?\\]", 
                         "", 
                         tatort$Besonderheiten)


# Kommissare
## scrape data from ARD page
url <- "http://www.daserste.de/unterhaltung/krimi/tatort/kommissare/kommissare-ausser-dienst-100.html"
kommissare.ehemalig <- url %>%
  read_html() %>%
  html_node(xpath='//*[@id="content"]/div/div[2]/div[1]/div/div/div/div/div[3]/table') %>%
  html_table()
names(kommissare.ehemalig)[names(kommissare.ehemalig)=="Die Kommissare"] <- "Kommissare"
kommissare.ehemalig <- kommissare.ehemalig[kommissare.ehemalig$Kommissare != "", -2]

## fix data using regex
### Kommissare
kommissare.ehemalig$Kommissare <- gsub(paste(dienstgrade, 
                                             collapse = "|"), 
                                       "", 
                                       kommissare.ehemalig$Kommissare) %>% 
  gsub("\\s{2,}", 
       " ", 
       .) %>% 
  gsub("^ +", 
       "", 
       .)


# write data to csv file
write.xlsx(tatort, 
           file = "Folgen.xlsx",
           sheetName = "Tatort", 
           row.names = FALSE)