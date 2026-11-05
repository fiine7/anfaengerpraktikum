#Question 3:
# "Wie unterscheidet sich der allgemeine Gesundheitszustand von Kindern 
#  mit und ohne berichtete ADHS-Diagnose?"
#
#1.Benötige Pakete laden
if (!requireNamespace("tidyverse", quietly = TRUE))
  install.packages("tidyverse")
if (!requireNamespace("scales", quietly = TRUE))
  install.packages("scales")

library(tidyverse)
library(scales)

#--------
#2.Datenimport
dateinamen <- c("child24(1).csv")
dateipfad <- dateinamen[file.exists(dateinamen)][1]

if (is.na(dateipfad)) {
  stop("FEHLER:Datei wurde nicht gefunden.\n ",
  "Bitte stellen Sie sicher, dass das Skript und die CSV-Datei im selben Ordner liegen.")
}
message("Lese Rohdaten ein von:",dateipfad)
nhis_raw <- read_csv(dateipfad, show_col_types = FALSE)

#3.Datenbereinigung und Definition der Studienpopulation
#-------
#Relevante Variablen:
# AGEP_C: Alter des Kindes (Universe-Filter: ab 2 Jahren erhoben)
# ADHDEV_C: Jemals ADHS-Diagnose (1 = Ja, 2= Nein;7/8/9 = Fehlwerte)
# PHSTAT_C: Allgemeiner Gesundheitszustand (1 = Excellent bis 5 = Poor;7/8/9 = Fehlwerte)

q3_daten <- nhis_raw %>%
  # Schritt 6 des Leitfadens: Angleichung des Alterbereichs auf 2 bis 17 Jahre
  filter(AGEP_C >= 2) %>%
  