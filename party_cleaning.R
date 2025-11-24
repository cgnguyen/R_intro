# Harmonization of Party Closeness Variables ----

## Vorbereitung: Konsistentes Handling von Werten ----

# Wir verwenden as.numeric() (Base R), um sicherzustellen, dass wir mit den 
# reinen Zahlencodes arbeiten und Label-Attribute ignoriert werden.

## 1. Harmonische Kodierung der Variablen (case_when) ----

dat <- dat %>%
  mutate(
    
    ### Welle 1: prtcldde ----
    h_prtcldde = case_when(
      # Umkodierung von Welle 1 (SPD ist hier 1, CDU/CSU ist 2)
      as.numeric(prtcldde) == 1 ~ 2, # SPD -> 2
      as.numeric(prtcldde) == 2 ~ 1, # CDU/CSU -> 1
      as.numeric(prtcldde) == 3 ~ 3, # GRÜNE -> 3
      as.numeric(prtcldde) == 4 ~ 4, # FDP -> 4
      as.numeric(prtcldde) == 5 ~ 5, # Die Linke -> 5
      as.numeric(prtcldde) == 8 ~ 7, # Piraten -> 7
      
      # Sonstige/Kleinparteien (6=REP, 7=NPD, 9=Other)
      as.numeric(prtcldde) %in% c(6, 7, 9) ~ 8, 
      
      # Fehlende Werte (66, 77, 88, 99) oder andere werden zu NA
      TRUE ~ NA_real_ 
    ),
    
    ### Welle 2: prtclede ----
    h_prtclede = case_when(
      # Hier stimmen 1 und 2 bereits (1=CDU, 2=SPD)
      as.numeric(prtclede) == 1 ~ 1, # CDU/CSU -> 1
      as.numeric(prtclede) == 2 ~ 2, # SPD -> 2
      as.numeric(prtclede) == 4 ~ 3, # GRÜNE -> 3
      as.numeric(prtclede) == 5 ~ 4, # FDP -> 4
      as.numeric(prtclede) == 3 ~ 5, # Die Linke -> 5
      as.numeric(prtclede) == 6 ~ 6, # AfD -> 6
      as.numeric(prtclede) == 7 ~ 7, # Piraten -> 7
      
      # Sonstige/Kleinparteien
      as.numeric(prtclede) %in% c(8, 9) ~ 8, 
      
      # Fehlende Werte zu NA
      TRUE ~ NA_real_ 
    ),
    
    ### Welle 3: prtclfde ----
    h_prtclfde = case_when(
      as.numeric(prtclfde) == 1 ~ 1, # CDU/CSU -> 1
      as.numeric(prtclfde) == 2 ~ 2, # SPD -> 2
      as.numeric(prtclfde) == 4 ~ 3, # GRÜNE -> 3
      as.numeric(prtclfde) == 5 ~ 4, # FDP -> 4
      as.numeric(prtclfde) == 3 ~ 5, # Die Linke -> 5
      as.numeric(prtclfde) == 6 ~ 6, # AfD -> 6
      
      # Sonstige/Kleinparteien
      as.numeric(prtclfde) == 7 ~ 8, 
      
      TRUE ~ NA_real_ 
    ),
    
    ### Welle 4: prtclgde ----
    h_prtclgde = case_when(
      as.numeric(prtclgde) == 1 ~ 1, # CDU/CSU -> 1
      as.numeric(prtclgde) == 2 ~ 2, # SPD -> 2
      as.numeric(prtclgde) == 4 ~ 3, # GRÜNE -> 3
      as.numeric(prtclgde) == 5 ~ 4, # FDP -> 4
      as.numeric(prtclgde) == 3 ~ 5, # Die Linke -> 5
      as.numeric(prtclgde) == 6 ~ 6, # AfD -> 6
      
      # Sonstige/Kleinparteien (Freie Wähler, dieBasis, PARTEI, Other)
      as.numeric(prtclgde) %in% c(7, 8, 9, 55) ~ 8, 
      
      TRUE ~ NA_real_ 
    )
  )

## 2. Zusammenführen der harmonisierten Variablen (Merging) ----

# coalesce() füllt die neue Variable auf: Es nimmt den Wert aus Welle 4.
# Ist dieser NA, nimmt es Welle 3, usw.

dat <- dat %>%
  mutate(
    prtcl_harmonized = coalesce(
      h_prtclgde, # Welle 4 (Priorität 1)
      h_prtclfde, # Welle 3 (Priorität 2)
      h_prtclede, # Welle 2 (Priorität 3)
      h_prtcldde  # Welle 1 (Priorität 4)
    )
  )

## 3. Zuweisen der Labels (Optional) ----

# Damit die Grafik später schöne Namen anzeigt statt Zahlen 1-8.
dat$prtcl_harmonized <- factor(dat$prtcl_harmonized,
                               levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                               labels = c(
                                 "CDU/CSU",
                                 "SPD",
                                 "Grüne",
                                 "FDP",
                                 "Die Linke",
                                 "AfD",
                                 "Piraten",
                                 "Sonstige"
                               )
)

# Überprüfung (Check)
dat %>% 
  count(prtcl_harmonized)



# ---------------------------------------------------------------------
# 4. Qualitätscheck: Wurden gültige Werte versehentlich gelöscht? ----
# ---------------------------------------------------------------------

# Wir zählen für jede Welle die "Fehler". Ein Fehler ist definiert als:
# 1. Der Originalwert war NICHT NA.
# 2. Der Originalwert war KEIN offizieller Missing-Code (66, 77, 88, 99).
# 3. ABER: Der neue harmonisierte Wert ist NA.

na_check <- dat %>%
  summarize(
    
    # Check Welle 1 (prtcldde)
    # Wir prüfen: Ist der Wert gültig (nicht 66-99), aber h_prtcldde ist NA?
    fehler_w1 = sum(
      !is.na(prtcldde) & 
        !as.numeric(prtcldde) %in% c(66, 77, 88, 99) & 
        is.na(h_prtcldde)
    ),
    
    # Check Welle 2 (prtclede)
    fehler_w2 = sum(
      !is.na(prtclede) & 
        !as.numeric(prtclede) %in% c(66, 77, 88, 99) & 
        is.na(h_prtclede)
    ),
    
    # Check Welle 3 (prtclfde)
    fehler_w3 = sum(
      !is.na(prtclfde) & 
        !as.numeric(prtclfde) %in% c(66, 77, 88, 99) & 
        is.na(h_prtclfde)
    ),
    
    # Check Welle 4 (prtclgde)
    fehler_w4 = sum(
      !is.na(prtclgde) & 
        !as.numeric(prtclgde) %in% c(66, 77, 88, 99) & 
        is.na(h_prtclgde)
    )
  ) %>%
  
  # Gesamtsumme der Fehler berechnen ("adding them up")
  mutate(total_fehler = fehler_w1 + fehler_w2 + fehler_w3 + fehler_w4)

# Ergebnis anzeigen
# Wenn hier überall "0" steht, ist die Harmonisierung perfekt verlaufen.
print(na_check)

# Falls Fehler auftauchen (total_fehler > 0), können wir uns die betroffenen 
# Original-Codes ansehen, um zu verstehen, was fehlt.
# Beispiel für Welle 1:
dat %>%
  filter(!is.na(prtcldde) & !as.numeric(prtcldde) %in% c(66, 77, 88, 99) & is.na(h_prtcldde)) %>%
  count(prtcldde) # Zeigt, welcher Code vergessen wurde

