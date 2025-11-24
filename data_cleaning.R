#Setup---------
  library(tidyverse)
  library(haven)
  library(tidyverse)
  library(regions)

##Read in data-------------
  dat<-read_dta("./ESS Prep/ess_germany.dta")

  
#Daten Säubern---------
  ##Ost oder West Deutschland------
    #Namen aus dem Regions Packet-  DEG0 zu DEG für Thüringen 
    name_regions<-
      google_nuts_matchtable%>%
        filter(country_code =="DE")%>%
        mutate(bundesland=as.factor(google_region_name),
               region=code_2016)%>%
        select(region,bundesland)%>%
        mutate(region=dplyr::recode(region,
                                        "DEG0"="DEG"))

    
    dat<-dat%>%
      left_join(name_regions, by="region")
    
    #Ost und West definition 
    ost_vec<-c("Berlin","Mecklenburg-Vorpommern","Saxony","Saxony-Anhalt","Brandenburg","Thuringia")
    west_vec<-c("Baden-Württemberg","Bavaria","Bremen","Hamburg","Hesse","Lower Saxony",
                "North Rhine-Westphalia","Rhineland-Palatinate","Saarland","Schleswig-Holstein")    
    
    dat<-
      dat%>%
        mutate(ost = as.factor(case_when(bundesland %in% ost_vec ~ 'Neues Bundesland',
                               bundesland %in% west_vec ~ 'Altes Bundesland')))
      
## Partei Affinität-----------
  #harmonize party in separate file
  #source(party_cleaning.R)
    # Parteinähe-Variablen Erstellen ----
    
    ## Variablen für jede große Partei generieren ----
    
    dat <- dat %>%
      mutate(
        # CDU/CSU (Code 1)
        cdu = case_when(
          prtcl_harmonized == 1 ~ prtdgcl,
          TRUE ~ 0
        ),
        
        # SPD (Code 2)
        spd = case_when(
          prtcl_harmonized == 2 ~ prtdgcl,
          TRUE ~ 0
        ),
        
        # Bündnis 90/Die Grünen (Code 3)
        gruene = case_when(
          prtcl_harmonized == 3 ~ prtdgcl,
          TRUE ~ 0
        ),
        
        # FDP (Code 4)
        fdp = case_when(
          prtcl_harmonized == 4 ~ prtdgcl,
          TRUE ~ 0
        ),
        
        # Die Linke (Code 5)
        linke = case_when(
          prtcl_harmonized == 5 ~ prtdgcl,
          TRUE ~ 0
        ),
        
        # AfD (Code 6)
        afd = case_when(
          prtcl_harmonized == 6 ~ prtdgcl,
          TRUE ~ 0
        )
      )
    
    # Überprüfung (Check) ----
    # Wir prüfen kurz, ob die Zuweisung geklappt hat (Mittelwerte sollten > 0 sein)
    dat %>%
      summarize(
        mean_cdu = mean(cdu, na.rm = TRUE),
        mean_spd = mean(spd, na.rm = TRUE),
        mean_afd = mean(afd, na.rm = TRUE)
      )
    
    
      
    ##Turn standard factors into labeled variables - necessary for export 

        
    ####*Daten Speichern####
    write
    
    write_dta(dat,"pstva.dta")
              