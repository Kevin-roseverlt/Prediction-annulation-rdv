library(tidyverse)
library(jsonlite)
library(lubridate)

# 1. Importation des données
data <- read_csv("C:/Users/neore/OneDrive/Bureau/M2/BDD/Data.csv")

# 2. Nettoyage global de data_clean
data_clean <- data %>% 
  # Supprimer la colonne d'index et les doublons stricts
  select(-`...1`) %>% 
  distinct() %>% 
  
  # Filtrer les locataires
  filter(type_dossier != "locataire") %>% 
  
  # Cadre commun : Filtre strict sur la cible Y (annuler client vs fait)
  filter(etat %in% c("annuler client", "fait")) %>% 
  mutate(
    Y = ifelse(etat == "annuler client", 1, 0),
    Y = factor(Y, levels = c(0, 1), labels = c("Fait", "Annule"))
  ) %>% 
  
  # Conversion des listes JSON/textuelles en sommes numériques
  mutate(
    immo_mensualite  = map_dbl(immo_mensualite,  ~ sum(unlist(fromJSON(if_else(is.na(.x) || .x == "[]", "[0]", .x))), na.rm = TRUE)),
    immo_crd         = map_dbl(immo_crd,         ~ sum(unlist(fromJSON(if_else(is.na(.x) || .x == "[]", "[0]", .x))), na.rm = TRUE)),
    conso_mensualite = map_dbl(conso_mensualite, ~ sum(unlist(fromJSON(if_else(is.na(.x) || .x == "[]", "[0]", .x))), na.rm = TRUE)),
    conso_crd        = map_dbl(conso_crd,        ~ sum(unlist(fromJSON(if_else(is.na(.x) || .x == "[]", "[0]", .x))), na.rm = TRUE))
  ) %>% 
  
  # Transformation des chaînes sentinelles en NA
  mutate(across(where(is.character), ~ na_if(.x, "0000-00-00"))) %>% 
  mutate(across(where(is.character), ~ na_if(.x, "Invalid date"))) %>% 
  
  # Imputation préalable à 0 pour le calcul des agrégats et binarisations
  mutate(across(
    c(salaire_emp, rev_foncier_emp, assistante_maternelle_emp, allocation_familiale_emp, 
      apl_emp, pension_alimentaire_emp, pension_invalidite_emp, charges_loyer_emp, 
      pension_versee_emp, charge_recurrente_emp, charge_courante_emp, tresorerie, 
      tresorerie_sur_facture, penalite_remboursement, retard_loyer_emp, dette_famille_ami_emp, 
      decouvert_emp, autre_dette_emp, saisie_sur_salaire_emp, avis_a_tiers_detenteurs_emp, 
      loyer_emp, nombre_rejets, nombre_commissions_intervention),
    ~ replace_na(as.numeric(.x), 0)
  )) %>% 
  
  # Binarisation des fichages
  mutate(across(starts_with("fichage_"), ~ as.integer(.x))) %>% 
  
  # Binarisation des charges, APL, trésorerie et incidents selon consignes enseignant
  mutate(
    penalite_remboursement          = ifelse(penalite_remboursement > 0, 1, 0),
    loyer_emp                       = ifelse(loyer_emp > 0, 1, 0),
    retard_loyer_emp                = ifelse(retard_loyer_emp > 0, 1, 0),
    dette_famille_ami_emp           = ifelse(dette_famille_ami_emp > 0, 1, 0),
    decouvert_emp                   = ifelse(decouvert_emp > 0, 1, 0),
    autre_dette_emp                 = ifelse(autre_dette_emp > 0, 1, 0),
    saisie_sur_salaire_emp          = ifelse(saisie_sur_salaire_emp > 0, 1, 0),
    avis_a_tiers_detenteurs_emp     = ifelse(avis_a_tiers_detenteurs_emp > 0, 1, 0),
    nombre_rejets                   = ifelse(nombre_rejets > 0, 1, 0),
    nombre_commissions_intervention = ifelse(nombre_commissions_intervention > 0, 1, 0),
    tresorerie                      = ifelse((tresorerie + tresorerie_sur_facture) > 0, 1, 0),
    apl_emp                         = ifelse(apl_emp > 0, 1, 0),
    apl_coemp                       = ifelse(replace_na(apl_coemp, 0) > 0, 1, 0),
    hebergement_gratuit_coemp       = ifelse(!is.na(hebergement_gratuit_coemp) & hebergement_gratuit_coemp == "Parent", 1, 0),
    retard_impot_emp                = ifelse(replace_na(retard_impot_emp, 0) > 0, 1, 0),
    contrat_emp                     = replace_na(as.character(contrat_emp), "Inconnu"),
    contrat_emp                     = as.factor(contrat_emp)
  ) %>% 
  
  # Calcul des agrégats métier : revenu_menage et charge_menage
  mutate(
    revenu_menage = salaire_emp + rev_foncier_emp + assistante_maternelle_emp + 
      allocation_familiale_emp + apl_emp + pension_alimentaire_emp + pension_invalidite_emp,
    
    charge_menage = charges_loyer_emp + pension_versee_emp + charge_recurrente_emp + charge_courante_emp
  ) %>% 
  
  # Correction des valeurs aberrantes d'ancienneté (> 60 ans)
  mutate(
    anciennete_emp = ifelse(
      anciennete_emp > 60, 
      median(anciennete_emp[anciennete_emp <= 60], na.rm = TRUE), 
      anciennete_emp
    )
  ) %>% 
  
  # Variables temporelles (années retraite, delta et jour de la semaine)
  mutate(
    date_retraite_emp    = as.Date(date_retraite_emp),
    date_rdv             = as.Date(date_rdv),
    date_aboutisant_azur = as.Date(date_aboutisant_azur),
    
    jour_semaine_azur    = as.factor(wday(date_aboutisant_azur, label = TRUE, abbr = FALSE)),
    
    annees_avant_retraite_emp = as.numeric(date_retraite_emp - date_rdv) / 365.25,
    annees_avant_retraite_emp = replace_na(
      annees_avant_retraite_emp, 
      median(annees_avant_retraite_emp, na.rm = TRUE)
    ),
    
    delta = as.numeric(date_rdv - date_aboutisant_azur)
  ) %>% 
  
  # Filtrage des durées négatives
  filter(
    delta >= 0,
    annees_avant_retraite_emp >= 0
  ) %>% 
  
  # Suppression définitive des colonnes inutiles, de fuite (leakage) ou sans variance
  select(
    # Suppressions métier / post-RDV
    -type_dossier,
    -statut_final,
    -etat,
    -hebergement_gratuit_emp,
    -segmentation,
    -conge_parental_emp,
    -conge_parental_coemp,
    -commentaire,
    -date_deja_rachat,
    -date_acquisition,
    -valeur_acquisition,
    -rc_id,
    -assistante_maternelle_coemp,
    -frais_notaire,
    -nature_de_projet,
    
    # Suppressions par préfixe
    -starts_with("immo_"), immo_mensualite, immo_crd,
    -starts_with("conso_"), conso_mensualite, conso_crd,
    -starts_with("csp_"),
    -starts_with("contrat_mariage_"),
    -starts_with("type_invalidite_"),
    -starts_with("valeur_"), valeur_bien_immobilier, valeur_totale,
    
    # Suppressions des composantes détaillées agrégées dans revenu_menage et charge_menage
    -salaire_emp, -rev_foncier_emp, -assistante_maternelle_emp, -allocation_familiale_emp,
    -pension_alimentaire_emp, -pension_invalidite_emp, -charges_loyer_emp, 
    -pension_versee_emp, -charge_recurrente_emp, -charge_courante_emp, -tresorerie_sur_facture,
    
    # Textes / Identifiants non modélisables
    -emp_id,
    -intitule_emp,
    -cp_emp,
    -profession_emp,
    -situation_fam_emp,
    -type_traitement_dossier,
    -affectation,
    -type_support,
    -local_agence,
    
    # Colonnes à variance nulle
    -fichage_FCC_cheque,
    -fichage_FCC_carte,
    -fichage_FICP,
    -nb_etablissement_ficp,
    -charge_future_eventuelle_emp,
    
    # Suppressions des dates brutes après calcul des dérivées
    -date_retraite_emp,
    -date_creation,
    -date_naissance_emp,
    -heure_rdv_debut
  )

# 3. Séparation : Dataset SANS co-emprunteur prêt pour la modélisation
data_sans_coemp <- data_clean %>% 
  filter(is.na(coemp_id)) %>% 
  select(-coemp_id, -contains("coemp"), -starts_with("date_retraite_"))

# 4. Séparation : Dataset AVEC co-emprunteur
data_avec_coemp <- data_clean %>% 
  filter(!is.na(coemp_id)) %>% 
  mutate(
    date_retraite_coemp = as.Date(date_retraite_coemp),
    annees_avant_retraite_coemp = as.numeric(date_retraite_coemp - date_rdv) / 365.25,
    annees_avant_retraite_coemp = replace_na(
      annees_avant_retraite_coemp, 
      median(annees_avant_retraite_coemp, na.rm = TRUE)
    )
  ) %>% 
  filter(annees_avant_retraite_coemp >= 0) %>% 
  select(-date_retraite_coemp)

# 5. Vérification des dimensions et de la répartition de Y
dim(data_clean)
dim(data_avec_coemp)
dim(data_sans_coemp)

# Répartition globale de la cible
prop.table(table(data_clean$Y))

# 6. Visualisation dans RStudio
View(data_clean)
View(data_sans_coemp)

# Résumé statistique général de toutes les colonnes
summary(data_sans_coemp)
