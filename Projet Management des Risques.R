library(tidyverse)

# 1. Importation des données
data <- read_csv("C:/Users/neore/OneDrive/Bureau/M2/BDD/Data.csv")

# 2. Supprimer la colonne d'index + les lignes doublons
data_clean <- data %>% 
  select(-`...1`) %>% 
  distinct()

# 3. Nettoyage et transformation du dataset principal
data_clean <- data_clean %>% 
  # Filtrer pour retirer les locataires
  filter(type_dossier != "locataire") %>% 
  
  # Supprimer des colonnes inutiles, très vides ou post-RDV
  select(
    -type_dossier,
    -hebergement_gratuit_emp,
    -segmentation,
    -conge_parental_emp,
    -conge_parental_coemp,
    -commentaire,
    -date_deja_rachat,
    -date_acquisition,
    -valeur_acquisition, # Suppression confirmée
    -rc_id,
    -assistante_maternelle_coemp,
    -starts_with("immo_"), immo_mensualite, immo_crd,
    -starts_with("csp_"),
    -starts_with("contrat_mariage_"),
    -starts_with("type_invalidite_")
  ) %>% 
  
  # Transformer les TRUE/FALSE en 1/0 pour les colonnes fichage_
  mutate(across(starts_with("fichage_"), ~ as.integer(.x))) %>% 
  
  # Nettoyer les valeurs sentinelles dans les chaînes de caractères
  mutate(across(where(is.character), ~ na_if(.x, "0000-00-00"))) %>% 
  mutate(across(where(is.character), ~ na_if(.x, "Invalid date"))) %>% 
  
  # Binariser loyer_emp et l'ensemble des charges financières
  mutate(
    loyer_emp = ifelse(replace_na(loyer_emp, 0) > 0, 1, 0),
    charges_loyer_emp = ifelse(replace_na(charges_loyer_emp, 0) > 0, 1, 0),
    pension_versee_emp = ifelse(replace_na(pension_versee_emp, 0) > 0, 1, 0),
    charge_recurrente_emp = ifelse(replace_na(charge_recurrente_emp, 0) > 0, 1, 0),
    charge_courante_emp = ifelse(replace_na(charge_courante_emp, 0) > 0, 1, 0),
    charge_future_eventuelle_emp = ifelse(replace_na(charge_future_eventuelle_emp, 0) > 0, 1, 0)
  ) %>% 
  
  # Binariser hebergement_gratuit_coemp (Parent -> 1, NA/autre -> 0)
  mutate(hebergement_gratuit_coemp = ifelse(!is.na(hebergement_gratuit_coemp) & hebergement_gratuit_coemp == "Parent", 1, 0)) %>% 
  
  # Traiter l'ancienneté > 60 ans remplacée par la médiane
  mutate(
    anciennete_emp = ifelse(
      anciennete_emp > 60, 
      median(anciennete_emp[anciennete_emp <= 60], na.rm = TRUE), 
      anciennete_emp
    )
  ) %>% 
  
  # Binariser les APL (NA -> 0, puis > 0 -> 1)
  mutate(
    apl_emp = ifelse(replace_na(apl_emp, 0) > 0, 1, 0),
    apl_coemp = ifelse(replace_na(apl_coemp, 0) > 0, 1, 0)
  ) %>% 
  
  # Imputer les montants de trésorerie à 0
  mutate(
    tresorerie = replace_na(tresorerie, 0),
    tresorerie_sur_facture = replace_na(tresorerie_sur_facture, 0)
  ) %>% 
  
  # Gérer nature_de_projet (imputation + passage en facteur)
  mutate(
    nature_de_projet = replace_na(nature_de_projet, "Inconnu"),
    nature_de_projet = as.factor(nature_de_projet)
  )

# 4. Séparation : Dataset AVEC co-emprunteur (avec transformation retraite)
data_avec_coemp <- data_clean %>% 
  filter(!is.na(coemp_id)) %>% 
  mutate(
    date_retraite_coemp = as.Date(date_retraite_coemp),
    date_rdv = as.Date(date_rdv),
    annees_avant_retraite_coemp = as.numeric(date_retraite_coemp - date_rdv) / 365.25,
    annees_avant_retraite_coemp = replace_na(
      annees_avant_retraite_coemp, 
      median(annees_avant_retraite_coemp, na.rm = TRUE)
    )
  ) %>% 
  select(-date_retraite_coemp)

# 5. Séparation : Dataset SANS co-emprunteur
data_sans_coemp <- data_clean %>% 
  filter(is.na(coemp_id)) %>% 
  # Pour les personnes seules, on enlève toutes les colonnes coemp + la date retraite
  select(-coemp_id, -contains("coemp"), -starts_with("date_retraite_"))

# 6. Vérification des dimensions
dim(data)
dim(data_clean)
dim(data_avec_coemp)
dim(data_sans_coemp)

# 7. Visualisation des datasets
View(data_clean)
View(data_avec_coemp)
View(data_sans_coemp)
