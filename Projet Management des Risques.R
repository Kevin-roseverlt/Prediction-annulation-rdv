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
  # Supprimer les colonnes inutiles, constantes ou secondaires
  select(
    -type_dossier,
    -hebergement_gratuit_emp,
    -segmentation,
    -conge_parental_emp,
    -conge_parental_coemp,
    -starts_with("immo_"), immo_mensualite, immo_crd,
    -starts_with("csp_"),
    -starts_with("contrat_mariage_")) %>% 
  # Transformer les TRUE en 1 et FALSE en 0 pour les colonnes fichage_
  mutate(across(starts_with("fichage_"), ~ as.integer(.x))) %>%
  # Remplacer 0000-00-00 dans date par NA
  mutate(across(where(is.character), ~ na_if(.x, "0000-00-00"))) %>%
  # Remplacer Invalide date par NA dans date_acquisition
  mutate(across(where(is.character), ~ na_if(.x, "Invalid date")))

# 4. Séparation : Dataset AVEC co-emprunteur
data_avec_coemp <- data_clean %>% 
  filter(!is.na(coemp_id))

# 5. Séparation : Dataset SANS co-emprunteur
data_sans_coemp <- data_clean %>% 
  filter(is.na(coemp_id)) %>% 
  # Pour les personnes seules, on enlève toutes les colonnes coemp
  select(-contains("coemp"))

# 6. Vérification des dimensions
dim(data)
dim(data_clean)
dim(data_avec_coemp)
dim(data_sans_coemp)

# 7. Visualisation dans RStudio
View(data_clean)
View(data_sans_coemp)
View(data_avec_coemp)