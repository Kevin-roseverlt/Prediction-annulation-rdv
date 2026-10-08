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
    consentement_emp                = as.integer(consentement_emp),
    accept_emailing                 = as.integer(accept_emailing),
    prospect_sms                    = as.integer(prospect_sms),
    penalite_remboursement          = ifelse(penalite_remboursement > 0, 1, 0),
    retard_loyer_emp                = ifelse(retard_loyer_emp > 0, 1, 0),
    dette_famille_ami_emp           = ifelse(dette_famille_ami_emp > 0, 1, 0),
    decouvert_emp                   = ifelse(decouvert_emp > 0, 1, 0),
    autre_dette_emp                 = ifelse(autre_dette_emp > 0, 1, 0),
    saisie_sur_salaire_emp          = ifelse(saisie_sur_salaire_emp > 0, 1, 0),
    avis_a_tiers_detenteurs_emp     = ifelse(avis_a_tiers_detenteurs_emp > 0, 1, 0),
    nombre_rejets                   = ifelse(nombre_rejets > 0, 1, 0),
    nombre_commissions_intervention = ifelse(nombre_commissions_intervention > 0, 1, 0),
    tresorerie                      = ifelse((tresorerie + tresorerie_sur_facture) > 0, 1, 0),
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

library(tidyverse)
library(tidymodels)
library(themis)
library(ranger)
library(parsnip)

# ------------------------------------------------------------------------------
# 1. PRÉPARATION ET DÉCOUPE CHRONOLOGIQUE
# ------------------------------------------------------------------------------
data_model <- data_sans_coemp %>% 
  select(-id_dossier, -date_aboutisant_azur, -date_rdv)

n <- nrow(data_model)
train_size <- floor(0.80 * n)

train_data <- data_model[1:train_size, ]
test_data  <- data_model[(train_size + 1):n, ]

# ------------------------------------------------------------------------------
# 2. DÉFINITION DES DEUX RECETTES
# ------------------------------------------------------------------------------
# A. Recette standard (SANS rééquilibrage)
recette_std <- recipe(Y ~ ., data = train_data) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# B. Recette AVEC rééquilibrage SMOTE (sur le train uniquement)
recette_smote <- recette_std %>% 
  step_smote(Y, over_ratio = 0.8) # Rééquilibre la classe minoritaire à ~80% de la classe majeure

# ------------------------------------------------------------------------------
# 3. SPÉCIFICATION DU MODÈLE (Random Forest)
# ------------------------------------------------------------------------------
spec_rf <- rand_forest(trees = 500) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

wf_std   <- workflow() %>% add_recipe(recette_std) %>% add_model(spec_rf)
wf_smote <- workflow() %>% add_recipe(recette_smote) %>% add_model(spec_rf)

# ------------------------------------------------------------------------------
# 4. ENTRAÎNEMENT
# ------------------------------------------------------------------------------
cat("Entraînement du modèle SANS rééquilibrage...\n")
fit_std <- fit(wf_std, data = train_data)

cat("Entraînement du modèle AVEC SMOTE...\n")
fit_smote <- fit(wf_smote, data = train_data)

# ------------------------------------------------------------------------------
# 5. ÉVALUATION ET COMPARAISON SUR LE JEU DE TEST
# ------------------------------------------------------------------------------
eval_std <- predict(fit_std, test_data, type = "prob") %>% 
  bind_cols(predict(fit_std, test_data)) %>% 
  bind_cols(test_data %>% select(Y)) %>% 
  mutate(Approche = "1. Sans rééquilibrage")

eval_smote <- predict(fit_smote, test_data, type = "prob") %>% 
  bind_cols(predict(fit_smote, test_data)) %>% 
  bind_cols(test_data %>% select(Y)) %>% 
  mutate(Approche = "2. Avec SMOTE")

preds_combinees <- bind_rows(eval_std, eval_smote)

# A. Métriques globales (Log-loss, ROC AUC, Brier Score)
metrics_eval <- metric_set(mn_log_loss, roc_auc, brier_class)

tableau_comparatif <- preds_combinees %>% 
  group_by(Approche) %>% 
  metrics_eval(truth = Y, .pred_Annule, event_level = "second") %>% 
  select(Approche, .metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate)

print("=== COMPARAISON DES MÉTRIQUES PROBABILISTES ===")
print(tableau_comparatif)

# B. Sensibilité (Rappel sur les Annulations) et Spécificité à seuil 0.5
cat("\n=== MATRICE DE CONFUSION : SANS RÉÉQUILIBRAGE ===\n")
conf_mat(eval_std, truth = Y, estimate = .pred_class) %>% print()

cat("\n=== MATRICE DE CONFUSION : AVEC SMOTE ===\n")
conf_mat(eval_smote, truth = Y, estimate = .pred_class) %>% print()

# ==============================================================================
# ÉTAPE 5.1 : OPTIMISATION DU SEUIL DE DÉCISION (THRESHOLD TUNING = 0.20)
# ==============================================================================
eval_smote_opt <- eval_smote %>% 
  mutate(
    .pred_class_020 = factor(
      ifelse(.pred_Annule >= 0.20, "Annule", "Fait"),
      levels = c("Fait", "Annule")
    )
  )

cat("\n=== MATRICE DE CONFUSION SMOTE (SEUIL = 0.20) ===\n")
mat_020 <- conf_mat(eval_smote_opt, truth = Y, estimate = .pred_class_020)
print(mat_020)

# Calcul des métriques au seuil 0.20
rappel_020 <- sens(eval_smote_opt, truth = Y, estimate = .pred_class_020, event_level = "second")
prec_020   <- precision(eval_smote_opt, truth = Y, estimate = .pred_class_020, event_level = "second")

cat(sprintf("\nAu seuil de 0.20 :\n - Rappel (Sensibilité) : %.1f%%\n - Précision : %.1f%%\n", 
            rappel_020$.estimate * 100, prec_020$.estimate * 100))

# ==============================================================================
# ÉTAPE 5.2 : IMPORTANCE DES VARIABLES (NATIVE RANGER + GGPLOT2)
# ==============================================================================
# 1. Extraction du modèle ranger sous-jacent
rf_engine <- extract_fit_engine(fit_smote)

# 2. Extraction des scores d'importance et mise en forme dataframe
importance_df <- tibble(
  Variable = names(rf_engine$variable.importance),
  Importance = rf_engine$variable.importance
) %>% 
  slice_max(Importance, n = 15) %>% 
  mutate(Variable = reorder(Variable, Importance))

# 3. Tracé du graphique avec ggplot2
ggplot(importance_df, aes(x = Importance, y = Variable)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Top 15 des variables les plus prédictives (Random Forest + SMOTE)",
    x = "Importance (Gini / Impurity)",
    y = "Variables"
  )

library(tidyverse)
library(tidymodels)
library(themis)
library(ranger)
library(xgboost)
library(rpart)
library(lubridate)

# Fixer la graine aléatoire pour la reproductibilité exigée (Section V du sujet)
set.seed(42)

# ==============================================================================
# ÉTAPE 0 : AUDIT, PRÉPARATION ET CADRE COMMUN (Cadre imposé)
# ==============================================================================
# Hypothèse : data_clean est déjà nettoyé selon la liste blanche (0000-00-00 et fuites supprimées)
data_model <- data_sans_coemp %>% 
  mutate(across(where(is.logical), as.integer)) %>% 
  select(-id_dossier, -date_aboutisant_azur, -date_rdv)

# Validation chronologique 80% Train / 20% Test
n <- nrow(data_model)
train_size <- floor(0.80 * n)

train_data <- data_model[1:train_size, ]
test_data  <- data_model[(train_size + 1):n, ]

# ==============================================================================
# ÉTAPE 1 : ANALYSE EXPLORATOIRE (Sur le jeu de Train uniquement !)
# ==============================================================================
cat("=== ÉTAPE 1 : EXPLORATION SUR LE TRAIN SET ===\n")
# Proportion de Y avec intervalle de confiance à 95%
tab_y <- table(train_data$Y)
prop_test_y <- prop.test(tab_y["Annule"], sum(tab_y))

cat(sprintf("Taux d'annulation global (Train) : %.2f%% [IC 95%%: %.2f%% - %.2f%%]\n",
            prop_test_y$estimate * 100, 
            prop_test_y$conf.int[1] * 100, 
            prop_test_y$conf.int[2] * 100))

# ==============================================================================
# ÉTAPE 2 & 3 & 4 : PIPELINES, MODÈLES ET STRATÉGIES DE RÉÉQUILIBRAGE
# ==============================================================================

# Recette de prétraitement commune
recette_smote <- recipe(Y ~ ., data = train_data) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_smote(Y, over_ratio = 0.8)

# Définition des 4 algorithmes requis (Étape 4)
spec_logit <- logistic_reg() %>% set_engine("glm") %>% set_mode("classification")
spec_tree  <- decision_tree() %>% set_engine("rpart") %>% set_mode("classification")
spec_rf    <- rand_forest(trees = 500) %>% set_engine("ranger", importance = "impurity") %>% set_mode("classification")
spec_xgb   <- boost_tree(trees = 100) %>% set_engine("xgboost") %>% set_mode("classification")

# Création des Workflows
wf_logit <- workflow() %>% add_recipe(recette_smote) %>% add_model(spec_logit)
wf_tree  <- workflow() %>% add_recipe(recette_smote) %>% add_model(spec_tree)
wf_rf    <- workflow() %>% add_recipe(recette_smote) %>% add_model(spec_rf)
wf_xgb   <- workflow() %>% add_recipe(recette_smote) %>% add_model(spec_xgb)

# Entraînement et mesure des temps de calcul
t_logit <- system.time({ fit_logit <- fit(wf_logit, data = train_data) })
t_tree  <- system.time({ fit_tree  <- fit(wf_tree,  data = train_data) })
t_rf    <- system.time({ fit_rf    <- fit(wf_rf,    data = train_data) })
t_xgb   <- system.time({ fit_xgb   <- fit(wf_xgb,   data = train_data) })

# ==============================================================================
# ÉTAPE 5 : COMPARISON CHRONOLOGIQUE MULTI-CRITÈRES SUR LE TEST
# ==============================================================================

evaluer_modele <- function(fit_obj, nom_modele, temps_exec) {
  preds <- predict(fit_obj, test_data, type = "prob") %>% 
    bind_cols(predict(fit_obj, test_data)) %>% 
    bind_cols(test_data %>% select(Y))
  
  # Métriques exigées par le sujet
  auc_val      <- roc_auc(preds, truth = Y, .pred_Annule, event_level = "second")$.estimate
  logloss_val  <- mn_log_loss(preds, truth = Y, .pred_Annule, event_level = "second")$.estimate
  brier_val    <- brier_class(preds, truth = Y, .pred_Annule, event_level = "second")$.estimate
  pr_auc_val   <- pr_auc(preds, truth = Y, .pred_Annule, event_level = "second")$.estimate
  
  tibble(
    Modèle = nom_modele,
    `Log-loss (Clé)` = round(logloss_val, 4),
    `Average Precision` = round(pr_auc_val, 4),
    `ROC AUC` = round(auc_val, 3),
    `Brier Score` = round(brier_val, 3),
    `Temps (s)` = round(temps_exec, 2)
  )
}

tableau_comparatif <- bind_rows(
  evaluer_modele(fit_logit, "Régression Logistique", t_logit["elapsed"]),
  evaluer_modele(fit_tree,  "Arbre de Décision",    t_tree["elapsed"]),
  evaluer_modele(fit_rf,    "Random Forest",        t_rf["elapsed"]),
  evaluer_modele(fit_xgb,   "XGBoost",              t_xgb["elapsed"])
) %>% 
  mutate(Interprétabilité = c("Très élevée", "Élevée", "Moyenne", "Faible"))

cat("\n=== TABLEAU COMPARATIF DES 4 MODÈLES (ÉTAPE 5) ===\n")
print(tableau_comparatif)

# ==============================================================================
# ÉTAPE 6 : INTERPRÉTATION DU MODÈLE RETENU (RANDOM FOREST)
# ==============================================================================

# A. Explication globale (Importance des variables)
rf_engine <- extract_fit_engine(fit_rf)
imp_df <- tibble(
  Variable = names(rf_engine$variable.importance),
  Importance = rf_engine$variable.importance
) %>% 
  slice_max(Importance, n = 10)

cat("\n=== TOP 10 IMPORTANCE GLOBALE (RANDOM FOREST) ===\n")
print(imp_df)

# B. Explication individuelle (Exemple d'un client spécifique)
client_test <- test_data[1, ]
proba_client <- predict(fit_rf, client_test, type = "prob")$.pred_Annule

cat(sprintf("\n--- EXPLICATION INDIVIDUELLE (CLIENT #1) ---\nProbabilité prédite d'annulation : %.1f%%\n", proba_client * 100))
cat("Facteurs principaux associés : Delta =", client_test$delta, "jours | Accord SMS =", client_test$prospect_sms, "\n")
cat("Note : Ces facteurs sont associés à une hausse du risque, sans relation de causalité directe.\n")

# ==============================================================================
# ÉTAPE 7 : APPLICATION MÉTIER & SCÉNARIOS CHIFFRÉS
# ==============================================================================

eval_rf <- predict(fit_rf, test_data, type = "prob") %>% 
  bind_cols(test_data %>% select(Y))

# Hypothèses chiffrées :
# - Coût d'un appel/SMS de relance préventive = 2 €
# - Valeur conservée par un RDV préservé = 50 €
# - Taux d'efficacité de la relance (conversion) = 20% des annulations ciblées sont sauvées

calculer_scenario <- function(seuil, nom_scenario) {
  df <- eval_rf %>% mutate(Alerte = ifelse(.pred_Annule >= seuil, 1, 0))
  
  nb_alertes <- sum(df$Alerte)
  vrais_annules <- sum(df$Alerte == 1 & df$Y == "Annule")
  
  cout_relances <- nb_alertes * 2
  rdv_sauves <- vrais_annules * 0.20
  gain_brut <- rdv_sauves * 50
  gain_net <- gain_brut - cout_relances
  
  tibble(
    Scénario = nom_scenario,
    Seuil = seuil,
    `Alertes (SMS/Appels)` = nb_alertes,
    `Annulations Ciblées` = vrais_annules,
    `Coût Relances (€)` = cout_relances,
    `Gains Net (€)` = gain_net
  )
}

scenarios_metier <- bind_rows(
  calculer_scenario(0.50, "1. Prudent (Seuil 0.50)"),
  calculer_scenario(0.20, "2. Équilibré (Seuil 0.20)"),
  calculer_scenario(0.10, "3. Offensif (Seuil 0.10)")
)

cat("\n=== SCÉNARIOS MÉTIER CHIFFRÉS (ÉTAPE 7) ===\n")
print(scenarios_metier)

# ==============================================================================
# SOUSMISSION DE LA COMPÉTITION (FORMAT EXIGÉ)
# ==============================================================================

# 1. Génération des probabilités sur le jeu de test
soumission <- test_data %>% 
  bind_cols(predict(fit_rf, test_data, type = "prob")) %>% 
  select(
    id_rdv = id_dossier,            # Ajuste le nom de la colonne d'identifiant si besoin
    proba_annulation = .pred_Annule # Probabilité P(Y=1|X)
  )

# 2. Vérifications de conformité aux contraintes du sujet
cat("Nombre de lignes :", nrow(soumission), "\n")
cat("Valeurs manquantes :", sum(is.na(soumission)), "\n")
cat("Aperçu des probabilités :\n")
print(head(soumission))

# 3. Export au format CSV (séparateur virgule, sans guillemets superflus)
write_csv(soumission, "groupe_05_public_1.csv")

cat("\nLe fichier 'groupe_05_public_1.csv' a été généré avec succès dans ton dossier de travail !\n")