evaluate_performance <- function(observed_file, predicted_file, prob_file = NULL, site_name = "Site") {
  library(dplyr)
  library(pROC)
  
  # Load files
  observed <- read.csv(observed_file)
  predicted <- read.csv(predicted_file)
  
  # Ensure matching structure
  common_species <- intersect(colnames(observed), colnames(predicted))
  common_species <- setdiff(common_species, "Cell_ID")
  
  observed_mat <- observed[, common_species]
  predicted_mat <- predicted[, common_species]
  
  # Convert to numeric (force, just in case)
  observed_mat <- as.data.frame(lapply(observed_mat, as.numeric))
  predicted_mat <- as.data.frame(lapply(predicted_mat, as.numeric))
  
  # --- Species Richness Error ---
  observed_richness <- rowSums(observed_mat, na.rm = TRUE)
  predicted_richness <- rowSums(predicted_mat, na.rm = TRUE)
  richness_rmse <- sqrt(mean((observed_richness - predicted_richness)^2))
  
  # --- Jaccard Similarity (mean across cells) ---
  jaccard_scores <- sapply(1:nrow(observed_mat), function(i) {
    obs <- observed_mat[i, ]
    pred <- predicted_mat[i, ]
    
    # Remove any pairs with NAs
    valid <- !(is.na(obs) | is.na(pred))
    obs <- obs[valid]
    pred <- pred[valid]
    
    if (length(obs) == 0 || length(pred) == 0) return(NA)
    
    intersect <- sum(obs == 1 & pred == 1)
    union <- sum(obs == 1 | pred == 1)
    
    if (is.na(union) || union == 0) return(NA)
    return(intersect / union)
  })
  
  mean_jaccard <- mean(jaccard_scores, na.rm = TRUE)
  
  # --- AUC ---
  mean_auc <- NA
  if (!is.null(prob_file)) {
    probs <- read.csv(prob_file)
    common_species_prob <- intersect(colnames(probs), common_species)
    aucs <- c()
    
    for (sp in common_species_prob) {
      # Make sure it's numeric
      obs_vals <- as.numeric(observed[[sp]])
      prob_vals <- as.numeric(probs[[sp]])
      
      # Skip if observed is only one class
      if (length(unique(obs_vals)) < 2) next
      
      roc_obj <- tryCatch({
        roc(observed[[sp]], probs[[sp]])
      }, error = function(e) NULL)
      
      if (!is.null(roc_obj) && !is.na(roc_obj$auc)) {
        aucs <- c(aucs, roc_obj$auc)
      } else {
        aucs <- c(aucs, NA)
      }
    }
    mean_auc <- mean(aucs, na.rm = TRUE)
  }
  
  # --- Output ---
  cat("\n--- Performance for:", site_name, "---\n")
  cat("Species Richness RMSE:", round(richness_rmse, 3), "\n")
  cat("Mean Jaccard Index:   ", round(mean_jaccard, 3), "\n")
  if (!is.na(mean_auc)) cat("Mean AUC:             ", round(mean_auc, 3), "\n")
  
  # Return as list
  return(list(
    Site = site_name,
    RMSE_Richness = richness_rmse,
    Mean_Jaccard = mean_jaccard,
    Mean_AUC = mean_auc
  ))
}

summary(observed_mat)
summary(predicted_mat)
anyNA(observed_mat)
anyNA(predicted_mat)


setwd("C:\\Users\\User\\Documents\\MSc\\Data\\WHSDM")
dir()
evaluate_performance(
  observed_file = "WH_Observed_Community.csv",
  predicted_file = "WH_Predicted_Community_Composition.csv",
  prob_file = "WH_Prediction_Probabilities.csv",
  site_name = "Witsieshoek"
)

setwd("C:\\Users\\User\\Documents\\MSc\\Data\\BGSDM")
dir()


observed <- read.csv("BG_Observed_Community.csv")
predicted <- read.csv("BG_Predicted_Community_Composition.csv")
probs <- read.csv("BG_Prediction_Probabilities.csv")


evaluate_performance(
  observed_file = "BG_Observed_Community.csv",
  predicted_file = "BG_Predicted_Community_Composition.csv",
  prob_file = "BG_Prediction_Probabilities.csv",
  site_name = "Bokong"
)
