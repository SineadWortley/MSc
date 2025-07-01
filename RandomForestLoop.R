run_sdm_loop <- function(site_name, species_file, env_file, final_species_list, output_prefix) {
  library(dplyr)
  library(tidyr)
  library(randomForest)
  
  cat("???? Reading environment data...\n")
  env_data <- read.csv(env_file) %>%
    select(-starts_with("X")) %>%
    mutate(Cell_ID = paste(Grid, Column, Row, sep = "_")) %>%
    relocate(Cell_ID)
  
  cat("???? Reading species data...\n")
  species_data <- read.csv(species_file)
  selected_species <- final_species_list$Species
  
  cat("???? Transforming to long format...\n")
  long_data <- species_data %>%
    pivot_longer(cols = -c(Grid, Column, Row), names_to = "Species", values_to = "Cover") %>%
    mutate(Cell_ID = paste(Grid, Column, Row, sep = "_")) %>%
    select(Cell_ID, Grid, Column, Row, Species, Cover)
  
  cat("???? Creating full species-grid matrix...\n")
  full_species_grid <- expand.grid(Cell_ID = unique(long_data$Cell_ID), Species = selected_species)
  
  cat("???? Joining species data with full grid...\n")
  filtered_df <- full_species_grid %>%
    left_join(long_data, by = c("Cell_ID", "Species")) %>%
    mutate(Presence = ifelse(is.na(Cover) | Cover == 0, 0, 1)) %>%
    select(Cell_ID, Grid, Column, Row, Species, Presence)
  
  cat("???? Merging with environmental data...\n")
  merged_data <- left_join(filtered_df, env_data, by = "Cell_ID", suffix = c("_sp", "_env"))
  cat("???? Rows in merged_data:", nrow(merged_data), "\n")
  cat("???? Rows with NA Grid:", sum(is.na(merged_data$Grid)), "\n")
  Grid_list <- unique(merged_data$Grid_sp)
  
  all_predictions <- NULL
  all_probabilities <- NULL
  
  # ???? Tracking totals across all grids
  total_skipped_species <- 0
  total_successful_species <- 0
  
  for (test_Grid in Grid_list) {
    train <- merged_data %>% filter(Grid_sp != test_Grid) %>% na.omit()
    test <- merged_data %>% filter(Grid_sp == test_Grid)
    
    cat("???? Testing grid:", test_Grid, "\n")
    
    skipped_species <- 0
    successful_species <- 0
    
    Grid_predictions <- data.frame(Cell_ID = unique(test$Cell_ID))
    Grid_probabilities <- data.frame(Cell_ID = unique(test$Cell_ID))
    
    for (species in selected_species) {
      Grid_predictions[[species]] <- NA
      Grid_probabilities[[species]] <- NA
    }
    
    for (species in selected_species) {
      species_data <- merged_data %>%
        filter(Species == species) %>%
        select(Cell_ID, Grid_sp, Presence, Northness, Vege_heig_max, Soil_moist_4_final, Soil_temp_4_adj)
      
      train_species <- species_data %>% filter(Grid_sp != test_Grid) %>% na.omit()
      test_species <- species_data %>% filter(Grid_sp == test_Grid)
      train_species <- species_data %>% filter(Grid_sp != test_Grid) %>% na.omit()
      test_species <- species_data %>% filter(Grid_sp == test_Grid)
      cat("   ???? Rows in test_species for", species, ":", nrow(test_species), "\n")
      
      if (length(unique(train_species$Presence)) < 2) {
        message(paste("?????? Skipping", species, "in grid", test_Grid, "- only one class in training"))
        skipped_species <- skipped_species + 1
        next
      }
      
      model <- randomForest(
        as.factor(Presence) ~ Northness + Vege_heig_max + Soil_moist_4_final + Soil_temp_4_adj,
        data = train_species,
        ntree = 200,
        mtry = 3
      )
      
      test_species$Prediction <- predict(model, newdata = test_species, type = "class")
      probs <- predict(model, newdata = test_species, type = "prob")
      test_species$Prob <- probs[, "1"]
      
      cat("   ??? Predictions made for", species, "\n")
      cat("   ???? Prediction values:\n")
      print(head(test_species$Prediction))
      
      Grid_predictions[match(test_species$Cell_ID, Grid_predictions$Cell_ID), species] <- test_species$Prediction
      Grid_probabilities[match(test_species$Cell_ID, Grid_probabilities$Cell_ID), species] <- test_species$Prob
      
      successful_species <- successful_species + 1
    }
    
    # ???? Grid summary
    cat("???? Grid", test_Grid, "- Species processed:", length(selected_species),
        "| Skipped:", skipped_species, "\n")
    
    total_skipped_species <- total_skipped_species + skipped_species
    total_successful_species <- total_successful_species + successful_species
    cat("???? Preview Grid_predictions for", test_Grid, "\n")
    print(head(Grid_predictions, 2))
    cat("???? Non-NA predictions count:\n")
    print(colSums(!is.na(Grid_predictions[, -1])))
    all_predictions <- bind_rows(all_predictions, Grid_predictions)
    all_probabilities <- bind_rows(all_probabilities, Grid_probabilities)
  }
  
  cat("???? Final prediction preview:\n")
  print(head(all_predictions, 3))
  
  cat("???? Writing output files...\n")
  write.csv(all_predictions, paste0(output_prefix, "_Predicted_Community_Composition.csv"), row.names = FALSE)
  write.csv(all_probabilities, paste0(output_prefix, "_Prediction_Probabilities.csv"), row.names = FALSE)
  
  # ???? Total summary
  cat("???? Total species processed per grid:", length(selected_species), "\n")
  cat("??? Total species skipped across all grids:", total_skipped_species, "\n")
  cat("??? Finished:", site_name, "\n")
}


# WH test
setwd("C:\\Users\\User\\Documents\\MSc\\Data\\WHSDM")
run_sdm_loop("WH", "WitsiesSpOccurance.csv", "WH_SDM_variables.csv", WH_species_list, "WH")

read.csv("WH_Predicted_Community_Composition.csv") %>% head()

# Check if outputs exist
file.exists("WH_Predicted_Community_Composition.csv")
file.exists("WH_Prediction_Probabilities.csv")

test_data <-read.csv("WH_Predicted_Community_Composition.csv") %>% head()

setwd("C:\\Users\\User\\Documents\\MSc\\Data\\BGSDM")
run_sdm_loop("BG", "Bokong_Transformed_Occurances_Fixed.csv", "BG_SDM_variables.csv", BG_species_list, "BG")