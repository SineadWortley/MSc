library(dplyr)
library(tidyr)

filter_species <- function(file_path, site_name) {
  # Load species occurrence data
  SpData <- read.csv(file_path)
  
  long_df <- SpData %>%
    pivot_longer(cols = -c(Grid, Column, Row),
                 names_to = "Species",
                 values_to = "Cover") %>%
    filter(Cover > 0) %>%
    group_by(Grid, Species) %>%
    summarize(Occurrences = n(), .groups = "drop")
  
  long_full <- SpData %>%
    pivot_longer(cols = -c(Grid, Column, Row),
                 names_to = "Species",
                 values_to = "Cover") %>%
    filter(Cover > 0)
  
  species_counts <- long_df %>%
    group_by(Species) %>%
    summarize(unique_grids = n_distinct(Grid), .groups = "drop")
  
  # Apply your rules
  species_2 <- species_counts %>%
    filter(unique_grids == 2) %>%
    pull(Species)
  
  species_2_qual <- long_df %>%
    filter(Species %in% species_2) %>%
    group_by(Species, Grid) %>%
    summarize(occurrences = sum(Occurrences), .groups = "drop") %>%
    group_by(Species) %>%
    filter(n() == 2, all(occurrences >= 5)) %>%
    pull(Species) %>%
    unique()
  
  species_3 <- species_counts %>%
    filter(unique_grids == 3) %>%
    pull(Species)
  
  species_3_qual <- long_df %>%
    filter(Species %in% species_3) %>%
    group_by(Species) %>%
    summarize(total_occurrences = sum(Occurrences), .groups = "drop") %>%
    filter(total_occurrences >= 10) %>%
    pull(Species)
  
  species_4plus <- species_counts %>%
    filter(unique_grids > 3) %>%
    pull(Species)
  
  final_filtered_df <- bind_rows(
    long_df %>% filter(Species %in% species_2_qual),
    long_df %>% filter(Species %in% species_3_qual),
    long_df %>% filter(Species %in% species_4plus)
  )
  
  final_species_list <- distinct(final_filtered_df, Species)
  
  cat("=== FILTERING SUMMARY FOR", site_name, "===\n")
  cat("Total species after filtering:", n_distinct(final_filtered_df$Species), "\n\n")
  
  return(final_species_list)
}

setwd("C:\\Users\\User\\Documents\\MSc\\Data\\WHSDM")  # Set working directory
dir()  # List files

df <- read.csv("WitsiesSpOccurance.csv")
colnames(df)

WH_species_list <- filter_species("WitsiesSpOccurance.csv", "WH")


setwd("C:\\Users\\User\\Documents\\MSc\\Data\\BGSDM")  # Set working directory
dir()  # List files

BG_species_list <- filter_species("Bokong_Transformed_Occurances_Fixed.csv", "BG")

print(BG_species_list)
nrow(BG_species_list)

head(read.csv("BG_SDM_variables.csv"))
head(read.csv("Bokong_Transformed_Occurances_Fixed.csv"))
