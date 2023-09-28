library(data.table)
library(dplyr)
# Create a sample dataset
institution_summary2 <- data.frame(
  institutionCode = c("A", "B", "C", "D", "E", # make 10 insitutions
                      "F", "G", "H", "I", "J",
                      "B", "B", "B", "B", "B", "B", "B",
                      "A", "A",
                      "J", "J", "J"
  ),
  genus = c("Genus1", "Genus2", "Genus3", "Genus4", "Genus5",
            "Genus6", "Genus7", "Genus8", "Genus9", "Genus10", # give uniq genera to each inst
            "Genus1", "Genus3", "Genus4", "Genus5", "Genus6", "Genus7", "Genus8", # Give inst B a total of 8 uniq genera
            "Genus2", "Genus5", # create a redundancy in A
            "Genus9", "Genus10", "Genus1") # Give J some missing genera + a redundancy
)



institution_summary2 <- as.data.table(institution_summary2)
institution_summary2 <- institution_summary2[, genusCountByInst := length(unique(genus)), by = institutionCode]

unique_g <- length(unique(institution_summary2$genus))
# remove na institutions
institution_summary2 <- institution_summary2[!is.na(institutionCode)]
institution_summary2 <- distinct(institution_summary2, institutionCode, genus, .keep_all = TRUE)
# Iterate through the institutions in descending order of unique genera names
institution_summary2 <- institution_summary2[order(-genusCountByInst)]

### Preference
perc_coverage_desired <-90
# Initialize variables
cumulative_unique_genera <- character(0)
institutions_to_visit <- character(0)
perc_coverage <- 0

# Create a set to track unique genera
unique_genera_set <- character(0)
# Create a preference field 
institution_summary2$preference <- ifelse(institution_summary2$institutionCode == "A", 1, 0)
# Separate institutions with preference 1 and others
preferred_institutions <- institution_summary2[institution_summary2$preference == 1, ]
remaining_institutions <- institution_summary2[institution_summary2$preference == 0, ]

# Iterate through the preferred institutions first
for (i in 1:nrow(preferred_institutions)) {
  if (perc_coverage >= perc_coverage_desired) {
    break  # Exit the loop when desired coverage is achieved
  }
  institution <- preferred_institutions[i, institutionCode]
  genera <- preferred_institutions[i, genus]
  
  # Check if any of the genera are not in the unique set
  new_genera <- genera[!genera %in% unique_genera_set]
  
  if (length(new_genera) > 0) {
    unique_genera_set <- c(unique_genera_set, new_genera)
    institutions_to_visit <- c(institutions_to_visit, institution)
    cumulative_unique_genera <- length(unique_genera_set)
    perc_coverage <- (cumulative_unique_genera / unique_g) * 100
  }
}

# Continue with the remaining institutions in order of laziness
for (i in 1:nrow(remaining_institutions)) {
  if (perc_coverage >= perc_coverage_desired) {
    break  # Exit the loop when desired coverage is achieved
  }
  institution <- remaining_institutions[i, institutionCode]
  genera <- remaining_institutions[i, genus]
  
  # Check if any of the genera are not in the unique set
  new_genera <- genera[!genera %in% unique_genera_set]
  
  if (length(new_genera) > 0) {
    unique_genera_set <- c(unique_genera_set, new_genera)
    institutions_to_visit <- c(institutions_to_visit, institution)
    cumulative_unique_genera <- length(unique_genera_set)
    perc_coverage <- (cumulative_unique_genera / unique_g) * 100
  }
}

unique(institutions_to_visit)
