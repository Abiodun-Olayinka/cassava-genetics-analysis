#!/usr/bin/env Rscript
#===============================================================================
# SCRIPT: add_doi_badge.R
# PURPOSE: Adds Zenodo DOI badge to README
#===============================================================================

# Read DOI from file
doi_file <- "zenodo_doi_info.txt"
if(file.exists(doi_file)) {
  zenodo_doi <- readLines(doi_file)[1]
  # Clean up any extra characters
  zenodo_doi <- gsub("echo .*", "", zenodo_doi)
  zenodo_doi <- gsub('"', '', zenodo_doi)
  zenodo_doi <- trimws(zenodo_doi)
} else {
  zenodo_doi <- readline(prompt = "Enter your Zenodo DOI: ")
}

# Create badge
badge_line <- sprintf("[![DOI](https://zenodo.org/badge/DOI/%s.svg)](https://doi.org/%s)", 
                     zenodo_doi, zenodo_doi)

# Update README
if(file.exists("README.md")) {
  readme <- readLines("README.md")
  
  # Check if badge already exists
  if(any(grepl("zenodo.org/badge", readme))) {
    for(i in seq_along(readme)) {
      if(grepl("zenodo.org/badge", readme[i])) {
        readme[i] <- badge_line
        break
      }
    }
    cat("✅ Updated existing DOI badge\n")
  } else {
    # Add after title
    title_idx <- grep("^#", readme)[1]
    if(!is.na(title_idx)) {
      readme <- append(readme, badge_line, after = title_idx)
      cat("✅ Added DOI badge after title\n")
    } else {
      readme <- c(badge_line, "", readme)
      cat("✅ Added DOI badge at top\n")
    }
  }
  
  writeLines(readme, "README.md")
  
  # Create backup
  backup_path <- paste0("README_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".md")
  file.copy("README.md", backup_path)
  
  cat("✅ README.md updated with DOI badge\n")
  cat("✅ Backup saved to:", backup_path, "\n")
  cat("\n📝 Your DOI:", zenodo_doi, "\n")
} else {
  cat("❌ README.md not found\n")
}
