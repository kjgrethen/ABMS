#clear workspace
rm(list = ls())

library("data.table")
library("stringr")


filepath = file.path("D:", "ABMS")

# List all zip files
dirs <- list.files(filepath, pattern = "\\.zip$", full.names = TRUE)

#for grouping by country
countries = c("croatia", "finland", "belgium", "bulgaria", "czech", "denmark", 
              "ireland", "italy", "slovakia", "spain", "sweden",
              "netherlands")

# Temporary unzip directory
temp_dir <- file.path(filepath, "temp_unzip")
dir.create(temp_dir, showWarnings = FALSE)

# Output directory for final country CSVs
output_dir <- file.path(filepath, "country_outputs")
#dir.create(output_dir, showWarnings = FALSE)

# --- Loop over countries (batches) ---
for (country in countries) {
  message("\nProcessing batch for: ", country)
  
  # Find zip files belonging to this country (case-insensitive)
  country_zips <- dirs[str_detect(tolower(basename(dirs)), country)]
  
  # Skip if no files found
  if (length(country_zips) == 0) {
    message("No zip files found for ", country)
    next
  }
  
  # Initialize list to store CSV data.tables for this country
  dt_list <- list()
  
  # Loop through each zip for the country
  for (z in country_zips) {
    message("  Unzipping: ", basename(z))
    unzip_dir <- file.path(temp_dir, tools::file_path_sans_ext(basename(z)))
    dir.create(unzip_dir, showWarnings = FALSE)
    
    unzip(z, exdir = unzip_dir)
    
    # Path to the Data folder inside the unzipped content
    data_path <- file.path(unzip_dir, "Data")
    csv_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)
    
    if (length(csv_files) == 0) {
      unlink(unzip_dir, recursive = TRUE)
      next
    }
    
    # Read CSVs into a list and tag metadata
    country_dt_list <- lapply(csv_files, function(f) {
      dt <- fread(f)
      dt[, `:=`(country = country, source_zip = basename(z), source_file = basename(f))]
      return(dt)
    })
    
    dt_list <- c(dt_list, country_dt_list)
    
    # Remove unzipped files to free disk space
    unlink(unzip_dir, recursive = TRUE)
    gc()
  }
  
  # Combine all CSVs for this country
  if (length(dt_list) > 0) {
    country_dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
    
    # Save the combined data for this country as CSV
    output_file <- file.path(output_dir, paste0(country, "_data.csv"))
    fwrite(country_dt, output_file)
    
    message("  ✅ Saved: ", output_file)
    
    # Clean up memory
    rm(dt_list, country_dt)
    gc()
  }
}

# Cleanup
unlink(temp_dir, recursive = TRUE)
message("\nAll country batches processed and saved.")