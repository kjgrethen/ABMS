set.seed(42)

#clear workspace
rm(list = ls())

library("data.table")
library("stringr")


filepath = file.path("D:", "ABMS", "country_outputs")

# List all csv files
country_files <- list.files(filepath, pattern = "\\.csv$", full.names = T)


file = fread(country_files[1])
n_detect = file[,.N]

file = file[det_prob > 0.495, ]
file[, date := as.IDate(sub(".*_(\\d{8})_.*", "\\1", source_zip), format = "%Y%m%d")]
file[, group := cumsum(id == 0)]
file[, val := sum(det_prob > 0.495) >= 2, by = group]
file = file[val == TRUE,]
