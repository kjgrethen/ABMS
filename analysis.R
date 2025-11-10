set.seed(42)

#clear workspace
rm(list = ls())

library("data.table")
library("stringr")
library("ggplot2")
library("ggrepel")


#filepath = file.path("D:", "ABMS", "country_outputs")
filepath = file.path("C:/Users/au784040/Documents_C/ABMS_Biodiversa/country_outputs")

# List all csv files
country_files <- list.files(filepath, pattern = "\\.csv$", full.names = T)

countries = c("Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Finland",
              "Ireland", "Italy", "The Netherlands", "Slovakia", "Spain",       
              "Sweden")


## extract data by country #####
n = length(countries)

files = list()
overview = data.table(country = countries, 
                      n_detect = numeric(n), 
                      n_files = numeric(n),
                      n_detect_valid = numeric(n),
                      n_files_valid = numeric(n),
                      n_species= numeric(n),
                      list_species = vector("list", n),
                      n_species_valid = numeric(n),
                      list_species_valid = vector("list", n),
                      tot_bat_activity = numeric(n))
i = 0

for (c in country_files){
  i = i+1
  
  message("Processing ", i, "/", length(country_files), ": ", basenmae(country_files[i]))

  file = fread(c)
  overview[i, n_detect := file[,.N]]
  overview[i, n_species := length(unique(file$class))]
  overview[i, list_species := unique(file$class)]
  #hist(file$det_prob)
  
  #TODO extract dates
  #file[, date := as.IDate(sub(".*_(\\d{8})_.*", "\\1", source_file), format = "%Y%m%d")]
  
  file[, group := cumsum(id == 0)]
  overview[i,n_files := file[, max(group)]]
 
  #filter for valid detections
  file = file[det_prob > 0.495, ]
  overview[i, n_detect_valid := file[,.N]]
  
  #filter for at least 2 valid detections per file
  file[, val := .N >= 2, by = group]
  file = file[val == TRUE,]
  overview[i, n_files_valid := file[, length(unique(group))]]
  
  #filter out most likely species per file
  species = file[, .(class_prob_sum = sum(class_prob),
                     class_prob_mean = mean(class_prob),
                     n_calls = .N),
                 by = .(group, class)]
  
  #keep only species with highest summed class probability
  species = species[species[, .I[class_prob_sum == max(class_prob_sum)], by = group]$V1]
  file = file[species[,.(group, class)], on = .(group, class), nomatch = 0]
  
  overview[i, n_species_valid := length(unique(file$class))]
  overview[i, list_species_valid := unique(file$class)]
  
  #compute duration
  file[, dur := end_time - start_time]
  overview[i, tot_bat_activity := sum(file$dur)]
  
  files[[basename(c)]] = file[, .(duration = sum(dur)), by = .(group, class, country)]
  
}

data <- rbindlist(files, use.names = TRUE, fill = TRUE)

data[, country := factor(country,  levels= c("belgium", "bulgaria", "croatia", "czech", "denmark", "finland",
                                             "ireland", "italy", "netherlands", "slovakia", "spain",       
                                             "sweden"),  
                         labels = c("Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Finland",
                                    "Ireland", "Italy", "The Netherlands", "Slovakia", "Spain",       
                                    "Sweden"))]

data[, genus := tstrsplit(class, " ")[[1]]]
data[genus == "Eptesicus" | genus == "Nyctalus", genus := "ENV"]


## data plots ####

### percent total activity ####

agg <- data[, .(tot_duration = sum(duration)), by = .(class)]
agg[, percent := tot_duration / sum(tot_duration) * 100]
agg[, class := factor(class, levels = sort(class))]
agg= agg[order(class),]

# Compute cumulative sum of percent in stacking order (factor levels)
agg[, pos := 100 - (cumsum(percent) - percent/2)]

# Optional: only show labels for slices > 2%
agg[, label := ifelse(percent > 2, as.character(class), "")]

tot_act = ggplot(agg, aes(x = "", y = percent, fill = class)) +
  geom_col(width = 1, color = "black") +
  coord_polar("y") +
  theme_void() +
  geom_label_repel( aes(y = pos, label = label),
                   size = 4, nudge_x = 1,
                   show.legend = FALSE, 
                   max.overlaps = Inf) +
  labs(title = "Percent total activity duration per species", 
       fill = "Species")

ggsave("tot_act.png",tot_act, width = 20, height = 17, dpi = 300, units = "cm", bg = "white")

fwrite(agg, "species_activity_aggr.csv", sep = ",")

### by country ####

agg <- data[, .(tot_duration = sum(duration)), by = .(genus, country)]
agg[, percent := tot_duration / sum(tot_duration) * 100, by = country]
agg[, genus := factor(genus, levels = sort(unique(genus)))]
agg= agg[order(genus),]

# Compute cumulative sum of percent in stacking order (factor levels)
agg[, pos := 100 - (cumsum(percent) - percent/2), by = country]

# Optional: only show labels for slices > 2%
agg[, label := ifelse(percent > 2, as.character(genus), ""), by = country]


tot_act_country = ggplot(agg, aes(x = "", y = percent, fill = genus)) +
  geom_col(width = 1, color = "black") +
  coord_polar("y") +
  facet_wrap(country~.)+
  geom_label_repel( aes(y = pos, label = label),
                    size = 4, nudge_x = 1,
                    show.legend = FALSE, 
                    max.overlaps = Inf) +
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold",
                              margin = margin(b = 10, t = 10)), # increase space around facet title
  ) +
  #theme(legend.position = "none") +
  labs(title = "Percent of total activity duration per genus by country", 
       fill = "Genus")

ggsave("tot_act_by_country.png",tot_act_country, width = 30, height = 27, dpi = 300, units = "cm", bg = "white")

fwrite(agg, "species_activity_by_country_aggr.csv", sep = ",")

### validation effort ####

overview[, percent_files_valid := n_files_valid/n_files *100]
overview[, country := factor(country, levels = country[order(-n_files)])]

valid_country = ggplot(overview, aes(x = country)) +
  geom_col(aes(y = n_files), fill = "steelblue", alpha = 0.25) +
  geom_col(aes(y = n_files_valid), fill = "steelblue", alpha = 0.7) +
  geom_text(aes(y = n_files_valid, label = sprintf("%.1f%%", percent_files_valid)),
            hjust = -0.1) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  labs(title = "Manual validation results", y = "Number of files with detections",
       x = "Country")

ggsave("validation_by_country.png",valid_country, width = 15, height = 14, dpi = 300, units = "cm", bg = "white")


#overview[, country := factor(country, levels = country[order(-tot_bat_activity)])]
overview[, tot_bat_activity := tot_bat_activity/60/60]

tot_act_country = ggplot(overview, aes(x = country)) +
  geom_col(aes(y = tot_bat_activity), fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )+
  labs(y = "Total bat activity (h)",
       x = "Country")

ggsave("activity_by_country.png",tot_act_country, width = 15, height = 14, dpi = 300, units = "cm", bg = "white")


## save data ####

overview[, species_excluded := Map(setdiff,list_species, list_species_valid)]

fwrite(data, "species_activity.csv", sep = ",")
fwrite(overview,"country_overview.csv",  sep = ",")
