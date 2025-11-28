
#clear workspace
rm(list = ls())

set.seed(42)

library("data.table")
library("stringr")
library("ggplot2")
library("ggrepel")
library("lubridate")
library("RColorBrewer")
library(mgcv)
library(tidyverse)
library(gratia) #model diagnostics
library(DHARMa) #model diagnostics

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
                      tot_bat_activity = numeric(n)
                      )
i = 0

for (c in country_files){
  i = i+1
  
  message("Processing ", i, "/", length(country_files), ": ", basename(country_files[i]))

  file = fread(c)
  overview[i, n_detect := file[,.N]]
  overview[i, n_species := length(unique(file$class))]
  overview[i, list_species := unique(file$class)]
  #hist(file$det_prob)
  
  overview[i,n_files := file[, length(unique(source_file))]]
  
 
  #filter for valid detections
  file[det_prob <= 0.495, class := "empty"]
  overview[i, n_detect_valid := file[class != "empty",.N]]
  
  #filter for at least 2 valid detections per file
  file[class != "empty", val := .N >= 2, by = source_file]
  file[val != TRUE,class := "empty"]
  overview[i, n_files_valid := file[class != "empty", length(unique(source_file))]]
  
  #filter out most likely species per file
  species = file[, .(class_prob_sum = sum(class_prob),
                     class_prob_mean = mean(class_prob),
                     n_calls = .N),
                 by = .(source_file, class)]
  
  #keep only species with highest summed class probability
  species = species[species[, .I[class_prob_sum == max(class_prob_sum)], by = source_file]$V1]
  file = file[species[,.(source_file, class)], on = .(source_file, class), nomatch = 0]
  
  overview[i, n_species_valid := length(unique(file$class))]
  overview[i, list_species_valid := unique(file$class)]
  
  #OLD:compute duration
  #file[, dur := end_time - start_time]
  #overview[i, tot_bat_activity := sum(file$dur)]

  files[[basename(c)]] = file[, unique(file[, .(source_zip, source_file, country, class)])]
  
}

data <- rbindlist(files, use.names = TRUE, fill = TRUE)

data[, habitat_code := sub(".*_([A-Z]\\d+)_.*", "\\1", source_zip)]
data[, habitat := substr(habitat_code, 1, 1)]

data[, country := factor(country,  levels= c("finland", "sweden", "denmark", "ireland", "netherlands", 
                                             "belgium", "czech", "slovakia", "croatia", "bulgaria", "italy", "spain"),  
                         labels = c("Finland", "Sweden", "Denmark", "Ireland", "Netherlands", 
                                    "Belgium", "Czechia", "Slovakia", "Croatia", "Bulgaria", "Bolzano", "Spain"))]
data[, class := factor(class, levels = sort(unique(class)))]
data[, genus := tstrsplit(class, " ")[[1]]]
#combine ENV complex
data[genus == "Eptesicus" | genus == "Nyctalus", genus := "ENV"]
#keep Pnat separate
data[class == "Pipistrellus nathusii", genus := class]
data[genus == "Pipistrellus", genus := "Pipistrellus pip./pyg."]
data[, genus := factor(genus, levels = sort(unique(genus)))]

data[, date := as.IDate(sub(".*_(\\d{8})_.*", "\\1", source_file), format = "%Y%m%d", tz = "UTC")]
data[, time := as.ITime(sub(".*_(\\d{6})\\..*", "\\1", source_file), format = "%H%M%S", tz = "UTC")]
data[, timestamp := ymd_hms(paste(as.character(date), as.character(time)), tz = "UTC")]
#some corrupted file names need to be removed
data = data[!is.na(timestamp),]
data[, night := as.IDate(ifelse(time >= as.ITime("12:01:00"), date, date-1))]
data[, julian_day := yday(night)+1]

fwrite(data, "overall_data.csv", sep = ",")

rm(species, files, file)

## data plots ####

### species distribution ####

#based on N detections per species

#TODO: careful may need to be averaged across the two loactions instead of just n files
agg <- data[, .(n_files = .N), by = .(country, habitat, genus)]
agg = agg[genus != "empty",]
agg[, percent := n_files/sum(n_files), by = .(country, habitat)]

agg[, country := factor(country, levels = rev(levels(country)))]
agg[, habitat := factor(habitat, levels = c("F", "G", "W"), 
                        labels = c("Forest", "Grassland", "Wetland"))]

colors <- brewer.pal(n = 12, name = "Paired")  # 12 colors available in Paired

# Horizontal stacked bar chart faceted by habitat
ggplot(agg, aes(x = percent, y = country, fill = genus)) +
  geom_col() +
  facet_wrap(~habitat) +
  scale_fill_manual(values = c(
    "Barbastellus" = colors[12],
    "ENV" = colors[7],
    "Myotis" = colors[5],
    "Pipistrellus pip./pyg." = colors[4], 
    "Pipistrellus nathusii" = colors[3], 
    "Plecotus" = colors[2],
    "Rhinolophus" = colors[6]
  )) +
  labs(title = NULL, 
       x = "% Detections", 
       y = NULL,
       fill = NULL)+
  theme_bw(base_size = 16)+
  theme(
    strip.background = element_rect(fill = "white", color = "black"),  # removes the grey background
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)  # rotate 45 degrees
  )

### available data ####

# N files with detections per day
agg <- data[, .(n_files = .N, 
                 no_bats = sum(genus == "empty"),
                 bats = sum(genus != "empty")), by = .(country, habitat_code, habitat, julian_day, night)]

agg[, country_label := factor(country,  levels= c("Belgium","Bulgaria","Czechia", "Denmark", "Spain",
                                                  "Finland", "Croatia", "Ireland", "Bolzano", "Netherlands",
                                                  "Sweden", "Slovakia"),  
                         labels = c("BE", "BG", "CZ", "DK", "ES", 
                                    "FI", "HR", "IE", "IT", "NL", "SE", "SK"))]


ggplot(agg, aes(x = julian_day, y = habitat_code)) +
  geom_point() +
  #facet_grid(rows = vars(country_label)) +
  facet_wrap(
    ~ country_label,
    nrow = 6,         # six rows
    ncol = 2,         # two columns
    strip.position = "right"
  )+
  scale_x_continuous(
    limits = c(75,300),
    breaks = c(100, 150, 200, 250, 300)   # keep only these gridlines
  ) +
  labs(title = NULL, 
       x = "Day of the Year", 
       y = "Location code")+
  theme_bw(base_size = 14)+
  theme(
      strip.placement = "outside",  # ensures strips appear on the outer right
      #strip.background = element_rect(fill = "white")
      panel.grid.major.y = element_blank(), # remove all major y gridlines
      panel.grid.minor.y = element_blank(), # remove minor y gridlines
      panel.grid.major.x = element_line(),  # keep default x gridlines
      panel.grid.minor.x = element_blank()  # optional: remove minor x gridlines
    )

### across days ##########

agg[, siteID := interaction(country, habitat_code, sep = "_")]

hist(agg$bats)
mean(agg$bats)
var(agg$bats)
# var > Mean -> (overdispersal) neg binom: make sure model is not zero-inflated

formula <- bats ~ 
  
  #interaction of habitat and country
  habitat*country +
  
  #time variable 
  s(julian_day, by = interaction(habitat, country), bs = "cc", k = 50) +
  
  #random effect of deployment within habitat
  s(siteID, bs = "re")
  
  
#full model
gam_nb <- bam(formula,
               family = nb(), 
               data = agg, 
               method = "fREML",
               discrete = T, # Uses a fast approximation method (good for large data)
               control = gam.control(trace = T), #for console output
               select = TRUE, # to shrink terms that aren't helping to zero
               knots = list(julian_day = c(1, 366))
)

#appraisal:
sim_res = simulateResiduals(gam_nb)
plot(sim_res)
testDispersion(sim_res) #neglectably small disp. value < 1
testZeroInflation(sim_res) #problem -> clear zero-inflation

appraise(
  gam_nb, 
  point_alpha = 0.25,
  method = "simulate",
  type = 'deviance'
)

agg %>% 
  as_tibble %>% 
  mutate(
    pred = predict(gam_nb, ., type = "response") %>% 
      as.vector()
  ) |> 
  ggplot(aes(pred, bats)) +
  geom_bin2d(binwidth = 0.1) +
  stat_summary_bin(fun.data = mean_se) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick") +
  scale_fill_viridis_c(trans = "log10")

#model inspection
summary(gam_nb)

#If the diagnostic "k' index" is near 1 or p-value < 0.05, your k may be too small.
#If the effective degrees of freedom (edf) is close to k-1, consider increasing k.
k.check(gam_nb)

#### binomial model instead ####
agg[, bats_present := as.numeric(I(bats > 0))]

hist(as.numeric(agg$bats_present))
mean(agg$bats_present)
var(agg$bats_present)
# var < Mean 

formula <- bats_present ~ 
  
  #interaction of habitat and country
  habitat*country +
  
  #time variable 
  s(julian_day, by = interaction(habitat, country), bs = "cc", k = 20) +
  
  #random effect of deployment within habitat
  s(siteID, bs = "re")


#full model
gam_bin <- bam(formula,
              family = binomial, 
              data = agg, 
              method = "fREML",
              discrete = T, # Uses a fast approximation method (good for large data)
              control = gam.control(trace = T), #for console output
              select = TRUE, # to shrink terms that aren't helping to zero
              knots = list(julian_day = c(1, 366))
)

#appraisal:
sim_res = simulateResiduals(gam_bin)
plot(sim_res)
testDispersion(sim_res) #neglectably small disp. value < 1
testZeroInflation(sim_res) #problem -> clear zero-inflation

appraise(
  gam_bin, 
  point_alpha = 0.25,
  method = "simulate",
  type = 'deviance'
)

agg %>% 
  as_tibble %>% 
  mutate(
    pred = predict(gam_nb, ., type = "response") %>% 
      as.vector()
  ) |> 
  ggplot(aes(pred, bats_present)) +
  geom_bin2d(binwidth = 0.1) +
  stat_summary_bin(fun.data = mean_se) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick") +
  scale_fill_viridis_c(trans = "log10")

#model inspection
summary(gam_bin)

#If the diagnostic "k' index" is near 1 or p-value < 0.05, your k may be too small.
#If the effective degrees of freedom (edf) is close to k-1, consider increasing k.
k.check(gam_bin)

###### plots ####

#plotting partial effects (effects of one predictor without influence of the others)
draw(gam_nb)

s_gam = smooth_estimates(gam_nb)
s_gam |>
  add_confint() |>
  # Split the interaction column into habitat and country
  separate(`interaction(habitat, country)`, 
           into = c("habitat", "country"), sep = "\\.") |>
  ggplot(aes(y = .estimate, x = julian_day)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
              alpha = 0.2, fill = "forestgreen"
  ) +
  geom_line(colour = "forestgreen", linewidth = 1.5) +
  facet_grid(country ~ habitat, scales = "free")+
  labs(
    y = "Partial effect",
    x = "Days of the year"
  )

All_days = ggplot(fv_trimmed, aes(x = julian_day, y = .fitted)) +
  # ADD VERTICAL DASHED LINES:
  geom_vline(
    xintercept = c(135.5, 227.5),  
    linetype = "dashed",
    color = "grey",
    size = 0.5
  )+
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
  geom_line(size = 0.7) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    limits = c(-0.1, 1.1)
  ) +
  facet_grid(area ~.)+
  stat_bin2d( # default bins = 30 
    data = dat_model,
    aes(x = julian_day, y = plot_bins, fill = after_stat(count)),
    #bins = dat_model[, length(unique(julian_day))]/5, 
    binwidth = c(5,0.05),
    drop = TRUE, 
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(name = "Number of\nobservations", 
                       trans = "log10",
                       #breaks = c(1, 10, 100, 500),
                       option = "viridis",
                       oob = scales::squish) +
  labs(y = "Modelled probability of bat activity", x = "Julian calendar day")+
  theme_bw(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
  )


ggsave(
  filename = "plots/All_days.png",   
  plot = All_days,             
  #device = cairo_pdf,          # high-quality PDF rendering
  width = 20,                  
  height = 30,
  unit = "cm",
  dpi = 300                    
)




###
agg <- data[, .(n_files = .N, 
                no_bats = sum(genus == "empty"),
                bats = sum(genus != "empty")), by = .(country, habitat, julian_day, night)]
agg[, habitat := factor(habitat, levels = c("F", "G", "W"), 
                        labels = c("Forest", "Grassland", "Wetland"))]

range_dat = agg[, .(min_day = min(night),
                    max_day = max(night)), by = country]

hist(agg[, bats])


### OLD - percent total activity ####

agg <- data[, .(tot_duration = sum(duration)), by = .(class)]
agg[, percent := tot_duration / sum(tot_duration) * 100]

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

#### by country ####

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

#### validation effort ####

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
