library(mgcv)
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
library(rlang)
library(tidymv)
library(viridis)

source("GAM/SAV4-Creation.R")

plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Arial"),
        # title = element_text(face="bold"),
        plot.title = element_text(hjust = 0.5, size = 12, color = "#314963"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#314963"),
        legend.title = element_text(size = 10),
        legend.text.align = 0,
        axis.title.x = element_text(size = 10, margin = margin(t = 5, r = 0,
                                                               b = 10, l = 0)),
        axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10,
                                                               b = 0, l = 0)),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = -45, hjust = 0))

sp_to_skip <- c("Drift algae", "Total seagrass", "Attached algae", "Total SAV")

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet",
               "Loxahatchee River-Lake Worth Creek", "Mosquito Lagoon", "Biscayne Bay", "Florida Keys NMS")

# Function plots GAM with inputs ma and hal
ggplot_gam <- function(ma, hal = "all") {
  
  data <- SAV4 %>% filter(ManagedAreaName==ma)

  if (hal == "combined"){
    species <- unique(data$analysisunit)
    au_col <- "analysisunit"
  } else if(hal == "only"){
    species <- str_subset(unique(data$analysisunit_halid), "Halophila")
    au_col <- "analysisunit_halid"
  } else if(hal == "none"){
    species <- str_subset(unique(data$analysisunit_halid), "Halophila", negate = TRUE)
    au_col <- "analysisunit_halid"
  } else {
    if(ma %in% ma_halspp){
      species <- unique(data$analysisunit)
      au_col <- "analysisunit"
      print(species)
    } else {
      species <- unique(data$analysisunit_halid)
      au_col <- "analysisunit_halid"
      print(species)
    }
  }
  
  min_years <- data %>% 
    group_by(!!sym(au_col)) %>% 
    summarise(n = n_distinct(Year)) %>% pull(n) %>% min()
  
  # k_value <- ifelse(min_years > 2, min_years - 1, 2)
  # print(k_value)
  k_value <- 3
  
  model_list <- list()
  
  for (i in 1:length(species)){
    s <- species[i]
    print(s)
    
    if (s %in% sp_to_skip){
      next
    } else {
      species_data <- data %>% filter(!!sym(au_col) == s, !is.na(BB_pct))
      # at least 10 years of data per species
      if (length(unique(species_data$Year)) >= 10){
        model_list[[s]] <- gam(BB_pct ~ s(relyear, k=k_value, fx = TRUE), data = species_data)
        print(s)
      }
    }
  }
  
  new_data <- expand.grid(relyear = seq(min(data$relyear), max(data$relyear), by = 1),
                          species = species)
  # model predict function
  get_predictions <- function(models, newdata) {
    preds <- lapply(names(models), function(sp) {
      pred_data <- newdata %>% filter(species == sp)
      pred <- predict(models[[sp]], newdata=pred_data, type="link", se.fit=TRUE)
      data.frame(relyear=pred_data$relyear, species=sp, fit=pred$fit, lwr=pred$fit-1.96*pred$se.fit, upr=pred$fit+1.96*pred$se.fit)
    })
    
    bind_rows(preds)
  }
  
  predictions <- get_predictions(model_list, new_data)
  
  color_palette <- scale_color_manual(values = rainbow(length(unique(predictions$species))))
  
  # Scale x-axis data
  year_list <- data %>%
    filter(relyear %in% unique(predictions$relyear)) %>%
    group_by(relyear) %>%
    summarise(Year = list(unique(Year))) %>%
    unnest(Year)
  
  breaks_seq <- seq(from = min(year_list$relyear),
                    to = max(year_list$relyear),
                    by = 3)
  labels_seq <- seq(from = min(year_list$Year),
                    to = max(year_list$Year),
                    by = 3)
  
  plot <- ggplot(predictions, aes(x = relyear, y = fit, color = species)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species), alpha = 0.2) + 
    # geom_line() +
    labs(title = paste0("BB_pct over Years for Seagrass Species in ", ma),
         y = "Braun-Blanquet Percentage",
         x = "Year") +
    color_palette +
    scale_fill_manual(values = rainbow(length(unique(predictions$species)))) +
    plot_theme
  
  return(plot)
}

# All species
ggplot_gam("Estero Bay", "all")

# Halophila combined
ggplot_gam("Estero Bay", "combined")

# Plot without Halophila
ggplot_gam("Estero Bay", "none")

# Plot only Halophila
ggplot_gam("Estero Bay", "only")


ebap_plot <- ggplot_gam("Estero Bay", "combined")


# var is either Salinity or Water_Temperature
add_env <- function(plot_object, var){
  
  # extract MA from plot_object
  ma <- unique(plot_object$plot_env$data$ManagedAreaName)
  
  files <- list.files(here::here("SAV/data/"))
  wq_files <- str_subset(files, "Combined_WQ_WC_NUT_")
  file <- str_subset(wq_files, var)
  
  data <- fread(file, sep='|') %>% filter(ManagedAreaName==ma)
  
  
}
ebap_plot + geom_line()