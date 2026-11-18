# Companion Code For:
# Human Mobility Networks and SARS-CoV-2 Infection Patterns
# across a Gradient of Market Integration in Northeast Madagascar

# Script to Generate Figures and Analyses in the Results Section
# Sections below follow the order of the manuscript Results.
# Date: November 17, 2026
# Author: Tyler M. Barrett, PhD (tyler.barrett@duke.edu)

#################
##  Preamble   ##
#################

# Load Packages
  library(tidyverse)
  library(glmmTMB)
  library(MuMIn)
  library(datawizard)
  library(performance)
  library(scales)
  library(sf)
  library(osmdata)
  library(patchwork)
  library(cowplot)
  library(ggnewscale)
  library(RColorBrewer)
  library(Polychrome)
  library(igraph)
  library(sna)
  library(reshape2)

# Script Identifier (names this script's output folder)
  SCRIPT_OUT <- "replicate_results_figures"
  
# Figure 3 Draws Administrative Boundaries From The OpenStreetMap Overpass API,
# which is slow and intermittently unavailable. The figure is drawn by default and
# nothing else in the script depends on it, so a caller can set RUN_FIG3 <- FALSE
# before sourcing this script to skip it when rerunning the other analyses.
  if (!exists("RUN_FIG3")) RUN_FIG3 <- TRUE

# Helper: Create Output Directories For This Script
  script_out_dirs <- function(outputs_root, script_out) {
    dirs <- list(
      fig = file.path(outputs_root, script_out, "figures"),
      tab = file.path(outputs_root, script_out, "tables"),
      mod = file.path(outputs_root, script_out, "models")
    )
    invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
    dirs
  }

# File Paths (user name resolves per machine)
# Inputs come from Box. Outputs go to outputs/<script>/ inside this project,
# one folder per analysis script.
  user <- Sys.getenv("USERNAME")
  if (user == "") user <- Sys.getenv("USER")   # macOS and Linux

  fp <- if (Sys.info()[["sysname"]] == "Windows") {
    file.path("C:/Users", user, "Box", "EEID_Data_public")
  } else {
    file.path("/Users", user, "Library/CloudStorage/Box-Box", "EEID_Data_public")
  }

  data_root <- file.path(fp, "manuscripts", " Completed-submitted",
                         "mobility_and_market_integration",
                         "Data")

  project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  outputs_root <- file.path(project_root, "outputs")

  cat("Running as user =", user, "\n")

# Readers without access to the Box folder: place the five data files in the
# working directory, or set data_root to the folder that contains them.
  if (!dir.exists(data_root)) data_root <- project_root

  data_files <- c("survey_infection_data.csv", "commune_edgelist.csv",
                  "commune_nodelist.csv", "travel_similarity_edgelist.csv",
                  "travel_destination_edgelist.csv")

  if (!all(file.exists(file.path(data_root, data_files)))) {
    stop("Data files not found in: ", data_root,
         "\nMissing: ", paste(data_files[!file.exists(file.path(data_root, data_files))],
                              collapse = ", "),
         "\nSet data_root to the folder containing the data files.")
  }

  cat("Data directory: ", data_root, "\n", sep = "")

  .d      <- script_out_dirs(outputs_root, SCRIPT_OUT)
  fig_dir <- .d$fig
  tab_dir <- .d$tab
  mod_dir <- .d$mod
  cat("Output directory: ", file.path(outputs_root, SCRIPT_OUT), "\n", sep = "")

# Load Data
  survey_infection_data <- read_csv(file.path(data_root, "survey_infection_data.csv"))
  commune_el <- read_csv(file.path(data_root, "commune_edgelist.csv"))
  commune_nl <- read_csv(file.path(data_root, "commune_nodelist.csv"))
  travel_similarity_el <- read_csv(file.path(data_root, "travel_similarity_edgelist.csv"))
  travel_destination_el <- read_csv(file.path(data_root, "travel_destination_edgelist.csv"))

# Functions
  # Function To Map Commune Network
    map_commune_network <- function(el, nl, plot_title) {
    # Add GPS Locations to Edgelist
      el <- el %>%
        left_join(nl, by = c("home_commune" = "commune")) %>%
        rename(lat_from = lat, lon_from = long) %>%
        left_join(nl, by = c("visited_commune" = "commune")) %>%
        rename(lat_to = lat, lon_to = long) %>%
        mutate(
          lon_from = as.numeric(lon_from),
          lat_from = as.numeric(lat_from),
          lon_to = as.numeric(lon_to),
          lat_to = as.numeric(lat_to)
        )

    # Convert Nodelist to Shapefile
      nodes_sf <- st_as_sf(nl, coords = c("long", "lat"), crs = 4326)

    # Convert Edgelist to Shapefile
      edge_lines <- lapply(1:nrow(el), function(i) {
        row <- el[i, ]
        st_linestring(matrix(c(row$lon_from, row$lat_from, row$lon_to, row$lat_to), ncol = 2, byrow = TRUE))
      })

      edges_sf <- st_sfc(edge_lines, crs = 4326)
      edges_sf <- st_sf(geometry = edges_sf)

    # Plot Network on OSM Base Map
      ggplot() +
        geom_sf(data = osm_admin_region$osm_multipolygons, fill = "#f5f5f5", alpha = 1) +  # Region boundaries
        geom_sf(data = osm_admin_commune$osm_multipolygons, fill = "#f5f5f5", alpha = 1) +  # Commune boundaries
        geom_sf(data = edges_sf, color = "navy", size = 0.8, alpha = 0.3) +  # Network edges
        geom_sf(data = nodes_sf, color = "orange", size = 2) +  # Network nodes
        theme_minimal() +
        labs(title = plot_title) +
        theme(
          panel.spacing = unit(0, "cm"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.text.y = element_text(size = 16),
          plot.margin = margin(5, 5, 15, 5, "pt"),
          plot.title = element_text(hjust = 0.5, size = 20)
        ) +
        coord_sf(expand = TRUE)
  }

  # Functions To Match Importance Values To Coefficient Names
    get_importance_value_mobility_models <- function(coef_name, importance_obj) {
      # Create mapping from coefficient names (with levels) to sw() variable names (simplified)
        name_mapping <- c(
            "Age" = "cond(age_std)",
            "Men (Ref: Women)" = "cond(gender)",
            "Secondary Education (Ref: Primary Education)" = "cond(school_level)",
            "Higher Education (Ref: Primary Education)" = "cond(school_level)",
            "Non-Farmer (Ref: Farmer)" = "cond(main_activity)",
            "Children Under Three" = "cond(household_under_3_std)",
            "Percent of Crops Sold" = "cond(sell_avg_std)",
            "Household Lifestyle Index" = "cond(house_sol_std)",
            "Durable Goods Owned" = "cond(durable_goods_index_std)"
          )

      # Map coefficient name to sw() variable name
        sw_name <- name_mapping[coef_name]
        if (!is.na(sw_name) && sw_name %in% names(importance_obj)) {
          return(as.numeric(importance_obj[sw_name]))
        } else {
          return(0.0)  # Default value if variable not found
        }
      }
    get_importance_value_sars2_model <- function(coef_name, importance_obj) {
      # Create mapping from coefficient names (with levels) to sw() variable names (simplified)
        name_mapping <- c(
            "Age" = "cond(age_std)",
            "Men (Ref: Women)" = "cond(gender)",
            "Secondary Education (Ref: Primary Education)" = "cond(school_level)",
            "Higher Education (Ref: Primary Education)" = "cond(school_level)",
            "Non-Farmer (Ref: Farmer)" = "cond(main_activity)",
            "Children Under Three" = "cond(household_under_3_std)",
            "Percent of Crops Sold" = "cond(sell_avg_std)",
            "House Lifestyle Index" = "cond(house_sol_std)",
            "Durable Goods Owned" = "cond(durable_goods_index_std)",
            "Travel Frequency" = "cond(travel_frequency_std)",
            "Distance Traveled in Past Year" = "cond(travel_distance_km_std)"
          )

      # Map coefficient name to sw() variable name
        sw_name <- name_mapping[coef_name]
        if (!is.na(sw_name) && sw_name %in% names(importance_obj)) {
          return(as.numeric(importance_obj[sw_name]))
        } else {
          return(0.0)  # Default value if variable not found
        }
    }

##############################################
##  Results: Participant Characteristics    ##
##############################################

# Age, Gender, Household Composition, Pandemic Travel Change
  survey_infection_data %>%
    summarise(n = n(),
              median_age = median(age),
              age_q1 = quantile(age, .25),
              age_q3 = quantile(age, .75),
              women = sum(gender == "Female"),
              pct_women = round(100 * mean(gender == "Female"), 1),
              kids = sum(household_under_3 > 0),
              pct_kids = round(100 * mean(household_under_3 > 0), 1),
              no_covid_change = sum(covid_travel_change == 0),
              pct_no_change = round(100 * mean(covid_travel_change == 0), 1))

# Education And Main Income-Generating Activity
  survey_infection_data %>% count(school_level) %>% mutate(pct = round(100 * n / sum(n), 1))
  survey_infection_data %>% count(main_activity) %>% mutate(pct = round(100 * n / sum(n), 1))

############################################################
##  Results: Characterizing Market Integration (Fig. 2)   ##
############################################################

# Market Integration Measures Across All Communities
  survey_infection_data %>%
    summarise(across(c(durable_goods_index, house_sol, sell_avg),
                     list(med = median,
                          q1 = ~ quantile(.x, .25),
                          q3 = ~ quantile(.x, .75))))

# Most And Least Market Integrated Communities, By Each Measure
  survey_infection_data %>%
    group_by(village) %>%
    summarise(goods = median(durable_goods_index),
              house = median(house_sol),
              crops = median(sell_avg), .groups = "drop") %>%
    summarise(across(c(goods, house, crops), list(min = min, max = max)))

# Interquartile Ranges For The Extreme Communities Named In The Results
  survey_infection_data %>%
    group_by(village) %>%
    summarise(goods_med = median(durable_goods_index),
              goods_q1 = quantile(durable_goods_index, .25),
              goods_q3 = quantile(durable_goods_index, .75),
              house_med = median(house_sol),
              house_q1 = quantile(house_sol, .25),
              house_q3 = quantile(house_sol, .75),
              crops_med = median(sell_avg),
              crops_q1 = quantile(sell_avg, .25),
              crops_q3 = quantile(sell_avg, .75),
              .groups = "drop") %>%
    arrange(desc(goods_med)) %>%
    slice(c(1, n())) %>%
    print(width = Inf)

##################################################
##  Figure 2. Market Integration by Community   ##
##################################################

# Create Custom Color Palette for Villages
  n_villages <- n_distinct(survey_infection_data$village)

  set.seed(42)
  custom_palette <- createPalette(n_villages, c("#FF0000", "#00FF00", "#0000FF"), M = 1000)
  names(custom_palette) <- NULL   # strip names Polychrome adds

# Assign Colors to Specific Villages
  all_villages <- unique(survey_infection_data$village)
  village_colors <- setNames(custom_palette, all_villages)

# Durable Goods Owned
  # Calculate Medians
    village_medians <- survey_infection_data %>%
      group_by(village) %>%
      summarise(median_goods = median(durable_goods_index, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(median_goods))
    survey_infection_data$village_ordered <- factor(survey_infection_data$village, levels = village_medians$village)

  # Make Plot
    goods <- ggplot(survey_infection_data, aes(durable_goods_index, village_ordered)) +
      geom_boxplot(aes(fill = village_ordered)) +
      labs(
        x = "Number of Durable Goods Owned",
        y = ""
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        text = element_text(size = 35),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      scale_fill_manual(values = village_colors) +
      scale_x_continuous(
        limits = c(0, 11),
        breaks = seq(0, 11, by = 1)
      )

# Household Lifestyle Index
  # Calculate Medians
    village_medians_house <- survey_infection_data %>%
      group_by(village) %>%
      summarise(median_house = median(house_sol, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(median_house))
    survey_infection_data$village_ordered_house <- factor(survey_infection_data$village, levels = village_medians_house$village)

  # Make Plot
    house <- ggplot(survey_infection_data, aes(house_sol, village_ordered_house)) +
      geom_boxplot(aes(fill = village_ordered_house)) +
      labs(
        x = "Less MI ↔ More MI",
        y = ""
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        text = element_text(size = 35),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      scale_fill_manual(values = village_colors) +
      scale_x_continuous(
        limits = c(-10, 12),
        breaks = seq(-10, 12, by = 1)
      )

# Crops Sold
  # Calculate Medians
    village_medians_crops <- survey_infection_data %>%
      group_by(village) %>%
      summarise(median_sell = median(sell_avg, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(median_sell))
    survey_infection_data$village_ordered_crops <- factor(survey_infection_data$village, levels = village_medians_crops$village)

  # Make Plot
    crops <- ggplot(survey_infection_data, aes(sell_avg, village_ordered_crops)) +
      geom_boxplot(aes(fill = village_ordered_crops)) +
      labs(
        x = "Crops Sold (%)",
        y = ""
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        text = element_text(size = 35),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      scale_fill_manual(values = village_colors) +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10)
      )

# Create a Shared Legend
  legend_plot <- ggplot(survey_infection_data, aes(durable_goods_index, village_ordered)) +
    geom_boxplot(aes(fill = village_ordered)) +
    scale_fill_manual(values = village_colors, name = "Community") +
    theme_minimal() +
    theme(
      legend.text = element_text(size = 35),
      legend.title = element_text(size = 35, face = "bold"),
      legend.key.size = unit(1, "cm")
    ) +
    guides(fill = guide_legend(ncol = 2))
  shared_legend <- cowplot::get_legend(legend_plot)

# Create a Four-Paneled Plot
  combined_mi_plot <- patchwork::wrap_plots(
    shared_legend, goods,
    house, crops,
    ncol = 2, nrow = 2,
    widths = c(1, 1),
    heights = c(1, 1)
  ) + patchwork::plot_annotation(
    tag_levels = list(c('', 'A', 'B', 'C')),
    theme = theme(plot.tag = element_text(size = 32, face = "bold"))
  )
print(combined_mi_plot)

# Save the Plot
  ggsave(file.path(fig_dir, "fig2_mi_panel_plot.png"),
         plot = combined_mi_plot,
         width = 24,
         height = 18,
         dpi = 600,
         bg = "white")

############################################################
##  Results: Characterizing Regional Mobility (Fig. 3)    ##
############################################################

# Destination Kaomininas Named, Including Participants Who Named None
  survey_infection_data %>%
    left_join(travel_destination_el %>% count(participant_id, name = "n_dest"),
              by = "participant_id") %>%
    mutate(n_dest = replace_na(n_dest, 0)) %>%
    summarise(median = median(n_dest),
              q1 = quantile(n_dest, .25),
              q3 = quantile(n_dest, .75),
              named_none = sum(n_dest == 0))

# Total Destination Kaomininas Reported, Against The 17 Where Communities Sat
  n_distinct(travel_destination_el$visited_commune)

# How Often The Five-Destination Survey Cap Was Reached
  travel_destination_el %>%
    count(participant_id, name = "n_destinations") %>%
    count(n_destinations) %>%
    mutate(pct = round(100 * n / sum(n), 1))

# Trip Reasons And Modes, Overall
# Participants could report more than one mode for a single trip, so the mode
# percentages are the share of trips involving each mode and do not sum to 100.
  survey_infection_data %>%
    summarise(across(starts_with("pct_") & ends_with("_trips"),
                     ~ round(mean(.x), 1))) %>%
    pivot_longer(everything(), names_to = "measure", values_to = "mean_pct") %>%
    print(n = Inf)

# Trip Reasons And Modes By Main Economic Activity
  survey_infection_data %>%
    group_by(main_activity) %>%
    summarise(n = n(),
              across(starts_with("pct_") & ends_with("_trips"),
                     ~ round(mean(.x, na.rm = TRUE), 1)),
              .groups = "drop") %>%
    pivot_longer(starts_with("pct_"), names_to = "measure", values_to = "mean_pct") %>%
    pivot_wider(names_from = main_activity, values_from = mean_pct) %>%
    print(n = Inf)

  survey_infection_data %>%
    group_by(main_activity) %>%
    summarise(n = n(),
              median_frequency = median(travel_frequency),
              median_distance_km = round(median(travel_distance_km), 1),
              .groups = "drop")

#############################################################
##  Figure 3. Regional Travel Network in SAVA Madagascar   ##
#############################################################

if (RUN_FIG3) {

# Create Separate Edgelists By Travel Reason For Plotting
  familyfriends_el <- commune_el %>%
    filter(travel_reason_family_friends > 0)
  econ_el <- commune_el %>%
    filter(travel_reason_econ > 0)
  school_el <- commune_el %>%
    filter(travel_reason_children_school > 0)
  health_el <- commune_el %>%
    filter(travel_reason_health > 0)
  other_el <- commune_el %>%
    filter(travel_reason_other > 0)

# Load Map Data
# Administrative boundaries come from the OpenStreetMap Overpass API, which is
# slow and intermittently unavailable, so they are fetched once and cached.
# Delete models/osm_boundaries.rds to re-fetch them.
  nodes_sf <- st_as_sf(commune_nl, coords = c("long", "lat"), crs = 4326)
  bbox <- st_bbox(nodes_sf)

  osm_cache <- file.path(mod_dir, "osm_boundaries.rds")

  if (file.exists(osm_cache)) {
    .osm <- readRDS(osm_cache)
    osm_admin_commune <- .osm$commune
    osm_admin_region  <- .osm$region
    cat("Loaded cached OSM boundaries from: ", osm_cache, "\n", sep = "")
  } else {
    osm_admin_commune <- opq(bbox = bbox, timeout = 300) %>%
      add_osm_feature(key = "boundary", value = "administrative") %>%
      add_osm_feature(key = "admin_level", value = "8") %>%
      osmdata_sf()
    osm_admin_region <- opq(bbox = bbox, timeout = 300) %>%
      add_osm_feature(key = "boundary", value = "administrative") %>%
      add_osm_feature(key = "admin_level", value = "4") %>%
      add_osm_feature(key = "name", value = "Sava") %>%
      osmdata_sf()
    saveRDS(list(commune = osm_admin_commune, region = osm_admin_region), osm_cache)
  }

# Generate Plots for Each Network
  plot_list <- list()
  el_list <- list('All Travel' = commune_el,
                  Economic = econ_el,
                  'Visit Family and Friends' = familyfriends_el,
                  Healthcare = health_el,
                  'Take Children to School' = school_el,
                  'Other Reason' = other_el)
  for (i in seq_along(el_list)) {
    nl <- commune_nl %>% filter(commune %in% unique(c(el_list[[i]]$home_commune, el_list[[i]]$visited_commune)))
    plot_list[[i]] <- map_commune_network(el_list[[i]], nl, plot_title = names(el_list)[i])
}

# Create a Six-Paneled Plot
  panel_map_plot <- (plot_list[[1]] + plot_list[[2]] + plot_list[[3]]) /
    (plot_list[[4]] + plot_list[[5]] + plot_list[[6]]) +
    plot_layout(
      guides = "collect",
      widths = rep(1, 3),
      heights = rep(1, 2)
    ) +
    plot_annotation(
      theme = theme(
        plot.margin = margin(0, 0, 0, 0)
      )
    )
print(panel_map_plot)

# Save The Plot
  ggsave(
    file.path(fig_dir, "fig3_travel_net_plot.tiff"),
    plot = panel_map_plot,
    width = 15,
    height = 13,
    dpi = 600,
    device = "tiff",
    compression = "lzw",
    bg = "white",
    units = "in"
  )

} else {
  cat("Skipping Figure 3 (RUN_FIG3 is FALSE).\n")
}

####################################################################
##  Results: Market Integration and Regional Mobility (Fig. 4)    ##
####################################################################

# Select Variables for Analysis
  mobility_model_df <- survey_infection_data %>%
    select(participant_id, age, gender, school_level, main_activity, household_under_3,
           village, house_sol, durable_goods_index, sell_avg, travel_frequency,
           travel_distance_km, starts_with("pct_"), covid_travel_change, vaccinated_covid)

# Standardize Continuous Predictor Variables
  mobility_model_df <- mobility_model_df %>%
    mutate(age_std = datawizard::standardize(age),
           household_under_3_std = datawizard::standardize(household_under_3),
           durable_goods_index_std = datawizard::standardize(durable_goods_index),
           house_sol_std =  datawizard::standardize(house_sol),
           sell_avg_std = datawizard::standardize(sell_avg))

# Specify Factor Levels
  mobility_model_df <- mobility_model_df %>%
    mutate(school_level = factor(school_level, levels = c("Primary", "Secondary", "Higher")),
           gender = factor(gender, levels = c("Female", "Male")),
           main_activity = factor(main_activity, levels = c("farmer", "nonfarmer")))

# Sample Size Reported In The Results
  nrow(mobility_model_df)

# Travel Distance Model
  # Build Global Model
    global_model_distance <- glmmTMB(travel_distance_km ~ age_std + gender + school_level + main_activity +
                                       household_under_3_std + durable_goods_index_std + house_sol_std + sell_avg_std + (1 | village),
                                     data = mobility_model_df,
                                     family = tweedie(link = "log"),
                                     na.action = na.fail)

    summary(global_model_distance)

  # Use Dredge for Model Comparison
    m_distance_dredge <- dredge(global_model_distance)

  # Average Models With Delta AICc < 2
    m_distance_avg <- model.avg(m_distance_dredge, subset = delta < 2)
    summary(m_distance_avg)

# Travel Frequency Model
  # Build Global Model
    global_model_frequency <- glmmTMB(travel_frequency ~ age_std + gender + school_level + main_activity +
                                        household_under_3_std + durable_goods_index_std + house_sol_std + sell_avg_std + (1 | village),
                                      data = mobility_model_df,
                                      family = nbinom2(link = "log"),
                                      na.action = na.fail)
    summary(global_model_frequency)

  # Use Dredge for Model Comparison
    m_frequency_dredge <- dredge(global_model_frequency)

  # Average Models With Delta AICc < 2
    m_frequency_avg <- model.avg(m_frequency_dredge, subset = delta < 2)
    summary(m_frequency_avg)

# Collinearity Diagnostics For The Mobility Models
  performance::check_collinearity(global_model_distance)
  performance::check_collinearity(global_model_frequency)

  mobility_model_df %>%
    select(age_std, household_under_3_std, durable_goods_index_std,
           house_sol_std, sell_avg_std) %>%
    cor(use = "pairwise.complete.obs") %>%
    round(2)

# Plot Model Averaging Results
  # Extract Variable Importance
    importance_distance <- sw(m_distance_avg)
    importance_frequency <- sw(m_frequency_avg)

  # Create Dataframe for Plotting
    # Distance Plot
      m_distance_summary <- summary(m_distance_avg)
      plot_df_distance <- as.data.frame(m_distance_summary$coefmat.full)
      plot_df_distance <- plot_df_distance %>%
        mutate(CI.min = Estimate - 1.96 * `Std. Error`) %>%
        mutate(CI.max = Estimate + 1.96 * `Std. Error`)
      plot_df_distance <- rownames_to_column(plot_df_distance, "coefficient")
      names(plot_df_distance) <- gsub(" ", "", names(plot_df_distance))
      plot_df_distance <- plot_df_distance %>%
        filter(coefficient != "cond((Int))") %>%
        mutate(coefficient = recode(coefficient,
                                    "cond(age_std)" = "Age",
                                    "cond(genderMale)" = "Men (Ref: Women)",
                                    "cond(school_levelSecondary)" = "Secondary Education (Ref: Primary Education)",
                                    "cond(school_levelHigher)" = "Higher Education (Ref: Primary Education)",
                                    "cond(main_activitynonfarmer)" = "Non-Farmer (Ref: Farmer)",
                                    "cond(household_under_3_std)" = "Children Under Three",
                                    "cond(house_sol_std)" = "Household Lifestyle Index",
                                    "cond(durable_goods_index_std)" = "Durable Goods Owned",
                                    "cond(sell_avg_std)" = "Percent of Crops Sold"))

      # Add Variable Importance to Plot Dataframe
        plot_df_distance <- plot_df_distance %>%
          mutate(importance = sapply(coefficient, function(x) get_importance_value_mobility_models(x, importance_distance)))

    # Frequency Plot
      m_frequency_summary <- summary(m_frequency_avg)
      plot_df_frequency <- as.data.frame(m_frequency_summary$coefmat.full)
      plot_df_frequency <- plot_df_frequency %>%
        mutate(CI.min = Estimate - 1.96 * `Std. Error`) %>%
        mutate(CI.max = Estimate + 1.96 * `Std. Error`)
      plot_df_frequency <- rownames_to_column(plot_df_frequency, "coefficient")
      names(plot_df_frequency) <- gsub(" ", "", names(plot_df_frequency))
      plot_df_frequency <- plot_df_frequency %>%
        filter(coefficient != "cond((Int))") %>%
        mutate(coefficient = recode(coefficient,
                                    "cond(age_std)" = "Age",
                                    "cond(genderMale)" = "Men (Ref: Women)",
                                    "cond(school_levelSecondary)" = "Secondary Education (Ref: Primary Education)",
                                    "cond(school_levelHigher)" = "Higher Education (Ref: Primary Education)",
                                    "cond(main_activitynonfarmer)" = "Non-Farmer (Ref: Farmer)",
                                    "cond(household_under_3_std)" = "Children Under Three",
                                    "cond(house_sol_std)" = "Household Lifestyle Index",
                                    "cond(durable_goods_index_std)" = "Durable Goods Owned",
                                    "cond(sell_avg_std)" = "Percent of Crops Sold"))

      # Add Variable Importance to Plot Dataframe
        plot_df_frequency <- plot_df_frequency %>%
          mutate(importance = sapply(coefficient, function(x) get_importance_value_mobility_models(x, importance_frequency)))

  # Prepare the Dataframes for Combined Plotting
    plot_df_distance$model <- "Distance"
    plot_df_frequency$model <- "Frequency"
    combined_df <- rbind(plot_df_distance, plot_df_frequency)

  # Create Combined Plot
    coef_plot_mobility <- ggplot() +
      geom_hline(yintercept=0, lty=2) +
      geom_errorbar(data = subset(combined_df, model == "Distance"),
                    aes(x = coefficient, y = Estimate, ymin = CI.min, ymax = CI.max, color = importance),
                    position = position_nudge(x = -0.1), linewidth = 2, width = 0.2) +
      geom_point(data = subset(combined_df, model == "Distance"),
                 aes(x = coefficient, y = Estimate, color = importance), shape = 16,
                 position = position_nudge(x = -0.1), size = 8) +
      scale_color_gradient(name = "Distance Model\nImportance (AICc Weight)",
                           low = "lightblue", high = "darkblue",
                           guide = guide_colorbar(title.vjust = 3)) +
      new_scale_color() +
      geom_errorbar(data = subset(combined_df, model == "Frequency"),
                    aes(x = coefficient, y = Estimate, ymin = CI.min, ymax = CI.max, color = importance),
                    position = position_nudge(x = 0.1), linewidth = 2, width = 0.2) +
      geom_point(data = subset(combined_df, model == "Frequency"),
                 aes(x = coefficient, y = Estimate, color = importance), shape = 17,
                 position = position_nudge(x = 0.1), size = 8) +
      scale_color_gradient(name = "Frequency Model\nImportance (AICc Weight)",
                           low = "#FFDBBB", high = "darkorange",
                           guide = guide_colorbar(title.vjust = 3)) +
      coord_flip() +
      xlab("") + ylab("Estimate (95% Confidence Interval)") +
      scale_x_discrete(limits = c("Percent of Crops Sold", "Durable Goods Owned", "Household Lifestyle Index",
                                  "Children Under Three", "Non-Farmer (Ref: Farmer)",
                                  "Higher Education (Ref: Primary Education)", "Secondary Education (Ref: Primary Education)",
                                  "Men (Ref: Women)", "Age")) +
      theme_classic() +
      theme(axis.text.y = element_text(size = 30, color = "black"),
            axis.text.x = element_text(size = 30, color = "black"),
            axis.title.y = element_text(size = 30),
            axis.title.x = element_text(size = 30),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 25),
            legend.text = element_text(size = 20),
            legend.key.size = unit(1.5, "cm"),
            legend.position = "right",
            legend.box = "vertical",
            legend.box.just = "left")

  print(coef_plot_mobility)

# Save The Plot
  ggsave(file.path(fig_dir, "fig4_coef_plot_mobility.png"),
         plot = coef_plot_mobility,
         width = 25,
         height = 15,
         dpi = 600,
         bg = "white")

####################################################################
##  Results: Gender and Life Cycle Effects on Regional Mobility   ##
####################################################################

# Trip Reasons And Modes By Gender
# Mean percentage of trips and its standard error, as reported in the Results.
  survey_infection_data %>%
    group_by(gender) %>%
    summarise(across(starts_with("pct_") & ends_with("_trips"),
                     list(mean = ~ round(mean(.x), 1),
                          se = ~ round(sd(.x) / sqrt(n()), 1))),
              .groups = "drop") %>%
    pivot_longer(-gender) %>%
    pivot_wider(names_from = gender) %>%
    print(n = Inf)

# Travel Frequency And Distance By Gender
  survey_infection_data %>%
    group_by(gender) %>%
    summarise(n = n(),
              median_frequency = median(travel_frequency),
              median_distance_km = round(median(travel_distance_km), 1),
              .groups = "drop")

######################################################################
##  Results: Mobility Patterns and SARS-CoV-2 Infection (Fig. 5)    ##
######################################################################

# Compute SARS-CoV-2 Infection Variables
# The variant columns in survey_infection_data are binary indicators of
# > 30% inhibition in the multiplex sVNT assay.
# Any Infection = any variant positive
# Pre-Omicron = any pre-omicron variant positive
# Omicron = any omicron variant positive
# Richness = count of positive variants
  survey_infection_data <- survey_infection_data %>%
    mutate(
      sars2_infection = if_else(rowSums(select(., `SARS-CoV-2`, Alpha, Beta, Gamma, Delta,
                                               `Delta Plus`, Lambda, Mu,
                                               BA.1, BA.2, BA.5, XBB, XBB1.5, XBB1.16,
                                               JN.1, XDV, EG.5)) >= 1, 1, 0),
      preomicron = if_else(
        rowSums(select(., c(`SARS-CoV-2`, Alpha, Beta, Gamma, Delta,
                            `Delta Plus`, Lambda, Mu))) >= 1, 1, 0),
      omicron = if_else(
        rowSums(select(., c(BA.1, BA.2, BA.5, XBB, XBB1.5, XBB1.16,
                            JN.1, XDV, EG.5))) >= 1, 1, 0),
      richness = rowSums(select(., `SARS-CoV-2`, Alpha, Beta, Gamma, Delta,
                                `Delta Plus`, Lambda, Mu,
                                BA.1, BA.2, BA.5, XBB, XBB1.5, XBB1.16,
                                JN.1, XDV, EG.5)))

# Filter to Participants with Infection Data
  sars_model_df <- survey_infection_data %>%
    filter(!is.na(richness))

# Sample Size And Infection Descriptives Reported In The Results
  nrow(sars_model_df)
  mean(sars_model_df$sars2_infection)
  mean(sars_model_df$preomicron)
  mean(sars_model_df$omicron)

# Counts Reported Alongside The Percentages
  c(any_variant = sum(sars_model_df$sars2_infection),
    pre_omicron = sum(sars_model_df$preomicron),
    omicron     = sum(sars_model_df$omicron),
    both        = sum(sars_model_df$preomicron == 1 & sars_model_df$omicron == 1))

  round(100 * mean(sars_model_df$preomicron == 1 & sars_model_df$omicron == 1), 1)

  table(sars_model_df$richness)
  summary(sars_model_df$richness)

# Panel A. Regression Model
  # Standardize Continuous Variables
    sars_model_df <- sars_model_df %>%
      mutate(age_std = as.numeric(datawizard::standardize(age)),
             household_under_3_std = as.numeric(datawizard::standardize(household_under_3)),
             sell_avg_std = as.numeric(datawizard::standardize(sell_avg)),
             durable_goods_index_std = as.numeric(datawizard::standardize(durable_goods_index)),
             house_sol_std = as.numeric(datawizard::standardize(house_sol)),
             travel_frequency_std = as.numeric(datawizard::standardize(travel_frequency)),
             travel_distance_km_std = as.numeric(datawizard::standardize(travel_distance_km)))

  # Specify Factor Levels
    sars_model_df <- sars_model_df %>%
      mutate(school_level = factor(school_level, levels = c("Primary", "Secondary", "Higher")),
             gender = factor(gender, levels = c("Female", "Male")),
             main_activity = factor(main_activity, levels = c("farmer", "nonfarmer")))

  # Variant Richness Model
    # Build Global Model
    # The predictor set matches the mobility models and adds travel frequency and
    # distance. Children under three was added during revision at a reviewer's
    # request; it is retained in the global model so the figure and Table S3 show
    # how consistently it is selected.
      global_model_richness <- glmmTMB(richness ~ age_std + gender + school_level + main_activity +
                                         household_under_3_std + sell_avg_std + house_sol_std +
                                         durable_goods_index_std + travel_frequency_std +
                                         travel_distance_km_std + (1|village),
                                       data = sars_model_df, family = tweedie(link = "log"),
                                       na.action = na.fail)
      summary(global_model_richness)

    # Collinearity Among Predictors
      performance::check_collinearity(global_model_richness)

      sars_model_df %>%
        select(age_std, household_under_3_std, sell_avg_std, house_sol_std,
               durable_goods_index_std, travel_frequency_std, travel_distance_km_std) %>%
        cor(use = "pairwise.complete.obs") %>%
        round(2)

    # Use Dredge for Model Comparison
      m_richness_dredge <- dredge(global_model_richness)

    # Average Models With Delta AICc < 2
      m_richness_avg <- model.avg(m_richness_dredge, subset = delta < 2)
      summary(m_richness_avg)
      sw(m_richness_avg)

  # Plot Richness Model Results
    # Extract Variable Importance
      importance_richness <- sw(m_richness_avg)

    # Create Plot Dataframe
      m_richness_summary <- summary(m_richness_avg)
      plot_df_richness <- as.data.frame(m_richness_summary$coefmat.full)
      plot_df_richness <- plot_df_richness %>%
        mutate(CI.min = Estimate - 1.96 * `Std. Error`) %>%
        mutate(CI.max = Estimate + 1.96 * `Std. Error`)
      plot_df_richness <- rownames_to_column(plot_df_richness, "coefficient")
      names(plot_df_richness) <- gsub(" ", "", names(plot_df_richness))

    # Recode Coefficient Names
      plot_df_richness <- plot_df_richness %>%
        filter(coefficient != "cond((Int))") %>%
        mutate(coefficient = recode(coefficient,
                                    "cond(age_std)" = "Age",
                                    "cond(genderMale)" = "Men (Ref: Women)",
                                    "cond(school_levelSecondary)" = "Secondary Education (Ref: Primary Education)",
                                    "cond(school_levelHigher)" = "Higher Education (Ref: Primary Education)",
                                    "cond(main_activitynonfarmer)" = "Non-Farmer (Ref: Farmer)",
                                    "cond(household_under_3_std)" = "Children Under Three",
                                    "cond(house_sol_std)" = "House Lifestyle Index",
                                    "cond(durable_goods_index_std)" = "Durable Goods Owned",
                                    "cond(sell_avg_std)" = "Percent of Crops Sold",
                                    "cond(travel_frequency_std)" = "Travel Frequency",
                                    "cond(travel_distance_km_std)" = "Distance Traveled in Past Year"))

    # Add Importance Values
      plot_df_richness <- plot_df_richness %>%
        mutate(importance = sapply(coefficient, function(x) get_importance_value_sars2_model(x, importance_richness)))

    # Axis Rows. Education and travel frequency are in the candidate set but are
    # never retained in a top model (delta AICc < 2), so they have no averaged
    # estimate to plot. They are reported in Table S3 rather than shown as
    # empty rows here. Add them back to this vector to show them.
      plot_axis_labels <- c(
        "Distance Traveled in Past Year",
        "House Lifestyle Index",
        "Percent of Crops Sold",
        "Durable Goods Owned",
        "Children Under Three",
        "Non-Farmer (Ref: Farmer)",
        "Men (Ref: Women)",
        "Age"
      )

    # Every Averaged Coefficient Must Have A Row, Or It Is Dropped Silently
      setdiff(plot_df_richness$coefficient, plot_axis_labels)   # expect character(0)

    # Create Plot
      coef_plot_richness <- ggplot(plot_df_richness, aes(x = coefficient, y = Estimate)) +
        geom_hline(yintercept = 0, lty = 2) +
        geom_errorbar(aes(ymin = CI.min, ymax = CI.max, colour = importance),
                      linewidth = 2, width = 0.2) +
        geom_point(aes(colour = importance), shape = 16, size = 6) +
        scale_color_gradient(
          name = "Variable Importance\n(AICc Weight)",
          low = "lightblue",
          high = "darkblue",
          limits = c(0, 1),
          oob = scales::squish,
          guide = guide_colorbar(title.vjust = 3)
        ) +
        scale_x_discrete(limits = plot_axis_labels) +
        coord_flip(clip = "off") +
        xlab("") +
        ylab("Estimate (95% Confidence Interval)") +
        theme_classic() +
        theme(
          axis.text.y = element_text(size = 14, color = "black", hjust = 1),
          axis.text.x = element_text(size = 16, color = "black"),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.key.size = unit(1, "cm"),
          legend.position = "right",
          aspect.ratio = 1,
          plot.margin = margin(0, 10, 0, 0),
          axis.ticks.length.y = unit(2, "pt"),
          panel.border = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )

    print(coef_plot_richness)

# Panel B. Network Analysis
  # Construct SARS-CoV-2 Variant Similarity Network
    variant_similarity_df <- sars_model_df %>%
      select(participant_id, `SARS-CoV-2`, Alpha, Beta, Gamma, Delta,
             `Delta Plus`, Lambda, Mu,
             BA.1, BA.2, BA.5, XBB, XBB1.5, XBB1.16,
             JN.1, XDV, EG.5)

    # Move Participant ID To Row Name
      variant_similarity_df <- variant_similarity_df %>%
          column_to_rownames(var = "participant_id")

    # Generate Network From Incidence Matrix
      sars_bipartite_graph <- graph_from_biadjacency_matrix(as.matrix(variant_similarity_df), weight = TRUE)
      sars_proj <- bipartite_projection(sars_bipartite_graph) # project network
      sars_proj_1 <- sars_proj[[1]] # select first projection - person-person net
      V(sars_proj_1)$name # check node names - should be pids
      E(sars_proj_1)$weight # check weights - should be number of variants in common

    # Remove Isolates (nodes with degree = 0)
      sars_proj_1_no_isolates <- igraph::delete_vertices(sars_proj_1, V(sars_proj_1)[igraph::degree(sars_proj_1) == 0])

  # Transform Travel Similarity Edgelist Into Network
    travel_similarity_net <- graph_from_data_frame(travel_similarity_el, directed = FALSE)

  # Identify Nodes That are Common in Both Networks
    common_connected_nodes <- intersect(V(travel_similarity_net)$name, V(sars_proj_1_no_isolates)$name)

    travel_subset <- induced_subgraph(travel_similarity_net, V(travel_similarity_net)[name %in% common_connected_nodes])
    sars_subset <- induced_subgraph(sars_proj_1_no_isolates, V(sars_proj_1_no_isolates)[name %in% common_connected_nodes])

  # Remove Isolates That Appear After Subsetting
    travel_subset <- igraph::delete_vertices(travel_subset, V(travel_subset)[igraph::degree(travel_subset) == 0])
    sars_subset <- igraph::delete_vertices(sars_subset, V(sars_subset)[igraph::degree(sars_subset) == 0])

  # Confirm Both Networks Have The Same Nodes
    final_nodes <- intersect(V(travel_subset)$name, V(sars_subset)$name)
    travel_subset <- induced_subgraph(travel_subset, V(travel_subset)[name %in% final_nodes])
    sars_subset <- induced_subgraph(sars_subset, V(sars_subset)[name %in% final_nodes])

  # Ensure Nodes Are In The Same Order
    travel_subset <- permute(travel_subset, match(V(sars_subset)$name, V(travel_subset)$name))

  # Convert Network Objects To Adjacency Matrices
    travel_adj <- as_adjacency_matrix(travel_subset, sparse = FALSE, attr = "weight")
    sars_adj <- as_adjacency_matrix(sars_subset, sparse = FALSE, attr = "weight")

  # Network Size Reported In The Results
    c(nodes = nrow(sars_adj),
      travel_ties = sum(travel_adj[upper.tri(travel_adj)] > 0),
      variant_ties = sum(sars_adj[upper.tri(sars_adj)] > 0))

  # Run MRQAP
  # The permutation test is slow, so the fitted object is cached. Delete
  # mrqap_main.rds to force a refit, which is required if the data change.
    mrqap_cache <- file.path(mod_dir, "mrqap_main.rds")
    if (file.exists(mrqap_cache)) {
      mrqap_result <- readRDS(mrqap_cache)
      cat("Loaded cached MRQAP from: ", mrqap_cache, "\n", sep = "")
    } else {
      set.seed(318)
      mrqap_result <- netlm(sars_adj, list(travel_adj), reps = 1000)
      saveRDS(mrqap_result, mrqap_cache)
    }
    summary(mrqap_result)

  # Secondary Analysis: Traveler-Resident Network
    # Participants And Their Home Kaomininas
      tr_nodes <- sars_model_df %>%
        select(participant_id, home_commune_name) %>%
        arrange(participant_id)
      node_ids <- tr_nodes$participant_id
      n_nodes <- length(node_ids)

    # Variant Similarity Among All Participants, Including Those Who Did Not Travel
      sars_all_adj <- as_adjacency_matrix(sars_proj_1, sparse = FALSE, attr = "weight")
      sars_all_adj <- sars_all_adj[node_ids, node_ids]
      diag(sars_all_adj) <- 0

    # Participant By Destination Incidence Matrix
      visits <- travel_destination_el %>%
        filter(participant_id %in% node_ids) %>%
        distinct(participant_id, visited_commune)

      all_dest <- sort(unique(visits$visited_commune))
      visit_mat <- matrix(0, n_nodes, length(all_dest), dimnames = list(node_ids, all_dest))
      visit_mat[cbind(match(visits$participant_id, node_ids),
                      match(visits$visited_commune, all_dest))] <- 1

    # Participant By Home Kaominina Incidence Matrix
      home_mat <- matrix(0, n_nodes, length(all_dest), dimnames = list(node_ids, all_dest))
      home_idx <- match(tr_nodes$home_commune_name, all_dest)
      home_mat[cbind(which(!is.na(home_idx)), home_idx[!is.na(home_idx)])] <- 1

    # Connect Participants If One Traveled To The Other's Home Kaominina
      trav_res_adj <- visit_mat %*% t(home_mat)
      trav_res_adj <- ((trav_res_adj + t(trav_res_adj)) > 0) * 1

    # Exclude Pairs Living In The Same Kaominina
      same_home <- outer(tr_nodes$home_commune_name, tr_nodes$home_commune_name, "==")
      trav_res_adj[same_home] <- 0
      diag(trav_res_adj) <- 0

    # Summarize Network Coverage
      ut <- upper.tri(sars_all_adj)
      c(participants = n_nodes,
        traveler_resident_pairs = sum(trav_res_adj[ut]),
        destination_communes = length(all_dest),
        sampled_communes = n_distinct(tr_nodes$home_commune_name),
        pct_trips_to_sampled = round(100 * mean(visits$visited_commune %in%
                                                  tr_nodes$home_commune_name), 1))

    # Mean Shared Variants By Pair Type
      tibble(
        shared = sars_all_adj[ut],
        trav_res = trav_res_adj[ut] == 1
      ) %>%
        group_by(trav_res) %>%
        summarise(n_pairs = n(), mean_shared_variants = mean(shared), .groups = "drop")

    # Run MRQAP
    # Cached as above. Delete mrqap_trav_res.rds to force a refit.
      mrqap_tr_cache <- file.path(mod_dir, "mrqap_trav_res.rds")
      if (file.exists(mrqap_tr_cache)) {
        mrqap_trav_res <- readRDS(mrqap_tr_cache)
        cat("Loaded cached MRQAP from: ", mrqap_tr_cache, "\n", sep = "")
      } else {
        set.seed(318)
        mrqap_trav_res <- netlm(sars_all_adj, list(trav_res_adj), reps = 1000)
        saveRDS(mrqap_trav_res, mrqap_tr_cache)
      }
      summary(mrqap_trav_res)

  # Visualize Network Correlation
    # Set Diagnol To NA Before Standardization
      travel_adj_nodiag <- travel_adj
      sars_adj_nodiag <- sars_adj
      diag(travel_adj_nodiag) <- NA
      diag(sars_adj_nodiag) <- NA

    # Normalize Using Off-Diagnol Values
      travel_z <- (travel_adj_nodiag - mean(travel_adj_nodiag, na.rm = TRUE)) / sd(travel_adj_nodiag, na.rm = TRUE)
      svnt_z <- (sars_adj_nodiag - mean(sars_adj_nodiag, na.rm = TRUE)) / sd(sars_adj_nodiag, na.rm = TRUE)

    # Create Difference Matrix
      diff_matrix <- travel_z - svnt_z

    # Set Diagnol Back To Zero For Visualization
      diag(diff_matrix) <- 0

    # Cluster Pairs Based On Similarity
      dist_matrix <- dist(diff_matrix, method = "euclidean")
      hc <- hclust(dist_matrix, method = "ward.D2")
      ordered_names <- rownames(diff_matrix)[hc$order]

    # Reorder Difference Matrix Based On Clustering
      diff_matrix_ordered <- diff_matrix[ordered_names, ordered_names]

    # Melt For GGPlot
      melted <- melt(diff_matrix_ordered)

    # Convert To Factors To Preserve Clustering Order
      melted$Var1_num <- as.numeric(melted$Var1)
      melted$Var2_num <- as.numeric(melted$Var2)
      melted <- melted[melted$Var2_num >= melted$Var1_num, ]

    # Plot Difference Matrix
      heatmap_plot <- ggplot(melted, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(
          low = "red",
          mid = "white",
          high = "blue",
          midpoint = 0,
          name = "Network Difference",
          breaks = c(-4, 0, 5),
          labels = c(
            "High variant similarity,\nlow travel similarity",
            "High travel and\nvariant similarity",
            "High travel similarity,\nlow variant similarity"
          )
        ) +
        scale_x_discrete(position = "top") +
        theme_minimal() +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.title.x.top = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12, lineheight = 0.9),
          legend.key.size = unit(1.2, "cm"),
          legend.position = c(0.82, 0.30),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.margin = margin(10, 10, 10, 10),
          aspect.ratio = 1,
          plot.margin = margin(10, 10, 10, 10),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        ) +
        labs(x = "Participants (Clustered by Similarity)",
             y = "Participants (Clustered by Similarity)") +
        coord_equal()

      print(heatmap_plot)

# Make Two-Paneled Plot
  combined_sars_plot <- (coef_plot_richness | heatmap_plot) +
    plot_annotation(tag_levels = 'A') &
    theme(
      plot.tag = element_text(size = 24, face = "bold",
                              margin = margin(0, 0, 0, 0)),
      plot.tag.position = c(0.02, 0.98),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )

  print(combined_sars_plot)

# Save The Plot
  ggsave(file.path(fig_dir, "fig5_combined_sars_plot.png"),
         plot = combined_sars_plot,
         width = 16, height = 7,
         dpi = 300,
         units = "in",
         bg = "white")

####################################
##  Results: Vaccination Subset   ##
####################################

# Self-Reported Vaccination Subset
# Vaccination was added to the survey partway through data collection, so it is
# available for a subset of participants and is coded "Unknown" otherwise.
  vax_df <- sars_model_df %>%
    mutate(vaccinated_covid = factor(vaccinated_covid,
                                     levels = c("Not Vaccinated", "Vaccinated", "Unknown"))) %>%
    filter(vaccinated_covid != "Unknown") %>%
    mutate(vaccinated_covid = droplevels(vaccinated_covid))

  nrow(vax_df)
  round(100 * nrow(vax_df) / nrow(sars_model_df), 1)
  table(vax_df$vaccinated_covid)
  round(100 * mean(vax_df$vaccinated_covid == "Vaccinated"), 1)

# Range Of Reported Vaccination Across The Communities Where The Question Was Asked
  vax_df %>%
    group_by(village) %>%
    summarise(n = n(),
              pct_vax = round(100 * mean(vaccinated_covid == "Vaccinated"), 1),
              .groups = "drop") %>%
    summarise(communities = n(), min_pct = min(pct_vax), max_pct = max(pct_vax))

  global_model_richness_vax <- glmmTMB(richness ~ age_std + gender + school_level + main_activity +
                                         sell_avg_std + house_sol_std + durable_goods_index_std +
                                         travel_frequency_std + travel_distance_km_std +
                                         vaccinated_covid + (1|village),
                                       data = vax_df, family = tweedie(link = "log"),
                                       na.action = na.fail)
  summary(global_model_richness_vax)
  confint(global_model_richness_vax)

  m_richness_vax_dredge <- dredge(global_model_richness_vax)
  m_richness_vax_avg <- model.avg(m_richness_vax_dredge, subset = delta < 2)
  summary(m_richness_vax_avg)
  sw(m_richness_vax_avg)

##############################################################
##  Figure S1. Variant Seroprevalence Over Sampling Period  ##
##############################################################

# Variant Columns In Order Of Emergence
  variant_cols <- c("SARS-CoV-2", "Alpha", "Beta", "Gamma", "Delta", "Delta Plus",
                    "Lambda", "Mu", "BA.1", "BA.2", "BA.5", "XBB", "XBB1.5",
                    "XBB1.16", "EG.5", "XDV", "JN.1")
  variant_labels <- c("Ancestral", "Alpha", "Beta", "Gamma", "Delta", "Delta Plus",
                      "Lambda", "Mu", "BA.1", "BA.2", "BA.5", "XBB", "XBB.1.5",
                      "XBB.1.16", "EG.5", "XDV", "JN.1")
  omicron_cols <- c("BA.1", "BA.2", "BA.5", "XBB", "XBB1.5", "XBB1.16", "EG.5", "XDV", "JN.1")

# Assign Communities To Sampling Periods (gap > 45 days between visits)
  village_periods <- sars_model_df %>%
    mutate(survey_date = as.Date(survey_date)) %>%
    group_by(village) %>%
    summarise(village_date = median(survey_date, na.rm = TRUE), .groups = "drop") %>%
    arrange(village_date) %>%
    mutate(period = cumsum(c(TRUE, diff(village_date) > 45)))

  village_periods %>% count(period)
  range(village_periods$village_date)

# Compute Seroprevalence By Variant And Sampling Period
  period_prev <- sars_model_df %>%
    select(participant_id, village, all_of(variant_cols)) %>%
    pivot_longer(cols = all_of(variant_cols), names_to = "variant_col", values_to = "positive") %>%
    left_join(village_periods, by = "village") %>%
    mutate(variant = factor(variant_labels[match(variant_col, variant_cols)], levels = variant_labels),
           lineage = if_else(variant_col %in% omicron_cols, "Omicron", "Pre-Omicron")) %>%
    group_by(variant, lineage, period) %>%
    summarise(period_date = median(village_date),
              n = n(),
              n_pos = sum(positive),
              prevalence = n_pos / n,
              .groups = "drop")

# Assign Colors By Lineage, Light To Dark By Emergence
  pre_levels <- variant_labels[1:8]
  omi_levels <- variant_labels[9:17]
  pre_colors <- setNames(colorRampPalette(c("#a6cee3", "#08306b"))(length(pre_levels)), pre_levels)
  omi_colors <- setNames(colorRampPalette(c("#fdd0a2", "#7f2704"))(length(omi_levels)), omi_levels)

  pre_df <- period_prev %>% filter(lineage == "Pre-Omicron") %>% droplevels()
  omi_df <- period_prev %>% filter(lineage == "Omicron") %>% droplevels()

# Create Plot
  variant_period_plot <- ggplot() +
    geom_line(data = pre_df, aes(period_date, prevalence, colour = variant, group = variant),
              linewidth = 0.8) +
    geom_point(data = pre_df, aes(period_date, prevalence, colour = variant), size = 2) +
    scale_colour_manual(values = pre_colors, name = "Pre-Omicron",
                        guide = guide_legend(order = 1)) +
    new_scale_colour() +
    geom_line(data = omi_df, aes(period_date, prevalence, colour = variant, group = variant),
              linewidth = 0.8) +
    geom_point(data = omi_df, aes(period_date, prevalence, colour = variant), size = 2) +
    scale_colour_manual(values = omi_colors, name = "Omicron",
                        guide = guide_legend(order = 2)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y",
                 expand = expansion(mult = 0.02)) +
    labs(x = "Sampling date (period median)", y = "Seroprevalence") +
    theme_classic() +
    theme(axis.text = element_text(size = 10, colour = "black"),
          axis.title = element_text(size = 12),
          legend.title = element_text(size = 11, face = "bold"),
          legend.position = "right")

  print(variant_period_plot)

# Save The Plot
  ggsave(file.path(fig_dir, "figS1_variant_period_plot.png"),
         plot = variant_period_plot,
         width = 11, height = 6,
         dpi = 600,
         bg = "white")

# Values Reported In The Results
  period_prev %>%
    filter(variant %in% c("XBB", "XBB.1.5", "XBB.1.16", "EG.5", "XDV", "JN.1"),
           period %in% c(3, max(period))) %>%
    mutate(prevalence = round(prevalence * 100, 1)) %>%
    select(variant, period, prevalence) %>%
    pivot_wider(names_from = period, values_from = prevalence, names_prefix = "period_")

# Seroprevalence By Period, Averaged Across Variants
  period_prev %>%
    group_by(period, period_date) %>%
    summarise(mean_prevalence = round(100 * mean(prevalence), 1), .groups = "drop")

########################################################
##  Figure S2. Variant Seroprevalence by Community    ##
########################################################

# Order Communities By Sampling Date, Most Recent At The Bottom
  village_order <- village_periods %>%
    arrange(village_date) %>%
    mutate(village = factor(village, levels = rev(village))) %>%
    select(village, period, village_date)

# Seroprevalence Of Each Variant Within Each Community
  community_prev <- sars_model_df %>%
    select(participant_id, village, all_of(variant_cols)) %>%
    pivot_longer(cols = all_of(variant_cols), names_to = "variant_col", values_to = "positive") %>%
    group_by(village, variant_col) %>%
    summarise(n = n(), n_pos = sum(positive), prevalence = n_pos / n, .groups = "drop") %>%
    mutate(variant = factor(variant_labels[match(variant_col, variant_cols)],
                            levels = variant_labels)) %>%
    left_join(village_order, by = "village") %>%
    mutate(village = factor(village, levels = levels(village_order$village)))

# Variant Composition Within Each Community (rows sum to 100%)
  community_prev <- community_prev %>%
    group_by(village) %>%
    mutate(composition = n_pos / sum(n_pos)) %>%
    ungroup()

# Panel A. Seroprevalence
  prev_heatmap <- ggplot(community_prev, aes(x = variant, y = village, fill = prevalence)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_viridis_c(name = "Seroprevalence", labels = scales::percent,
                         option = "mako", direction = -1) +
    labs(x = "", y = "Community (ordered by sampling date)", title = "A. Seroprevalence") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, colour = "black"),
          axis.text.y = element_text(size = 8, colour = "black"),
          axis.title.y = element_text(size = 11),
          panel.grid = element_blank(),
          plot.title = element_text(size = 12, face = "bold"))

# Panel B. Variant Composition
  comp_heatmap <- ggplot(community_prev, aes(x = variant, y = village, fill = composition)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_viridis_c(name = "Share of\npositives", labels = scales::percent,
                         option = "rocket", direction = -1) +
    labs(x = "", y = "", title = "B. Variant composition") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, colour = "black"),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(size = 12, face = "bold"))

# Combine Panels
  community_variant_plot <- prev_heatmap | comp_heatmap

  print(community_variant_plot)

# Save The Plot
  ggsave(file.path(fig_dir, "figS2_community_variant_plot.png"),
         plot = community_variant_plot,
         width = 13, height = 7,
         dpi = 600,
         bg = "white")

# Range Of Community-Level Seroprevalence, For The Results Text
  community_variant_range <- community_prev %>%
    group_by(variant) %>%
    summarise(min_prev = round(100 * min(prevalence), 1),
              max_prev = round(100 * max(prevalence), 1),
              .groups = "drop")

  print(community_variant_range, n = Inf)
  write_csv(community_variant_range,
            file.path(tab_dir, "community_variant_prevalence_range.csv"))

######################################################
##  Supplementary Tables S1-S3. Component Models    ##
######################################################

  make_component_table <- function(dredge_obj, term_labels) {
    d <- as.data.frame(subset(dredge_obj, delta < 2))

    marks <- as.data.frame(lapply(names(term_labels),
                                  function(t) ifelse(is.na(d[[t]]), "", "X")),
                           stringsAsFactors = FALSE)
    names(marks) <- unname(term_labels)

    model_rows <- tibble(
      Model = as.character(seq_len(nrow(d))),
      `Degrees of Freedom` = as.character(d$df),
      `Log Likelihood` = sprintf("%.2f", d$logLik),
      AICc = sprintf("%.2f", d$AICc),
      `Delta AICc` = sprintf("%.2f", d$delta),
      Weight = sprintf("%.2f", d$weight)
    ) %>%
      bind_cols(marks)

    imp <- sw(model.avg(dredge_obj, subset = delta < 2))
    imp_vals <- vapply(names(term_labels),
                       function(t) if (t %in% names(imp)) sprintf("%.2f", imp[[t]]) else "0.00",
                       character(1))
    names(imp_vals) <- unname(term_labels)

    imp_row <- tibble(
      Model = "Importance (Summed AICc Weight)",
      `Degrees of Freedom` = "", `Log Likelihood` = "",
      AICc = "", `Delta AICc` = "", Weight = ""
    ) %>%
      bind_cols(as_tibble_row(imp_vals))

    bind_rows(model_rows, imp_row)
  }

# Predictor Columns, In The Order They Appear In The Published Tables
  mobility_terms <- c(
    "cond(age_std)"                 = "Age",
    "cond(gender)"                  = "Gender",
    "cond(school_level)"            = "Education",
    "cond(main_activity)"           = "Occupation",
    "cond(household_under_3_std)"   = "Children Under Three",
    "cond(house_sol_std)"           = "House Lifestyle Index",
    "cond(durable_goods_index_std)" = "Durable Goods Owned",
    "cond(sell_avg_std)"            = "Percent of Crops Sold"
  )

  richness_terms <- c(
    "cond(age_std)"                 = "Age",
    "cond(gender)"                  = "Gender",
    "cond(school_level)"            = "Education",
    "cond(main_activity)"           = "Occupation",
    "cond(household_under_3_std)"   = "Children Under Three",
    "cond(house_sol_std)"           = "House Lifestyle Index",
    "cond(durable_goods_index_std)" = "Durable Goods Owned",
    "cond(sell_avg_std)"            = "Percent of Crops Sold",
    "cond(travel_frequency_std)"    = "Travel Frequency",
    "cond(travel_distance_km_std)"  = "Travel Distance"
  )

  table_s1 <- make_component_table(m_frequency_dredge, mobility_terms)
  table_s2 <- make_component_table(m_distance_dredge,  mobility_terms)
  table_s3 <- make_component_table(m_richness_dredge,  richness_terms)

  print(table_s1, n = Inf, width = Inf)
  print(table_s2, n = Inf, width = Inf)
  print(table_s3, n = Inf, width = Inf)

  write_csv(table_s1, file.path(tab_dir, "table_s1_frequency_component_models.csv"))
  write_csv(table_s2, file.path(tab_dir, "table_s2_distance_component_models.csv"))
  write_csv(table_s3, file.path(tab_dir, "table_s3_richness_component_models.csv"))

######################################
##  Save Model Objects For Reuse    ##
######################################

  saveRDS(
    list(
      distance      = global_model_distance,
      frequency     = global_model_frequency,
      richness      = global_model_richness,
      richness_vax  = global_model_richness_vax,
      mrqap         = mrqap_result,
      mrqap_trav_res = mrqap_trav_res,
      dredges       = list(distance  = m_distance_dredge,
                           frequency = m_frequency_dredge,
                           richness  = m_richness_dredge)
    ),
    file.path(mod_dir, "fitted_models.rds")
  )

  cat("Figures written to: ", fig_dir, "\n",
      "Tables written to:  ", tab_dir, "\n",
      "Models written to:  ", mod_dir, "\n", sep = "")