# Companion Code For:
# Characterizing Human Mobility Patterns and their Associations with
# SARS-CoV-2 Infection across a Gradient of Market Integration

# Script to Generate Figures in Results Section
# Date: January 27, 2026
# Author: Tyler Barrett (tyler.barrett@duke.edu)

#################
##  Preamble   ##
#################

# Load Packages
  library(tidyverse)
  library(glmmTMB)
  library(MuMIn)
  library(datawizard)
  library(scales)
  library(sf)
  library(osmdata)
  library(patchwork)
  library(cowplot)
  library(ggnewscale)
  library(RColorBrewer)
  library(igraph)
  library(sna)
  library(reshape2)

# Set File Path
  fp <- "your_file_path"
  
# Load Data
  survey_infection_data <- read_csv(paste0(fp, "/survey_infection_data.csv"))
  commune_el <- read_csv(paste0(fp, "/commune_edgelist.csv"))
  commune_nl <- read_csv(paste0(fp, "/commune_nodelist.csv"))
  travel_similarity_el <- read_csv(paste0(fp, "/travel_similarity_edgelist.csv"))

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
          name_mapping <- c(
            "Age" = "cond(age_std)",
            "Men (Ref: Women)" = "cond(gender)",
            "Secondary Education (Ref: Primary Education)" = "cond(school_level)",
            "Higher Education (Ref: Primary Education)" = "cond(school_level)", 
            "Non-Farmer (Ref: Farmer)" = "cond(main_activity)",
            "Percent of Crops Sold" = "cond(sell_avg_std)",
            "House Lifestyle Index" = "cond(house_sol_std)",
            "Durable Goods Owned" = "cond(durable_goods_index_std)",
            "Distance Traveled in Past Year" = "cond(travel_distance_km_std)"
          )
        )
      
      # Map coefficient name to sw() variable name
        sw_name <- name_mapping[coef_name]
        if (!is.na(sw_name) && sw_name %in% names(importance_obj)) {
          return(as.numeric(importance_obj[sw_name]))
        } else {
          return(0.0)  # Default value if variable not found
        }
    }

##################################################
##  Figure 3. Market Integration by Community   ##
##################################################
    
# Create Custom Color Palette for Villages
  custom_palette <- colorRampPalette(brewer.pal(34, "Paired"))(34)

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
# ggsave("mi_panel_plot.png",
#        plot = combined_mi_plot,
#        width = 24,      
#        height = 18,     
#        dpi = 600,
#        bg = "white")

#############################################################
##  Figure 4. Regional Travel Network in SAVA Madagascar   ##
#############################################################

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
  nodes_sf <- st_as_sf(commune_nl, coords = c("long", "lat"), crs = 4326)
  bbox <- st_bbox(nodes_sf)
  osm_admin_commune <- opq(bbox = bbox) %>%
    add_osm_feature(key = "boundary", value = "administrative") %>%
    add_osm_feature(key = "admin_level", value = "8") %>%
    osmdata_sf()  
  osm_admin_region <- opq(bbox = bbox) %>%
    add_osm_feature(key = "boundary", value = "administrative") %>%
    add_osm_feature(key = "admin_level", value = "4") %>%
    add_osm_feature(key = "name", value = "Sava") %>%
    osmdata_sf()

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
# ggsave(
#   "travel_net_plot.png",
#   plot = panel_map_plot,
#   width = 15,
#   height = 13,
#   dpi = 600,
#   device = "tiff",
#   compression = "lzw",
#   bg = "white",
#   units = "in"
# )
    
##############################################################
##  Figure 5. Predictors of Travel Frequency and Distance   ##
##############################################################

# Select Variables for Analysis
  mobility_model_df <- survey_infection_data %>%
    select(participant_id:vaccinated_covid)

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
  
  # Average_std Models With Delta AICc < 2
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
  
  # Average_std Models With Delta AICc < 2
    m_frequency_avg <- model.avg(m_frequency_dredge, subset = delta < 2)
    summary(m_frequency_avg)

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
# ggsave("coef_plot_mobility.png",
#        plot = coef_plot_mobility,
#        width = 25,
#        height = 15,
#        dpi = 600,
#        bg = "white")
  
#################################################################################
##  Figure 6. Association Betweeen Mobility Patterns and SARS-CoV-2 Infection  ##
#################################################################################

# Compute SARS-CoV-2 Infection Variables
# Any Infection = any variant > 30% inhibition
# Pre-Omicron = any pre-omicron variant > 30% inhibition
# Omicron = any omicron variant > 30% inhibition
# Richness = count of all variants > 30% inhibition
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
  
# Panel A. Regression Model    
  # Standardize Continuous Variables  
    sars_model_df <- sars_model_df %>%  
      mutate(age_std = as.numeric(datawizard::standardize(age)),
             sell_avg_std = as.numeric(datawizard::standardize(sell_avg)),
             durable_goods_index_std = as.numeric(datawizard::standardize(durable_goods_index)),
             house_sol_std = as.numeric(datawizard::standardize(house_sol)),
             travel_frequency_std = as.numeric(datawizard::standardize(travel_frequency)),
             travel_distance_km_std = as.numeric(datawizard::standardize(travel_distance_km)))
    
  # Variant Richness Model
    # Build Global Model
      global_model_richness <- glmmTMB(richness ~ age_std + gender + school_level + main_activity +
                                         sell_avg_std + house_sol_std + durable_goods_index_std + 
                                         travel_frequency_std + travel_distance_km_std + (1|village),
                                       data = sars_model_df, family = tweedie(link = "log"), 
                                       na.action = na.fail)
      summary(global_model_richness)  
      
    # Use Dredge for Model Comparison
      m_richness_dredge <- dredge(global_model_richness)
      
    # Average_std_std Models With Delta AICc < 2
      m_richness_avg <- model.avg(m_richness_dredge, subset = delta < 2)
      summary(m_richness_avg)
      
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
                                    "cond(main_activitynonfarmer)" = "Non-Farmer (Ref: Farmer)",
                                    "cond(house_sol_std)" = "House Lifestyle Index",
                                    "cond(durable_goods_index_std)" = "Durable Goods Owned",
                                    "cond(sell_avg_std)" = "Percent of Crops Sold",
                                    "cond(travel_distance_km_std)" = "Distance Traveled in Past Year"))
      
    # Add Importance Values
      plot_df_richness <- plot_df_richness %>%
        mutate(importance = sapply(coefficient, function(x) get_importance_value_sars2_model(x, importance_richness)))
    
    # Create Plot
      coef_plot_richness <- ggplot(plot_df_richness, aes(x = coefficient, y = Estimate)) +
        geom_hline(yintercept = 0, lty = 2) +
        geom_errorbar(aes(ymin = CI.min, ymax = CI.max, colour = importance), 
                      linewidth = 2, width = 0.15) +
        geom_point(aes(colour = importance), shape = 16, size = 6) +
        scale_color_gradient(
          name = "Variable Importance\n(AICc Weight)",
          low = "lightblue", 
          high = "darkblue",
          limits = c(0, 1), 
          oob = scales::squish,
          guide = guide_colorbar(title.vjust = 3)
        ) +
        scale_x_discrete(limits = c(
          "Distance Traveled in Past Year",
          "House Lifestyle Index",
          "Percent of Crops Sold",
          "Durable Goods Owned",
          "Non-Farmer (Ref: Farmer)",
          "Men (Ref: Women)",
          "Age"                
        )) +
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
    
  # Run MRQAP
    set.seed(318)
    mrqap_result <- netlm(sars_adj, list(travel_adj), reps = 1000)
    summary(mrqap_result)
    
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
# ggsave("combined_sars_plot.png",
#        plot = combined_sars_plot,
#        width = 16, height = 7,
#        dpi = 300,
#        units = "in")
