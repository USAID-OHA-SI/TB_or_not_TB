# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  make map of TB/HIV coinfection for Uganda [Union conf 2024 - RCA]
# REF ID:   017ee919 
# LICENSE:  MIT
# DATE:     2024-10-24
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

    filepath <- si_path() %>% return_latest("Uganda")
    shpdata <- si_path("path_vector")

  # Grab metadata
   metadata <- get_metadata(filepath) 
  
  ref_id <- "017ee919"

# IMPORT ------------------------------------------------------------------
  
  df_msd <- read_psd(filepath)

# MUNGE -------------------------------------------------------------------
  
  df_uga <- df_msd %>% 
    filter(indicator %in% c("TB_STAT"),
           fiscal_year %in% c(2023),
        #   country %in% include_cntry,
           standardizeddisaggregate %in% c("Age/Sex/KnownNewPosNeg", "Age/Sex/KnownNewPosNegRecentNeg") 
    ) %>% 
    group_by(fiscal_year, snu1, snu1uid, indicator, numeratordenom, statushiv) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    #reshape_msd() %>% 
    pivot_wider(names_from = "statushiv", values_from = "cumulative") %>% 
    mutate(total = `Negative` + `Positive`) %>% 
    mutate(tb_stat_pos = `Positive`/total) 
  
  # Load the shapefiles to grab boundaries from below
  spdf_pepfar <- get_vcpolygons(shpdata, name = "VcPepfarPolygons.shp")
  cntry <- "Uganda"
  
  df_uga_map  <- df_uga %>% 
    left_join(spdf_pepfar, by = c("snu1uid" = "uid")) %>% 
    mutate(text_color = ifelse(tb_stat_pos > .4, "white", "black"))

 #MAP 
  df_uga_map %>% 
  ggplot() +
    #geom_sf(fill = NA) +
    #geom_sf(data = adm0, fill = gray(.92), lty = "dashed") +
    geom_sf(data = df_uga_map, aes(geometry = geometry, fill = tb_stat_pos),
            color = grey10k) +
    scale_fill_si(palette = "scooters",
                  discrete = FALSE,
                  #limits = c(0, 1),
                  labels = scales::percent) +
    si_style_map() +
    geom_sf_text(ggplot2::aes(geometry = geometry,color = text_color,
                              label = percent(tb_stat_pos, 1)),
                 family = "Source Sans Pro") +
    geom_sf_text(ggplot2::aes(geometry = geometry,color = text_color,
                              label = snu1),
                 vjust = -1,
                 family = "Source Sans Pro") +
    scale_color_identity() + 
    labs(title = "TB/HIV Coinfection Rate in Uganda, 2023" %>% toupper(),
         caption = glue::glue("{metadata$caption}
                              Presented at 2024 Union Conference on Lung Health"))
  
  si_save("Graphics/20241024_TB_STAT_UGA_map.svg")
    