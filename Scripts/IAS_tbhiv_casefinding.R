# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  IAS - HIV case finding analytics
# REF ID:   8242685e 
# LICENSE:  MIT
# DATE:     2023-12-14
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

  #store filepath
    old_filepath <- si_path() %>% return_latest("OU_IM_FY15")
    filepath <- si_path() %>% return_latest("OU_IM_FY21")

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "8242685e"
  
  #exclude countries that do not report TB indics + HTS TB mod
  exclude_cntry <- c("Benin", "Burkina Faso","Colombia", "El Salvador", "Ghana",
                     "Guatemala", "Honduras", "Liberia", "Panama", "Rwanda", "Sierra Leone",
                     "Togo", "Ukraine", "Vietnam", "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Senegal",
                     "Brazil", "Burma", "Cambodia", "Dominican Republic", "India", "Indonesia", "Jamaica",
                     "Laos","Mali", "Nepal", "Nicaragua", "Papua New Guinea", "Peru", "Philippines", "Thailand",
                     "Trinidad and Tobago", "Guyana", "Barbados", "China", "Suriname", "Nigeria", "Tanzania")

  #rounding function
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
# IMPORT ------------------------------------------------------------------
  
  df_msd <- read_psd(filepath)
  
  df_old <- read_psd(old_filepath)
  
  si_path() %>% 
    return_latest("MER_Structured_Datasets_OU_IM_FY15-20_20231215_v2_1.zip") %>% 
    read_psd()
  
  df_msd <- df_msd %>% 
    bind_rows(df_old)

# VALIDATION -------------------------------------------------------------------
  
  # TB clinics % yield for finding new HIV cases FY21-23 Annual PEPFAR results
  # TB_STAT
  # TB_STAT % coverage
  # TB_ART Coverage,
  # %TB_STAT_POS
  # %HTS_TST_POS TB modality, ranking on positivity yield from other modalities,
  # comparing volume of patients (%difference with the first HTS modality with higher yield,
  #                                Median results for all measures) 
  

  
  #check to include only countries that report HTS_TST with TB modality in all FY21-FY23
  df_msd %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result"),
           fiscal_year %in% c(2019, 2020, 2021, 2022, 2023),
           modality == "TBClinic"
           ) %>% 
    group_by(fiscal_year, country, modality) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>%
    reshape_msd() %>% 
    filter(value != 0) %>% 
    #count(country) %>% pull(country)
    count(period, country) %>% 
    arrange(country) %>%  
    group_by(country) %>% 
    mutate(sum = sum(n)) %>% 
    ungroup() %>% 
    filter(sum ==20) %>% 
    distinct(country)
    
    
    #check to include only countries that report TB_STAT in all FY21-FY23
    df_msd %>% 
      filter(indicator %in% c("TB_STAT"),
             numeratordenom == "D",
             standardizeddisaggregate %in% c("Age/Sex"),
             fiscal_year %in% c(2019, 2020, 2021, 2022, 2023),
      ) %>% 
      group_by(fiscal_year, indicator, country) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>%
      reshape_msd() %>% 
      filter(value != 0) %>% 
      #count(country) %>% pull(country)
      count(period, country) %>% 
      arrange(country) %>% 
      group_by(country) %>% 
      mutate(sum = sum(n)) %>% 
      ungroup() %>% 
      filter(sum ==20) %>% 
      distinct(country)
  
    
  #check TB_ART countries 
    df_msd %>% 
      filter(indicator %in% c("TB_ART"),
             standardizeddisaggregate %in% c("Age/Sex/KnownNewPosNeg"),
             fiscal_year %in% c(2019, 2020, 2021, 2022, 2023),
      ) %>% 
     #count(standardizeddisaggregate)
      group_by(fiscal_year, indicator, country) %>% 
      summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>%
      #count(country) %>% pull(country)
    count(fiscal_year, country) %>% 
      arrange(country) %>% 
      group_by(country) %>% 
      mutate(sum = sum(n)) %>% 
      ungroup() %>% 
      filter(sum ==5) %>% 
      distinct(country)

    
  # MUNGE -------------------------------------------------------------------
    
    
    df_msd %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             country %ni% exclude_cntry,
             fiscal_year %in% c(2019,2020, 2021, 2022,2023),
             standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
      count(modality)
    
  #calculate TB_STAT known status 
    df_msd %>% 
      filter(indicator %in% c("TB_STAT"),
             fiscal_year %in% c(2019,2020, 2021, 2022,2023),
             country %in% include_cntry,
             standardizeddisaggregate %in% c("Age/Sex/KnownNewPosNeg", "Age/Sex") 
             ) %>% 
      group_by(fiscal_year, indicator, numeratordenom) %>% 
      summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
      pivot_wider(names_from = "numeratordenom", values_from = "cumulative") %>% 
      mutate(pct_known_status = `N` / `D`)
    
    
    #calculate TB_STAT_POS  
    df_msd %>% 
      filter(indicator %in% c("TB_STAT"),
             fiscal_year %in% c(2019,2020, 2021, 2022,2023),
             country %in% include_cntry,
             standardizeddisaggregate %in% c("Age/Sex/KnownNewPosNeg") 
      ) %>% 
      group_by(fiscal_year, indicator, numeratordenom, statushiv) %>% 
      summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
      #  reshape_msd() %>% 
      pivot_wider(names_from = "statushiv", values_from = "cumulative") %>% 
      mutate(total = `Negative` + `Positive`) %>% 
      mutate(tb_stat_pos = `Positive`/total)
    
  # TB_STAT_D by gender
    
    df_msd %>% 
      filter(indicator %in% c("TB_STAT"),
             fiscal_year %in% c(2019,2020, 2021, 2022,2023),
             country %in% include_cntry,
             standardizeddisaggregate %in% c("Age/Sex/KnownNewPosNeg", "Age/Sex") 
      ) %>% 
      group_by(indicator, numeratordenom, sex) %>% 
      summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
      pivot_wider(names_from = "sex", values_from = "cumulative") %>% 
      mutate(total = Female + Male) %>% 
      pivot_longer(cols = c("Female", "Male"), names_to = "sex") %>% 
      mutate(share = value / total)
    
    
  #TB_STAT_POS by gender
    df_msd %>% 
      filter(indicator %in% c("TB_STAT"),
             fiscal_year %in% c(2019,2020, 2021, 2022,2023),
             country %in% include_cntry,
             standardizeddisaggregate %in% c("Age/Sex/KnownNewPosNeg") 
      ) %>% 
      group_by(indicator, numeratordenom, statushiv, sex) %>% 
      summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
      pivot_wider(names_from = "statushiv", values_from = "cumulative") %>% 
      mutate(total = `Negative` + `Positive`) %>% 
      mutate(tb_stat_pos = `Positive`/total)
  
    #coinfection by age
    df_msd %>% 
      filter(indicator %in% c("TB_STAT"),
             fiscal_year %in% c(2019,2020, 2021, 2022,2023),
             country %in% include_cntry,
             standardizeddisaggregate %in% c("Age/Sex/KnownNewPosNeg") 
      ) %>% 
      group_by(indicator, numeratordenom, statushiv, ageasentered) %>% 
      summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
      pivot_wider(names_from = "statushiv", values_from = "cumulative") %>% 
      mutate(total = `Negative` + `Positive`) %>% 
      mutate(tb_stat_pos = `Positive`/total) %>% View()
    
    df_msd %>% 
      filter(indicator %in% c("TB_STAT"),
             fiscal_year %in% c(2019,2020, 2021, 2022,2023),
             country %in% include_cntry,
             standardizeddisaggregate %in% c("Age/Sex/KnownNewPosNeg") 
      ) %>% 
      group_by(fiscal_year, indicator, numeratordenom, statushiv, country) %>% 
      summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
      pivot_wider(names_from = "statushiv", values_from = "cumulative") %>% 
      mutate(total = `Negative` + `Positive`) %>% 
      mutate(tb_stat_pos = `Positive`/total) %>% View()
    
    
#calculate HTS tb modality positivity    
  df_hts_mod <-  df_msd %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             country %in% include_cntry,
           #  country %ni% c("Nigeria", "Tanzania"),
             fiscal_year %in% c(2019,2020, 2021, 2022,2023),
             standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
    # count(country)
      group_by(fiscal_year, indicator, modality) %>% 
      summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
      #  reshape_msd() %>% 
      pivot_wider(names_from = "indicator", values_from = "cumulative") %>% 
      mutate(positivity = HTS_TST_POS/HTS_TST)
  
  df_msd %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           country %ni% exclude_cntry,
           country %ni% c("Nigeria", "Tanzania"),
           fiscal_year %in% c(2019,2020, 2021, 2022,2023),
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
    # count(country)
    group_by(fiscal_year, indicator, target_modality_2024) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    #  reshape_msd() %>% 
    pivot_wider(names_from = "indicator", values_from = "cumulative") %>% 
    mutate(positivity = HTS_TST_POS/HTS_TST) %>% 
    filter(target_modality_2024 == "Active Index")
      
  # VIZ -------------------------------------------------------------------
       
#distrbituion of HIV pos yield by mod over time      
df_hts_mod %>% 
 # ggplot(aes(fct_reorder(primepartner, HTS_TST, sum, .desc = TRUE), fct_reorder(modality, positivity), fill = positivity))
  ggplot(aes(x = fiscal_year, 
             y = fct_reorder(modality, positivity, sum, .desc = FALSE), 
             fill = positivity)) +
  geom_tile(color = "white", 
            size = 0.9) +
  geom_text(aes(label = percent(positivity, 0.1),
                color = if_else(positivity <= 0.1, grey90k, "white")),
            size = 4) +
  # scale_x_discrete(position = "top", 
  #                  guide = guide_axis(n.dodge = 2)) +
  scale_fill_si(palette = "scooters", discrete = FALSE) +
  scale_color_identity() +
  si_style_nolines() +
  theme(
    panel.background = element_rect(fill = "#f6f6f6", color ="white"),
        legend.position = "none") +
  labs(x = NULL, 
       y = NULL,
       title = "Among HIV testing modalities, TB patients tested at TB clinics remains the second highest HIV positivity yield" %>% toupper(),
       subtitle = "MER data for countries that report HIV testing results at TBClinic modality from 2019-2023",
       caption = metadata$caption) 

si_save("Graphics/02_modality_heatmap.svg")
si_save("Images/02_modality_heatmap.png")

#HTS Modality visuals - volume by modality

nudge_space <- 0.125

viz_bar <- df_hts_mod %>% 
  filter(modality %in% c("Active Index", "TBClinic")) %>% 
  ggplot(aes(x = fiscal_year)) +
  geom_col(aes(y = HTS_TST), width = 0.75, fill = scooter) +
 # geom_col(aes(y = HTS_TST_POS), width = 0.75, position = position_nudge(x = nudge_space), fill = scooter_med) +
 # geom_col(aes(y = `Specimen Return`), width = 0.75, position = position_nudge(x = nudge_space*2), fill = golden_sand) +
  facet_wrap(~modality) +
  si_style_ygrid() +
  geom_text(aes(y = HTS_TST, label = clean_number(HTS_TST)), size = 12/.pt, hjust = 0,
            color = scooter,
            family = "Source Sans Pro") +
  # geom_text(aes(y = `Specimen Sent Xpert`, label = clean_number(`Specimen Sent Xpert`)), size = 12/.pt, hjust = 0,
  #           color = "white",
  #           family = "Source Sans Pro") +
  # geom_text(aes(y = `Specimen Return`, label = clean_number(`Specimen Return`)), size = 12/.pt, hjust = 0,
  #           color = "white",
  #           family = "Source Sans Pro") +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(x = NULL, y = NULL,
       caption = glue("{metadata$caption}"))+
  theme(strip.text = element_blank())


#positivity by modality
viz_line <- df_hts_mod %>% 
  filter(modality %in% c("Active Index", "TBClinic")) %>% 
  ggplot(aes(fiscal_year, positivity, group = modality, color = scooter, fill = scooter)) +
  geom_line(alpha = .4, size = .9, position = "identity") +
  geom_point(na.rm = TRUE) +
  geom_text(aes(label = percent(positivity, 1)), na.rm = TRUE,
            hjust = .5, vjust = -0.4,family = "Source Sans Pro") +
  facet_wrap(~modality) +
  scale_color_identity() + 
  scale_fill_identity() +
  si_style_nolines() +
  scale_y_continuous(labels = percent) +
  labs(x = NULL,
       y = NULL,
      # title = "Adults (15+) account for a higher positivity yield than pediatrics (<15) amongst index testing and HIV testing in TB Clinics" %>% toupper(),
       title = "HIV testing volume and positivity yield by Index testing and TB Clinic modality") + 
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_blank())

viz_line / viz_bar +
  plot_layout(heights = c(1,2))

si_save("Graphics/01_ias_modality_hts.svg")

# VALS FOR ABSTRACT -------------------------------------------------------------------

#median val for TBClinic Positivity yield
df_hts_mod %>% 
  filter(modality %in% c("Active Index", "TBClinic")) %>% 
  group_by(modality) %>% 
  summarise(median_pos = median(positivity)) %>% 
  ungroup()

df_hts_mod %>% 
  filter(target_modality_2024 %in% c("Active Index", "TBClinic"),
         fiscal_year %in% c(2019, 2023)) %>% 
  select(-c(HTS_TST, positivity)) %>% 
  pivot_wider(names_from = "target_modality_2024", values_from = "HTS_TST_POS") %>% 
  group_by(fiscal_year) %>% 
  mutate(diff = (`Active Index`- TBClinic)) %>% 
  ungroup()

df_hts_mod_age <-  df_msd %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         country %in% include_cntry,
         target_modality_2024 %in% c("Active Index", "TBClinic"),
         fiscal_year %in% c(2019,2020, 2021, 2022, 2023),
         standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
  # count(country)
  group_by(fiscal_year, indicator, target_modality_2024, trendscoarse) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  #  reshape_msd() %>% 
  pivot_wider(names_from = "indicator", values_from = "cumulative") %>% 
  mutate(positivity = HTS_TST_POS/HTS_TST)

df_hts_mod_sex <-  df_msd %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         country %ni% exclude_cntry,
         target_modality_2024 %in% c("Active Index", "TBClinic"),
         fiscal_year %in% c(2019,2020, 2021, 2022, 2023),
         standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
  # count(country)
  group_by(fiscal_year, indicator, target_modality_2024, sex) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  #  reshape_msd() %>% 
  pivot_wider(names_from = "indicator", values_from = "cumulative") %>% 
  mutate(positivity = HTS_TST_POS/HTS_TST)

df_hts_mod_age %>%
  mutate(endpoints = case_when(fiscal_year %in% c(max(fiscal_year), min(fiscal_year))~positivity)) %>% 
 # filter(country %in% top_cntry) %>%
  ggplot(aes(fiscal_year, positivity, group = target_modality_2024, color = scooter, fill = scooter)) +
  geom_area(alpha = .4, size = .9, position = "identity") +
   geom_point(aes(y = endpoints), na.rm = TRUE) +
  geom_text(aes(label = percent(positivity, 1)), na.rm = TRUE,
            hjust = .5, vjust = -0.4,family = "Source Sans Pro") +
  facet_grid(target_modality_2024~trendscoarse, space = "free") +
  scale_color_identity() + 
  scale_fill_identity() +
  si_style_ygrid() +
  scale_y_continuous(labels = percent) +
  labs(x = NULL,
       y = NULL,
       title = "Adults (15+) account for a higher positivity yield than pediatrics (<15) amongst index testing and HIV testing in TB Clinics" %>% toupper(),
       subtitle = "HIV positivity yield by course age and modality",
       caption = metadata$caption) + 
  theme(strip.text.y = element_text(angle = 0))


df_hts_mod_sex %>%
  filter(sex != "Unknown Sex") %>% 
  mutate(fill_color = ifelse(sex == "Female", moody_blue, genoa)) %>% 
  mutate(endpoints = case_when(fiscal_year %in% c(max(fiscal_year), min(fiscal_year))~positivity)) %>% 
  # filter(country %in% top_cntry) %>%
  ggplot(aes(fiscal_year, positivity, group = target_modality_2024, color = fill_color, fill = fill_color)) +
  geom_area(alpha = .4, size = .9, position = "identity") +
  geom_point(aes(y = endpoints), na.rm = TRUE) +
  geom_text(aes(label = percent(positivity, 1)), na.rm = TRUE,
            hjust = .5, vjust = -0.4,family = "Source Sans Pro") +
  facet_grid(sex ~target_modality_2024, space = "free") +
  scale_color_identity() + 
  scale_fill_identity() +
  si_style_ygrid() +
  scale_y_continuous(labels = percent) +
  labs(x = NULL,
       y = NULL,
       title = "Females have a higher positivity yield than males amongst index testing and HIV testing in TB Clinics" %>% toupper(),
       subtitle = "HIV positivity yield by sex and modality, FY19-FY23",
       caption = metadata$caption) + 
  theme(strip.text.y = element_text(angle = 0))
