# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TB case finding manuscript visuals
# REF ID:   8bbff34b 
# LICENSE:  MIT
# DATE:     2023-09-27
# UPDATED:  2023-10-03

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

  # read in OU level MSD and hitoric MSD
  filepath <- si_path() %>% return_latest("OU_IM_FY21")
  filepath_hist <- si_path() %>% return_latest("OU_IM_FY15")
  

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "8bbff34b"
  
  #rounding function
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }

# IMPORT ------------------------------------------------------------------
  
  df_msd <- read_psd(filepath) 
  df_hist <- read_psd(filepath_hist)
  
  #bind dfs together
  df_msd <- df_msd %>% 
    rbind(df_hist) %>% 
    filter(indicatortype != "CS")
  
  df_msd %>% 
   # resolve_knownissues() %>% 
    count(indicatortype)

# MUNGE -------------------------------------------------------------------
  
  #Number and percent of ART patients screened (W4SS) for TB and presumptive for active TB disease, 2019 -2022. 

  #grab total PLHIV screen rate for TB
  df_txcurr <- df_msd %>% 
   # resolve_knownissues() %>% 
    clean_indicator() %>%
    filter(indicator %in% c("TX_TB_D", "TX_CURR"),
           fiscal_year <= 2022 & fiscal_year >=2019,
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
    ) %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>%
    reshape_msd() %>% 
    pivot_wider(names_from = "indicator", values_from = "value") %>% 
    mutate(screen_rate = `TX_TB_D`/`TX_CURR`) %>%
    filter(screen_rate !=0)
  
  #grab positives
 df_tx_viz <- df_msd %>%
    clean_indicator() %>%
    filter(indicator %in% c("TX_TB_D"),
           fiscal_year <= 2022 & fiscal_year >=2019,
           standardizeddisaggregate %in% c("Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus")) %>% 
    mutate(tb_screen_status = ifelse(str_detect(otherdisaggregate, "Positive"), "Positive", "Negative")) %>% 
    group_by(indicator, fiscal_year, tb_screen_status) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    group_by(indicator, period) %>% 
    mutate(total_screen = sum(value)) %>%
   ungroup() %>% 
    rename(screen_pos = value) %>% 
    filter(screen_pos != 0,
           tb_screen_status == "Positive") %>% 
    select(period, screen_pos, total_screen) %>% 
    left_join(df_txcurr, by = c("period")) %>% 
    select(-c(total_screen, period_type)) %>% 
    rename(`TX_TB_D_Pos` = screen_pos)
 
 #Figure 3: Percent yield of new TB diagnosed among PLHIV New to ART (<6 months) and Already on ART (> 6 months), 2019 -2022. ---
 
 #grab total screening of already vs new on art
df_total <- df_msd %>% 
   clean_indicator() %>%
   filter(indicator %in% c("TX_TB_D"),
          standardizeddisaggregate  %in% c("Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus")) %>% 
   group_by(fiscal_year, indicator,
            otherdisaggregate
   ) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
   mutate(status_tb = ifelse(str_detect(otherdisaggregate, "Positive"), "Positive", "Negative")) %>% 
   mutate(art_status = ifelse(str_detect(otherdisaggregate, "Already"), "Already", "New")) %>% 
   group_by(fiscal_year, indicator, art_status) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
   reshape_msd() %>% 
   filter(value !=0) %>% 
   rename(total_screen = value)
 
#grab pos and bind
 df_yield_viz <- df_msd %>% 
   clean_indicator() %>%
   filter(indicator %in% c("TX_TB_D"),
          # fiscal_year ==2023,
          standardizeddisaggregate  %in% c("Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus")) %>% 

   group_by(fiscal_year, indicator,
            otherdisaggregate
   ) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
   mutate(status_tb = ifelse(str_detect(otherdisaggregate, "Positive"), "Positive", "Negative")) %>% 
   mutate(art_status = ifelse(str_detect(otherdisaggregate, "Already"), "Already", "New")) %>% 
   filter(status_tb == "Positive") %>% 
   group_by(fiscal_year, indicator, art_status) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
   reshape_msd() %>% 
   filter(value !=0) %>% 
   rename(pos_screen = value) %>% 
   left_join(df_total, by = c("indicator", "period", "period_type", "art_status")) %>% 
   mutate(screen_rate = pos_screen/total_screen)
   
# Figure 5: Number and percent of samples sent and returned positive among PLHIV with presumptive TB, 2019 - 2022.
 
 df_testtype <- df_msd %>% 
   filter(indicator == "TX_TB",
          fiscal_year %in% c(2019, 2020, 2021, 2022),
          numeratordenom == "D",
          #operatingunit == "Zambia",
          standardizeddisaggregate %in% c("Specimen Sent/HIVStatus","Specimen Return/HIVStatus", "TB Test Type/HIVStatus")) %>% 
   group_by(fiscal_year, indicator,
            standardizeddisaggregate, otherdisaggregate) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
   reshape_msd() %>% 
   mutate(standardizeddisaggregate = glue("{standardizeddisaggregate}-{otherdisaggregate}")) %>% 
   filter(value != 0) %>% 
   # filter(value != 0,
   #        standardizeddisaggregate %in% c("TB Test Type/HIVStatus-Xpert", "Specimen Sent/HIVStatus-NA", "Specimen Return/HIVStatus-Positive")) %>% 
   mutate(standardizeddisaggregate = case_when(standardizeddisaggregate == "Specimen Sent/HIVStatus-NA" ~ "Specimen Sent",
                                               standardizeddisaggregate == "Specimen Return/HIVStatus-Positive" ~ "Specimen Return",
                                               standardizeddisaggregate == "TB Test Type/HIVStatus-Xpert" ~ "Specimen Sent Xpert",
                                               standardizeddisaggregate == "TB Test Type/HIVStatus-Smear" ~ "Specimen Sent Smear",
                                               standardizeddisaggregate == "TB Test Type/HIVStatus-Other - TB Test Type" ~ "Specimen Sent Other TB Test Type")) %>% 
   select(-c(otherdisaggregate, period_type)) %>% 
   pivot_wider(names_from = "standardizeddisaggregate") %>% 
   mutate(`Total Known Test Type` = `Specimen Sent Other TB Test Type` + `Specimen Sent Smear`+`Specimen Sent Xpert`) %>% 
   mutate(pct_positive = `Specimen Return`/`Specimen Sent`,
          old_xpert =  `Specimen Sent Xpert`/`Specimen Sent`,
          pct_xpert = `Specimen Sent Xpert`/`Total Known Test Type`)


 

 # VIZ -------------------------------------------------------
 
 nudge_space <- 0.125
 
 #Figure 1 ----
 v_bar <- df_tx_viz %>% 
   ggplot(aes(x = period)) +
   geom_col(aes(y = `TX_CURR`), width = 0.75, fill = trolley_grey_light) +
   geom_col(aes(y = `TX_TB_D`), width = 0.75, position = position_nudge(x = nudge_space), fill = scooter) +
   geom_col(aes(y = `TX_TB_D_Pos`), width = 0.75, position = position_nudge(x = nudge_space*2), fill = scooter_light) +
   si_style_ygrid() +
   geom_text(aes(y = `TX_CURR`, label = clean_number(`TX_CURR`, 1)), size = 12/.pt, hjust = 0,
             color = "black",
             family = "Source Sans Pro") +
   geom_text(aes(y = `TX_TB_D`, label = clean_number(`TX_TB_D`, 1)), size = 12/.pt, hjust = 0,
             color = "white",
             family = "Source Sans Pro") +
   geom_text(aes(y = `TX_TB_D_Pos`, label = clean_number(`TX_TB_D_Pos`, 1)), size = 12/.pt, hjust = 0,
             color = "white",
             family = "Source Sans Pro") +
   scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
   labs(x = NULL, y = NULL,
        caption = glue("{metadata$caption}"))
 
v_line <- df_tx_viz %>% 
   mutate(group =1) %>% 
   ggplot(aes(period, screen_rate, group = group,
              color = "#004964", fill = "#004964")) +
   geom_blank(aes(y = 1.1 * screen_rate)) +
   geom_line(size = 1.5) +
   geom_point(shape = 21, size = 10, stroke = 2) +
   scale_fill_identity() +
   scale_color_identity() +
   scale_y_continuous(labels = percent, limits = c(.7, .9)) +
   geom_text(aes(label = percent(screen_rate, 1)), color = "white",
             family = "Source Sans Pro",
             size = 12/.pt) +
   expand_limits(y = .2) +
   si_style_ygrid() +
   labs(x = NULL, y = NULL,
        subtitle = "Number and percent of ART patients screened (W4SS) for TB and presumptive for active TB disease, 2019 -2022") 
 
v_line / v_bar +
  plot_layout(heights = c(1,2))

si_save("Graphics/01_ART_screen.svg")          


#Figure 3: Percent yield of new TB diagnosed among PLHIV New to ART (<6 months) and Already on ART (> 6 months), 2019 -2022. ---

df_yield_viz %>% 
  filter(!str_detect(period, "FY23")) %>% 
  mutate(fill_color = ifelse(art_status == "Already", genoa, genoa_light)) %>% 
ggplot(aes(period, screen_rate, group = art_status,
           color = fill_color, fill = fill_color)) +
  geom_blank(aes(y = 1.1 * screen_rate)) +
  geom_line(size = 1.5) +
  geom_point(shape = 21, size = 10, stroke = 2) +
  geom_hline(yintercept = 0.15, linetype = "dashed", size = .75, color = genoa_light) +
  geom_hline(yintercept = 0.05, linetype = "dashed", size = .75, color = genoa) +
  scale_fill_identity() +
  scale_color_identity() +
  # geom_text(aes(label = df_viz$funding_agency,
  #               family = "Source Sans Pro",
  #               size = 10/.pt) +
   facet_wrap(~art_status) +
  #si_style_nogrid() +
  scale_y_continuous(labels = percent, limits = c(0, .16), breaks = c(0.02, 0.04, 0.06, 0.08, .1, .12, .14, .16)) +
  geom_text(aes(label = percent(screen_rate, 1)), color = "white",
            family = "Source Sans Pro",
            size = 12/.pt) +
  expand_limits(y = .2) +
  si_style_ygrid() +
  labs(x = NULL, y = NULL,
       subtitle = "Percent yield of new TB diagnosed among PLHIV New to ART (<6 months) and Already on ART (> 6 months), 2019-2022") 


si_save("Graphics/02_TB_screenrate_by_art_status.svg")

# Figure 5: Number and percent of samples sent and returned positive among PLHIV with presumptive TB, 2019 - 2022.

viz_bar <- df_testtype %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = `Specimen Sent`), width = 0.75, fill = scooter) +
  geom_col(aes(y = `Specimen Sent Xpert`), width = 0.75, position = position_nudge(x = nudge_space), fill = scooter_med) +
  geom_col(aes(y = `Specimen Return`), width = 0.75, position = position_nudge(x = nudge_space*2), fill = golden_sand) +
  si_style_ygrid() +
  geom_text(aes(y = `Specimen Sent`, label = clean_number(`Specimen Sent`)), size = 12/.pt, hjust = 0,
            color = "black",
            family = "Source Sans Pro") +
  geom_text(aes(y = `Specimen Sent Xpert`, label = clean_number(`Specimen Sent Xpert`)), size = 12/.pt, hjust = 0,
            color = "white",
            family = "Source Sans Pro") +
  geom_text(aes(y = `Specimen Return`, label = clean_number(`Specimen Return`)), size = 12/.pt, hjust = 0,
            color = "white",
            family = "Source Sans Pro") +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(x = NULL, y = NULL,
       caption = glue("{metadata$caption}"))

viz_line <- df_testtype %>% 
  mutate(group =1) %>% 
  ggplot(aes(period, group = group)) +
  geom_blank(aes(y = 1.1 * pct_positive)) +
  geom_line(aes(y = pct_positive, color = golden_sand, fill = golden_sand), size = 1.5) +
  geom_point(aes(y = pct_positive, color = golden_sand, fill = golden_sand),shape = 21, size = 10, stroke = 2) +
  geom_line(aes(y = pct_xpert, color = scooter_med, fill = scooter_med), size = 1.5) +
  geom_point(aes(y = pct_xpert, color = scooter_med, fill = scooter_med),shape = 21, size = 10, stroke = 2) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(labels = percent, limits = c(.1, .85)) +
  geom_text(aes(y = pct_positive,label = percent(pct_positive, 1)), color = "white",
            family = "Source Sans Pro",
            size = 12/.pt) +
  geom_text(aes(y = pct_xpert,label = percent(pct_xpert, 1)), color = "white",
            family = "Source Sans Pro",
            size = 12/.pt) +
  expand_limits(y = .2) +
  si_style_nolines() +
  labs(x = NULL, y = NULL,
       subtitle = "Number and percent of samples sent and returned positive among PLHIV with presumptive TB, 2019 - 2022") 

viz_line / viz_bar +
  plot_layout(heights = c(1,2))

si_save("Graphics/03_TB_sample_sent.svg")

