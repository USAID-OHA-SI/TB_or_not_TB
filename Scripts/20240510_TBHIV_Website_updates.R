# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  re-work TB visuals for TB/HIV website
# REF ID:   8c0b07a5 
# LICENSE:  MIT
# DATE:     2024-05-10
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
library(mindthegap)
library(gt)
library(gtExtras)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()
  
  ref_id <- "8c0b07a5"
  
  #rounding function
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }

# IMPORT ------------------------------------------------------------------
  
  #Current MSD
  filepath <- si_path() %>% 
    return_latest("MER_Structured_Datasets_OU_IM_FY22-24")
  
  metadata_msd <- get_metadata()
  
  #read MSD
  df_msd <- read_psd(filepath)
  
  #read NGA genie file
  df_nga <- si_path() %>% 
    return_latest("All indicators_FY23Q4_Nigeria") %>% 
    read_csv()
  
  #select only names in MSD
  names_to_keep <- names(df_msd)
  setdiff(names(df_msd), names(df_nga)) #only qtrs
  setdiff(names(df_nga), names(df_msd))
  
  #select all names from NGA file that match MSD (minus qtr1-qtr4)
  df_nga_new <- df_nga %>% 
    select(any_of(names_to_keep))
  
  #rbind together removing TZ and NGA for FY23 ONLY
  df <- df_msd %>% 
    filter(!(operatingunit=="Nigeria" & fiscal_year=="2023")
           # ,
           # !(operatingunit=="Tanzania" & fiscal_year=="2023")
    ) %>% 
    select(-c(qtr1:qtr4)) %>% 
    rbind(df_nga_new)
  
  
  #Current MSD
  arch_filepath <- si_path() %>% 
    return_latest("MER_Structured_Datasets_OU_IM_FY15")
  
  df_arch <- read_psd(arch_filepath)

# MUNGE -------------------------------------------------------------------
  
#  TX_TB Den for FY23 Q4 for absolute numbers of people who was screened for TB
#  % TPT completion with trends overtime (no volume)
#  Volume of patients with TB that learned their status (TB_STAT Num) and separate visual  % TB_STAT coverage

  
 # TB SCREEN -------------------------------------------------------------------
  
  # TB screen test type - qc
 txtb_qc <- df %>% 
    clean_indicator() %>% 
    filter(fiscal_year == 2023,
           indicator == "TX_TB_D",
           funding_agency == "USAID",
           standardizeddisaggregate == "TB Test Type/HIVStatus") %>%
    mutate(ou_qc = case_when(operatingunit == "Nigeria" ~ "Nigeria",
                             #   operatingunit == "Tanzania" ~ "Tanzania",
                             TRUE ~ "All other OUs")) %>% 
    group_by(fiscal_year, indicator, funding_agency, otherdisaggregate, ou_qc) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    group_by(ou_qc) %>% 
    mutate(total = sum(cumulative)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = "otherdisaggregate", values_from = "cumulative") %>% 
    mutate(pct_xpert = `Xpert`/total)
  
  txtb_qc %>%
    select(fiscal_year, funding_agency, ou_qc, Xpert, total, pct_xpert) %>% 
    gt() %>% 
    fmt_number(columns = c(4,5), 
               decimals = 0) %>% 
    fmt_percent(columns = c(6), 
               decimals = 0) %>% 
    grand_summary_rows(
      columns = c(4,5),
      fns = list(
        Overall = ~ sum(., na.rm = T)
      ),
      formatter = fmt_number,
      decimals = 0
    ) %>% 
    tab_header(
      title = glue("Volume and Percentage of Molecular Tests" %>% toupper())) %>% 
    gt_theme_nytimes() 
  
  
  # TB screen test type - viz 
  xpert_viz <- df %>% 
    bind_rows(df_arch) %>% 
    clean_indicator() %>% 
    filter(
      #fiscal_year == 2023,
           indicator == "TX_TB_D",
           funding_agency == "USAID",
           standardizeddisaggregate == "TB Test Type/HIVStatus") %>%
    # mutate(ou_qc = case_when(operatingunit == "Nigeria" ~ "Nigeria",
    #                          #   operatingunit == "Tanzania" ~ "Tanzania",
    #                          TRUE ~ "All other OUs")) %>% 
    group_by(fiscal_year, indicator, funding_agency, otherdisaggregate) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    group_by(fiscal_year) %>% 
    mutate(total = sum(cumulative)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = "otherdisaggregate", values_from = "cumulative") %>% 
    mutate(pct_xpert = `Xpert`/total)
  
 xpert_bar <-  xpert_viz %>% 
    ggplot(aes(x = fiscal_year)) +
    geom_col(aes(y= Xpert), fill = hw_midnight_blue) +
    geom_text(aes(y = Xpert, label = clean_number(Xpert)), size = 12/.pt,
              hjust = 0.5, vjust = -0.5,
              color = "black",
              family = "Source Sans Pro") +
    si_style_nolines() +
    scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
    scale_y_continuous(label = scales::label_number(scale_cut = cut_short_scale()), limits = c(0, 200000)) +
    labs(x = NULL, y = NULL,
         caption = glue::glue("{metadata_msd$caption}")) +
   theme(plot.title = element_markdown(),
         #axis.text.x = element_blank(),
         axis.text.y = element_blank())
 
 plot_title <- "THE BIGGEST <span style= 'color:#002A6C;'> 
  VOLUME OF MOLECULAR TESTS</span> AND THE HIGHEST <span style = 'color:#5BB5D5;'> PERCENTAGE USE </span> OF THEM FOR TB DIAGNOSIS"
 
 
 xpert_line <- xpert_viz %>% 
   ggplot(aes(x = fiscal_year, y = pct_xpert, color = hw_viking)) +
   geom_line(size = 1) + 
   geom_point(size = 3) +
   scale_y_continuous(label = percent, limits = c(0.6, 0.9)) +
  # scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
   geom_text(aes(label = percent(pct_xpert, 1)),
             vjust = -1, hjust = 0.1,
            # color = "black",
             family = "Source Sans Pro") +
   scale_color_identity() +
   si_style_nolines() +
   labs(x = NULL, y = NULL,
        title = glue::glue("{plot_title}")) +
   theme(plot.title = element_markdown(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank())
   
   
 xpert_line / xpert_bar +
   plot_layout(heights = c(1,2))
 
 si_save("Images/TBHIV Website/01_xpert_tb_screen.png")
 
    
# TPT -----------------------------------------------------------------------
  
  #TPT completion - qc
  tpt_qc <- df %>% 
    clean_indicator() %>% 
    filter(fiscal_year == 2023,
           indicator %in% c("TB_PREV", "TB_PREV_D") ,
           funding_agency == "USAID",
           standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>%
    mutate(ou_qc = case_when(operatingunit == "Nigeria" ~ "Nigeria",
                             #   operatingunit == "Tanzania" ~ "Tanzania",
                             TRUE ~ "All other OUs")) %>%
    group_by(fiscal_year, indicator, funding_agency, ou_qc) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = "indicator", values_from = "cumulative") %>% 
    mutate(tpt_completion = `TB_PREV`/`TB_PREV_D`)
 
 tpt_qc %>% 
   gt() %>% 
   fmt_number(columns = c(4,5), 
              decimals = 0) %>% 
   fmt_percent(columns = c(6), 
               decimals = 0) %>% 
   grand_summary_rows(
     columns = c(4,5),
     fns = list(
       Overall = ~ sum(., na.rm = T)
     ),
     formatter = fmt_number,
     decimals = 0
   ) %>% 
   tab_header(
     title = glue("TPT Completion" %>% toupper())) %>% 
   gt_theme_nytimes() 
  
  #TPT completion - vziz
  tpt_viz <- df %>% 
    bind_rows(df_arch) %>% 
    clean_indicator() %>% 
    filter(indicator %in% c("TB_PREV", "TB_PREV_D") ,
           funding_agency == "USAID",
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    group_by(fiscal_year, indicator, funding_agency) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = "indicator", values_from = "cumulative") %>% 
    mutate(tpt_completion = `TB_PREV`/`TB_PREV_D`)
  
  plot_title <- "AN INCREMENT OF <span style= 'color:#419164;'> 
  THE PERCENTAGE OF TPT COMPLETIONS</span> AMONG PLWH ON ART"
  
  
  
 tpt_viz %>% 
   filter(fiscal_year != 2024) %>% 
   ggplot(aes(x = fiscal_year, y = tpt_completion, color = hw_hunter)) +
   geom_line(size = 1) + 
   geom_point(size = 3) +
   scale_y_continuous(label = percent) +
    scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
   geom_text(aes(label = percent(tpt_completion, 1)),
             vjust = -1.5, hjust = 0.5,
             # color = "black",
             family = "Source Sans Pro") +
   scale_color_identity() +
   si_style_nolines() +
   labs(x = NULL, y = NULL,
        title = glue::glue("{plot_title}"),
       # subtitle = "Subtitle",
        caption = glue::glue("{metadata_msd$caption}")) +
   theme(plot.title = element_markdown(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank())
  
  
  si_save("Images/TBHIV Website/02_tpt_completion.png")
  
  
# TB_STAT ----------------------------------------------------------------
  
  #QC
  tb_stat_qc <- df %>% 
    clean_indicator() %>% 
    filter(fiscal_year == 2023,
           indicator %in% c("TB_STAT", "TB_STAT_D") ,
           funding_agency == "USAID",
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    mutate(ou_qc = case_when(operatingunit == "Nigeria" ~ "Nigeria",
                             #   operatingunit == "Tanzania" ~ "Tanzania",
                             TRUE ~ "All other OUs")) %>%
    group_by(fiscal_year, indicator, funding_agency, ou_qc) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = "indicator", values_from = "cumulative") %>% 
    mutate(tb_stat_cov = `TB_STAT`/`TB_STAT_D`)
  
  
  tb_stat_qc %>% 
    gt() %>% 
    fmt_number(columns = c(4,5), 
               decimals = 0) %>% 
    fmt_percent(columns = c(6), 
                decimals = 0) %>% 
    grand_summary_rows(
      columns = c(4,5),
      fns = list(
        Overall = ~ sum(., na.rm = T)
      ),
      formatter = fmt_number,
      decimals = 0
    ) %>% 
    tab_header(
      title = glue("TB_STAT Coverage" %>% toupper())) %>% 
    gt_theme_nytimes() 
  
  #TB_STAT - viz
  tbstat_viz <- df %>% 
    bind_rows(df_arch) %>% 
    clean_indicator() %>% 
    filter(indicator %in% c("TB_STAT", "TB_STAT_D") ,
           funding_agency == "USAID",
           fiscal_year != 2024,
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    group_by(fiscal_year, indicator, funding_agency) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = "indicator", values_from = "cumulative") %>% 
    mutate(tb_stat_cov = `TB_STAT`/`TB_STAT_D`)
  
  tb_stat_bar <- tbstat_viz %>% 
    filter(fiscal_year >= 2017) %>% 
    ggplot(aes(x = fiscal_year)) +
    geom_col(aes(y= TB_STAT), fill = hw_electric_indigo) +
    geom_text(aes(y = TB_STAT, label = clean_number(TB_STAT)), size = 12/.pt,
              hjust = 0.5, vjust = -0.5,
              color = "black",
              family = "Source Sans Pro") +
    si_style_nolines() +
    scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
    scale_y_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
    labs(x = NULL, y = NULL,
         caption = glue::glue("{metadata_msd$caption}")) +
    theme(plot.title = element_markdown(),
         # axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  
  plot_title <- "<span style= 'color:#3B5BBE;'> 96%</span> of TB PATIENTS <span style = 'color:#3B5BBE;'> KNOW THEIR HIV STATUS </span>"
  
  
  tb_stat_line <- tbstat_viz %>% 
    filter(fiscal_year >= 2017) %>% 
    ggplot(aes(x = fiscal_year, y = tb_stat_cov, color = hw_electric_indigo)) +
    geom_line(size = 1) + 
    geom_point(size = 3) +
    scale_y_continuous(label = percent, limits = c(.87, .98)) +
    # scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
    geom_text(aes(label = percent(tb_stat_cov, 1)),
              vjust = -1, hjust = 0.1,
              # color = "black",
              family = "Source Sans Pro") +
    scale_color_identity() +
    si_style_nolines() +
    labs(x = NULL, y = NULL,
         title = glue::glue("{plot_title}")) +
    theme(plot.title = element_markdown(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  
  tb_stat_line / tb_stat_bar +
    plot_layout(heights = c(1,2))  
  
  si_save("Images/TBHIV Website/03_tb_stat.png")
  
  
  