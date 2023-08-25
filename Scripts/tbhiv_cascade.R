# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TB/HIV Cascade
# REF ID:   f3be3189 
# LICENSE:  MIT
# DATE:     2022-11-30
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


# Grab metadata
get_metadata()

ref_id <- "f50503e6"

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% return_latest("OU_IM") %>% 
  read_msd()

# MUNGE -------------------------------------------------------------------

#get total numerator and denom
total_num_denom <- df %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         #numeratordenom == "N",
         funding_agency == "USAID",
         # operatingunit == "Zambia",
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  #(standardizeddisaggregate)
  group_by(fiscal_year, indicator, standardizeddisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd)

#get TX_TB D POS and NEG
tx_pos_neg <- df %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         numeratordenom == "D",
         # operatingunit == "Zambia",
         standardizeddisaggregate == "Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus",
         funding_agency == "USAID") %>% 
  group_by(fiscal_year, indicator,
           funding_agency, standardizeddisaggregate, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  mutate(standardizeddisaggregate = ifelse(str_detect(otherdisaggregate, "Positive"), "TX_TB_D_Pos", "TX_TB_D_Neg")) %>% 
  group_by(fiscal_year, indicator, standardizeddisaggregate) %>% 
  #filter(str_detect(otherdisaggregate, "Positive")) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd)
  
#specimen sent
#Get Specimen sent
specimen_sent <- df %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         numeratordenom == "D",
         #operatingunit == "Zambia",
         standardizeddisaggregate == "Specimen Sent/HIVStatus",
         funding_agency == "USAID") %>% 
  group_by(fiscal_year, indicator,
           standardizeddisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd)

# test type
test_type <- df %>% 
  # resolve_knownissues() %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         # operatingunit == "Zambia",
         standardizeddisaggregate == "TB Test Type/HIVStatus",
         funding_agency == "USAID") %>%
  group_by(fiscal_year, indicator,
           standardizeddisaggregate, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  mutate(standardizeddisaggregate = glue("{standardizeddisaggregate}-{otherdisaggregate}")) %>% 
  select(-c(otherdisaggregate))
  


#get specimen return
specimen_return <- df %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         numeratordenom == "D",
         # operatingunit == "Zambia",
         standardizeddisaggregate == "Specimen Return/HIVStatus",
         funding_agency == "USAID") %>% 
  group_by(fiscal_year, indicator,
           standardizeddisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd)

# TPT 

tb_prev <- df %>% 
  #resolve_knownissues() %>% 
  filter(indicator == "TB_PREV",
         fiscal_year == metadata$curr_fy,
         funding_agency == "USAID",
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  group_by(fiscal_year, indicator, standardizeddisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd)

# bind rows --------------------------------------

df_viz <-bind_rows(total_num_denom, tx_pos_neg, tb_prev, specimen_sent, specimen_return) %>% 
  mutate(x_group = case_when(
    indicator == "TX_TB" & standardizeddisaggregate == "Total Denominator" ~ "TX_TB Total Denominator",
    indicator == "TX_TB" & standardizeddisaggregate == "Total Numerator" ~ "TX_TB Total Numerator",
    standardizeddisaggregate == "TX_TB_D_Pos" ~ "TX_TB D Positive",
    standardizeddisaggregate == "TX_TB_D_Neg" ~ "TX_TB D Negative",
    standardizeddisaggregate == "Specimen Sent/HIVStatus" ~ "Specimens Sent",
    standardizeddisaggregate == "Specimen Return/HIVStatus" ~ "Specimens Returned",
    indicator == "TB_PREV" & standardizeddisaggregate == "Total Denominator" ~ "TB_PREV D",
    indicator == "TB_PREV" & standardizeddisaggregate == "Total Numerator" ~ "TB_PREV N"
    # ,
    # 
    # TRUE ~ "OVC_HIVSTAT"
  )
  ) %>% 
  # mutate(count = c(1, 1, 1, 1, 1, 1, 2, 1, 1, 1,1)) %>% 
  # uncount(count) %>% 
  mutate(row_num = row_number(),
         x_group = ifelse(row_num %in% c(10:12), "Specimens Sent", x_group),
         x_order = fct_relevel(x_group, 
                               c("TB_PREV N",
                                 "TB_PREV D",
                                 "TX_TB D Negative", 
                                 "TX_TB Total Denominator", 
                                 "TX_TB D Positive",
                                 "Specimens Sent",
                                 "Specimens Returned",
                                 "TX_TB Total Numerator")
         ),
         bar_color = case_when(
           x_order == "TX_TB Total Denominator" ~ "#1F6F75",
           x_order == "TX_TB D Negative" ~ "#30728b",
           x_order == "TB_PREV D" ~ "#5cb6d6",
           x_order == "TB_PREV N" ~ "#8CE4FE",
           x_order == "TX_TB D Positive" ~ "#0D6C5F",
           x_order == "Specimens Sent" ~ "#459688",
           x_order == "Specimens Returned" ~ "#A1E1D7",
           x_order == "TX_TB Total Numerator" ~ "#C7EDE7"
           
         ))


df_viz %>% 
  #filter(x_group != "not used") %>% 
  ggplot() +
  geom_col(aes(x = x_order, y = value, fill = bar_color)) +
  geom_text(aes(x = x_order, y = value, label = comma(value)))+
  si_style_ygrid() +
  scale_fill_identity() +
  scale_y_continuous(labels = comma) +
  #coord_flip() +
  scale_x_discrete(limits = rev(levels(df_viz$x_order))) +
  labs(x = NULL, y = NULL,
       caption = glue::glue("{metadata$caption}")) 


#alluvial

df_viz %>% 
  