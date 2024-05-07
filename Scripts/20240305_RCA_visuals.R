# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TB RCA Uganda visits
# REF ID:   6193cd58 
# LICENSE:  MIT
# DATE:     2024-03-05
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

  
  ref_id <- "6193cd58"

# IMPORT ------------------------------------------------------------------
  
data_folder <- "Data"
  
df_tsr <- data_folder %>% 
  return_latest("Uganda_TSR") %>% 
  read_xlsx()

df_mort <- data_folder %>% 
  return_latest("Data review TBHIV Uganda LPHS") %>% 
  read_xlsx()

# MUNGE -------------------------------------------------------------------

#TX_TB cascade viz
df_viz_cascade <- df_mort %>%
  janitor::clean_names() %>%
  select(hiv_status, entry_point_e_g_iss_clinic, final_out_come, documented_cause_of_death) %>% 
  mutate(total_patients  = nrow(df_mort),
         tbhiv_patients = nrow(df_mort %>%
                                 filter(str_detect(hiv_status, "pos"))),
         tbhiv_patients_newpos = nrow(df_mort %>%
                                 filter(str_detect(hiv_status, "New pos"))),
         tbhiv_patients_tbclin = nrow(df_mort %>%
                                 filter(str_detect(hiv_status, "pos") & entry_point_e_g_iss_clinic == "Tb Clinic")),
         tbhiv_patients_hivclin = nrow(df_mort %>%
                                        filter(str_detect(hiv_status, "pos") & entry_point_e_g_iss_clinic == "HIV clinic")),
         tbhiv_deaths = nrow(df_mort %>%
                               filter(str_detect(hiv_status, "pos") & final_out_come == "died")),
         tbhiv_deaths_newpos = nrow(df_mort %>%
                               filter(str_detect(hiv_status, "New pos") & final_out_come == "died")),
         tb_rel_deaths = nrow(df_mort %>%
                               filter(str_detect(hiv_status, "pos") & final_out_come == "died"
                                      & str_detect(documented_cause_of_death, "TB")))) %>% 
  select(-c(1:4)) %>% 
  distinct() %>% 
  pivot_longer(cols = c(1:8), names_to = "indicator", values_to = "num") %>% 
  mutate(group = case_when(str_detect(indicator, "newpos") ~ "New Positive",
                           str_detect(indicator, "tbclin") ~ "TB Clinic Entry Point",
                           str_detect(indicator, "hivclin") ~ "HIV Clinic Entry Point",
                           TRUE ~ "NA")) %>% 
  mutate(indicator = case_when(indicator == "tbhiv_patients_newpos" ~ "tbhiv_patients",
                               indicator ==  "tbhiv_patients_tbclin" ~ "tbhiv_patients",
                               indicator ==  "tbhiv_patients_hivclin" ~ "tbhiv_patients",
                               indicator == "tbhiv_deaths_newpos" ~ "tbhiv_deaths",
                               TRUE ~ indicator))

#Cause of death viz
df_death <- df_mort %>%
  janitor::clean_names() %>%
  select(hiv_status, entry_point_e_g_iss_clinic, final_out_come, documented_cause_of_death) %>% 
  filter(final_out_come == "died",
         str_detect(hiv_status, "pos")) %>% 
  mutate(cod = case_when(str_detect(documented_cause_of_death, "TB") ~ "TB-related disease",
                         str_detect(documented_cause_of_death, "Cancer") ~ "Cancer",
                         str_detect(documented_cause_of_death, "DIB") ~ "Heart disease",
                         str_detect(documented_cause_of_death, "Liver") ~ "Liver disease",
                         TRUE ~ "Unknown/Unspecified")) %>% 
  count(cod)

#TB/HIV deaths - VL result
df_death_vl <- df_mort %>%
  janitor::clean_names() %>%
  select(hiv_status, entry_point_e_g_iss_clinic, final_out_come,documented_cause_of_death,
         cd4_during_tb_illness, vl_result_with_in_last_12_months_6_mon_paed) %>% 
  filter(str_detect(hiv_status, "pos"),
         final_out_come == "died") %>% 
  count(vl_result_with_in_last_12_months_6_mon_paed)

#TB/HIV deaths - CD4 count
df_death_cd4 <- df_mort %>%
  janitor::clean_names() %>%
  select(hiv_status, entry_point_e_g_iss_clinic, final_out_come,documented_cause_of_death,
         cd4_during_tb_illness, vl_result_with_in_last_12_months_6_mon_paed) %>% 
  filter(str_detect(hiv_status, "pos"),
         final_out_come == "died") %>% 
  count(cd4_during_tb_illness)


all_adm <- df_mort %>%
  janitor::clean_names() %>%
  select(hiv_status, entry_point_e_g_iss_clinic, final_out_come, documented_cause_of_death) %>% 
  count(entry_point_e_g_iss_clinic) %>% 
  rename(all_adm = n)

tbhiv_adm <- df_mort %>%
  janitor::clean_names() %>%
  select(hiv_status, entry_point_e_g_iss_clinic, final_out_come, documented_cause_of_death) %>% 
  filter(str_detect(hiv_status, "pos")) %>% 
  count(entry_point_e_g_iss_clinic) %>% 
  rename(tbhiv_adm = n)

df_entry <- all_adm %>% 
  left_join(tbhiv_adm) %>% 
  pivot_longer(cols = c(2,3), names_to = "pop") %>% 
  mutate(fill_color = ifelse(entry_point_e_g_iss_clinic  == "HIV clinic", genoa, golden_sand))

# VIZ ABOUT TX_TB CASCADE-----------------------------------------------------

df_viz_cascade %>% 
  mutate(indicator = case_when(indicator == "total_patients" ~ "Total patients",
                               indicator == "tbhiv_patients" ~ "TB/HIV patients",
                               indicator == "tbhiv_deaths" ~ "TB/HIV deaths",
                               indicator == "tb_rel_deaths" ~ "TB-related deaths")) %>% 
  filter(group %in% c("New Positive", "NA")) %>%
 #filter(!str_detect(group, "Entry Point")) %>% 
  ggplot(aes(x = fct_reorder(indicator, desc(num)), y = num, fill = group)) +
  geom_bar(stat = "identity", width = 0.5) +
  #geom_col(position="fill",width = 0.4) +
  scale_fill_manual(values = c("New Positive" = scooter_light, "NA" = scooter)) +
  geom_text(aes(label = num,
                  hjust = 0.5, vjust = -0.2, family = "Source Sans Pro"),
            size = 14/.pt,color = grey80k) +
  si_style_ygrid() + 
  labs(x = NULL, y = NULL,
       title= "TX_TB Cascade",
       #subtitle = 
       caption = "Source: IP TBHIV Mortality Audit data base") + 
  theme(legend.text = element_blank())

si_save("Graphics/RCA_TXTB_cascade.svg")

# VIZ ABOUT COD ---------------------------------------------------------------
  
df_death %>% 
  mutate(total = sum(n)) %>% 
  mutate(share= n/total) %>% 
  mutate(fill_color = ifelse(cod == "TB-related disease", hw_midnight_blue, scooter_med)) %>% 
  ggplot(aes(fct_reorder(cod, n), n, fill = fill_color)) +
  geom_col() +
  coord_flip()+ 
  scale_fill_identity() +
  scale_y_continuous(limits = c(0,12)) + 
  geom_text(aes(label = glue::glue("{n} ({percent(share)})"),
           hjust = -0.1,
           # vjust = -1,
            family = "Source Sans Pro"),  size = 12/.pt) +
  # geom_text(aes(label = percent(share)),
  #           size = 10/.pt,
  #           hjust = 0.5,
  #           vjust = 2,
  #           family = "Source Sans Pro") +
  si_style_nolines() +
  labs(x = NULL,
       y = NULL,
       title = "Stated cause of death for TB/HIV patients who died (n=19)" %>% toupper(),
       subtitle = "Recording and reporting couldnot distinguish between immediate/underlying/contributing cause of death",
       caption = "Source: IP TBHIV Mortality Audit data base")

si_save("Images/02_COD.png")

#TSR Uganda viz
df_tsr %>% 
  mutate(group = 1) %>%
  mutate(fill_color = ifelse(TSR > goal, hw_orchid_bloom, hw_midnight_blue)) %>% 
  ggplot(aes(x = fct_reorder(timeframe, order), y = TSR,
             group = group,  color = fill_color)) +
  geom_line() +
  geom_point(size =3) +
  geom_hline(yintercept = 0.9, color = usaid_lightgrey,
             linetype = "dashed") +
  geom_text(aes(label = percent(TSR)), size = 12/.pt,
            hjust = 0.5,
            vjust = -1,
            family = "Source Sans Pro") +
  geom_text(aes(x = "Oct/Dec 2021", y = 0.9, label = "National target of 90%"),
            size = 12/.pt,
            hjust = -0.2,
            vjust = -1,
            color =trolley_grey,
            family = "Source Sans Pro") +
  geom_text(aes(x = "Jan/March 2023", y = 0.85, label = "Start of the RCA pilot"),
            size = 12/.pt,
            #hjust = -0.1,
            vjust = -1.5,
            color =trolley_grey,
            family = "Source Sans Pro") +
  si_style_nolines() +
  scale_color_identity() +
  scale_y_continuous(labels = percent, limits = c(0.8 , 0.95)) + 
  labs(x = NULL,
       y = NULL,
       title = "TB Treatment Success Rate (TSR) in Ankole improved above 90% after the start of the RCA pilot" %>% toupper(),
       subtitle = "Similar trends observed 2 other regions (Kampala and Acholi) that implemented the RCA pilot",
       captionm = "Source: Ankole Regional Data
       [For internal use only]")

si_save("Graphics/01_TSR.svg")
si_save("Images/01_TSR.png")

# VIZ ABOUT CD4 AND VL FOR PATIENTS WHO DIED ----------------------------------


cd4 <- df_death_cd4 %>% 
  mutate(total = sum(n),
         share = n/total) %>% 
  mutate(fill_color = case_when(cd4_during_tb_illness == "more than 200" ~ golden_sand,
                                cd4_during_tb_illness == "less than 200" ~ golden_sand_light,
                                TRUE ~ trolley_grey_light)) %>% 
  mutate(cd4_during_tb_illness = case_when(cd4_during_tb_illness == "more than 200" ~ ">200 cells",
                                           cd4_during_tb_illness == "less than 200" ~ "<200 cells (AHD)",
                                           cd4_during_tb_illness == "not done" ~ "CD4 not taken")) %>% 
  mutate(cd4_during_tb_illness = fct_relevel(cd4_during_tb_illness, c("CD4 not taken",
                                              "<200 cells (AHD)",">200 cells"))) %>% 
  ggplot(aes(cd4_during_tb_illness, n, fill = fill_color)) +
  geom_col() +
  coord_flip()+ 
  scale_fill_identity() +
  scale_y_continuous(limits = c(0,10), breaks = ) + 
  geom_text(aes(label = glue::glue("{n} ({percent(share)})"),
                hjust = -0.1,
                # vjust = -1,
                family = "Source Sans Pro"),  size = 12/.pt) + 
  si_style_yline() +
  labs(x = NULL, y= NULL,
       subtitle = "CD4 during treatment") +
  theme(axis.text.x = element_blank())


vl <- df_death_vl %>%
  rename(vl_result = vl_result_with_in_last_12_months_6_mon_paed) %>% 
  mutate(vl_result = ifelse(is.na(vl_result), "due but not done", vl_result)) %>% 
  group_by(vl_result) %>% 
  summarise(across(starts_with("n"), sum, na.rm = T), .groups = "drop") %>% 
  mutate(total = sum(n),
         share = n/total) %>% 
  mutate(fill_color = case_when(vl_result == "suppressed (<1000)" ~ scooter,
                                vl_result == "non suppressed >1000" ~ scooter_med,
                                TRUE ~ trolley_grey_light)) %>% 
  mutate(vl_result = case_when(vl_result == "suppressed (<1000)" ~ "Suppressed (<1000 copies/ml)",
                                           vl_result == "non suppressed >1000" ~ "Unsuppressed (>1000 copies/ml)",
                                           TRUE  ~ "Unknown/VL not done")) %>% 
  mutate(vl_result = fct_relevel(vl_result, c("Unknown/VL not done",
                                              "Unsuppressed (>1000 copies/ml)","Suppressed (<1000 copies/ml)"))) %>% 
  ggplot(aes(vl_result, n, fill = fill_color)) +
  geom_col() +
  coord_flip()+ 
  scale_fill_identity() +
  scale_y_continuous(limits = c(0,16), breaks = ) + 
  geom_text(aes(label = glue::glue("{n} ({percent(share)})"),
                hjust = -0.1,
                # vjust = -1,
                family = "Source Sans Pro"),  size = 12/.pt) + 
  si_style_yline() +
  labs(x = NULL, y= NULL,
       subtitle = "Viral suppression during treatment",
       caption = "Source: IP TB/HIV Mortality Audit database") + 
  theme(axis.text.x = element_blank())


cd4 + vl +
  plot_annotation(title = "Immunological and virological status of TB/HIV patients who died during treatment (N=19)" %>% toupper()) &
  theme(plot.title = element_text(family = "Source Sans Pro",
                                  size = 14,
                                  face = "bold",
                                  color =  "#202020",
                                  hjust = 0,
                                  vjust = -1))


si_save("Graphics/remake_cd4_vl_tbhiv_deaths.svg")

# VIZ ABOUT ENTRY POINTS-----------------------------------------------------

df_entry %>% 
  group_by(pop) %>% 
  mutate(total = sum(value),
         share = value/total) %>% 
    ungroup() %>% 
  #filter(pop == "all_adm") %>% 
  ggplot(aes(x="", y=value, fill=fill_color)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
   facet_wrap(~pop) +
  scale_fill_identity() +
  geom_text(aes(y = value, label = value), color = "white") +
  si_style_nolines() +
  theme(axis.text.x = element_blank())


df_entry %>% 
  filter(pop == "tbhiv_adm") %>% 
  ggplot(aes(x="", y=value, fill=fill_color)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  #  facet_wrap(~pop) +
  scale_fill_identity() +
  si_style_nolines() +
  theme(axis.text.y = element_blank())

