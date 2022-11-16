# Source

library(gtsummary)

## Outcome psychological support

df %>% 
  select(wave,
         ps_need_rec,
         ps_inperson,
         ps_inperson_rec,
         ps_online,
         ps_online_rec) %>% 
  group_by(wave) %>% 
  skimr::skim()

# Agrupo kis outcomes bajo las variables "_rec" 

df %>% 
  mutate(
    ps_inperson_rec = case_when(wave == "1" ~ ps_inperson,
                                wave != "1" ~ ps_inperson_rec),
    ps_online_rec = case_when(wave == "1" ~ ps_online,
                              wave != "1" ~ ps_online_rec),
    ps_need_rec = case_when(wave == "1" ~ ps_need,
                            wave != "1" ~ ps_need_rec)
  ) %>% 
  select(wave,
         ps_need,
         ps_inperson,
         ps_inperson_rec,
         ps_online,
         ps_online_rec) %>% 
  group_by(wave) %>% 
  skimr::skim()

df <- 
  df %>% 
  mutate(
    ps_inperson_rec = case_when(wave == "1" ~ ps_inperson,
                                wave != "1" ~ ps_inperson_rec),
    ps_online_rec = case_when(wave == "1" ~ ps_online,
                              wave != "1" ~ ps_online_rec),
    ps_need_rec = case_when(wave == "1" ~ ps_need,
                            wave != "1" ~ ps_need_rec)
  )

df %>% 
  mutate(
    ps_use_rec = if_else(ps_inperson_rec == "1" | ps_online_rec == "1",
                         "1",
                         "0") %>% as_factor()
  ) %>% 
  select(starts_with("ps"),
         wave) %>%
  group_by(wave) %>% 
  skimr::skim()

df <- 
  df %>% 
  mutate(
    ps_use_rec = if_else(ps_inperson_rec == "1" | ps_online_rec == "1",
                         "1",
                         "0") %>% as_factor()
  )

## Filter dataset (3 waves)

df %>% 
  group_by(Identificador) %>% 
  count() %>%
  filter(n == 3) %>% 
  select(Identificador)-> full_respondents_ids

df_full <- 
  df %>% 
  filter(Identificador %in% full_respondents_ids$Identificador)

# I need to copy information from waves 1 and 2 to wave 3

df_full <- 
  df_full %>% 
  group_by(Identificador) %>% 
  mutate(SD03_base = SD03,
         edad_base = edad,
         jobtype_base = jobtype,
         MP18_base = MP18,
         MP19_base = MP19,
         MP21_base = MP21,
         MP23_base = MP23,
         MP25c_base = MP25c,
         EP32_base = EP32,
         RA50_base = RA50,
         BRSt_base = BRSt,
         PHQt_base = PHQt,
         GHQs_base = GHQs,
         SP65_base = SP65,
         across(
           .cols = ends_with("_base"),
           .fns = ~ .[wave=1]
         )
         ) %>% 
  ungroup()

## Tables

## Main stratum: requiring

df_full %>% 
  filter(wave == "1",
         !is.na(ps_need_rec)) %>% 
  select(edad,
         SD03,
         jobtype,
         SD04,
         PHQt,
         GHQs,
         ps_need_rec,
         ps_use_rec) %>% 
  mutate(ps_need_rec = fct_recode(ps_need_rec,
                                  "Not required" = "0",
                                  "Required" = "1"),
         ps_use_rec = fct_recode(ps_use_rec,
                                 "Not received" = "0",
                                 "Received" = "1")) %>% 
  tbl_strata(strata = ps_need_rec,
             .tbl_fun =
               ~ .x %>% 
               tbl_summary(by = ps_use_rec,
                           statistic = all_continuous() ~ "{median} ({min}, {max})",
                           missing_text = "Missing",
               ) %>% 
               add_overall() %>% 
               modify_caption("**Wave 1**")
  )

df_full %>% 
  filter(wave == "2",
         !is.na(ps_need_rec)) %>% 
  select(edad,
         SD03,
         jobtype,
         SD04,
         PHQt,
         GHQs,
         ps_need_rec,
         ps_use_rec) %>% 
  mutate(ps_need_rec = fct_recode(ps_need_rec,
                                  "Not required" = "0",
                                  "Required" = "1"),
         ps_use_rec = fct_recode(ps_use_rec,
                                 "Not received" = "0",
                                 "Received" = "1")) %>% 
  tbl_strata(strata = ps_need_rec,
             .tbl_fun =
               ~ .x %>% 
               tbl_summary(by = ps_use_rec,
                           statistic = all_continuous() ~ "{median} ({min}, {max})",
                           missing_text = "Missing",
               ) %>% 
               add_overall() %>% 
               modify_caption("**Wave 2**")
  )

df_full %>% 
  filter(wave == "3",
         !is.na(ps_need_rec)) %>% 
  select(edad,
         SD03,
         jobtype,
         SD04,
         PHQt,
         GHQs,
         ps_need_rec,
         ps_use_rec) %>% 
  mutate(ps_need_rec = fct_recode(ps_need_rec,
                                  "Not required" = "0",
                                  "Required" = "1"),
         ps_use_rec = fct_recode(ps_use_rec,
                                 "Not received" = "0",
                                 "Received" = "1")) %>% 
  tbl_strata(strata = ps_need_rec,
             .tbl_fun =
               ~ .x %>% 
               tbl_summary(by = ps_use_rec,
                           statistic = all_continuous() ~ "{median} ({min}, {max})",
                           missing_text = "Missing",
               ) %>% 
               add_overall() %>% 
               modify_caption("**Wave 3**")
  )

## Main stratum: receiving

df_full %>% 
  filter(wave == "1",
         !is.na(ps_use_rec)) %>% 
  select(edad,
         SD03,
         jobtype,
         SD04,
         PHQt,
         GHQs,
         ps_need_rec,
         ps_use_rec) %>% 
  mutate(ps_need_rec = fct_recode(ps_need_rec,
                                  "Not required" = "0",
                                  "Required" = "1"),
         ps_use_rec = fct_recode(ps_use_rec,
                                 "Not received" = "0",
                                 "Received" = "1")) %>% 
  tbl_strata(strata = ps_use_rec,
             .tbl_fun =
               ~ .x %>% 
               tbl_summary(by = ps_need_rec,
                           statistic = all_continuous() ~ "{median} ({min}, {max})",
                           missing_text = "Missing",
               ) %>% 
               add_overall() %>% 
               modify_caption("**Wave 1**")
  )

df_full %>% 
  filter(wave == "2",
         !is.na(ps_use_rec)) %>% 
  select(edad,
         SD03,
         jobtype,
         SD04,
         PHQt,
         GHQs,
         ps_need_rec,
         ps_use_rec) %>% 
  mutate(ps_need_rec = fct_recode(ps_need_rec,
                                  "Not required" = "0",
                                  "Required" = "1"),
         ps_use_rec = fct_recode(ps_use_rec,
                                 "Not received" = "0",
                                 "Received" = "1")) %>% 
  tbl_strata(strata = ps_use_rec,
             .tbl_fun =
               ~ .x %>% 
               tbl_summary(by = ps_need_rec,
                           statistic = all_continuous() ~ "{median} ({min}, {max})",
                           missing_text = "Missing",
               ) %>% 
               add_overall() %>% 
               modify_caption("**Wave 2**")
  )

df_full %>% 
  filter(wave == "3",
         !is.na(ps_use_rec)) %>% 
  select(edad,
         SD03,
         jobtype,
         SD04,
         PHQt,
         GHQs,
         ps_need_rec,
         ps_use_rec) %>% 
  mutate(ps_need_rec = fct_recode(ps_need_rec,
                                  "Not required" = "0",
                                  "Required" = "1"),
         ps_use_rec = fct_recode(ps_use_rec,
                                 "Not received" = "0",
                                 "Received" = "1")) %>% 
  tbl_strata(strata = ps_use_rec,
             .tbl_fun =
               ~ .x %>% 
               tbl_summary(by = ps_need_rec,
                           statistic = all_continuous() ~ "{median} ({min}, {max})",
                           missing_text = "Missing",
               ) %>% 
               add_overall() %>% 
               modify_caption("**Wave 3**")
  )

## Reduced table

df_full <- 
  df_full %>% 
  mutate(
    PHQcut_base = if_else(PHQt_base < 10,
                          "No",
                          "Yes"),
    GHQcut_base = if_else(GHQs_base < 3,
                          "No",
                          "Yes"),
    MP18_base = fct_recode(MP18_base,
                           NULL = "2"),
    MP19_base = fct_recode(MP19_base,
                           "Insufficient" = "0",
                           "Insufficient" = "1",
                           "Insufficient" = "2",
                           "Sufficient" = "3",
                           NULL = "4"),
    MP21_base = fct_recode(MP21_base,
                           "Not or slightly worried" = "0",
                           "Not or slightly worried" = "1",
                           "Moderately or extremely worried" = "2",
                           "Moderately or extremely worried" = "3"),
    MP23_base = fct_recode(MP23_base,
                           "Not or slightly worried" = "0",
                           "Not or slightly worried" = "1",
                           "Moderately or extremely worried" = "2",
                           "Moderately or extremely worried" = "3"),
    MP25c_base = fct_recode(MP25c_base,
                           "No" = "1",
                           "No" = "2",
                           "Yes" = "3",
                           "Yes" = "4"),
    RA50_base = fct_recode(RA50_base,
                           "No" = "1",
                           "No" = "2",
                           "Yes" = "3",
                           "Yes" = "4")
    
    
  ) # TODO: Transform remaining variables



df_full %>% 
  filter(!is.na(ps_use_rec)) %>% 
  select(wave,
         ps_use_rec,
         c(ends_with("_base"))
         ) %>% 
  mutate(ps_use_rec = fct_recode(ps_use_rec,
                                 "Not received" = "0",
                                 "Received" = "1")) %>% 
  tbl_strata(strata = wave,
             .tbl_fun =
               ~ .x %>% 
               tbl_summary(by = ps_use_rec,
                           statistic = all_continuous() ~ "{mean} ({sd})",
                           missing_text = "Missing",
               ) %>% 
               add_overall()
             ) %>%  
  as_flex_table() %>% 
  flextable::save_as_html(path = "20221011_t1.html")

# TODO: include labels
  

df %>% 
  group_by(wave) %>% 
  select(SP65)

table(df$wave, df$SP65)


#1 PREPARAR DATASET
#
#EXPORTO Y SE LO PASO A BLANCA
#
#2 HACER MODELOS
#
#REPLICAR EN SPSS
