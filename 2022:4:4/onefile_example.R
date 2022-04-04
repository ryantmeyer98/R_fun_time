library(tidyverse)
library(readxl)
library(purrr)

# 
e.df <- read_excel("2022:4:4/block2_CR_0-2_10172021.xlsx") %>% 
  select(-14) %>% 
  rename(letter = 1)




el.df <- e.df %>% 
  pivot_longer(
    cols=-c(letter),
    names_to = "number",
    values_to = "abs"
  )

el.df <- el.df %>%   
  mutate(
    letter = tolower(letter),
    letter_number = paste(letter, number, sep="_")
         )
  
el.df <- el.df %>% 
  mutate(well = case_when(
    letter_number == "a_1" ~ "water",
    letter_number == "a_2" ~ "b_methyl_d_glucoside",
    letter_number == "a_3" ~ "d_galactonic_acid_g_lactone",
    letter_number == "a_4" ~ "l_arginine",
    letter_number == "a_5" ~ "water",
    letter_number == "a_6" ~ "b_methyl_d_glucoside",
    letter_number == "a_7" ~ "d_galactonic_acid_g_lactone",
    letter_number == "a_8" ~ "l_arginine",
    letter_number == "a_9" ~ "water",
    letter_number == "a_10" ~ "b_methyl_d_glucoside",
    letter_number == "a_11" ~ "d_galactonic_acid_g_lactone",
    letter_number == "a_12" ~ "l_arginine",
    letter_number == "b_1" ~ "pyruvic_acid_methyl_ester",
    letter_number == "b_2" ~ "d_xylose",
    letter_number == "b_3" ~ "d_galacturonic_acid",
    letter_number == "b_4" ~ "l_asparagine",
    letter_number == "b_5" ~ "pyruvic_acid_methyl_ester",
    letter_number == "b_6" ~ "d_xylose",
    letter_number == "b_7" ~ "d_galacturonic_acid",
    letter_number == "b_8" ~ "l_asparagine",
    letter_number == "b_9" ~ "pyruvic_acid_methyl_ester",
    letter_number == "b_10" ~ "d_xylose",
    letter_number == "b_11" ~ "d_galacturonic_acid",
    letter_number == "b_12" ~ "l_asparagine",
    letter_number == "c_1" ~ "tween_40",
    letter_number == "c_2" ~ "i_erythritol",
    letter_number == "c_3" ~ "hydroxy_2_benzoic_acid",
    letter_number == "c_4" ~ "l_phenylalanine",
    letter_number == "c_5" ~ "tween_40",
    letter_number == "c_6" ~ "i_erythritol",
    letter_number == "c_7" ~ "hydroxy_2_benzoic_acid",
    letter_number == "c_8" ~ "l_phenylalanine",
    letter_number == "c_9" ~ "tween_40",
    letter_number == "c_10" ~ "i_erythritol",
    letter_number == "c_11" ~ "hydroxy_2_benzoic_acid",
    letter_number == "c_12" ~ "l_phenylalanine",
    letter_number == "d_1" ~ "tween_80",
    letter_number == "d_2" ~ "d_mannitol",
    letter_number == "d_3" ~ "hydroxy_4_benzoic_acid",
    letter_number == "d_4" ~ "l_serine",
    letter_number == "d_5" ~ "tween_80",
    letter_number == "d_6" ~ "d_mannitol",
    letter_number == "d_7" ~ "hydroxy_4_benzoic_acid",
    letter_number == "d_8" ~ "l_serine",
    letter_number == "d_9" ~ "tween_80",
    letter_number == "d_10" ~ "d_mannitol",
    letter_number == "d_11" ~ "hydroxy_4_benzoic_acid",
    letter_number == "d_12" ~ "l_serine",
    letter_number == "e_1" ~ "a_cyclodextrin",
    letter_number == "e_2" ~ "n_acetyl_d_glucosamine",
    letter_number == "e_3" ~ "g_hydroxybutyric_acid",
    letter_number == "e_4" ~ "l_threonine",
    letter_number == "e_5" ~ "a_cyclodextrin",
    letter_number == "e_6" ~ "n_acetyl_d_glucosamine",
    letter_number == "e_7" ~ "g_hydroxybutyric_acid",
    letter_number == "e_8" ~ "l_threonine",
    letter_number == "e_9" ~ "a_cyclodextrin",
    letter_number == "e_10" ~ "n_acetyl_d_glucosamine",
    letter_number == "e_11" ~ "g_hydroxybutyric_acid",
    letter_number == "e_12" ~ "l_threonine",
    letter_number == "f_1" ~ "glycogen",
    letter_number == "f_2" ~ "d_glucosaminic_acid",
    letter_number == "f_3" ~ "itaconic_acid",
    letter_number == "f_4" ~ "glycyl_l_glutamic_acid",
    letter_number == "f_5" ~ "glycogen",
    letter_number == "f_6" ~ "d_glucosaminic_acid",
    letter_number == "f_7" ~ "itaconic_acid",
    letter_number == "f_8" ~ "glycyl_l_glutamic_acid",
    letter_number == "f_9" ~ "glycogen",
    letter_number == "f_10" ~ "d_glucosaminic_acid",
    letter_number == "f_11" ~ "itaconic_acid",
    letter_number == "f_12" ~ "glycyl_l_glutamic_acid",
    letter_number == "g_1" ~ "d_cellobiose",
    letter_number == "g_2" ~ "glucose_1_phosphate",
    letter_number == "g_3" ~ "a_ketobutyric_acid",
    letter_number == "g_4" ~ "phenylethylamine",
    letter_number == "g_5" ~ "d_cellobiose",
    letter_number == "g_6" ~ "glucose_1_phosphate",
    letter_number == "g_7" ~ "a_ketobutyric_acid",
    letter_number == "g_8" ~ "phenylethylamine",
    letter_number == "g_9" ~ "d_cellobiose",
    letter_number == "g_10" ~ "glucose_1_phosphate",
    letter_number == "g_11" ~ "a_ketobutyric_acid",
    letter_number == "g_12" ~ "phenylethylamine",
    letter_number == "h_1" ~ "a_d_lactose",
    letter_number == "h_2" ~ "d_l_g_glycerol_phosphate",
    letter_number == "h_3" ~ "d_malic_acid",
    letter_number == "h_4" ~ "putrescine",
    letter_number == "h_5" ~ "a_d_lactose",
    letter_number == "h_6" ~ "d_l_g_glycerol_phosphate",
    letter_number == "h_7" ~ "d_malic_acid",
    letter_number == "h_8" ~ "putrescine",
    letter_number == "h_9" ~ "a_d_lactose",
    letter_number == "h_10" ~ "d_l_g_glycerol_phosphate",
    letter_number == "h_11" ~ "d_malic_acid",
    letter_number == "h_12" ~ "putrescine",
    TRUE~"other"))
    
el.df <- el.df %>% 
    mutate(set = case_when(
      number == 1 ~ "set_1",
      number == 2 ~ "set_1",
      number == 3 ~ "set_1",
      number == 4 ~ "set_1",
      number == 5 ~ "set_2",
      number == 6 ~ "set_2",
      number == 7 ~ "set_2",
      number == 8 ~ "set_2",
      number == 9 ~ "set_3",
      number == 10 ~ "set_3",
      number == 11 ~ "set_3",
      number == 12 ~ "set_3"))

# do water correction---
# https://stackoverflow.com/questions/67139356/how-to-subtract-value-of-one-group-from-other-groups-in-r
el.df <- el.df %>% group_by(set) %>% mutate(water_corr = abs - abs[well == 'water'] )
 

