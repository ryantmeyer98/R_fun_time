# install.packages("tidyverse")

library(tidyverse)
library(readxl)
library(purrr)


# #Data acquisition ----
# Get the file path and file list----
# This is the directory the files are in
data_path <- "ecoplates/" 
# This is the directory and the file patter and recurisve brings in all sub directories
files <- dir("ecoplates/",
             pattern = "*.xlsx", recursive = TRUE)

# read in files and get header and file name-----
eco.df <- tibble(filename = files) %>% # create a data frame holding the file names
  mutate(
    file_contents = map(
      filename, # read files into
      ~ read_excel(file.path(data_path, .)))) # reads in all the files with this command
  
eco.df

#unnest -----
# this unnests the list file that is a different way of working with data
eco.df <- unnest(eco.df, cols = c(file_contents))

names(eco.df)

eco.df <- eco.df %>% 
  select(-...14) %>% 
  rename(letter = ...1)


eco_long.df <- eco.df %>% 
  pivot_longer(
    cols=-c(filename, letter),
    names_to = "number",
    values_to = "abs"
  )

eco_long.df <- eco_long.df %>%   
  mutate(
    letter = tolower(letter),
    letter_number = paste(letter, number, sep="_")
         )
  
eco_long.df <- eco_long.df %>% 
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
    
eco_long.df <- eco_long.df %>% 
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
eco_long.df <- eco_long.df %>% group_by(filename, set) %>% mutate(water_corr = abs - abs[well == 'water'] )

eco_long.df <- eco_long.df %>% 
  separate(filename,  c("block", "crop", "depth", "some_number" ), sep="_", remove=FALSE)

eco_long.df <- eco_long.df %>% 
  mutate(group = case_when(
    well ==  "water" ~ "water",
    well ==  "phenylethylamine" ~ "amines",
    well ==  "putrescine" ~ "amines",
    well ==  "glycyl_l_glutamic_acid" ~ "amino_acids",
    well ==  "l_arginine" ~ "amino_acids",
    well ==  "l_asparagine" ~ "amino_acids",
    well ==  "l_phenylalanine" ~ "amino_acids",
    well ==  "l_serine" ~ "amino_acids",
    well ==  "l_threonine" ~ "amino_acids",
    well ==  "a_d_lactose" ~ "carbohydrates",
    well ==  "b_methyl_d_glucoside" ~ "carbohydrates",
    well ==  "d_cellobiose" ~ "carbohydrates",
    well ==  "d_galactonic_acid_g_lactone" ~ "carbohydrates",
    well ==  "d_l_g_glycerol_phosphate" ~ "carbohydrates",
    well ==  "d_mannitol" ~ "carbohydrates",
    well ==  "d_xylose" ~ "carbohydrates",
    well ==  "glucose_1_phosphate" ~ "carbohydrates",
    well ==  "i_erythritol" ~ "carbohydrates",
    well ==  "n_acetyl_d_glucosamine" ~ "carbohydrates",
    well ==  "a_ketobutyric_acid" ~ "carboxylic_acids",
    well ==  "d_galacturonic_acid" ~ "carboxylic_acids",
    well ==  "d_glucosaminic_acid" ~ "carboxylic_acids",
    well ==  "d_malic_acid" ~ "carboxylic_acids",
    well ==  "g_hydroxybutyric_acid" ~ "carboxylic_acids",
    well ==  "itaconic_acid" ~ "carboxylic_acids",
    well ==  "pyruvic_acid_methyl_ester" ~ "carboxylic_acids",
    well ==  "hydroxy_2_benzoic_acid" ~ "phenoplic_compounds",
    well ==  "hydroxy_4_benzoic_acid" ~ "phenoplic_compounds",
    well ==  "a_cyclodextrin" ~ "polymers",
    well ==  "glycogen" ~ "polymers",
    well ==  "tween_40" ~ "polymers",
    well ==  "tween_80" ~ "polymers",
    TRUE ~ "other"
  ))



write_csv(eco_long.df, "emilys_ecoplates.csv")

eco_long.df <- eco_long.df %>% 
  separate(set, c("junk", "set_no"))


eco_long_means.df <- eco_long.df %>% 
  group_by(filename, block, crop, depth, some_number, well) %>% 
  summarize(water_corr_mean = mean(water_corr, na.rm=TRUE),
            water_corr_std = sd(water_corr, na.rm = TRUE))

write_csv(eco_long_means.df, "eco_means_long.csv")

eco_wide_means.df <- eco_long_means.df %>% 
  pivot_wider(
    id_cols = c(filename, block, crop, depth, some_number),
    names_from = "well",
    values_from = "water_corr_mean"
  )

write_csv(eco_wide_means.df, "eco_means_wide.csv")


eco_wide_prep.df <- eco_long.df %>% 
  mutate(well_set = paste(well, set_no, sep="_")) %>% 
  select(-abs, -letter, -number,  -letter_number, - group, -well, -junk, -set_no)

eco_wide.df <- eco_wide_prep.df %>% 
  pivot_wider(
    id_cols = c("filename", "block", "crop", "depth", "some_number"),
    names_from = "well_set",
    values_from = "water_corr"
  )

write_csv(eco_wide.df, "ecowide.csv")

# # install.packages("viridis")
# library(viridis)


eco_long.df %>% 
  filter(group != "water") %>% 
  ggplot(aes(crop, abs, color=group))+
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3,
               position = position_dodge(width = 0.3)) + 
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(width = 0.3))  +
  # scale_colour_viridis_d(option = "magma") +
  facet_wrap(~depth)
  