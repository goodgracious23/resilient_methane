library(tidyverse)
library(ggpubr)
library(scales)
library(mgcv)
# library(lme4)
# library(sjPlot)
library(ggeffects)
library(performance)
library(broom)
# library(MuMIn)
library("Hmisc")
library(corrplot)


# ===================================
# PREPARE THE DATA!
#====================================
### Methane concentration profiles
all_years_ch4 = read_csv("all years GHG data.csv") %>% 
  mutate(year = factor(year, levels = c("2018" ,"2019", "2024")),
         meanCH4 = case_when(meanCH4 <= 0 ~ 0.01,
                             meanCH4 >= 0 ~ meanCH4)) %>%
  filter(!(year == 2024 & lake == "Paul" & doy == 159), 
         !(year == 2019 & doy < 148), # removed first 2 weeks to align with start of other years
         !(year == 2019 &  doy == 241), # not a complete profile taken in either lake
         !(depth == 15 & lake == "Peter"), #depth not sampled in 2024, remove from data set
         !(depth == 10 & lake == "Paul")) %>%  #depth not sampled in 2024, remove from data set
  mutate(year = case_when(year == "2018" ~ "Ambient (2018)",
                          year == "2019" ~ "Nutrients (2019)",
                          year == "2024" ~ "Dye + Nutrients (2024)"),
         lake = case_when(lake == "Peter" ~ "Manipulated Lake",
                          lake == "Paul" ~ "Reference Lake"),
         year = factor(year, levels = c("Ambient (2018)",
                                        "Nutrients (2019)",
                                        "Dye + Nutrients (2024)")))

# Create a data frame of methane sampling depths
sampling_depths = all_years_ch4 %>%
  group_by(lake, year, depth) %>%
  dplyr::summarize(sampling_depth = mean(depth, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = factor(year, levels = c("Ambient (2018)",
                                        "Nutrients (2019)",
                                        "Dye + Nutrients (2024)"))) %>% select(-depth)

### Anoxic Depth, Thermocline, 1% Light
clines = read_csv("Anoxic_Thermo_Light_Peter_Paul.csv") %>% 
  select(-`...1`) %>% 
  mutate(lake = case_when(lake == "Reference" ~ "Reference Lake",
                          lake == "Manipulated"  ~ "Manipulated Lake")) %>%
  mutate(year = factor(year, levels = c("Ambient (2018)",
                                        "Nutrients (2019)",
                                        "Dye + Nutrients (2024)")))

#=========================================
# INTERPOLATE PROFILES AND CALC AREAL MASS
#=========================================
# interpolate methane data to half meters
new_depths <- seq(0, 16, by = 0.5)

interpolate_methane <- function(df) {
  interpolated <- approx(df$depth, df$meanCH4, xout = new_depths, rule = 2)
  return(data.frame(depth = interpolated$x, 
                    meanCH4 = interpolated$y))}

interpolated_methane <- all_years_ch4 %>%
  group_by(lake, date) %>%
  nest() %>%
  mutate(interpolated = map(data, interpolate_methane)) %>%
  unnest(interpolated) %>%
  select(-data)

# Interpolate the volumes to have those at every half meter
vol = read.csv("R_L_Vol.csv")

interpolated_volume <- function(df) {
  interpolated <- approx(df$depth, df$volume, xout = new_depths, rule = 2)
  return(data.frame(depth = interpolated$x, 
                    volume = interpolated$y))}

interpolated_volumes <- vol %>%
  group_by(lake) %>%
  nest() %>%
  mutate(interpolated = map(data, interpolated_volume)) %>%
  unnest(interpolated) %>%
  select(-data) %>%
  mutate(lake = case_when(lake == "L" ~ "Reference Lake",
                          lake == "R" ~ "Manipulated Lake"))

# Combine the interpolated CH4 and volume data frames
combined_vol_methane <-
  left_join(interpolated_methane, interpolated_volumes, 
            by = c("lake", "depth")) %>%
  mutate(methane_mass = ((meanCH4 * 1000) * volume)/1000000, #moles of CH4
         date = mdy(date),
         doy = yday(date),
         year = year(date)) %>% 
  filter(!(lake == "Reference Lake" & depth >= 12)) %>% # get rid of the bonus depths interpolated in Paul
  mutate(year = case_when(year == "2018" ~ "Ambient (2018)",
                          year == "2019" ~ "Nutrients (2019)",
                          year == "2024" ~ "Dye + Nutrients (2024)"))


# Sum up storage and plot over time (sensu Bastviken et al. 2004 GBC)
sum_ch4mass = combined_vol_methane %>%
  group_by(lake, date) %>%
  dplyr::summarize(total_ch4mass = sum(methane_mass)) %>%
  ungroup() %>%
  mutate(year = year(date),
         doy = yday(date),
         # convert to areal estimate, mg C per m2, dividing my lake surface area
         areal_ch4mass = case_when(lake == "Reference Lake" ~ (total_ch4mass/17441)*12.01*1000, 
                                   lake == "Manipulated Lake" ~ (total_ch4mass/26523)*12.01*1000),
         lake = factor(lake, levels = c("Reference Lake", "Manipulated Lake")),
         year = case_when(year == "2018" ~ "Ambient (2018)",
                          year == "2019" ~ "Nutrients (2019)",
                          year == "2024" ~ "Dye + Nutrients (2024)"),
         year = factor(year, levels = c("Ambient (2018)", "Nutrients (2019)", "Dye + Nutrients (2024)")),
         doy = case_when(lake == "Manipulated Lake" & year == "Dye + Nutrients (2024)" & doy == 170 ~ 169,
                         lake == "Manipulated Lake" & year == "Dye + Nutrients (2024)" & doy == 227 ~ 225,
                         TRUE ~ doy)) 

scaled_sum_ch4mass <- sum_ch4mass %>%
  group_by(lake) %>%
  mutate(areal_ch4mass_scaled = scale(areal_ch4mass, center = TRUE, scale = TRUE)) %>%
  ungroup()

## Let's align the anoxic depth dates (clines dataframe) with the methane profile data
## The Cascade project does routine sampling (light, DO, temp) on Monday = Peter, Tuesday = Paul
## Methane profiles were not always taken on the same days in the lakes

cline_clean_wide = clines %>% 
  pivot_wider(id_cols = doy:year, names_from = depth_type, values_from = value) %>% 
  rename(Light = `1% Light`) %>% 
  # Manually validated and added anoxic_depth for 2024 dates missing a value
  add_row(lake = "Manipulated Lake", year = "Dye + Nutrients (2024)", doy = 159, 
          Anoxia = 7, Light = 4.6, Thermocline = 1.5, BGA = 127.7) %>%
  mutate(doy = case_when(lake == "Reference Lake" & year == "Dye + Nutrients (2024)"& doy > 170 ~ doy -1, 
                         TRUE ~ doy),
         doy = case_when(lake == "Reference Lake" & year == "Dye + Nutrients (2024)" & doy == 183 ~ 186,
                         TRUE ~ doy)) %>%
  # Aligning cline and profile dates for 2019 - manually validated shift in dates based on anoxia depth pattern
  mutate(doy = case_when(lake == "Reference Lake" & year == "Nutrients (2019)" ~ doy + 2, TRUE ~ doy),
         doy = case_when(lake == "Reference Lake" & year == "Nutrients (2019)" & doy == 157 ~ doy - 1,
                         lake == "Reference Lake" & year == "Nutrients (2019)" & doy == 185 ~ doy - 1,
                         lake == "Reference Lake" & year == "Nutrients (2019)" & doy == 150 ~ doy - 1,
                         TRUE ~ doy)) %>%
  mutate(doy = case_when(lake == "Manipulated Lake" & year == "Nutrients (2019)" ~ doy + 3, TRUE ~ doy),
         doy = case_when(lake == "Manipulated Lake" & year == "Nutrients (2019)" & doy == 157 ~ doy - 1,
                         lake == "Manipulated Lake" & year == "Nutrients (2019)" & doy == 185 ~ doy - 1,
                         lake == "Manipulated Lake" & year == "Nutrients (2019)" & doy == 150 ~ doy - 1,
                         TRUE ~ doy)) %>%
  # Aligning cline and profile dates for 2019 - manually validated shift in dates based on anoxia depth pattern
  mutate(doy = case_when(lake == "Reference Lake" & year == "Ambient (2018)" ~ doy + 1, TRUE ~ doy)) %>%
  mutate(doy = case_when(lake == "Manipulated Lake" & year == "Ambient (2018)" ~ doy + 2, TRUE ~ doy)) %>%
  mutate(year = factor(year, levels = c("Ambient (2018)", "Nutrients (2019)", "Dye + Nutrients (2024)")),
         lake = factor(lake, levels = c("Reference Lake", "Manipulated Lake")))
# Trim interpolated methane mass profiles by cline depth
combined_mass = left_join(combined_vol_methane, cline_clean_wide, by = c("lake", "year", "doy"))
combined_storage_cline = left_join(sum_ch4mass, cline_clean_wide, 
                                   by = c("lake", "year", "doy")) 

peter_storage_cline = combined_storage_cline %>% 
  filter(lake == "Manipulated Lake") %>%
  mutate(arealmass_scaled = scale(log10(areal_ch4mass)),
         light_scaled = scale(Light),
         thermo_scaled = scale(Thermocline),
         oxycline_scaled = scale(Anoxia))

paul_storage_cline = combined_storage_cline %>% filter(lake == "Reference Lake") %>%
  mutate(arealmass_scaled = scale(log10(areal_ch4mass)),
         light_scaled = scale(Light),
         thermo_scaled = scale(Thermocline),
         oxycline_scaled = scale(Anoxia))


#========================================
# COMBINE CLINE TO INVESTIGATE BY DEPTH
#========================================
all_years_ch4_clines = left_join(all_years_ch4, cline_clean_wide, by = c("lake", "year", "doy")) %>%
  mutate(lake = factor(lake, levels = c("Reference Lake", "Manipulated Lake")),
         log10_do = log10(do_mgl),
         log10_light = log10(light + 0.0001),
         log10_meanCH4 = log10(meanCH4),
         log10_bga = log10(surface_bga),
         fdepth = factor(depth),
         year = factor(year))

# PETER LAKE STATISTICAL MODELS
peter_meanCH4 = all_years_ch4_clines %>% 
  filter(lake == "Manipulated Lake",
         depth <= 6) %>%
  mutate(cDOY = case_when(year == "Ambient (2018)" ~ doy - 164,
                          year == "Nutrients (2019)" ~ doy - 149,
                          year == "Dye + Nutrients (2024)" ~ doy - 159),
         log10_do = log10(do_mgl),
         log10_light = log10((light + 0.0001)*100),
         log10_meanCH4 = log10(meanCH4),
         log10_bga = log10(surface_bga),
         fdepth = factor(depth),
         year = factor(year))


# PAUL LAKE STATISTICAL MODELS
paul_meanCH4 = all_years_ch4_clines %>% 
  filter(lake == "Reference Lake",
         depth <= 8) %>%
  mutate(cDOY = case_when(year == "Ambient (2018)" ~ doy - 164,
                          year == "Nutrients (2019)" ~ doy - 149,
                          year == "Dye + Nutrients (2024)" ~ doy - 163),
         log10_do = log10(do_mgl),
         log10_light = log10((light + 0.0001)*100),
         log10_meanCH4 = log10(meanCH4),
         log10_bga = log10(surface_bga),
         fdepth = factor(depth),
         year = factor(year))

