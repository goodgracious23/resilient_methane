
# Run 'resilientmethane_munging.R' to set up the data frames for these figures

#=======================================
# FIGURE 1  - SHOW ME THE METHANE DATA
#=======================================
# Profiles - Methane
ggplot(all_years_ch4, 
       aes(x = meanCH4, y = depth, color = doy)) +
  scale_color_gradient(low = "#023047", high = "#d9ed92", 
                       name = "Day of Year") +
  geom_point(alpha = 0.75) + 
  geom_path(aes(group = doy), linewidth = 0.75, alpha = 0.75) +
  facet_grid(vars(lake), vars(year)) +
  theme_bw() + 
  scale_x_continuous(transform = 'log10', labels = label_number()) +
  labs(y = "Depth (m)", 
       x = bquote("Methane Concentration (umol"~L^-1~")")) +
  ylim(16,0) 


#==============================
# FIGURE 2 - PHYSICAL VARIABLES
#==============================
ggplot(clines %>% filter(!(year == "Nutrients (2019)" & doy < 148),
                         !(year == "Nutrients (2019)" & doy > 237),
                         !depth_type == "BGA"),
       aes(x = doy, y = value, color = depth_type)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_line(linewidth = 1, alpha = 0.5) +
  # scale_color_manual(values = c("#FFB000", "#D21572", "#370381"),
  #                    name = "Depth") +
  scale_color_manual(values = c("#ffbc42", "#8f2d56", "#003566"),
                     name = "Depth") +
  geom_hline(data = sampling_depths %>% filter(sampling_depth >0), 
             aes(yintercept = sampling_depth), linewidth = 0.5, linetype = 2, color = "black") +
  geom_point(data = clines %>% filter(depth_type == "BGA",
                                      !(year == "Nutrients (2019)" & doy < 148),
                                      !(year == "Nutrients (2019)" & doy > 237)), 
             aes(x = doy, y = 0.25, size = log10(value)), color = "#7F9A1A", alpha = 0.5) +
  facet_grid(lake~year) +
  theme_bw() + ylim(8,0) + xlab("Day of Year") + ylab("Depth (m)")


#==============================
# FIGURE 3 - AREAL METHANE STORAGE
#==============================
# SCALED plot areal mass over time by lake and year - GAM
ggplot(scaled_sum_ch4mass %>% 
         mutate(lake = factor(lake, levels = c("Manipulated Lake", "Reference Lake"))), 
       aes(x = doy, y = areal_ch4mass_scaled, fill = year, color = year)) +
  geom_point(size = 2) + 
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 3)) +
  scale_fill_manual(values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_color_manual(values = c("#e9c46a", "#4f772d", "#023047")) +
  facet_wrap(facets = "lake") +
  theme_bw() +
  xlab("Day of Year") + ylab("Areal CH4 Mass (mg C/m2)") +
  geom_hline(yintercept = 0, linetype = "dashed")


# plot areal mass over time by lake and year - GAM
ggplot(sum_ch4mass %>% 
         mutate(lake = factor(lake, levels = c("Manipulated Lake", "Reference Lake"))), 
       aes(x = doy, y = areal_ch4mass, fill = year, color = year)) +
  geom_point(size = 2) + 
  # scale_y_continuous(transform = "log10", labels = label_number()) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 3)) +
  scale_fill_manual(values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_color_manual(values = c("#e9c46a", "#4f772d", "#023047")) +
  facet_wrap(facets = "lake", scale = "free") +
  theme_bw() +
  xlab("Day of Year") + ylab("Areal CH4 Mass (mg C/m2)")

#==================================================
# FIGURE 4 - RATE OF CHANGE IN CH4 CONC BY DEPTH
#==================================================
# Calculate slopes over depth
ch4_slopes = all_years_ch4 %>%
  mutate(transformed_meanCH4 = log10(meanCH4)) %>%
  group_by(year, lake, depth) %>%
  summarise(
    slope = summary(lm(transformed_meanCH4 ~ doy))$coefficients[2,1],
    intercept = summary(lm(transformed_meanCH4 ~ doy))$coefficients[1,1],
    r2 = summary(lm(transformed_meanCH4 ~ doy))$r.squared,
    pvalue = summary(lm(transformed_meanCH4 ~ doy))$coefficients[2,4]) %>%
  ungroup() %>%
  mutate(pvalue_alpha = case_when(pvalue < 0.05 ~ 'sig',
                                  pvalue > 0.05 ~ 'non-sig'),
         pvalue_alpha = factor(pvalue_alpha, levels = c("sig","non-sig")))

lm_estimates <- all_years_ch4 %>%
  group_by(year, lake, depth) %>%
  do(tidy(lm(log10(meanCH4) ~ doy, data = .)))
lm_slopes <- lm_estimates %>%
  filter(term == "doy")

# Plot the slopes by depth and year
ggplot(ch4_slopes, 
       aes(x = depth, y = slope, 
           fill = year,  alpha = pvalue_alpha)) + 
  # scale_fill_manual(values = c("#f9c74f", "#90be6d", "#277da1")) +
  # scale_fill_manual(values = c("#ffc300", "#a7c957", "#003566")) +
  scale_fill_manual(values = c("#e9c46a", "#4f772d", "#023047")) +
  # scale_color_manual(values = c('white', NA)) +
  scale_alpha_discrete(range = c(1, 0.4)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_flip() + scale_x_reverse() +
  # scale_x_continuous(transform = "log10") +
  facet_grid(vars(lake), vars(year)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ylab(bquote("Rate of Methane Increase (log10 umol "~L^-1~d^-1~")")) +
  xlab("Depth (m)") +
  theme(legend.position = 'none') 
# Average depth of Anoxia lines
# geom_vline(data = vlines_df, 
#            aes(xintercept = depth_line), 
#            linetype = "dashed", 
#            inherit.aes = FALSE)


# ======================================
# FIGURE 4 - Mixed effects model output
# ======================================
# Mixed effects CH4 concentration models
model_outputs = read_csv("R:/Cascade/Data/GHG Data/Resilient Methane/manuscript analysis/ch4_concentration_mixedeffectsmodels_noHypo.csv") %>% 
  mutate(lake = factor(lake, levels = c("Manipulated Lake", 
                                        "Reference Lake")),
         year = factor(year, levels = c("Ambient (2018)", 
                                        "Nutrients (2019)",
                                        "Dye + Nutrients (2024)")),
         lakeyear = factor(lakeyear, levels = c(
           "Manipulated - Ambient (2018)",
           "Manipulated - Nutrients (2019)",
           "Manipulated - Dye + Nutrients (2024)",
           "Reference - Ambient (2018)",
           "Reference - Nutrients (2019)",
           "Reference - Dye + Nutrients (2024)")),
         variable = factor(variable, levels = c(
           "intercept",
           "Depth",
           "Temperature",
           "Irradiance",
           "Depth x Irradiance",
           "Dissolved Oxygen",
           "Temp x DO",
           "DO x Irradiance")),
         pvalue_shape = case_when(pvalue < 0.05 ~ "sig", TRUE ~ "non-sig"),
         pvalue_shape = factor(pvalue_shape, levels = c("sig", "non-sig")),
         fill_interaction = interaction(variable, pvalue_shape))

# Plot the predictors
ggplot(model_outputs, aes(x = variable, y = estimate)) + 
  geom_segment(aes(x = variable, xend = variable,
                   y = estimate-SE, yend = estimate + SE,
                   color = variable),
               linewidth = 1, lineend = "round", na.rm = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # Points: shape and fill depend on pvalue_shape
  geom_point(aes(shape = pvalue_shape, 
                 fill = fill_interaction, 
                 color = variable),
             size = 2.5, stroke = 1) +  
  scale_shape_manual(values = c("sig" = 19, "non-sig" = 21)) +
  # Fill mapping: combine year + pvalue_shape to assign white only to "ns"
  scale_fill_manual(values = c(
    "intercept.sig" = "gray60",
    "Depth.sig" = "black",
    "Temperature.sig" = "#007DEE",
    "Irradiance.sig" = "#FFC761",
    "Depth x Irradiance.sig" = "#FFA401",
    "Dissolved Oxygen.sig" = "#E3A7C1",
    "Temp x DO.sig" = "#D3739C",
    "DO x Irradiance.sig" = "#8F2D56",
    
    "intercept.non-sig" = "white",
    "Depth.non-sig" = "white",
    "Temperature.non-sig" = "white",
    "Irradiance.non-sig" = "white",
    "Depth x Irradiance.non-sig" = "white",
    "Dissolved Oxygen.non-sig" = "white",
    "Temp x DO.non-sig" = "white",
    "DO x Irradiance.non-sig" = "white")) +
  
  # Color (outline) mapping
  scale_color_manual(values = c(    "intercept" = "gray60",
                                    "Depth" = "black",
                                    "Temperature" = "#007DEE",
                                    "Irradiance" = "#FFC761",
                                    "Depth x Irradiance" = "#FFA401",
                                    "Dissolved Oxygen" = "#E3A7C1",
                                    "Temp x DO" = "#D3739C",
                                    "DO x Irradiance" = "#8F2D56")) +
  facet_grid(year~lake) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.95, hjust = 1,
                                   color = c("gray60", "black",
                                              "#007DEE","#FFC761", "#FFA401",
                                             "#E3A7C1", "#D3739C", "#8F2D56")),
        legend.position = "none") +
  labs(x = "", y = "Estimated Effect")


#===================================================================
#===================================================================
# Figure S1 - CH4 conc slopes bar plot


# Plot the data over time and by depth
ggplot(all_years_ch4 %>% filter(lake == "Reference Lake"), 
       aes(x = doy, y = meanCH4, color = year)) + 
  scale_color_manual(values = c("#e9c46a", "#4f772d", "#023047")) +
  geom_point(size = 1.5) +
  geom_path(aes(group = year), linewidth = 1, alpha = 0.75) +
  facet_wrap('depth', scale = 'free') +
  theme_bw() +
  scale_y_continuous(trans = 'log10') +
  labs(x = "Day of Year", 
       y = bquote("Methane Concentration (umol"~L^-1*")")) +
  ggtitle("Reference Lake") +
  theme(legend.position = c(0.85, 0.1))

# Plot the data over time and by depth
ggplot(all_years_ch4 %>% filter(lake == "Manipulated Lake"), 
       aes(x = doy, y = meanCH4, color = year)) + 
  scale_color_manual(values = c("#e9c46a", "#4f772d", "#023047")) +
  geom_point(size = 1.5) +
  geom_path(aes(group = year), linewidth = 1, alpha = 0.75) +
  facet_wrap('depth', scale = 'free') +
  theme_bw() +
  scale_y_continuous(trans = 'log10') +
  labs(x = "Day of Year", 
       y = bquote("Methane Concentration (umol"~L^-1*")")) +
  ggtitle("Manipulated Lake") +
  theme(legend.position = c(0.85, 0.1))


