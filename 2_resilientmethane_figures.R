
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
  scale_color_manual(values = c("#FFAA11", "#8f2d56", "#003566"),
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


