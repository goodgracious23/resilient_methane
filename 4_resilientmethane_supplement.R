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



## Peter Patterns ==================================
gglight_peter = 
  ggplot(all_years_ch4_clines %>% 
         filter(lake == "Manipulated Lake" & depth == 6 | 
                  lake == "Manipulated Lake" & depth == 3.5 |
                  lake == "Manipulated Lake" & depth == 0) %>%
         mutate(depth = case_when(
           depth == 6 ~ "Near Anoxia",
           depth == 3.5 ~ "Oxic Metalimnion",
           depth == 0 ~ "Surface")) %>%
         mutate(depth = factor(depth, levels = c("Surface",
                                                 "Oxic Metalimnion",
                                                 "Near Anoxia"))),
       aes(y = meanCH4, 
           x = (light+0.001)*100, 
           color = factor(year), 
           fill = factor(year))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(
    values = c("#e9c46a", "#4f772d", "#023047"), 
                     guide = "none") +
  scale_fill_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_y_continuous(transform = "log10", 
                     labels = label_number()) + 
  scale_x_continuous(transform = "log10", 
                     labels = label_number()) +
  facet_grid(facets = c("depth"), scale = "free_x") +
  stat_smooth(method = "glm") +
  theme_bw() + theme(legend.position = 'none') +
  labs(y = bquote("CH4 (umol"~L^-1~")"), 
       x = "% Surface Irradiance") +
  geom_vline(xintercept = 1, linetype = "dashed")

ggtemp_peter = 
  ggplot(all_years_ch4_clines %>% 
         filter(lake == "Manipulated Lake" & depth == 6 | 
                  lake == "Manipulated Lake" & depth == 3.5 |
                  lake == "Manipulated Lake" & depth == 0) %>%
         mutate(depth = case_when(
           depth == 6 ~ "Near Anoxia",
           depth == 3.5 ~ "Oxic Metalimnion",
           depth == 0 ~ "Surface")) %>%
         mutate(depth = factor(depth, levels = c("Surface",
                                                 "Oxic Metalimnion",
                                                 "Near Anoxia"))),
       aes(y = meanCH4, 
           x = temp, 
           color = factor(year), 
           fill = factor(year))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(
    values = c("#e9c46a", "#4f772d", "#023047"), 
    guide = "none") +
  scale_fill_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_y_continuous(transform = "log10", 
                     labels = label_number()) + 
  scale_x_continuous(transform = "log10", 
                     labels = label_number()) +
  facet_grid(facets = c("depth"), scale = "free_x") +
  stat_smooth(method = "glm") +
  theme_bw() + theme(legend.position = 'none') +
  labs(y = bquote("CH4 (umol"~L^-1~")"), 
       x = "Temperature") 

ggDO_peter = 
  ggplot(all_years_ch4_clines %>% 
         filter(lake == "Manipulated Lake" & depth == 6 | 
                  lake == "Manipulated Lake" & depth == 3.5 |
                  lake == "Manipulated Lake" & depth == 0) %>%
         mutate(depth = case_when(
           depth == 6 ~ "Near Anoxia",
           depth == 3.5 ~ "Oxic Metalimnion",
           depth == 0 ~ "Surface")) %>%
         mutate(depth = factor(depth, levels = c("Surface",
                                                 "Oxic Metalimnion",
                                                 "Near Anoxia"))),
       aes(y = meanCH4, 
           x = do_mgl, 
           color = factor(year), 
           fill = factor(year))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(
    values = c("#e9c46a", "#4f772d", "#023047"), 
    guide = "none") +
  scale_fill_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_y_continuous(transform = "log10", 
                     labels = label_number()) + 
  scale_x_continuous(transform = "log10", 
                     labels = label_number()) +
  facet_grid(facets = c("depth"), scale = "free_x") +
  stat_smooth(method = "glm") +
  theme_bw() + theme(legend.position = 'none') +
  labs(y = bquote("CH4 (umol"~L^-1~")"), 
       x = "Dissolved Oxygen (mg/L)") +
  geom_vline(xintercept = 1, linetype = "dashed")

ggBGA_peter = 
  ggplot(all_years_ch4_clines %>% 
         filter(lake == "Manipulated Lake" & depth == 6 | 
                  lake == "Manipulated Lake" & depth == 3.5 |
                  lake == "Manipulated Lake" & depth == 0) %>%
         mutate(depth = case_when(
           depth == 6 ~ "Near Anoxia",
           depth == 3.5 ~ "Oxic Metalimnion",
           depth == 0 ~ "Surface")) %>%
         mutate(depth = factor(depth, levels = c("Surface",
                                                 "Oxic Metalimnion",
                                                 "Near Anoxia"))),
       aes(y = meanCH4, 
           x = BGA, 
           color = factor(year), 
           fill = factor(year))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(
    values = c("#e9c46a", "#4f772d", "#023047"), 
    guide = "none") +
  scale_fill_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_y_continuous(transform = "log10", 
                     labels = label_number()) + 
  scale_x_continuous(transform = "log10", 
                     labels = label_number()) +
  facet_grid(facets = c("depth"), scale = "free_x") +
  stat_smooth(method = "glm") +
  theme_bw() + theme(legend.position = 'none') +
  labs(y = bquote("CH4 (umol"~L^-1~")"), 
       x = "Phycocyanin (RFU)") 

ggarrange(ggtemp_peter, gglight_peter, ggDO_peter, ggBGA_peter)

## Paul Patterns ==================================
gglight_paul = 
  ggplot(all_years_ch4_clines %>% 
         filter(lake == "Reference Lake" & depth == 6.5 | 
                  lake == "Reference Lake" & depth == 4.5 |
                  lake == "Reference Lake" & depth == 0) %>%
         mutate(depth = case_when(
           depth == 6.5 ~ "Near Anoxia",
           depth == 4.5 ~ "Oxic Metalimnion",
           depth == 0 ~ "Surface")) %>%
         mutate(depth = factor(depth, levels = c("Surface",
                                                 "Oxic Metalimnion",
                                                 "Near Anoxia"))),
       aes(y = meanCH4, x = (light+0.001)*100, color = factor(year), 
           fill = factor(year))) +
  geom_point(alpha = 0.3, size = 2) +
  scale_color_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_fill_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_y_continuous(transform = "log10", 
                     labels = label_number()) + 
  scale_x_continuous(transform = "log10", 
                     labels = label_number()) +
  facet_grid(facets = c("depth")) +
  stat_smooth(method = "glm") +
  theme_bw() + theme(legend.position = 'none') +
  labs(y = bquote("Methane Concentration (umol"~L^-1~")"), 
       x = "% Surface Irradiance") +
  geom_vline(xintercept = 1, linetype = "dashed")


ggtemp_paul = 
  ggplot(all_years_ch4_clines %>% 
         filter(lake == "Reference Lake" & depth == 6.5 | 
                  lake == "Reference Lake" & depth == 4.5 |
                  lake == "Reference Lake" & depth == 0) %>%
         mutate(depth = case_when(
           depth == 6.5 ~ "Near Anoxia",
           depth == 4.5 ~ "Oxic Metalimnion",
           depth == 0 ~ "Surface")) %>%
         mutate(depth = factor(depth, levels = c("Surface",
                                                 "Oxic Metalimnion",
                                                 "Near Anoxia"))),
       aes(y = meanCH4, 
           x = temp,
           color = factor(year), 
           fill = factor(year))) +
  geom_point(alpha = 0.3, size = 2) +
  scale_color_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_fill_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_y_continuous(transform = "log10", 
                     labels = label_number()) + 
  scale_x_continuous(transform = "log10", 
                     labels = label_number()) +
  facet_grid(facets = c("depth")) +
  stat_smooth(method = "glm") +
  theme_bw() + theme(legend.position = 'none') +
  labs(y = bquote("Methane Concentration (umol"~L^-1~")"), 
       x = "Temperature") 


ggDO_paul = 
  ggplot(all_years_ch4_clines %>% 
         filter(lake == "Reference Lake" & depth == 6.5 | 
                  lake == "Reference Lake" & depth == 4.5 |
                  lake == "Reference Lake" & depth == 0) %>%
         mutate(depth = case_when(
           depth == 6.5 ~ "Near Anoxia",
           depth == 4.5 ~ "Oxic Metalimnion",
           depth == 0 ~ "Surface")) %>%
         mutate(depth = factor(depth, levels = c("Surface",
                                                 "Oxic Metalimnion",
                                                 "Near Anoxia"))),
       aes(y = meanCH4, 
           x = do_mgl,
           color = factor(year), 
           fill = factor(year))) +
  geom_point(alpha = 0.3, size = 2) +
  scale_color_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_fill_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_y_continuous(transform = "log10", 
                     labels = label_number()) + 
  scale_x_continuous(transform = "log10", 
                     labels = label_number()) +
  facet_grid(facets = c("depth")) +
  stat_smooth(method = "glm") +
  theme_bw() + theme(legend.position = 'none') +
  labs(y = bquote("Methane Concentration (umol"~L^-1~")"), 
       x = "Dissolved Oxygen (mg/L)") +
  geom_vline(xintercept = 1, linetype = "dashed")


ggBGA_paul = 
  ggplot(all_years_ch4_clines %>% 
         filter(lake == "Reference Lake" & depth == 6.5 | 
                  lake == "Reference Lake" & depth == 4.5 |
                  lake == "Reference Lake" & depth == 0) %>%
         mutate(depth = case_when(
           depth == 6.5 ~ "Near Anoxia",
           depth == 4.5 ~ "Oxic Metalimnion",
           depth == 0 ~ "Surface")) %>%
         mutate(depth = factor(depth, levels = c("Surface",
                                                 "Oxic Metalimnion",
                                                 "Near Anoxia"))),
       aes(y = meanCH4, 
           x = BGA,
           color = factor(year), 
           fill = factor(year))) +
  geom_point(alpha = 0.3, size = 2) +
  scale_color_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_fill_manual(
    values = c("#e9c46a", "#4f772d", "#023047")) +
  scale_y_continuous(transform = "log10", 
                     labels = label_number()) + 
  scale_x_continuous(transform = "log10", 
                     labels = label_number()) +
  facet_grid(facets = c("depth")) +
  stat_smooth(method = "glm") +
  theme_bw() + theme(legend.position = 'none') +
  labs(y = bquote("Methane Concentration (umol"~L^-1~")"), 
       x = "Phycocyanin (RFU)") 

ggarrange(ggtemp_paul, gglight_paul, ggDO_paul, ggBGA_paul)



# Trying to combine both lakes in one visual...
ggplot(all_years_ch4_clines %>% 
         filter(lake == "Manipulated Lake" & depth == 6 | 
                  lake == "Manipulated Lake" & depth == 3.5 |
                  lake == "Reference Lake" & depth == 6.5 |
                  lake == "Reference Lake" & depth == 4.5) %>%
         mutate(depth = case_when(depth == 6 ~ "Near Anoxia",
                                  depth == 6.5 ~ "Near Anoxia",
                                  depth == 3.5 ~ "Oxic Metalimnion",
                                  depth == 4.5 ~ "Oxic Metalimnion")),
       aes(x = (light*100)+0.01, y = meanCH4, 
           color = factor(year), 
           fill = factor(year), 
           shape = factor(depth))) +
  geom_point(size = 0, alpha = 0) +
  geom_point(aes(color = factor(year), 
                 size = log10(do_mgl)), 
             alpha = 0.3, stroke = 1) +
  scale_shape_manual(values = c(19, 23)) +
  scale_color_manual(values = c("#e9c46a", "#4f772d", "#023047"), guide = "none") +
  scale_fill_manual(values = c("#e9c46a", "#4f772d", "#023047"), guide = "none") +
  scale_y_continuous(transform = "log10", 
                     labels = label_number()) + 
  scale_x_continuous(transform = "log10", 
                     labels = label_number()) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_grid(facets = c("lake", "year"), scale = "free") +
  theme_bw() + 
  labs(x = "% Surface Irradiance", 
       y = "CH4 (umol/L)")

# Trying to combine both lakes in one visual...
ggplot(all_years_ch4_clines %>% 
         filter(lake == "Manipulated Lake" & depth == 6 | 
                  lake == "Manipulated Lake" & depth == 3.5 |
                  lake == "Reference Lake" & depth == 6.5 | 
                  lake == "Reference Lake" & depth == 4.5) %>%
         mutate(depth = case_when(
           depth == 6 ~ "Near Anoxia",
           depth == 6.5 ~ "Near Anoxia",
           depth == 3.5 ~ "Oxic Metalimnion",
           depth == 4.5 ~ "Oxic Metalimnion")),
       aes(y = do_mgl, 
           x = (light+0.001)*100, 
           color = factor(year), 
           fill = factor(year), 
           shape = factor(depth))) +
  geom_point(size = 0, alpha = 0) +
  geom_point(aes(color = factor(year), 
                 size = log10(meanCH4)), 
             alpha = 0.4, stroke = 1) +
  scale_shape_manual(values = c(19, 1)) +
  scale_color_manual(values = c("#e9c46a", "#4f772d", "#023047"), 
                     guide = "none") +
  scale_fill_manual(values = c("#e9c46a", "#4f772d", "#023047"), 
                    guide = "none") +
  scale_y_continuous(transform = "log10", 
                     labels = label_number()) + 
  scale_x_continuous(transform = "log10", 
                     labels = label_number()) +
  geom_vline(xintercept = 1, 
             linetype = "dashed") +
  geom_hline(yintercept = 1, 
             linetype = "dashed") +
  facet_grid(facets = c("lake", "year")) +
  theme_bw() + 
  labs(y = "Dissolved Oxygen (mg/L)", 
       x = "% Surface Irradiance")  
