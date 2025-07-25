
# Run 'resilientmethane_munging.R' to set up the data frames for these figures

## Generate a model for each lake to test if there are differences among years within each lake
# Manipulated Lake Linear Model
peter = sum_ch4mass %>% filter(lake == "Manipulated Lake")
lm_peter <- lm(areal_ch4mass ~ doy * year, data = peter)
summary(lm_peter)

# Manipulated Lake GAM Model
peter_gam_model <- gam(areal_ch4mass ~ s(doy, by = year, bs = 'cs', k = 3) + year, 
                       data = peter)
summary(peter_gam_model)
gam.check(peter_gam_model)

# Compare models - modest support for GAM wiggles over linear fit
# k-index > 1 in GAM model, so wiggliness is probably real and k is set okay
# Also checked linear against non-linear fits and non-linear is the winner
AIC(peter_gam_model, lm_peter)


# Reference Lake Linear Model
paul = sum_ch4mass %>% filter(lake == "Reference Lake")
lm_paul <- lm(areal_ch4mass ~ doy * year, data = paul)
summary(lm_paul)

# Reference Lake GAM Model
paul_gam_model <- gam(areal_ch4mass ~ s(doy, by = year, bs = 'cs', k = 3) + year, data = paul)
summary(paul_gam_model)
gam.check(paul_gam_model)
# plot(paul_gam_model, pages = 1)

# k-index indicated that k needed to be increased to 5... but that just makes Paul overly wiggly so we'll stick with k = 3 to tamp down our expectations of wiggles
AIC(lm_paul, paul_gam_model)


# 2018 Peter Model ========================
peter2018_0m = peter_meanCH4 %>% filter(year == "Ambient (2018)", depth == 0) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_peter2018_0m = rcorr(as.matrix(peter2018_0m), type = "spearman")
corrplot(cor_peter2018_0m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_peter2018_0m$P, sig.level = 0.05, insig = "blank", 
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Manipulated Lake (2018) - 0 m")

peter2018_3.5m = peter_meanCH4 %>% filter(year == "Ambient (2018)", depth == 3.5) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_peter2018_3.5m = rcorr(as.matrix(peter2018_3.5m), type = "spearman")
corrplot(cor_peter2018_3.5m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_peter2018_3.5m$P, sig.level = 0.05, 
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Manipulated Lake (2018) - 3.5 m")

peter2018_6m = peter_meanCH4 %>% filter(year == "Ambient (2018)", depth == 6) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_peter2018_6m = rcorr(as.matrix(peter2018_6m), type = "spearman")
corrplot(cor_peter2018_6m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_peter2018_6m$P, sig.level = 0.05,  
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Manipulated Lake (2018) - 6 m")

# 2019 Peter Model ========================
peter2019_0m = peter_meanCH4 %>% filter(year == "Nutrients (2019)", depth == 0) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_peter2019_0m = rcorr(as.matrix(peter2019_0m), type = "spearman")
corrplot(cor_peter2019_0m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_peter2019_0m$P, sig.level = 0.05,  
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Manipulated Lake (2019) - 0 m")

peter2019_3.5m = peter_meanCH4 %>% filter(year == "Nutrients (2019)", depth == 3.5) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_peter2019_3.5m = rcorr(as.matrix(peter2019_3.5m), type = "spearman")
corrplot(cor_peter2019_3.5m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_peter2019_3.5m$P, sig.level = 0.05, 
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Manipulated Lake (2019) - 3.5 m")

peter2019_6m = peter_meanCH4 %>% filter(year == "Nutrients (2019)", depth == 6) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_peter2019_6m = rcorr(as.matrix(peter2019_6m), type = "spearman")
corrplot(cor_peter2019_6m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_peter2019_6m$P, sig.level = 0.05,  
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Manipulated Lake (2019) - 6 m")


# 2024 Peter Model ========================
peter2024_0m = peter_meanCH4 %>% filter(year == "Dye + Nutrients (2024)", depth == 0) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_peter2024_0m = rcorr(as.matrix(peter2024_0m), type = "spearman")
corrplot(cor_peter2024_0m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_peter2024_0m$P, sig.level = 0.05, 
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Manipulated Lake (2024) - 0 m")

peter2024_3.5m = peter_meanCH4 %>% filter(year == "Dye + Nutrients (2024)", depth == 3.5) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_peter2024_3.5m = rcorr(as.matrix(peter2024_3.5m), type = "spearman")
corrplot(cor_peter2024_3.5m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_peter2024_3.5m$P, sig.level = 0.05, 
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Manipulated Lake (2024) - 3.5 m")

peter2024_6m = peter_meanCH4 %>% filter(year == "Dye + Nutrients (2024)", depth == 6) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_peter2024_6m = rcorr(as.matrix(peter2024_6m), type = "spearman")
corrplot(cor_peter2024_6m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_peter2024_6m$P, sig.level = 0.05, 
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Manipulated Lake (2024) - 6 m")


# 2018 paul Model ========================
paul2018_0m = paul_meanCH4 %>% filter(year == "Ambient (2018)", depth == 0) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_paul2018_0m = rcorr(as.matrix(paul2018_0m), type = "spearman")
corrplot(cor_paul2018_0m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_paul2018_0m$P, sig.level = 0.05,  
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Reference Lake (2018) - 0 m")

paul2018_4.5m = paul_meanCH4 %>% filter(year == "Ambient (2018)", depth == 4.5) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_paul2018_4.5m = rcorr(as.matrix(paul2018_4.5m), type = "spearman")
corrplot(cor_paul2018_4.5m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_paul2018_4.5m$P, sig.level = 0.05,  
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Reference Lake (2018) - 3.5 m")

paul2018_6m = paul_meanCH4 %>% filter(year == "Ambient (2018)", depth == 6.5) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_paul2018_6m = rcorr(as.matrix(paul2018_6m), type = "spearman")
corrplot(cor_paul2018_6m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_paul2018_6m$P, sig.level = 0.05,  
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Reference Lake (2018) - 6 m")

# 2019 paul Model ========================
paul2019_0m = paul_meanCH4 %>% filter(year == "Nutrients (2019)", depth == 0) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_paul2019_0m = rcorr(as.matrix(paul2019_0m), type = "spearman")
corrplot(cor_paul2019_0m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_paul2019_0m$P, sig.level = 0.05, 
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Reference Lake (2019) - 0 m")

paul2019_4.5m = paul_meanCH4 %>% filter(year == "Nutrients (2019)", depth == 4.5) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_paul2019_4.5m = rcorr(as.matrix(paul2019_4.5m), type = "spearman")
corrplot(cor_paul2019_4.5m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_paul2019_4.5m$P, sig.level = 0.05, 
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Reference Lake (2019) - 3.5 m")

paul2019_6m = paul_meanCH4 %>% filter(year == "Nutrients (2019)", depth == 6.5) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_paul2019_6m = rcorr(as.matrix(paul2019_6m), type = "spearman")
corrplot(cor_paul2019_6m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_paul2019_6m$P, sig.level = 0.05,
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Reference Lake (2019) - 6 m")


# 2024 paul Model ========================
paul2024_0m = paul_meanCH4 %>% filter(year == "Dye + Nutrients (2024)", depth == 0) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_paul2024_0m = rcorr(as.matrix(paul2024_0m), type = "spearman")
corrplot(cor_paul2024_0m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_paul2024_0m$P, sig.level = 0.05, 
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Reference Lake (2024) - 0 m")

paul2024_4.5m = paul_meanCH4 %>% filter(year == "Dye + Nutrients (2024)", depth == 4.5) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_paul2024_4.5m = rcorr(as.matrix(paul2024_4.5m), type = "spearman")
corrplot(cor_paul2024_4.5m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_paul2024_4.5m$P, sig.level = 0.05, 
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Reference Lake (2024) - 4.5 m")

paul2024_6m = paul_meanCH4 %>% filter(year == "Dye + Nutrients (2024)", depth == 8) %>%
  select(meanCH4, temp, light, do_mgl, surface_bga) %>%
  mutate(light = light*100)

cor_paul2024_6m = rcorr(as.matrix(paul2024_6m), type = "spearman")
corrplot(cor_paul2024_6m$r, method = "circle", type = "lower", order = "original", 
         p.mat = cor_paul2024_6m$P, sig.level = 0.05,
         addgrid.col = NULL, addCoef.col = "black",
         diag = FALSE, title = "Reference Lake (2024) - 6 m")
