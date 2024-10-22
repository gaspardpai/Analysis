
## Title: Regression models using Movement and Place dataset and the Urban Liveability Index (ULI)
## Author: Gaspard Pairault
## Date: June 2024



library(tidyverse)
library(jtools)
library(kableExtra)

final_df <- full_join(
  x = AUO_SYD_SA2_df,
  y = GSYD_Final_MaP,
  by = join_by("SA2_CODE21", "SA2_NAME21")
)



final_analysis <- subset(final_df, select = -c(SA2_CODE21, SA2_NAME21))
line_analysis <- final_df %>% 
  select(ULI_mean, colnames(Line_SA2_NSW_df)) %>%
  subset(select = -SA2_CODE21)

polygon_analysis <- final_df %>% 
  select(ULI_mean, colnames(GSYD_Polygon_df)) %>%
  subset(select = - c(SA2_CODE21, SA2_NAME21))
visdat::vis_miss(final_analysis)                                  

#### Table 1 ####

table1::table1(~ ., data = polygon_analysis)
table1::table1(~ ., data = line_analysis)

# Let's categorise the Urban Liveability index


fabricatr::split_quantile(final_df$ULI_mean, type = 5)
final_analysis %>%
  ggplot() +
  aes(x = ULI_mean) +
  geom_histogram(fill = 'darkblue', col = 'lightblue',
                 bins = 20) +
  labs(x = 'Urban Liveability Index', y = 'Total across selected SA2s in Greater Sydney') +
  #stat_function(fun = dnorm, args = list(mean = mean(final_analysis$ULI_mean), sd = sd(final_analysis$ULI_mean))) +aes(y=after_stat(density))
  theme_minimal()

summary(final_analysis$ULI_mean)

quantile(final_analysis$ULI_mean, probs = seq(0, 1, 0.33))

#final_analysis <- final_analysis %>%
 # mutate(ULI_cat = case_when(
  #  ULI_mean < 97.7 ~ "Very Low",
   # ULI_mean >= 97.7 & ULI_mean < 98.95 ~ "Low",
    #ULI_mean >= 98.95 & ULI_mean < 100.09 ~ "Normal",
    #ULI_mean >= 100.09 & ULI_mean < 102 ~ "High",
    #ULI_mean > 102 ~ "Very High" )
    #)
final_analysis <- final_analysis %>%
  mutate(ULI_q = case_when(
    ULI_mean < 98.5 ~ 0,
    ULI_mean >= 98.5 & ULI_mean < 100.5 ~ 1,
    ULI_mean >= 100.5 ~ 2),
    ULI_cat = factor(ULI_q,
                   levels = c(0, 1, 2),
                   labels = c('Low', 'Normal', 'High'))
  )

table1::table1(~ UHI_mean + BuildingDensity_mean + BuildingHeight_mean +PTAL_mean +
                 LocalJobs_mean	+ PopGrowth_mean + ImperviousSurface_mean + LocalLiving_mean +
                 CrashRate_mean + BusDelay_mean
                 |ULI_cat, data = final_analysis)


table1::table1(~.|ULI_cat, data = final_analysis)




 # Scatter7 plot of ULI vs Mif aP indicators

final_analysis %>%
  select(-c('ULI_cat', 'ULI_q')) %>%
  tidyr::gather(var, val, -ULI_mean) %>% 
  ggplot() +
  aes(y = ULI_mean, x = val) +
  geom_point(size = 1) + 
  geom_smooth(method = 'lm', formula = y ~ splines::ns(x, df = 5), se = T) +
  facet_wrap(~var, scales = 'free') +
  theme_minimal()


# Univariate Linear Regression
gtsummary::tbl_uvregression(
  data = final_analysis,
  method = lm,
  y = ULI_mean,
  hide_n = TRUE
)

# Distribution of variables at the SA2 scale using histogramm
#final_analysis %>%
 # tidyr::gather(var, val) %>%
  #ggplot() +
  #aes(x = val) +
  #geom_histogram(fill = 'darkblue', col = 'lightblue') +
  #facet_wrap(~var, scales = 'free') +
  #theme_minimal()

quantile(final_analysis$PopGrowth_mean)

#####Linarity and Homoskedasticity using fitted vs residuals plot ####
vis_vars_no <-c('BuildingDensity_mean', 'LocalJobs_mean', 'StreetAspectRatio_mean', 'TreeCanopy_mean', 'UHI_mean')
quantile(final_analysis$TreeCanopy_mean)
#Non-Log

final_analysis%>%
  dplyr::select(ULI_mean, dplyr::all_of(vis_vars_no)) %>% 
  #dplyr::mutate(dplyr::across(-ULI_mean, ~log(.x + 1))) %>% 
  tidyr::gather(var, val, -ULI_mean) %>% 
  dplyr::mutate(fitted = fitted(lm(ULI_mean ~ val )),
                resid = resid(lm(ULI_mean ~ val),
                .by = var)) %>% 
  ggplot(aes(x = fitted, y = resid)) + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey70') +
  geom_point() + 
  geom_smooth(method = 'loess', formula = y ~ x, se = F) + 
  facet_wrap(~var, scales = 'free', nrow = length(vis_vars_no)) +
  theme_minimal()


# Log with all no variables
final_analysis%>%
  dplyr::select(ULI_mean, dplyr::all_of(vis_vars_no)) %>% 
  dplyr::mutate(dplyr::across(-ULI_mean, ~log(.x + 1))) %>% 
  tidyr::gather(var, val, -ULI_mean) %>% 
  dplyr::mutate(fitted = fitted(lm(ULI_mean ~ log(val + 1))),
                resid = resid(lm(ULI_mean ~log(val + 1))),
                .by = var) %>% 
  ggplot(aes(x = fitted, y = resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey70') +
  geom_smooth(method = 'loess', formula = y ~ x, se = F) + 
  facet_wrap(~var, scales = 'free', nrow = length(vis_vars_no)) +
  theme_minimal()



#vis_vars_maybe <- c('BuildingHeight_mean', 'CommunitySafety_mean', 'EmploymentGrowth_mean', 'HousingDiversity_mean',
                    #'ImperviousSurface_mean', 'StreetSpacePedestrian_mean' )

final_analysis %>%
  dplyr::select(ULI_mean, dplyr::all_of(vis_vars_maybe)) %>% 
  tidyr::gather(var, val, -ULI_mean) %>% 
  dplyr::mutate(fitted = fitted(lm(ULI_mean ~ val )),
                resid = resid(lm(ULI_mean ~ val),
                              .by = var)) %>% 
  ggplot(aes(x = fitted, y = resid)) + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey70') +
  geom_point() + 
  geom_smooth(method = 'loess', formula = y ~ x, se = F) + 
  facet_wrap(~var, scales = 'free', nrow = length(vis_vars_maybe)) +
  theme_minimal()

final_analysis%>%
  dplyr::select(ULI_mean, dplyr::all_of(vis_vars_maybe)) %>% 
  dplyr::mutate(dplyr::across(-ULI_mean, ~log(.x))) %>% 
  tidyr::gather(var, val, -ULI_mean) %>% 
  dplyr::mutate(fitted = fitted(lm(ULI_mean ~ log(val))),
                resid = resid(lm(ULI_mean ~log(val ))),
                .by = var) %>% 
  ggplot(aes(x = fitted, y = resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey70') +
  geom_smooth(method = 'loess', formula = y ~ x, se = F) + 
  facet_wrap(~var, scales = 'free', nrow = length(vis_vars_maybe)) +
  theme_minimal()

#NA in Bus Density 'RoadSafety_mean', 'Waterways_mean', Primary School

vis_vars_yes <- c('BusDelay_mean', 'CrashRate_mean', 'CultureHeritage_mean',
                  'LandDivision_mean', 'LocalLiving_mean', 'ModeShare_mean',
                  'PopDensity_mean', 'Legibility_mean', 'PTAL_mean', 'PublicSpace_mean',
                  'Waterways_mean', 'PrimarySchool_mean')

quantile(final_analysis$PTAL_mean)

final_analysis%>%
  filter(Waterways_mean != 'NA' & PrimarySchool_mean != 'NA') %>%
  dplyr::select(ULI_mean, dplyr::all_of(vis_vars_yes)) %>% 
  #dplyr::mutate(dplyr::across(-ULI_mean, ~log(.x + 1))) %>% 
  tidyr::gather(var, val, -ULI_mean) %>% 
  dplyr::mutate(fitted = fitted(lm(ULI_mean ~ val )),
                resid = resid(lm(ULI_mean ~ val),
                              .by = var)) %>% 
  ggplot(aes(x = fitted, y = resid)) + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey70') +
  geom_point() + 
  geom_smooth(method = 'loess', formula = y ~ x, se = F) + 
  facet_wrap(~var, scales = 'free', nrow = 6) +
  theme_minimal()

final_analysis%>%
  filter(Waterways_mean != 'NA' & PrimarySchool_mean != 'NA') %>%
  dplyr::select(ULI_mean, dplyr::all_of(vis_vars_yes)) %>% 
  dplyr::mutate(dplyr::across(-ULI_mean, ~log(.x + 1))) %>% 
  tidyr::gather(var, val, -ULI_mean) %>% 
  dplyr::mutate(fitted = fitted(lm(ULI_mean ~ log(val +1))),
                resid = resid(lm(ULI_mean ~log(val +1))),
                .by = var) %>% 
  ggplot(aes(x = fitted, y = resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey70') +
  geom_smooth(method = 'loess', formula = y ~ x, se = F) + 
  facet_wrap(~var, scales = 'free', nrow = 6) +
  theme_minimal()



# We need to determine the appropriate transformation for the remaining variables


final_analysis%>%  ggplot() +
  aes(x = TreeCanopy_mean, y = ULI_mean) +
  geom_point() +
  geom_smooth(method = 'loess')

final_analysis%>%  ggplot() +
  aes(x = UHI_mean, y = ULI_mean) +
  geom_point() +
  geom_smooth(method = 'loess')



# Transformation population growth

final_analysis%>%  ggplot() +
  aes(x = PopGrowth_mean, y = ULI_mean) +
  geom_point() +
  geom_smooth(method = 'lm')

plot(lm(ULI_mean ~ PopGrowth_mean, data = final_analysis), which = 1)
quantile(final_analysis$PopGrowth_mean, na.rm = T)  

# Transformation Cycling Lane, Pop Growth, UHI, Tree Canopy and Housing diversity to factor

quantile(final_analysis$CyclingLane_mean)
final_analysis <-final_analysis %>%
  mutate(CyclingLane_cat = factor(ifelse(CyclingLane_mean == 0, 0, 1)))
final_analysis$CyclingLane_cat

final_analysis <- final_analysis %>%
  mutate(HousingDiversity_mean = factor(round(HousingDiversity_mean)))
final_analysis$HousingDiversity_mean

glimpse(final_analysis)

# UHI & Tree Canopy using inflection point (estimated using visualisation)

final_analysis %>% ggplot() +
  aes(x= UHI_mean, y = ULI_mean) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_vline(xintercept = 4, linetype="dotted")+
  labs(x = 'Urban Heat', y = 'Urban Liveability Index' ) +
  theme_minimal()

final_analysis <- final_analysis %>%
  mutate(UHI_cat = factor(ifelse(UHI_mean <= 4, 0, 1)))

final_analysis %>% ggplot() +
  aes(x= TreeCanopy_mean, y = ULI_mean) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_vline(xintercept = 0.22, linetype="dotted") +
  labs(x = 'Tree Canopy', y = 'Urban Liveability Index' ) +
  theme_minimal()

final_analysis <- final_analysis %>%
  mutate(TreeCanopy_cat = factor(ifelse(TreeCanopy_mean <= 0.22, 0, 1)))

#Pop growth categorised using quartile
quantile(final_analysis$PopGrowth_mean)
final_analysis %>% ggplot() +
  aes(x= PopGrowth_mean, y = ULI_mean) +
  geom_point() +
  geom_smooth(method = 'loess')  +
  lims(y = c(80, 110)) +
  ggforce::facet_zoom(x = dplyr::between(PopGrowth_mean,-70, 200))
 
final_analysis <- final_analysis %>%
  mutate(PopGrowth_cat = factor(case_when(
    PopGrowth_mean <=  11 ~ 0,
    PopGrowth_mean > 11 & PopGrowth_mean <= 38 ~ 1,
    PopGrowth_mean > 38 & PopGrowth_mean <= 167 ~ 2,
    PopGrowth_mean > 167 ~ 3
    
  )))
final_analysis$PopGrowth_cat
final_analysis$UHI_cat
final_analysis %>% select(c(PopGrowth_mean, PopGrowth_cat))

#### Correlation and multicollinearity ####

cor_matrix <- final_analysis %>%
  select(-c(PopGrowth_cat,TreeCanopy_cat, UHI_cat, ULI_cat, CyclingLane_cat)) %>%
  mutate(HousingDiversity_mean = as.double(HousingDiversity_mean)) %>%
  drop_na() %>%
  ungroup() %>%
  cor()

corrplot::corrplot(cor_matrix, method="color",
                   type="upper", order="hclust", tl.col="black")

heatmap(cor_matrix)

# Multicolinearity

subset_analysis <- subset(final_analysis, select = 
                            # remove variable included in AUO 
                            -c(BuildingDensity_mean, LocalLiving_mean, LocalJobs_mean, PublicSpace_mean, 
                                      CultureHeritage_mean, PrimarySchool_mean, BusStopDensity, 
                            # remove continous variable that have been categorised
                                      UHI_mean, TreeCanopy_mean, CyclingLane_mean, PopGrowth_mean, ULI_q, ULI_cat))
full_lm <- lm(ULI_mean ~ ., data = subset_analysis)
vif_values <- car::vif(full_lm)
vif_values %>% data.frame() %>% filter(GVIF >5)
vif_values

#### Early Modelling ####
modelling_subset <- subset(subset_analysis,
                           select = -c(BuildingHeight_mean, StreetAspectRatio_mean))
modelling_subset <- modelling_subset %>% 
  mutate(
    CrashRate_t = log(CrashRate_mean +1),
    CommunitySafety_t = log(CommunitySafety_mean),
    EmploymentGrowth_t = log(EmploymentGrowth_mean),
    BusDelay_t = log(BusDelay_mean),
    ModeShare_t = log(ModeShare_mean),
    PTAL_t = log(PTAL_mean),
    ImperviousSurface_t = log(ImperviousSurface_mean)
    ) %>%
  select(-c(CrashRate_mean, CommunitySafety_mean, EmploymentGrowth_mean, BusDelay_mean, ModeShare_mean,
            PTAL_mean, ImperviousSurface_mean))

head(modelling_subset)

#
modelling_subset %>%
  select(-c('CyclingLane_cat', 'HousingDiversity_mean', 'HousingDiversity_mean', 'PopGrowth_cat', 'TreeCanopy_cat', 'UHI_cat')) %>%
  tidyr::gather(var, val, -ULI_mean) %>% 
  ggplot() +
  aes(y = ULI_mean, x = val) +
  geom_point(size = 1) + 
  geom_smooth(method = 'lm', formula = y ~x) +
  facet_wrap(~var, scales = 'free') +
  theme_minimal()



# Univariate simple linar regression model
gtsummary::tbl_uvregression(
  data = modelling_subset,
  method = lm,
  y = ULI_mean,
  hide_n = TRUE
)

# Full multiple linear regression model

lm1 <- lm(ULI_mean~., data = modelling_subset)

# Univariate model

lm_impervious_surface <- lm(ULI_mean ~ ImperviousSurface_t, data = modelling_subset)
summary(lm_impervious_surface)$r.squared
AIC(lm_impervious_surface)

lm_bus_delay <- lm(ULI_mean ~ BusDelay_t, data = modelling_subset)
summary(lm_bus_delay)$r.squared
AIC(lm_bus_delay)

lm_PTAL<- lm(ULI_mean ~ PTAL_t, data = modelling_subset)
summary(lm_PTAL)$r.squared
AIC(lm_PTAL)

# Linear regression model with ALL strong correlation  var (> .5)
lm_strong_cor <- lm(
  ULI_mean ~ PTAL_t + BusDelay_t + PopDensity_mean + ImperviousSurface_t + ModeShare_t  + CrashRate_t,
  data = modelling_subset)

lm_strong_cor %>% summary()
AIC(lm_strong_cor)

drop1(lm_strong_cor)

modelling_subset%>%
  select(c( ULI_mean, PTAL_t, BusDelay_t, PopDensity_mean, ImperviousSurface_t, ModeShare_t, CrashRate_t)) %>%
  tidyr::gather(var, val, -ULI_mean) %>% 
  ggplot() +
  aes(y = ULI_mean, x = val) +
  geom_point(size = 1) + 
  geom_smooth(method = 'lm', formula = y ~ x, se = T) +
  facet_wrap(~var, scales = 'free') +
  theme_minimal()

# Investigating relationship between bus delay and PTAL

car::vif(lm(ULI_mean ~ BusDelay_t + PTAL_t, data = modelling_subset))

cor(modelling_subset$PTAL_t, modelling_subset$BusDelay_t)

modelling_subset %>%
  ggplot() +
  aes(x = BusDelay_t, y = PTAL_t) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal()

lm(ULI_mean ~ BusDelay_t*PTAL_t , data = modelling_subset) %>% summary()

lm(PTAL_t ~ BusDelay_t, data = modelling_subset) %>% summary()

lm_strong_cor1 <- lm(
  ULI_mean ~ BusDelay_t + PopDensity_mean + ImperviousSurface_t + ModeShare_t  + CrashRate_t,
  data = modelling_subset)

lm_strong_cor2 <- lm(
  ULI_mean ~ PTAL_t + PopDensity_mean + ImperviousSurface_t + ModeShare_t  + CrashRate_t,
  data = modelling_subset)

lmtest::waldtest(lm_strong_cor,
                 lm_strong_cor2)

summary(lm_strong_cor)
summary(lm_strong_cor2)


#### Modelling using forward approach

# Variable with p-value less than 0.25

lm_initial <- lm(
  ULI_mean ~ LandDivision_mean + StreetSpacePedestrian_mean + RoadSafety_mean + Legibility_mean +
    UHI_cat + PopGrowth_cat + CrashRate_t + CommunitySafety_t + EmploymentGrowth_t + BusDelay_t + 
    ModeShare_t + PTAL_t + ImperviousSurface_t, data = modelling_subset
)

# COmparing initial model to full model
sum_lm_i <- jtools::summ(lm_initial, digits = 3)


summary(lm_initial)$r.squared

AIC(lm_initial)
AIC(lm1)
summary(lm1)$r.squared
lm1 %>% summary()

drop1(lm_initial)

# dropping Road Safety 
lm_i_1 <- lm(
  ULI_mean ~ LandDivision_mean + StreetSpacePedestrian_mean + Legibility_mean +
    UHI_cat + PopGrowth_cat + CrashRate_t + CommunitySafety_t + EmploymentGrowth_t + BusDelay_t + 
    ModeShare_t + PTAL_t + ImperviousSurface_t, data = modelling_subset
)
stargazer::stargazer(lm_initial, lm_i_1, type = 'text')

drop1(lm_i_1)

# dropping Land Division

lm_i_2 <- lm(
  ULI_mean ~StreetSpacePedestrian_mean + Legibility_mean +
    UHI_cat + PopGrowth_cat + CrashRate_t + CommunitySafety_t + EmploymentGrowth_t + BusDelay_t + 
    ModeShare_t + PTAL_t + ImperviousSurface_t, data = modelling_subset
)
stargazer::stargazer(lm_i_1, lm_i_2, type = 'text')

drop1(lm_i_2)

# dropping BusDelay

lm_i_3 <- lm(
  ULI_mean ~StreetSpacePedestrian_mean + Legibility_mean +
    UHI_cat + PopGrowth_cat + CrashRate_t + CommunitySafety_t + EmploymentGrowth_t + 
    ModeShare_t + PTAL_t + ImperviousSurface_t, data = modelling_subset
)
stargazer::stargazer(lm_i_2, lm_i_3, type = 'text')

drop1(lm_i_3)

# dropping Community Safety

lm_i_4 <- lm(
  ULI_mean ~StreetSpacePedestrian_mean + Legibility_mean +
    UHI_cat + PopGrowth_cat + CrashRate_t + EmploymentGrowth_t + 
    ModeShare_t + PTAL_t + ImperviousSurface_t, data = modelling_subset
)
stargazer::stargazer(lm_i_3, lm_i_4, type = 'text')

drop1(lm_i_4)
AIC(lm_i_4)

# No more variable to drop
# Checking if any of the original excluded variable should be included

lm_i_tc <- lm(
  ULI_mean ~ TreeCanopy_cat + StreetSpacePedestrian_mean + Legibility_mean +
    UHI_cat + PopGrowth_cat + CrashRate_t + EmploymentGrowth_t + 
    ModeShare_t + PTAL_t + ImperviousSurface_t, data = modelling_subset)
lm_i_cl <- lm(
  ULI_mean ~ CyclingLane_cat + StreetSpacePedestrian_mean + Legibility_mean +
    UHI_cat + PopGrowth_cat + CrashRate_t + EmploymentGrowth_t + 
    ModeShare_t + PTAL_t + ImperviousSurface_t, data = modelling_subset)
lm_i_wa <- lm(
  ULI_mean ~ Waterways_mean + StreetSpacePedestrian_mean + Legibility_mean +
    UHI_cat + PopGrowth_cat + CrashRate_t + EmploymentGrowth_t + 
    ModeShare_t + PTAL_t + ImperviousSurface_t, data = modelling_subset)
lm_i_hd <- lm(
  ULI_mean ~ HousingDiversity_mean + StreetSpacePedestrian_mean + Legibility_mean +
    UHI_cat + PopGrowth_cat + CrashRate_t + EmploymentGrowth_t + 
    ModeShare_t + PTAL_t + ImperviousSurface_t, data = modelling_subset)

AIC(lm_i_tc)
AIC(lm_i_cl)
AIC(lm_i_wa)
AIC(lm_i_hd)
AIC(lm_i_4)

# Adding waterways reduce the AIC from 1319 to 1275
# Checking with stargazer

plot(lm_i_4)
summary(lm_i_4)

caret::R2(fitted(lm_i_4), modelling_subset$ULI_mean)

modelling_subset$ImperviousSurface_t

stargazer::stargazer(lm_i_4, lm_i_wa, type = 'text')
# Number of observation decrease due to missing data in waterways

AIC(lm_i_4)
AIC(lm1)

stargazer::stargazer(lm_i_4, lm1, type = 'text')

MASS::stepAIC(lm(ULI_mean ~., data = modelling_subset %>% drop_na()), trace = F)

# Interpretation final model

summ(lm_i_4)
summ(lm_i_4)$coeftable %>% kbl(format = 'html', digits = 2) %>% kable_styling(latex_options = "hold_position")

equatiomatic::extract_eq(lm_i_4, use_coefs = T)

IQR(modelling_subset$CrashRate_t)
IQR(modelling_subset$EmploymentGrowth_t)
IQR(modelling_subset$ModeShare_t)
IQR(modelling_subset$PTAL_t)
IQR(modelling_subset$ImperviousSurface_t)

quantile(final_analysis$ImperviousSurface_mean)


# Spatial autocorrelation using Moran's Index


sa2_descr <- final_df %>% select(c('SA2_CODE21', 'SA2_NAME21'))
sa2_descr$resid <- lm_i_4$residuals
sa2_descr$fit <- lm_i_4$fitted.values

sa2_descr %>%
  ggplot() +
  aes(x = fit, y = resid) +
  geom_text(label = sa2_descr$SA2_NAME21)


sa2_res_vect <- merge(x = SA2_NSW, y = sa2_descr, na.rm = TRUE)
plot(sa2_res_vect, 'resid', breaks = 5, col = c('navyblue', 'purple', 'blue', 'lightgreen', 'skyblue'))

pol_res <- sf::st_as_sf(sa2_res_vect)
w <- spdep::poly2nb(pol_res, queen = TRUE)

list_res <- spdep::nb2listw(w, zero.policy = T)
spdep::lm.morantest(lm_i_4, list_res)

plot(lm_i_4)



# Plot change in R-squared

AIC(lm_i_4)
lm(
  ULI_mean ~Legibility_mean + UHI_cat+PopGrowth_cat
  +CrashRate_t + EmploymentGrowth_t +ModeShare_t +PTAL_t + ImperviousSurface_t
       , data = modelling_subset
) %>% AIC()


plot_data_auo <- data.frame(
  Variable = c('StreetSpacePedestrian', 'Legibility', 'Urban Heat', 'Population Growth',
               'Crash Rate', 'Employment Growth', 'Mode Share', 'PTAL', 'Impervious Surface'),
  R.squared = c(0.65, 0.65, 0.63, 0.64, 0.62, 0.65, 0.64, 0.61, 0.65),
  AIC = c(1323.471, 1324.095, 1343.932, 1328.352, 1349.924, 1324.442, 1335.719, 1364.441,1324.085 )
) %>% mutate(
  R2_dif = 0.66 - R.squared,
  AIC_diff = 1319.368 - AIC
) 


plot_data_auo %>%
  ggplot() +
  aes(x = Variable, y = R2_dif, fill = AIC_diff) +
  geom_bar(stat = 'identity') +
  # lims(y = c(-2.5,3.5)) +
  labs(x='', y = 'Decrease in R-squared') +
  scale_fill_continuous(type = "viridis") +
  labs(fill="Loss in AIC") +
  theme_minimal()


drop1(lm_i_4)
