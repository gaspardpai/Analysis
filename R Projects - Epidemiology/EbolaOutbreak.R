
###----- Ebola Outbreak ------###

# loading required packages

library(readxl)
library(incidence)
library(tidyverse)
library(epicontacts)
library(epitrix)
library(outbreaks)
library(EpiEstim)
library(projections)
set.seed(0)


# loading datasets

linelist = read_excel("linelist_early_v2.xlsx", na = c("", "NA"))
contacts = read_excel("contacts_early.xlsx", na = c("", "NA"))

####---- Using linelist data ----####

# Epicurve daily and weekly

#compute incidence dayly and weekly based on date of onset using incidence package

i_day = incidence(linelist$date_of_onset, interval = 1)
i_week = incidence(linelist$date_of_onset, interval = 7)

i_day_df = as.tibble(i_day) %>%
  rename(date_onset = dates, n = counts)

i_week_df = as.tibble(i_week)%>%
  rename(date_onset = dates, n = counts)

i_day_df %>% ggplot() +
  aes(x = date_onset, y = n) +
  geom_bar(stat = 'identity', fill = "#C00000") +
  labs(x = "Date of onset",
       y = "Number of cases") +
  theme_bw()

i_week_df %>% ggplot() +
  aes(x = date_onset, y = n) +
  geom_bar(stat = 'identity', fill = "#C00000") +
  labs(x = "Date of onset",
       y = "Number of cases") +
  theme_bw()

#Log(weekly incidence)

i_week_df %>% ggplot() + 
  aes(x = date_onset, y = log(n)) +
  geom_point(col = "#C00000", size = 3) + 
  geom_smooth(method = "lm", linetype = "dashed") + #fit linear regression model
  scale_x_incidence(i_week) +
  labs(x = "Date of onset",
       y = "log weekly incidence") +
  theme_bw()

#Log-linear model and weekly incidence

#fit log-linear model

mod1 = incidence::fit(i_week)
mod1$model %>% summary()
get_info(mod1, "r") #growth rate
dt_week = get_info(mod1, "doubling") #doubling time of outbreak per day
dt_week

#different method for growth rate and doubling time
mod1$model$coefficients['dates.x']
log(2) / mod1$model$coefficients['dates.x']

plot(i_week, fit = mod1, color = "#C00000")


####---- Using contact tracing data ----####


#create epi contact network object

epi_contacts = make_epicontacts(linelist = linelist,  
                                 contacts = contacts, 
                                 id = "case_id", 
                                 from = "infector", 
                                 to = "case_id",
                                 directed = T)

#Emperical distribution of the serial interval

#date_of_onset was stored as character which caused issue when computing pairwise difference
epi_contacts$linelist$date_of_onset = as.Date(epi_contacts$linelist$date_of_onset) 

si_obs = get_pairwise(epi_contacts, "date_of_onset") #difference in date of onset between infectors and infected individuals
summary(si_obs)

hist(si_obs, breaks = 0:15,
     xlab = "Days after symptom onset", ylab = "Frequency",
     main = "Serial interval (empirical distribution)",
     col = "#C00000", border = "white")

#estimating distribution of serial interval based on gamma distribution

si_fit <- fit_disc_gamma(si_obs, w = 1) #from the epitrix package
si_fit # obtain mean from gamma distribution
si = si_fit$distribution


# Histogram of emirical distribution with curve for gamma distribution
hist(si_obs, xlab = "Days after symptom onset", ylab = "Frequency",
     main = "", col = "#C00000", border = "white",
     50, ylim = c(0, 0.15), freq = FALSE, breaks = 0:35)
points(0:35, si$d(0:35), col = "blue", pch = 20)
points(0:35, si$d(0:35), col = "blue", type = "l", lty = 2, lwd = 3)
legend(x = "topright",bty = 'n', lty = c(1,2), lwd = c(7,3),
       col= c("#C00000","blue"), title.adj = 0.2, legend=c("Empirical Distribution", "Gamma Distribution")) 


# overall estimate of reproductive number based on gamma distribution

# use make_config function from EpiEstim to define settings for estimate_R function

config = make_config(mean_si = si_fit$mu, std_si = si_fit$sd, t_start = 2, t_end = length(i_day$counts)) 

#estimate overall reproductive number based on the whole period
R = estimate_R(incid = i_day, method = "parametric_si", config = config) 
plot(R, legend = FALSE)
R_mean = R$R$`Mean(R)`
R_median = R$R$`Median(R)`
R_mean
R_median

# project incident cases 30 days past final observation with epicurve using projections package
epi_projection = project(i_day,
                        R = R_median, 
                        si = si,      # serial interval based om gamma distribution
                        n_sim = 100,  # simulate 100 trajectories
                        n_days = 30,  # over 30 days
                        R_fix_within = TRUE) # keep the same value of R every day

# Visualisation of incidence projections in June

plot(i_day,
     color = "red", border = "black")%>% 
  add_projections(epi_projection, c(0.025, 0.5, 0.975))

# forecast of cumulative incidence June
june_incidence = colSums(epi_projection) #cumulative incidence for each of the 100 simulation
mean_june= mean(june_incidence) # mean of the cumulative incidence
mean_june


# Confidence interval from projections

projection_summary = summary(epi_projection)
lower_limit = sum(projection_summary$`quantiles.2.5%`) #cumulative incidence for June using 2.5 quantile
upper_limit = sum(projection_summary$`quantiles.97.5%`)#cumulative incidence for June using 97.5 quantile
lower_limit
upper_limit


# Reproductive number using time-varying estimate

config_time = make_config(list(mean_si = si_fit$mu, std_si = si_fit$sd))  

R_time = estimate_R(incid = i_day, method = "parametric_si", config = config_time) 

head(R_time$R[, c("t_start", "t_end", "Median(R)", 
              "Quantile.0.025(R)", "Quantile.0.975(R)")])

# look at the most recent Rt estimates:

tail(R_time$R[, c("t_start", "t_end", "Median(R)", 
              "Quantile.0.025(R)", "Quantile.0.975(R)")])

max(R_time$R$`Mean(R)`) # maximum reproductive number based on the mean

# Plot the estimate of R over time:

plot(R_time, legend = FALSE)











     