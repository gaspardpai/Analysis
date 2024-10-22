#######--------Analysis SIR Model-----#####

library(EpiModel)
library(tidyverse)


#fitting closed model

p1 = param.dcm(inf.prob = 0.25, act.rate = 1, rec.rate = 1/14) 
init_1 = init.dcm(s.num = 100000, i.num = 1, r.num = 0)
control_1 = control.dcm(type = "SIR", nsteps = 200)
mod_closed_1 = dcm(param = p1, init = init_1, control = control_1)

#convert to tibble to allow plotting using ggplot

options(scipen=999) # remove scientific notation
df_mc1 = as.tibble(mod_closed_1) 
df_mc1

#plot of S(t), I(t) and R(t)
df_mc1 %>% ggplot() +
  aes(x = time) +
  geom_line(aes(y = s.num, color = "Susceptible"), size = 1.5) +
  geom_line(aes(y = r.num, color = "Recovered"), size = 1.5)  +
  geom_line(aes(y = i.num, color = "Infected"), size = 1.5) +
  labs(x = "Time (days)",
       y = "Number of individuals",
       color = "Compartment") +
  theme_bw() +
  theme(text = element_text(size = 14))

#finding peak day
peak_mc1 = df_mc1 %>% 
  filter(i.num == max(i.num)) %>%
  select(c(time, i.num))
peak_mc1

#creating R dataframe
R_mc1 = df_mc1 %>%
  mutate(R = si.flow / ir.flow) %>%
  select(time, R)

#finding R0 at start of epidemic
head(R_mc1, 1)

#Effective reproductive number at peak day
R_mc1 %>%
  filter(time == peak_mc1$time)

#Effective reproductive number at final day
tail(R_mc1, 1)

###---- Question 2----###

p2 = param.dcm(inf.prob = 0.25, act.rate = 1, rec.rate = 1/7) # new recovery rate
mod_closed_2 = dcm(param = p2, init = init_1, control = control_1) #initial condition and control are unchanged

#convert to tibble to allow analysis using dplyr instead of base R

df_mc2 = as.tibble(mod_closed_2) 
df_mc2

#plot of S(t), I(t) and R(t)
df_mc2 %>% ggplot() +
  aes(x = time) +
  geom_line(aes(y = s.num, color = "Susceptible"), size = 1.5) +
  geom_line(aes(y = r.num, color = "Recovered"), size = 1.5)  +
  geom_line(aes(y = i.num, color = "Infected"), size = 1.5) +
  labs(x = "Time (days)",
       y = "Number of individuals",
       color = "Compartment") +
  theme_bw() +
  theme(text = element_text(size = 14))

#finding peak day
peak_mc2 = df_mc2 %>% 
  filter(i.num == max(i.num)) %>%
  select(c(time, i.num))
peak_mc2

#creating R dataframe
R_mc2 = df_mc2 %>%
  mutate(R = si.flow / ir.flow) %>%
  select(time, R)

#finding R0 at start of epidemic
head(R_mc2, 1)

#Effective reproductive number at peak day
R_mc2 %>%
  filter(time == peak_mc2$time)

#Effective reproductive number at final day
tail(R_mc2, 1)



# open model with birth and mortality

p_open1 = param.dcm(inf.prob = 0.25, act.rate = 1, rec.rate = 1/14,
                   a.rate = 1/95, ds.rate = 1/100, di.rate = 1/100, dr.rate = 1/100) #mortality rate is similar across all compartments

mod_open_1 = dcm(param = p_open1, init = init_1, control = control_1) #only the param object is different compared to the other models


df_mo1 = as_tibble(mod_open_1) #convert to tibble for ggplot

df_mo1 %>% ggplot() +
  aes(x = time) +
  geom_line(aes(y = s.num, color = "Susceptible"), size = 1.5) +
  geom_line(aes(y = r.num, color = "Recovered"), size = 1.5)  +
  geom_line(aes(y = i.num, color = "Infected"), size = 1.5) +
  labs(x = "Time (days)",
       y = "Number of individuals",
       color = "Compartment") +
  theme_bw() +
  theme(text = element_text(size = 14))


#finding peak day
peak_mo1 = df_mo1 %>% 
  filter(i.num == max(i.num)) %>%
  select(c(time, i.num))
peak_mo1

#creating R dataframe
R_mo1 = df_mo1 %>%
  mutate(R = si.flow / ir.flow) %>%
  select(time, R)

#finding R0 at start of epidemic
head(R_mo1, 1)

#Effective reproductive number at peak day
R_mo1 %>%
  filter(time == peak_mo1$time)

#Effective reproductive number at final day
tail(R_mo1, 1)



## Open model 2

control_2 = control.dcm(type = "SIR", nsteps = 700) #increase number of days to 700
mod_open_2 = dcm(param = p_open1, init = init_1, control = control_2) #only the control object is different compared to the other models


df_mo2 = as_tibble(mod_open_2)

df_mo2 %>% ggplot() +
  aes(x = time) +
  geom_line(aes(y = s.num, color = "Susceptible"), size = 1.5) +
  geom_line(aes(y = r.num, color = "Recovered"), size = 1.5)  +
  geom_line(aes(y = i.num, color = "Infected"), size = 1.5) +
  labs(x = "Time (days)",
       y = "Number of individuals",
       color = "Compartment") +
  theme_bw() +
  theme(text = element_text(size = 14))

# determine second peak by filtering for days between 150 and 300 and
# select maximum number of infected during this period

second_peak_mo2 = df_mo2 %>% 
  filter(time >= 150 & time <= 300) %>%
  filter(i.num == max(i.num)) %>%
  select(c(time, i.num))

second_peak_mo2

#creating a Re dataframe to obtain value during second peak and day 700

R_mo2 = df_mo2 %>%
  mutate(R = si.flow / ir.flow) %>%
  select(time, R)

#Effective reproductive number at second peak day
R_mo2 %>%
  filter(time == second_peak_mo2$time)

#Effective reproductive number at final day
tail(R_mo2, 10)

# Fit new SIR model to find the Re at day 2000
mod_simulation = dcm(param = p_open1, init = init_1, 
                     control = control.dcm(type = "SIR", nsteps = 2000))

#convert to tibble and use dplyr to obtain Re on final day
mod_simulation %>%
  as.tibble() %>%
  mutate(R = si.flow / ir.flow) %>%
  select(time, R) %>%
  tail(1)

# plot of SIR dynamis in open population over 2000 days (not included in report)
mod_simulation %>%
  as.tibble() %>%
  ggplot() +
  aes(x = time) +
  geom_line(aes(y = s.num, color = "Susceptible"), size = 1.5) +
  geom_line(aes(y = r.num, color = "Recovered"), size = 1.5)  +
  geom_line(aes(y = i.num, color = "Infected"), size = 1.5) +
  labs(x = "Time (days)",
       y = "Number of individuals",
       color = "Compartment") +
  theme_bw() +
  theme(text = element_text(size = 14))
  

## Open model 3

p_open3 = param.dcm(inf.prob = 0.25, act.rate = 1.5, rec.rate = 1/7,
                    a.rate = 1/95, ds.rate = 1/100, di.rate = 1/100, dr.rate = 1/100)

mod_open_3 = dcm(param = p_open3, init = init_1, control = control_1)

df_mo3 = as_tibble(mod_open_3)

df_mo3 %>% ggplot() +
  aes(x = time) +
  geom_line(aes(y = s.num, color = "Susceptible"), size = 1.5) +
  geom_line(aes(y = r.num, color = "Recovered"), size = 1.5)  +
  geom_line(aes(y = i.num, color = "Infected"), size = 1.5) +
  labs(x = "Time (days)",
       y = "Number of individuals",
       color = "Compartment") +
  theme_bw() +
  theme(text = element_text(size = 14))

#finding peak day
peak_mo3 = df_mo3 %>% 
  filter(i.num == max(i.num)) %>%
  select(c(time, i.num))
peak_mo3

#creating R dataframe
R_mo3 = df_mo3 %>%
  mutate(R = si.flow / ir.flow) %>%
  select(time, R)

#finding R0 at start of epidemic
head(R_mo3, 1)

#Effective reproductive number at peak day
R_mo3 %>%
  filter(time == peak_mo3$time)

#Effective reproductive number at final day
tail(R_mo3, 1)


