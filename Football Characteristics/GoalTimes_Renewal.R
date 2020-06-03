# Clean up / libraries ----
if(!is.null(dev.list())) dev.off()                  # Clear plots
rm(list=ls())                                       # Clean workspace
cat("\014")                                         # Clear console

source("Libraries.R")
source("Football Characteristics/Functions/WeibullRenewal_sim.R")


# Load data ----
load("Football Characteristics/Data/EPL_list_data.Rdata")


# Get goal time data ------------------------------------------------------

home_goals_time <- sort(Football[[1]][[2]])
away_goals_time <- sort(Football[[1]][[3]])

l <- length(Football)

pb <- txtProgressBar(min = 2, max = l, style = 3)

for (i in 2:l) {
  home_goals_time <- c(home_goals_time, sort(Football[[i]][[2]]))
  away_goals_time <- c(away_goals_time, sort(Football[[i]][[3]]))
  setTxtProgressBar(pb, i)
}

close(pb)

hom <- home_goals_time[-(which(home_goals_time>93))]
awa <- away_goals_time[-(which(away_goals_time>93))]

l <- length(hom) - length(awa)

dat <- tibble(Home = hom) %>% 
  bind_cols(tibble(Away = c(awa, rep(NA, l)))) %>% 
  gather(Side, value, 1:2) %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away")))

gf_emp_dens <- gf_dhistogram(~ value | Side, data = dat, alpha = 0.75, binwidth = 3, center = 1.5, color = "white", fill = "darkgray") %>% 
  gf_density( ~ value | Side, data = dat, alpha = 1, binwidth = 1, color = "orangered", fill = "white", geom="line") + 
  labs(x="Minute", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))
gf_emp_dens


# Goal times in Weibull Renewal -------------------------------------------

Countr::evWeibullCount(xmax = 1000, shape = 0.9062415, scale = 1.4507183)
Countr::evWeibullCount(xmax = 1000, shape = 0.8491849, scale = 1.0583299)

theta_hom <- c((1/1.4507183)^0.9062415, 0.9062415)
sim_hom <- WeibullRenewal_sim(theta_hom, m = 2000000)

theta_awa <- c((1/1.0583299)^0.8491849, 0.8491849)
sim_awa <- WeibullRenewal_sim(theta_awa, m = 2000000)

dat2 <- tibble(Home = sim_hom * 93) %>% 
  bind_cols(tibble(Away = c(sim_awa * 93, rep(NA, length(sim_hom) - length(sim_awa))))) %>% 
  pivot_longer(cols = 1:2, names_to = "Side", values_to = "value") %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away")))

p_dens <- gf_density(~ value | Side, data = dat2, fill = "white", color = "black", alpha = 1, linetype = "dashed", geom="line") + 
  ylim(c(0,0.015)) +
  labs(x="Minute", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))
p_dens

p_dens_full <- p_dens %>% gf_density( ~ value | Side, data = dat, alpha = 1, binwidth = 1, color = "orangered", fill = "white", geom="line")
p_dens_full

# ggsave("WeibullRenewalIntensityDensity.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)



# Non Score dist consistent -----------------------------------------------


Countr::evWeibullCount(xmax = 1000, shape = 1.125896, scale = 1.659439)
Countr::evWeibullCount(xmax = 1000, shape = 1.148385, scale = 1.221129)

theta_hom <- c((1/1.659439)^1.125896, 1.125896)
sim_hom <- WeibullRenewal_sim(theta_hom, m = 10000000)

theta_awa <- c((1/1.221129)^1.148385 , 1.148385)
sim_awa <- WeibullRenewal_sim(theta_awa, m = 10000000)

dat2 <- tibble(Home = sim_hom * 93) %>% 
  bind_cols(tibble(Away = c(sim_awa * 93, rep(NA, length(sim_hom) - length(sim_awa))))) %>% 
  pivot_longer(cols = 1:2, names_to = "Side", values_to = "value") %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away")))

p_dens <- gf_density(~ value | Side, data = dat2, fill = "white", color = "black", alpha = 1, linetype = "dashed", geom="line") + 
  ylim(c(0,0.015)) +
  labs(x="Minute", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))
p_dens

p_dens_full <- p_dens %>% gf_density( ~ value | Side, data = dat, alpha = 1, binwidth = 1, color = "orangered", fill = "white", geom="line")
p_dens_full

# ggsave("WeibullRenewalIntensityDensity_NonConsistent.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)

