# Clean up / libraries ----
if(!is.null(dev.list())) dev.off()                  # Clear plots
rm(list=ls())                                       # Clean workspace
cat("\014")                                         # Clear console

source("Libraries.R")
source("Football Characteristics/Functions/WeibullProcess_sim.R")
source("Football Characteristics/Functions/WeibullHazard_pmf_fit.R")

# Load empirical data -----------------------------------------------------

load("Football Characteristics/Data/EPL_list_data.Rdata")

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

# ggsave("GoalTimesEmpericalDensity.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)


# Density simulation  -----------------------------------------------------

# tictoc::tic()
# optim(par = 1.15, fn = WeibullHazard_pmf_fit, emperical_dat = home_goals_time, mean_val = 1.536491, end_time = 93, m = 1000000, lower = 1, upper = 1.5, method = "Brent")
# tictoc::toc()

# tictoc::tic()
# optim(par = 1.15, fn = WeibullHazard_pmf_fit, emperical_dat = away_goals_time, mean_val = 1.138246, end_time = 93, m = 1000000, lower = 1, upper = 1.5, method = "Brent")
# tictoc::toc()


theta_hom <- c(1.536491/1.125896, 1.125896)
sim_hom <- WeibullProcess_sim(theta_hom, m = 20000000)

theta_awa <- c(1.138246/1.148385, 1.148385)
sim_awa <- WeibullProcess_sim(theta_awa, m = 20000000)

dat2 <- tibble(Home = sim_hom * 93) %>% 
  bind_cols(tibble(Away = c(sim_awa * 93, rep(NA, length(sim_hom) - length(sim_awa))))) %>% 
  gather(Side, value) %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away")))

# gf_dhistogram(~ Home, data = dat2, binwidth = 0.25, center = 0.125)

p_dens <- gf_density(~ value | Side, data = dat2, fill = "white", color = "black", alpha = 1, linetype = "dashed", geom="line") + 
  ylim(c(0,0.015)) +
  labs(x="Minute", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

# p_dens

p_dens_full <- p_dens %>% gf_density( ~ value | Side, data = dat, alpha = 1, binwidth = 1, color = "orangered", fill = "white", geom="line")
p_dens_full

# ggsave("WeibullIntensityDensity.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)










