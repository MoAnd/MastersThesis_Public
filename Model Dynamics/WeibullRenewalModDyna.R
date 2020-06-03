
# Housekeeping ------------------------------------------------------------

rm(list = ls())  # Clear all
graphics.off()   # Close all
cat("\014")      # Clears console

source("Libraries.R")
source("Model Dynamics/Functions_and_Data/Weibull_renewal_match_sim.R")
source("Model Dynamics/Functions_and_Data/Expected_goal_rest.R")


t <- 0
Ti <- 1
n <- 10 * 90 + 1

dt <- (Ti-t) / (n-1)

time <- seq(from = t, to = Ti, length.out = n)

alph_h <- 1.4
bet_h <- 1.2
alph_a <- 1.1
bet_a <- 1.2

theta <- c(alph_h, bet_h, alph_a, bet_a)


# Match simulation --------------------------------------------------------

# match <- Weibull_renewal_match_sim(theta)
# save(match, file = "R Scripts/Model Dynamics/WeibullRenewalMatchSim.Rdata")

load("Model Dynamics/Functions_and_Data/WeibullRenewalMatchSim.Rdata")
match

{home <- rep(NA, length(time))
  for(i in 1:length(match$Home)){
    home[first(which(match$Home[i] <= time))] <- i
  }
  home[1] <- 0
  home <- na.locf(home)
  
  away <- rep(NA, length(time))
  for(i in 1:length(match$Away)){
    away[first(which(match$Away[i] <= time))] <- i
  }
  away[1] <- 0
  away <- na.locf(away)}


df <- tibble(Time = time, Home = home, Away = away) 

# exp_sim <- Expected_goal_rest(match = match, theta = theta, par = 25, time = time, Ti = Ti)
# 
# save(exp_sim, file = "R Scripts/Model Dynamics/WeibullRenewalSim.Rdata")

load("Model Dynamics/Functions_and_Data/WeibullRenewalSim.Rdata")

S_home <- rep(NA, length(time))
S_away <- rep(NA, length(time))

for(i in 1:length(exp_sim)){
  S_home[i] <- exp_sim[[i]]$Home
  S_away[i] <- exp_sim[[i]]$Away
}

df <- df %>% bind_cols("S_home" = S_home) %>% bind_cols("S_away" = S_away)

plt <- df %>% pivot_longer(2:5, names_to = "Side", values_to = "val") %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away", "S_home", "S_away"))) %>% 
  ggplot(aes(x = Time, y = val, col = Side, linetype = Side)) + 
  geom_step() +
  scale_color_manual(values = c("Blue", "red", "steelblue", "darkred"), labels = c(expression(N^1), expression(N^2), expression(S^1), expression(S^2))) +
  scale_linetype_manual(values = c("dashed", "dashed", "solid", "solid"),  labels = c(expression(N^1), expression(N^2), expression(S^1), expression(S^2))) +
  labs(x="Time", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"),
        legend.title = element_blank())

plt

# ggsave("WeibullRenewal.png", device = "png", path = "Plots/ModelDynamics", width = 8, height = 4)

{ind1 <- which(c(0,diff(df$Home)) > 0)
  Nt_last1 <- rep(NA,nrow(df))
  Nt_last1[1] <- 0
  for(i in ind1){
    Nt_last1[i+1] <- df$Time[i]
  }
  Nt_last1 <- na.locf(Nt_last1)
  df <- df %>% bind_cols(Nt_last1 = Nt_last1)
  
  ind2 <- which(c(0,diff(df$Away)) > 0)
  Nt_last2 <- rep(NA,nrow(df))
  Nt_last2[1] <- 0
  for(i in ind2){
    Nt_last2[i+1] <- df$Time[i]
  }
  Nt_last2 <- na.locf(Nt_last2)
  df <- df %>% bind_cols(Nt_last2 = Nt_last2)}


plt2 <- df %>% mutate(Home_int = alph_h * bet_h * (Time - Nt_last1)^(bet_h-1)) %>% mutate(Away_int = alph_a * bet_a * (Time - Nt_last2)^(bet_a-1)) %>% 
  dplyr::select(Time, Home, Away, Home_int, Away_int) %>% 
  pivot_longer(2:5, names_to = "Side", values_to = "val") %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away", "Home_int", "Away_int"))) %>% 
  ggplot(aes(x = Time, y = val, col = Side, linetype = Side)) + 
  geom_step() +
  scale_color_manual(values = c("Blue", "red", "blue", "red"), labels = c(expression(N^1), expression(N^2), expression(lambda^1), expression(lambda^2))) +
  scale_linetype_manual(values = c("dashed", "dashed", "solid", "solid"),  labels = c(expression(N^1), expression(N^2), expression(lambda^1), expression(lambda^2))) +
  labs(x="Time", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"),
        legend.title = element_blank())

plt2

# ggsave("WeibullRenewal_Intensity.png", device = "png", path = "Plots/ModelDynamics", width = 8, height = 4)
