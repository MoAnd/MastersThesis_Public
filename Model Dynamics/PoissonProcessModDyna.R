# Housekeeping ------------------------------------------------------------
rm(list = ls())  # Clear all
graphics.off()   # Close all
cat("\014")      # Clears console

source("Libraries.R")
source("Football Characteristics/Functions/WeibullProcess_sim.R")
source("Football Characteristics/Functions/WeibullRenewal_sim.R")

t <- 0
T <- 1
n <- 100 * 90 + 1

dt <- (T-t) / (n-1)

time <- seq(from = t, to = T, length.out = n)

alph_h <- 1.4
bet_h <- 1
alph_a <- 1.1
bet_a <- 1


# Home sim ----------------------------------------------------------------

sim <- WeibullProcess_sim(theta = c(alph_h, bet_h), m = 1, listed = TRUE)

home <- rep(NA, length(time))

for(i in 1:length(sim)){
  home[first(which(sim[i] <= time))] <- i
}

home[1] <- 0

home <- na.locf(home)


# Away sim ----------------------------------------------------------------

sim <- WeibullProcess_sim(theta = c(alph_a, bet_a), m = 1, listed = TRUE)

away <- rep(NA, length(time))

for(i in 1:length(sim)){
  away[first(which(sim[i] <= time))] <- i
}

away[1] <- 0

away <- na.locf(away)


# Create df ---------------------------------------------------------------

df <- tibble(Time = time, Home = home, Away = away)

df %>% pivot_longer(2:3, names_to = "Side", values_to = "val") %>% ggplot(aes(x = Time, y = val, col = Side)) + geom_step()


df <- df %>% mutate(S_home = alph_h * (1-Time^bet_h) + Home) %>% mutate(S_away = alph_a * (1-Time^bet_a) + Away)

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

# ggsave("PoissonProcess.png", device = "png", path = "Plots/ModelDynamics", width = 8, height = 4)

plt2 <- df %>% mutate(Home_int = rep(alph_h, nrow(df))) %>% mutate(Away_int = rep(alph_a, nrow(df))) %>% 
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
        legend.title = element_blank()) +
  ylim(0, 1.6)

plt2

# ggsave("PoissonProcess_Intensity.png", device = "png", path = "Plots/ModelDynamics", width = 8, height = 4)
