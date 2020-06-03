# Housekeeping + Source ---------------------------------------------------

rm(list = ls())  # Clear all
graphics.off()   # Close all
cat("\014")      # Clears console

source("Libraries.R")
source("Calibration/Functions/clean_calibration_data.R")
source("Calibration/Functions/n_calib_checker.R")
source("Pricing/WeibProcess_Pricing.R")
source("Pricing/WeibRenewal_Pricing.R")

# Load the data -----------------------------------------------------------

load("Odds_df.Rdata")
load("Calibration/CalibratedData/WeibProcess_opt.Rdata")


# Number of bets in calibration -------------------------------------------

names(Odds_df) <- names(Odds_df) %>% str_replace_all("The Draw", replacement = "Draw")

# Odds_df <- Odds_df[,-c(16:19, 22:25)]
# Odds_df[62,5:ncol(Odds_df)] <- NA
# Odds_df[3,22] <- NA
# Odds_df[86,c(22,24)] <- NA
# Odds_df[127,c(16,17,23,24,27)] <- NA

n_bets_calib <- n_calib_checker(Odds_df)

# Obtain and Clean Calibration Data ---------------------------------------

Odds_df_par <- clean_calibration_data(WeibProcess_opt, Odds_df)

Calib_error_plot <- tibble(MinGamePlay = Odds_df_par$MinGamePlay, Num = n_bets_calib, R = Odds_df_par$CalibError) %>% 
  gather(key, value, 2:3) %>%
  dplyr::mutate(key = factor(key, levels = c("R", "Num"), labels = c("Calibration error", "# of bets in calibration"), ordered = TRUE)) %>% 
  ggplot(aes(x = MinGamePlay, y = value)) + 
  geom_path() + facet_grid(key ~ ., scales = "free_y") + 
  geom_vline(xintercept = c(42, 62.5), linetype = "dotted") +
  labs(x="Time (Minutes)", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

Calib_error_plot

# ggsave("WeibProc_CalibError.png", device = "png", path = "Plots/Calibration/Weibull Process", width = 8, height = 4)


p <- Odds_df_par[,c(1:5)] %>% 
  gather(key, value, 2:5) %>% 
  dplyr::mutate(Bet2 = c(rep("Scale", 192), rep("Shape", 192),rep("Scale", 192), rep("Shape", 192))) %>% 
  dplyr::mutate(Bet2 = factor(Bet2, levels = c("Shape", "Scale"))) %>% 
  dplyr::mutate(Side = c(rep("Home", 192*2), rep("Away", 192*2))) %>% 
  dplyr::mutate(Side = factor(Side, levels = c("Home", "Away"))) %>% 
  ggplot(aes(x = MinGamePlay, y = value, col = Side)) + 
  geom_line() +  geom_vline(xintercept = c(42, 62.5), linetype = "dotted") + facet_grid(Bet2 ~ ., scales = "free_y") +  
  labs(x="Time (Minutes)", y = "") +
  scale_color_manual(values = c("blue", "red")) +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))
p

# ggsave("WeibProc_Params.png", device = "png", path = "Plots/Calibration/Weibull Process", width = 8, height = 4)


tim <- seq(0,1,length.out = nrow(Odds_df) + 1)

{ind1 <- which(c(0,diff(Odds_df$HomeScore)) > 0)
  Nt_last1 <- rep(NA,nrow(Odds_df))
  Nt_last1[1] <- 0
  for(i in ind1){
    Nt_last1[i] <- tim[i]
  }
  Nt_last1 <- na.locf(Nt_last1)}

{ind2 <- which(c(0,diff(Odds_df$AwayScore)) > 0)
  Nt_last2 <- rep(NA,nrow(Odds_df))
  Nt_last2[1] <- 0
  for(i in ind2){
    Nt_last2[i] <- tim[i]
  }
  Nt_last2 <- na.locf(Nt_last2)}

Price <- tibble()

for (i in 1:nrow(Odds_df)){
  l <- WeibProcess_Pricing(theta = WeibProcess_opt[[i]][[1]], t = tim[i], Nt = c(Odds_df$HomeScore[i], Odds_df$AwayScore[i]))
  Price <- Price %>% bind_rows(tibble(Home = l[[1]], Away = l[[2]], Draw = l[[3]]))
}


matchodds_Odds_df <- Odds_df[,c(2,5:7, 42:44,79:81)]
matchodds_Odds_df[,2:4] <- Price
names(matchodds_Odds_df)[2:4] <- c("Home", "Away", "Draw")

matchodds_Odds_df <- matchodds_Odds_df[,1:4] %>% gather(Bet, value, 2:4) %>% dplyr::mutate(Bet = factor(Bet, levels = c("Home", "Away", "Draw"))) %>% bind_cols(matchodds_Odds_df[,5:7] %>% gather(key_lwr, value_lwr, 1:3)) %>% bind_cols(matchodds_Odds_df[,8:10] %>% gather(key_upp, value_upp, 1:3))

matchodds_Odds_df[,c(1,2,3,5,7)] %>% ggplot(aes(x = MinGamePlay, y = value, ymin = value_lwr, ymax = value_upp, col = Bet)) + 
  geom_line() + 
  geom_vline(xintercept = c(42, 62.5), linetype = "dotted") +
  geom_ribbon(alpha = 0.3, aes(fill = Bet), color = NA) +  
  scale_color_manual(values = c("blue", "red", "green")) +
  scale_fill_manual(values = c("blue", "red", "green")) +
  labs(x="Time (Minutes)", y = "Price") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

# ggsave("WeibProc_Prices.png", device = "png", path = "Plots/Calibration/Weibull Process", width = 8, height = 4)

# ZOOMED! ---

matchodds_Odds_df[c(1:50, 193:(192+50), (192*2+1):(192*2+50)),c(1,2,3,5,7)] %>% ggplot(aes(x = MinGamePlay, y = value, ymin = value_lwr, ymax = value_upp, col = Bet)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.3, aes(fill = Bet), color = NA) +  
  labs(x="Time (Minutes)", y = "Price") +
  scale_color_manual(values = c("blue", "red", "green")) +
  scale_fill_manual(values = c("blue", "red", "green")) +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

# ggsave("WeibProc_Prices_Zoomed.png", device = "png", path = "Plots/Calibration/Weibull Process", width = 8, height = 4)

# OU2.5

Price <- tibble()

for (i in 1:nrow(Odds_df)){
  l <- WeibProcess_Pricing(theta = WeibProcess_opt[[i]][[1]], t = tim[i], Nt = c(Odds_df$HomeScore[i], Odds_df$AwayScore[i]))
  Price <- Price %>% bind_rows(tibble(O25 = l[[16]], U25 = l[[17]], O15 = l[[18]], U15 = l[[19]]))
}


matchodds_Odds_df2 <- Odds_df[,c(2,8:11, 45:48,82:85)]
matchodds_Odds_df2[,2:5] <- dplyr::select(Price, U15, O15, U25, O25)
matchodds_Odds_df2[126:192,2:3] <- NA
names(matchodds_Odds_df2)[2:5] <- c("U15", "O15", "U25", "O25")

matchodds_Odds_df2 <- matchodds_Odds_df2[,1:5] %>% gather(Bet, value, 2:5) %>% dplyr::mutate(Bet = factor(Bet, levels = c("U15", "O15", "U25", "O25"), labels = c("Under 1.5", "Over 1.5", "Under 2.5", "Over 2.5"))) %>% bind_cols(matchodds_Odds_df2[,6:9] %>% gather(key_lwr, value_lwr, 1:4)) %>% bind_cols(matchodds_Odds_df2[,10:13] %>% gather(key_upp, value_upp, 1:4))

matchodds_Odds_df2[,c(1,2,3,5,7)] %>% ggplot(aes(x = MinGamePlay, y = value, ymin = value_lwr, ymax = value_upp, col = Bet)) + 
  geom_line() + 
  geom_vline(xintercept = c(42, 62.5), linetype = "dotted") +
  geom_ribbon(alpha = 0.3, aes(fill = Bet), color = NA) +  
  scale_color_manual(values = c("purple", "orangered", "olivedrab", "saddlebrown")) +
  scale_fill_manual(values = c("purple", "orangered", "olivedrab", "saddlebrown")) +
  labs(x="Time (Minutes)", y = "Price") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

# ggsave("WeibProc_Prices_OU.png", device = "png", path = "Plots/Calibration/Weibull Process", width = 8, height = 4)

# ZOOMED! ---

matchodds_Odds_df2[c(1:50, 193:(192+50), (192*2+1):(192*2+50), (192*3+1):(192*3+50)),c(1,2,3,5,7)] %>% ggplot(aes(x = MinGamePlay, y = value, ymin = value_lwr, ymax = value_upp, col = Bet)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.3, aes(fill = Bet), color = NA) +  
  labs(x="Time (Minutes)", y = "Price") +
  scale_color_manual(values = c("purple", "orangered", "olivedrab", "saddlebrown")) +
  scale_fill_manual(values = c("purple", "orangered", "olivedrab", "saddlebrown")) +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

# ggsave("WeibProc_Prices_OU_Zoomed.png", device = "png", path = "Plots/Calibration/Weibull Process", width = 8, height = 4)

# Correct score 

Price <- tibble()

for (i in 1:nrow(Odds_df)){
  l <- WeibProcess_Pricing(theta = WeibProcess_opt[[i]][[1]], t = tim[i], Nt = c(Odds_df$HomeScore[i], Odds_df$AwayScore[i]))
  Price <- Price %>% bind_rows(tibble(CS00 = l[[22]], CS10 = l[[23]], CS01 = l[[26]], CS11 = l[[29]]))
}


matchodds_Odds_df3 <- Odds_df[,c(2,26:27,30:31, 63:64, 67:68,100:101,104:105)]
matchodds_Odds_df3[,2:5] <- dplyr::select(Price, CS00, CS01, CS10, CS11)
matchodds_Odds_df3[85:192,2:3] <- NA
matchodds_Odds_df3[126:192,4] <- NA
names(matchodds_Odds_df3)[2:5] <- c("CS00", "CS01", "CS10", "CS11")

matchodds_Odds_df3 <- matchodds_Odds_df3[,1:5] %>% 
  gather(Bet, value, 2:5) %>% 
  dplyr::mutate(Bet = factor(Bet, levels = c("CS00", "CS01", "CS10", "CS11"))) %>% 
  bind_cols(matchodds_Odds_df3[,6:9] %>% gather(key_lwr, value_lwr, 1:4)) %>% 
  bind_cols(matchodds_Odds_df3[,10:13] %>% gather(key_upp, value_upp, 1:4))

matchodds_Odds_df3[,c(1,2,3,5,7)] %>% ggplot(aes(x = MinGamePlay, y = value, ymin = value_lwr, ymax = value_upp, col = Bet)) + 
  geom_path() + 
  geom_vline(xintercept = c(42, 62.5), linetype = "dotted") +
  geom_ribbon(alpha = 0.3, aes(fill = Bet), color = NA) +  
  scale_color_manual(values = c("purple", "orangered", "olivedrab", "saddlebrown")) +
  scale_fill_manual(values = c("purple", "orangered", "olivedrab", "saddlebrown")) +
  labs(x="Time (Minutes)", y = "Price") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

# ggsave("WeibProc_Prices_CS.png", device = "png", path = "Plots/Calibration/Weibull Process", width = 8, height = 4)

# ZOOMED! ---

matchodds_Odds_df3[c(1:50, 193:(192+50), (192*2+1):(192*2+50), (192*3+1):(192*3+50)),c(1,2,3,5,7)] %>% ggplot(aes(x = MinGamePlay, y = value, ymin = value_lwr, ymax = value_upp, col = Bet)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.3, aes(fill = Bet), color = NA) +  
  labs(x="Time (Minutes)", y = "Price") +
  scale_color_manual(values = c("purple", "orangered", "olivedrab", "saddlebrown")) +
  scale_fill_manual(values = c("purple", "orangered", "olivedrab", "saddlebrown")) +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

# ggsave("WeibProc_Prices_CS_Zoomed.png", device = "png", path = "Plots/Calibration/Weibull Process", width = 8, height = 4)


# Implied intensity -------------------------------------------------------

inten <- tibble()

for (i in 1:nrow(Odds_df)){
  l1 <- Odds_df_par$Par1[i] * Odds_df_par$Par2[i] * tim[i]^(Odds_df_par$Par2[i]-1)
  l2 <- Odds_df_par$Par3[i] * Odds_df_par$Par4[i] * tim[i]^(Odds_df_par$Par4[i]-1)
  
  inten <- inten %>% bind_rows(tibble(Home = l1, Away = l2))
  
}

inten[1,] <- NA

inten %>% bind_cols(Odds_df %>% select(MinGamePlay)) %>% 
  gather(Side, value, 1:2) %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away"))) %>% 
  ggplot(aes(x = MinGamePlay, y = value, col = Side)) + 
  geom_vline(xintercept = c(42, 62.5), linetype = "dotted") +
  geom_path() + labs(x="Time (Minutes)", y = "Implied intensity") +
  scale_color_manual(values = c("blue", "red")) +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

# ggsave("WeibProc_Implied_Inten.png", device = "png", path = "Plots/Calibration/Weibull Process", width = 8, height = 4)

