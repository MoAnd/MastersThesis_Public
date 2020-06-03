# Clean up / libraries ----
if(!is.null(dev.list())) dev.off()                  # Clear plots
rm(list=ls())                                       # Clean workspace
cat("\014")                                         # Clear console

source("Libraries.R")
source("Football Characteristics/Functions/WeibullCountDif_pmf_sim.R")

# Load data ----
load("Football Characteristics/Data/EPL_tibble_data.Rdata")


# Initial plot ----

football_tbl_tidy <- football_tbl 
names(football_tbl_tidy)[c(3,5)] <- c("Home", "Away")
football_tbl_tidy <- football_tbl_tidy %>% select(MatchID, Home, Away) %>% 
  pivot_longer(c(Home, Away), names_to = "key", values_to = "value") %>% 
  mutate(key = factor(key, levels = c("Home", "Away")))

p <- gf_dhistogram( ~ value | key, data = football_tbl_tidy, alpha = 0.75, binwidth = 1, color = "white", fill = c(rep("blue",10), rep("red",10))) +
  labs(x="Number of goals", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

p

# ggsave("GoalHist.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)


# Diff hist ----

ScoreDif_tbl <- football_tbl %>% mutate(ScoreDif = HomeScore - AwayScore) %>% dplyr::select(MatchID, ScoreDif)

p_dif <- gf_dhistogram( ~ ScoreDif, data = ScoreDif_tbl, alpha = 0.75, binwidth = 1, color = "white", fill = "darkgreen") +
  labs(x="Goal difference", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

p_dif

# ggsave("GoalDifHist.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)

# Poisson distribution fit ----

mh <- mean(football_tbl$HomeScore); mh
vh <- sd(football_tbl$HomeScore)^2; vh

ma <- mean(football_tbl$AwayScore); ma
va <- sd(football_tbl$AwayScore)^2; va

# kable(tibble("Team" = c("EV", "Variance"), "Home" = c(mh, vh), "Away" = c(ma, va)), "latex", booktabs = T) %>%
#   kable_styling(latex_options = c("striped", "scale_down"))

gf_home <- goodfit(table(football_tbl$HomeScore), type = "poisson", method = "MinChisq", par = list("lambda" = mh))
summary(gf_home)

gf_away <- goodfit(table(football_tbl$AwayScore), type = "poisson", method = "MinChisq", par = list("lambda" = ma))
summary(gf_away)


# Poisson fit plots ----

p2 <- p %>% gf_fitdistr(dist = "dpois", geom = "point", size = 2, n = 10)

p2

# ggsave("PoissonGoalHist.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)


# Weibull Count Model ----

table(football_tbl$HomeScore)
table(football_tbl$AwayScore)

hom <- fitdistr(football_tbl$HomeScore, densfun = dWeibullCount, start = list(shape = 1, scale = 1.5)); hom
awa <- fitdistr(football_tbl$AwayScore, densfun = dWeibullCount, start = list(shape = 1, scale = 1.5)); awa

# kable(tibble("." = c("Shape", "Scale"), "Home" = hom$estimate, "Away" = awa$estimate), "latex", booktabs = T) %>%
#   kable_styling(latex_options = c("striped", "scale_down"))


dWeiCount_home <- dWeibullCount(0:9, shape = hom$estimate[1], scale = hom$estimate[2]) %>% as.vector()
evWeibullCount(9, shape = hom$estimate[1], scale = hom$estimate[2])

dWeiCount_away <- dWeibullCount(0:9, shape = awa$estimate[1], scale = awa$estimate[2]) %>% as.vector()
evWeibullCount(7, shape = awa$estimate[1], scale = awa$estimate[2])

WeibCount_df <- tibble(x = 0:9, Home = dWeiCount_home, Away = dWeiCount_away) %>% 
  gather(key, value, 2:3) %>% 
  mutate(key = factor(key, levels = c("Home", "Away")))

p3 <- p %>% gf_point(gformula = value ~ x | key, data = WeibCount_df, size = 2)

p3

# ggsave("WeibullCountGoalHist.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)

# Chi-squared test

dWeiCount_home <- dWeibullCount(0:100, shape = hom$estimate[1], scale = hom$estimate[2]) %>% as.vector()
dWeiCount_home[10] <- sum(dWeiCount_home[10:100])
dWeiCount_home <- dWeiCount_home[1:10]
X2 <- sum((table(football_tbl$HomeScore) - dWeiCount_home*5700)^2 / (dWeiCount_home*5700))
X2
1-pchisq(X2, df = 8)

dWeiCount_away <- dWeibullCount(0:100, shape = awa$estimate[1], scale = awa$estimate[2]) %>% as.vector()
dWeiCount_away[8] <- sum(dWeiCount_away[8:100])
dWeiCount_away <- dWeiCount_away[1:8]
X2 <- sum((table(football_tbl$AwayScore) - dWeiCount_away*5700)^2 / (dWeiCount_away*5700))
X2
1-pchisq(X2, df = 6)

# Skellam & Weib Count Dif ----

skel <- tibble(x = -6:8, y = skellam::dskellam(-6:8, mh, ma))

p_dif %>% gf_point(gformula = y ~ x, data = skel, size = 2)

theta <- c(hom$estimate[2]^(-1/hom$estimate[1]), hom$estimate[1], awa$estimate[2]^(-1/awa$estimate[1]), awa$estimate[1])
m <- 10000000
tab <- WeibullCountDif_pmf_sim(theta, m = m)

ind <- which(names(tab) %in% c("-6",  "-5",  "-4",  "-3",  "-2",  "-1",  "0",   "1",   "2",   "3",   "4",   "5",   "6",   "7",   "8"))
# tab2 <- tab
tab <- tab[ind]

tab_tbl <- tibble(x = -6:8, y = tab/m)

tab_tbl <- tab_tbl %>% bind_cols(select(skel, y))

names(tab_tbl) <- c("x", "Weibull Count Dif.", "Skellam")

tab_tbl <- tab_tbl %>% gather(key, val, 2:3)

p_dif_pmf <- p_dif %>% gf_point(gformula = val ~ x | key, data = tab_tbl, size = 2)

p_dif_pmf

# ggsave("Goal_Dif_Hist_pmf.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)


obs <- table(football_tbl$HomeScore - football_tbl$AwayScore)
exp_skel <- diff(c(0,skellam::pskellam(-6:7, mh, ma),1))
X2 <- sum((obs - exp_skel*5700)^2 / (exp_skel*5700))
X2
1-pchisq(X2, df = 13)

chisq.test(obs, p = diff(c(0,skellam::pskellam(-6:7, mh, ma),1)))

exp_weibcount <- diff(c(0,cumsum(tab[-length(tab)])/m,1))
X2 <- sum((obs - exp_weibcount*5700)^2 / (exp_weibcount*5700))
X2
1-pchisq(X2, df = 11)

chisq.test(obs, p =  diff(c(0,cumsum(tab[-length(tab)])/m,1)))




