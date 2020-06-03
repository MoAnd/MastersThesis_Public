# Clean up / libraries ----
if(!is.null(dev.list())) dev.off()                  # Clear plots
rm(list=ls())                                       # Clean workspace
cat("\014")                                         # Clear console

source("Libraries.R")
source("Football Characteristics/Functions/WeibullRenewal_sim.R")
source("Football Characteristics/Functions/WeibullProcess_sim.R")


# Load data ----
load("Football Characteristics/Data/EPL_list_data.Rdata")

first_home_goal_times <- c(Football[[1]][[2]]) %>% sort() %>% .[1]
second_home_goal_times <- c(diff(Football[[1]][[2]])) %>% sort() %>% .[1]

first_away_goal_times <- c(Football[[1]][[3]]) %>% sort() %>% .[1]
second_away_goal_times <- c(diff(Football[[1]][[3]])) %>% sort() %>% .[1]

l <- length(Football)

pb <- txtProgressBar(min = 2, max = l, style = 3)
for (i in 2:l) {
  
  home_goal_time <- c(Football[[i]][[2]]) %>% sort()
  away_goal_time <- c(Football[[i]][[3]]) %>% sort()
  
  if(rlang::is_true(any(home_goal_time > 144))){
    ind <- which(home_goal_time > 144)
    home_goal_time <- home_goal_time[-ind]
  }
  
  if(rlang::is_true(any(away_goal_time > 144))){
    ind <- which(away_goal_time > 144)
    away_goal_time <- away_goal_time[-ind]
  }
  
  if(length(home_goal_time) > 1) {

    first_home_goal_times <- c(first_home_goal_times, home_goal_time[1])
    
    home_goal_time_diff <- diff(home_goal_time)
    
    if(home_goal_time_diff[1] == 0){home_goal_time_diff[1] <- home_goal_time[2]-45}
    
    second_home_goal_times <- c(second_home_goal_times, home_goal_time_diff[1])
    
  } else if(length(home_goal_time) > 0) {
    
    first_home_goal_times <- c(first_home_goal_times, home_goal_time[1])
    
  }
  
  if (length(away_goal_time) > 1){

    first_away_goal_times <- c(first_away_goal_times, away_goal_time[1])
    
    away_goal_time_diff <- diff(away_goal_time)
    
    if(away_goal_time_diff[1] == 0){away_goal_time_diff[1] <- away_goal_time[2]-45}
    second_away_goal_times <- c(second_away_goal_times, away_goal_time_diff[1])
  } else if(length(away_goal_time) > 0) {
    first_away_goal_times <- c(first_away_goal_times, away_goal_time[1])
  }
  
  setTxtProgressBar(pb, i)
}

close(pb)

first_home_goal_times <- as.numeric(na.omit(first_home_goal_times))
second_home_goal_times <- as.numeric(na.omit(second_home_goal_times))
first_away_goal_times <- as.numeric(na.omit(first_away_goal_times))
second_away_goal_times <- as.numeric(na.omit(second_away_goal_times))

l <- length(first_home_goal_times) - length(first_away_goal_times)
ll <- length(second_home_goal_times) - length(second_away_goal_times)

dat2 <- tibble(Home = first_home_goal_times) %>% 
  bind_cols(tibble(Away = c(first_away_goal_times, rep(NA, l)))) %>% 
  gather(Side, value, 1:2) %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away")))

dat3 <- tibble(Home = second_home_goal_times) %>% 
  bind_cols(tibble(Away = c(second_away_goal_times, rep(NA, ll)))) %>% 
  gather(Side, value, 1:2) %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away")))

gf_hist_goal_wait <- gf_dhistogram(~ value | Side, data = dat2, alpha = 0.75, binwidth = 3, boundary = 3, color = "white", fill = "darkgray") %>% 
  gf_dhistogram(~ value | Side, data = dat3, alpha = 0.55, binwidth = 3, boundary = 3, color = "white", fill = "pink") %>% 
  gf_density(~ value | Side, data = dat2, alpha = 1, binwidth = 1, color = "black", fill = "white", geom="line") %>% 
  gf_density(~ value | Side, data = dat3, alpha = 1, binwidth = 1, color = "deeppink", fill = "white", geom="line") + 
  labs(x="Minute", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))
gf_hist_goal_wait

# ggsave("GoalWaitingTimes_EmpericalDensity.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)


df_emp_dens <- gf_density(~ value | Side, data = dat2, alpha = 1, binwidth = 1, color = "black", fill = "white", geom="line") %>% 
  gf_density(~ value | Side, data = dat3, alpha = 1, binwidth = 1, color = "deeppink", fill = "white", geom="line") + 
  labs(x="Minute", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))

df_emp_dens

# ggsave("GoalWaitingTimes_EmpericalDensity.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)


# Theoretical Weibull process distribution --------------------------------

theta_hom <- c(1.536491/1.125896, 1.125896)
sim_hom <- WeibullProcess_sim(theta_hom, m = 2000000, listed = TRUE)

l <- length(sim_hom)
ll <- length(sim_hom[[1]])

hom1sim <- c(sim_hom[[1]][[1]][1])

hgoal <- diff(sim_hom[[1]][[1]])

if (length(hgoal) > 0){
  hom2sim <- hgoal[1]
} else{
  hom2sim <- NA
}

hom1sim
hom2sim


{pb <- txtProgressBar(min = 2, max = l*ll, style = 3)
  for (i in 1:l){
    if(i == 1){
      for(j in 2:ll){
        goals <- as.numeric(na.omit(sim_hom[[i]][[j]])) * 93
        if(length(goals) > 0){
          hom1sim <- c(hom1sim, goals[1])
        }
        if(length(goals) > 1){
          hom2sim <- c(hom2sim, diff(goals)[1])
        }
        setTxtProgressBar(pb, i * j)
      }  
    } else{
      for(j in 1:ll){
        goals <- as.numeric(na.omit(sim_hom[[i]][[j]])) * 93
        if(length(goals) > 0){
          hom1sim <- c(hom1sim, goals[1])
        }
        if(length(goals) > 1){
          hom2sim <- c(hom2sim, diff(goals)[1])
        }
        setTxtProgressBar(pb, i*j)
      }
    }
  }
  close(pb)}

length(hom1sim)
length(hom2sim)


theta_awa <- c(1.138246/1.148385, 1.148385)
sim_awa <- WeibullProcess_sim(theta_awa, m = 2000000, listed = TRUE)

l <- length(sim_awa)
ll <- length(sim_awa[[1]])

awa1sim <- c(sim_awa[[1]][[1]][1])

agoal <- diff(sim_awa[[1]][[1]])

if (length(hgoal) > 0){
  awa2sim <- agoal[1]
} else{
  awa2sim <- NA
}

awa1sim
awa2sim


{pb <- txtProgressBar(min = 2, max = l*ll, style = 3)
  for (i in 1:l){
    if(i == 1){
      for(j in 2:ll){
        goals <- as.numeric(na.omit(sim_awa[[i]][[j]])) * 93
        if(length(goals) > 0){
          awa1sim <- c(awa1sim, goals[1])
        }
        if(length(goals) > 1){
          awa2sim <- c(awa2sim, diff(goals)[1])
        }
        setTxtProgressBar(pb, i * j)
      }  
    } else{
      for(j in 1:ll){
        goals <- as.numeric(na.omit(sim_awa[[i]][[j]])) * 93
        if(length(goals) > 0){
          awa1sim <- c(awa1sim, goals[1])
        }
        if(length(goals) > 1){
          awa2sim <- c(awa2sim, diff(goals)[1])
        }
        setTxtProgressBar(pb, i*j)
      }
    }
  }
  close(pb)}

length(awa1sim)
length(awa2sim)

l <- length(hom1sim) - length(awa1sim)
ll <- length(hom2sim) - length(awa2sim)

dat2 <- tibble(Home = hom1sim) %>% 
  bind_cols(tibble(Away = c(awa1sim, rep(NA, l)))) %>% 
  gather(Side, value, 1:2) %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away")))

dat3 <- tibble(Home = hom2sim) %>% 
  bind_cols(tibble(Away = c(awa2sim, rep(NA, ll)))) %>% 
  gather(Side, value, 1:2) %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away")))

gf_hist_goal_wait <- gf_dhistogram(~ value | Side, data = dat2, alpha = 0.75, binwidth = 3, boundary = 3, color = "white", fill = "darkgray") %>% 
  gf_dhistogram(~ value | Side, data = dat3, alpha = 0.55, binwidth = 3, boundary = 3, color = "white", fill = "pink") %>% 
  gf_density(~ value | Side, data = dat2, alpha = 1, binwidth = 1, color = "black", fill = "white", geom="line") %>% 
  gf_density(~ value | Side, data = dat3, alpha = 1, binwidth = 1, color = "deeppink", fill = "white", geom="line") + 
  labs(x="Minute", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))
gf_hist_goal_wait

p_dens <- gf_density(~ value | Side, data = dat2, fill = "white", color = "black", alpha = 1, linetype = "dashed", geom="line") %>% 
  gf_density(~ value | Side, data = dat3, fill = "white", color = "deeppink", alpha = 1, linetype = "dashed", geom="line") + 
  labs(x="Minute", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))
p_dens

p_den_comp <- df_emp_dens %>% gf_density(~ value | Side, data = dat2, fill = "white", color = "black", alpha = 1, linetype = "dashed", geom="line") %>% 
  gf_density(~ value | Side, data = dat3, fill = "white", color = "deeppink", alpha = 1, linetype = "dashed", geom="line")

p_den_comp

# ggsave("GoalWaitingTimes_Emperical_v_theo_Density_.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)

beepr::beep()

# Theoretical Weibull renewal distribution --------------------------------

theta_hom <- c((1/1.659439)^1.125896, 1.125896)
sim_hom <- WeibullRenewal_sim(theta_hom, m = 2000000, listed = TRUE)

l <- length(sim_hom)
ll <- length(sim_hom[[1]])

hom1sim <- c(sim_hom[[1]][[1]][1])

hgoal <- diff(sim_hom[[1]][[1]])

if (length(hgoal) > 0){
  hom2sim <- hgoal[1]
} else{
  hom2sim <- NA
}

hom1sim
hom2sim


{pb <- txtProgressBar(min = 2, max = l*ll, style = 3)
  for (i in 1:l){
    if(i == 1){
      for(j in 2:ll){
        goals <- as.numeric(na.omit(sim_hom[[i]][[j]])) * 93
        if(length(goals) > 0){
          hom1sim <- c(hom1sim, goals[1])
        }
        if(length(goals) > 1){
          hom2sim <- c(hom2sim, diff(goals)[1])
        }
        setTxtProgressBar(pb, i * j)
      }  
    } else{
      for(j in 1:ll){
        goals <- as.numeric(na.omit(sim_hom[[i]][[j]])) * 93
        if(length(goals) > 0){
          hom1sim <- c(hom1sim, goals[1])
        }
        if(length(goals) > 1){
          hom2sim <- c(hom2sim, diff(goals)[1])
        }
        setTxtProgressBar(pb, i*j)
      }
    }
  }
  close(pb)}

length(hom1sim)
length(hom2sim)


length(hom1sim)
length(hom2sim)


theta_awa <- c((1/1.221129)^1.148385 , 1.148385)
sim_awa <- WeibullRenewal_sim(theta_awa, m = 2000000, liste = TRUE)

l <- length(sim_awa)
ll <- length(sim_awa[[1]])

awa1sim <- c(sim_awa[[1]][[1]][1])

agoal <- diff(sim_awa[[1]][[1]])

if (length(hgoal) > 0){
  awa2sim <- agoal[1]
} else{
  awa2sim <- NA
}

awa1sim
awa2sim


{pb <- txtProgressBar(min = 2, max = l*ll, style = 3)
  for (i in 1:l){
    if(i == 1){
      for(j in 2:ll){
        goals <- as.numeric(na.omit(sim_awa[[i]][[j]])) * 93
        if(length(goals) > 0){
          awa1sim <- c(awa1sim, goals[1])
        }
        if(length(goals) > 1){
          awa2sim <- c(awa2sim, diff(goals)[1])
        }
        setTxtProgressBar(pb, i * j)
      }  
    } else{
      for(j in 1:ll){
        goals <- as.numeric(na.omit(sim_awa[[i]][[j]])) * 93
        if(length(goals) > 0){
          awa1sim <- c(awa1sim, goals[1])
        }
        if(length(goals) > 1){
          awa2sim <- c(awa2sim, diff(goals)[1])
        }
        setTxtProgressBar(pb, i*j)
      }
    }
  }
  close(pb)}

length(awa1sim)
length(awa2sim)

l <- length(hom1sim) - length(awa1sim)
ll <- length(hom2sim) - length(awa2sim)

dat2 <- tibble(Home = hom1sim) %>% 
  bind_cols(tibble(Away = c(awa1sim, rep(NA, l)))) %>% 
  gather(Side, value, 1:2) %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away")))

dat3 <- tibble(Home = hom2sim) %>% 
  bind_cols(tibble(Away = c(awa2sim, rep(NA, ll)))) %>% 
  gather(Side, value, 1:2) %>% 
  mutate(Side = factor(Side, levels = c("Home", "Away")))

gf_hist_goal_wait <- gf_dhistogram(~ value | Side, data = dat2, alpha = 0.75, binwidth = 3, boundary = 3, color = "white", fill = "darkgray") %>% 
  gf_dhistogram(~ value | Side, data = dat3, alpha = 0.55, binwidth = 3, boundary = 3, color = "white", fill = "pink") %>% 
  gf_density(~ value | Side, data = dat2, alpha = 1, binwidth = 1, color = "black", fill = "white", geom="line") %>% 
  gf_density(~ value | Side, data = dat3, alpha = 1, binwidth = 1, color = "deeppink", fill = "white", geom="line") + 
  labs(x="Minute", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))
gf_hist_goal_wait

p_dens <- gf_density(~ value | Side, data = dat2, fill = "white", color = "black", alpha = 1, linetype = "dashed", geom="line") %>% 
  gf_density(~ value | Side, data = dat3, fill = "white", color = "deeppink", alpha = 1, linetype = "dashed", geom="line") + 
  labs(x="Minute", y = "") +
  theme_classic() + 
  theme(strip.background  = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.1, 0.5, 0.2, 0.1, "cm"))
p_dens

p_den_comp <- df_emp_dens %>% gf_density(~ value | Side, data = dat2, fill = "white", color = "black", alpha = 1, linetype = "dashed", geom="line") %>% 
  gf_density(~ value | Side, data = dat3, fill = "white", color = "deeppink", alpha = 1, linetype = "dashed", geom="line")

p_den_comp

# ggsave("GoalWaitingTimes_Emperical_v_theo_Density_WeibRenewal.png", device = "png", path = "Plots/FootballGoals", width = 8, height = 4)

beepr::beep()





