library(tidyverse)
# sat_data = 
#   read_html("https://blog.prepscholar.com/average-sat-scores-by-state-most-recent") %>% 
#   html_element("table") %>% 
#   html_table(header = T)
# sat_data = sat_data %>% 
#   rename(p_rate = "Participation Rate") %>% 
#   filter(str_detect(p_rate, "[:digit:]")) %>% 
#   mutate(p_rate = 
#            p_rate %>% 
#            str_remove_all("[:punct:]") %>% 
#            as.numeric() %>% 
#            magrittr::divide_by(100))
sigmoid = function(x){
  x = x - mean(x)
  x = x * 6 / max(x)
  1/(1 + exp(-1*x))
}

estimate_sat = function(sat_samp_mean, part_rate, n = 1e5){
  
  pop = sort(rnorm(n))
  pop[pop>3] = 3 #can't get above a 1600
  pop[pop<-3] = -3 #can't below 400
  samp = sample(pop, 
                size = part_rate*n, 
                prob = seq(from = -3, to = 5, length.out = n) %>% exp())
  
  z = mean(samp)
  true_mean = .2763713
  true_var = .3407498
  sat_sd_true = 210 
  #true ish
  sat_samp_mean-z*sat_sd_true
}
sat_data_2019 = read_csv("data/sat_by_state.csv") %>% 
  rename(p_rate = "Percent taking SAT",
         mean_total = "Mean score") %>% 
  mutate(p_rate = p_rate/100)
  
sat_data_2019 = sat_data_2019 %>% 
  mutate(sat_est = map2_dbl(mean_total, p_rate, ~estimate_sat(.x,.y, n = 2e4)))


state_demos = read_rds("data/state_demos.RDS") %>% 
  mutate(across(Black_count:White_not_Hispanic_or_Latino_count, 
                ~ 100*.x/population_2014))
sat_data_2019 = left_join(sat_data_2019, state_demos, by = "State")
  
sat_data_2019 %>% 
  ggplot(aes(x = White_not_Hispanic_or_Latino_count, 
             y = sqrt_pop, color = sat_est)) + 
  geom_point(size = 5) + 
  scale_color_gradient2(low = "red", 
                        high = "limegreen", 
                        mid = "yellow", midpoint = 900)
  
