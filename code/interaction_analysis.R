source("code/fxs.R")
library(ordinal)
nat = get_all_usnews %>% 
  pull(school_data) %>% .[[1]]

nat_clm = make_clm(nat, y_var, vars)
nat_clm_pop_locale = make_clm(nat, y_var, vars %>% c(., "sqrt_pop:Locale"))

nat_clm_noCal = make_clm(nat %>% filter(State_ab != "CA"), y_var, vars)
nat_clm_pop_locale_noCal = make_clm(nat %>% filter(State_ab != "CA"), 
                                    y_var, vars %>% c(., "sqrt_pop:Locale"))
nat = nat %>% add_clm_res(nat_clm)

nat %>% 
  select(School, sqrt_pop, Locale, resid) %>% 
  filter(sqrt_pop>=quantile(sqrt_pop, .9))

sum_of_ones = function(x) sum(x == "1")

get_all_usnews %>% 
  mutate(HBCU_n = map_dbl(school_data, 
                          ~.x %>% pull(HBCU) %>% sum_of_ones())) %>% 
  mutate(TRIBAL_n = map_dbl(school_data, 
                            ~.x %>% pull(TRIBAL) %>% sum_of_ones())) %>%
  mutate(MEN_n = map_dbl(school_data, 
                         ~.x %>% pull(MENONLY) %>% sum_of_ones())) %>%
  mutate(WOMEN_n = map_dbl(school_data, 
                           ~.x %>% pull(WOMENONLY) %>% sum_of_ones()))

trans_dates = read_csv("data/school_transition_dates.csv") %>% 
  select(School, ID, secular_date, coed_date)
         
nat = left_join(nat, trans_dates %>% select(-School), by = "ID")

nat %>% 
  mutate(Sec_Age = 2022 - secular_date) %>% 
  filter(!is.na(secular_date)) %>% 
  make_clm(., y_var, c("Age", "Sec_Age")) %>% 
  summary()

nat %>% 
  mutate(Sec_Age = 2022 - secular_date) %>% 
  filter(!is.na(secular_date)) %>% 
  make_clm(., y_var, c("Age")) %>% 
  summary()

nat %>% 
  mutate(Sec_Age = 2022 - secular_date) %>% 
  filter(!is.na(secular_date)) %>% 
  make_clm(., y_var, c("Sec_Age")) %>% 
  summary()

  
nat %>% 
  mutate(Sec_Age = 2022 - secular_date) %>% 
  filter(!is.na(secular_date)) %>% 
  lm("log_rank ~ Age + Sec_Age", data = .) %>% 
  summary()
         

test = 
  tibble(data_name = data_names) %>% 
  mutate(steps = map(data_name, 
                     ~.x %>% 
                       str_c("plots/step_", ., "_clm.txt") %>% 
                       read_lines()),
         steps2 = map2(data_name, steps, ~c(.x, .y) %>% str_c(collapse = " ")))

max_step_n = 
  test %>% 
  pull(steps) %>% 
  map_dbl(length) %>% 
  max()

test = 
  test %>% 
  mutate(steps3 = map(steps, ~c(.x, rep(NA, max_step_n-length(.x)))))
  
steps_df = data.frame(test %>% pull(steps3))
colnames(steps_df) = test %>% pull(data_name)

write_csv(steps_df, "plots/clm_steps.csv")




         