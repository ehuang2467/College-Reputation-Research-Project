library(modelr)
source("code/fxs.R")
library(gridExtra)

nat = get_usnews("national")

nominal_test(nat_clm)
scale_test(nat_clm)

nat %>% filter(Control != "For_profit") %>% add_clm_res(nat_clm) %>% 
  ggplot(aes(Age, resid)) + geom_jitter(width = 0, height = .2)

#look at obs with the highest resid
nat = nat %>% filter(Control != "For_profit") %>% add_clm_res(nat_clm)
nat %>% filter(abs(resid) >= 4) %>% select(School, Rank_fac, pred, resid) %>% 
  print(n = Inf)
#out of 41 obs with abs(resid) >= 4, 32 of them are predict a higher rank
#than the data
nat %>% filter(resid >= 4) %>% 
  select(School, Rank_fac, pred, resid, State_ab, Control, Founded, sqrt_pop, is_religious) %>% 
  print(n = Inf)
nat %>% filter(str_detect(School, "University of California"))
#5/32 are UC schools, 
#while 16/438 are CA public schools in the whole data set
#and there are 9 UC schools in the whole data set

nat %>% mutate(is_ca_pub = (State_ab == "CA" & Control == "Public")) %>% 
  ggplot(aes(y = resid, x = is_ca_pub, color = is_ca_pub)) + 
  geom_boxplot() + geom_jitter(alpha = .4)

nat %>% group_by(State_ab) %>% 
  summarise(n = n(), resid_mean = mean(resid, na.rm = T), 
            resid_mean_n =  mean(resid, na.rm = T)*n^(1/5)) %>% 
  arrange(abs(resid_mean_n))
  


test = clm(model_formula %>% str_c(., " + Age:Control"), 
           data = nat %>% filter(Control != "For_profit"))


nat %>% 
  ggplot(aes(x = Age, color = Control, y = resid, group = Control)) + 
  geom_jitter(width = .1) + geom_smooth()

#find signif and coef of Age by State for states n > 5, with a new
#response Rank_fac2 made for each State nested data
test = nat %>% group_by(State_ab) %>% nest() %>%
  filter(map_dbl(data, ~nrow(.x)) > 5) %>%
  mutate(data = map(data, ~.x %>% mutate(Rank_fac2 =
                                           factor(Rank_num, ordered = T,
                                                  levels = sort(Rank_num) %>% unique())))) %>%
  mutate(clm = map(data, ~make_clm(.x, "Rank_fac2", vars) %>% summary() %>% coef() %>% .["Age", c("Estimate", "Pr(>|z|)")])) %>%
  mutate(age_coef = map_dbl(clm, ~.[1]),
         age_pval = map_dbl(clm, ~.[2]))

nat %>% 
  ggplot(aes(x = Age, y = log_rank, color = Control)) +
  geom_point()


nat %>% 
  filter(!is.na(Rank_fac)) %>% 
  ggplot(aes(x = Age, y = Rank_fac, color = Control)) +
  geom_jitter(height = .3)



clm(model_formula, 
    data = nat %>% filter(Age > 200)) %>% summary()

clm(model_formula, 
    data = nat %>% filter(Age <= 200)) %>% summary()
#Very signif for both, although the coef is higher in the old
#subset by about 40%, possibly suggesting polynomial

#signif of Age by Age
clm(model_formula, 
    data = nat %>% filter(Age > 150)) %>% summary()

clm(model_formula, 
    data = nat %>% filter(Age <= 150)) %>% summary()

liberal = get_usnews("liberal")

liberal %>% 
  filter(!is.na(Rank_fac)) %>% 
  ggplot(aes(x = Age, y = Rank_fac, color = Control)) +
  geom_jitter(height = .3)

liberal %>% 
  ggplot(aes(x = Age, y = log_rank, color = Control)) +
  geom_point()

lm(model_formula_lm,
   data = liberal %>% filter(Age > 150)) %>% 
  summary()

lm(model_formula_lm,
   data = liberal %>% filter(Age <= 150)) %>% 
  summary()

#signif of Age by Age
clm(model_formula, 
    data = liberal %>% filter(Age > 150)) %>% summary()


clm(model_formula, 
    data = liberal %>% filter(Age <= 150)) %>% summary()

nat %>% ggplot(aes(OffMainland, Rank_fac)) + geom_jitter()
reguni_south = get_usnews("reguni_south")
get_usnews("regcol_west") %>% count(OffMainland)

nat = nat %>% 
  mutate(SAT_scaled = scale(SAT))

nat %>% 
  ggplot(aes(SpecialPurpose, SAT)) + 
  geom_boxplot() +
  labs(subtitle = "Dataset: National.\n79 missing SAT Scores")

nat %>% 
  mutate(HBCU_Wom = ifelse(HBCU == "1", "HBCU",
                           ifelse(WOMENONLY == "1", "WOMENONLY",
                           "NEITHER")) %>% 
           factor(levels = c("NEITHER", "HBCU", "WOMENONLY"))) %>% 
  ggplot(aes(HBCU_Wom, SAT)) + 
  geom_boxplot() +
  labs(subtitle = "Dataset: National.\n79 missing SAT Scores")
  

nat %>% 
  filter(is.na(SAT)) %>% 
  count(SpecialPurpose)

nat %>% 
  mutate(sat_na = is.na(SAT)) %>% 
  count(sat_na)


sat_rank_fac = 
  nat %>% 
  ggplot(aes(SAT, Rank_fac)) + 
  geom_jitter(width = 0, height = .3)

sat_rank_num = 
  nat %>% 
  ggplot(aes(SAT, Rank_num)) + 
  geom_jitter(width = 0, height = .3)

grid.arrange(sat_rank_num, sat_rank_fac, nrow = 1)

#one really interesting thing is the shape of the curve with
#response Rank_fac and with Rank_num - when SAT gets only the lower end,
#the ranks of schools decrease much faster without. If we take SAT to be a
#good measure of school/student quality, this gives good evidence to our
#assumption that the difference in reputation/quality of a school for a 
#fixed changed in rank gets smaller in the high ranks
#we also notice that the slope of Rank_fac is much smoother, which gives
#good evidence that it tracks school "quality" more closely

nat %>% mutate(sat_is_na = is.na(SAT)) %>% 
  ggplot(aes(sat_is_na, Rank_num)) + geom_boxplot()
make_clm(nat, y_var, vars %>% c(., "SAT_scaled")) %>% summary()
clm("Rank_fac ~ SpecialPurpose + SAT_scaled", data = nat) %>% summary()
clm(model_formula %>% str_c(., " + SAT_scaled"), data = nat) %>% summary()

#we can compare the chanage in SP when there was no SAT
nat_clm %>% summary()
#The coef of SP actually totally flips direction, now becoming a positive
#effect on rank (and a signif one at that)

#set NA's to the 25th percentile SAT
SAT_25th = nat %>% select(SAT) %>% quantile(.25, na.rm = T)
nat = nat %>% mutate(SAT_fill = map_dbl(SAT, ~ifelse(is.na(.x), SAT_25th, .x)))
nat %>% ggplot(aes(SAT_fill, Rank_fac)) + geom_jitter(width = 0, height = .3)

clm("Rank_fac ~ SpecialPurpose + SAT_fill", data = nat) %>% 
  summary()
clm(model_formula %>% str_c(., " + SAT_fill"), data = nat) %>% summary()

clm(model_formula, 
    data = nat %>% 
      filter(Rank_num < 50) %>% 
      mutate(Rank_fac = ord_factor_maker(Rank_num, interval = 5))
    ) %>% summary()

clm(model_formula, 
    data = nat %>% 
      filter(Rank_num >= 50)
) %>% summary()

reguni_south = 
  get_all_usnews %>% 
  filter(list_name == "reguni_south") %>% 
  pull(school_data) %>% .[[1]]

regcol_south = 
  get_all_usnews %>% 
  filter(list_name == "regcol_south") %>% 
  pull(school_data) %>% .[[1]]


reguni_south %>% count(OffMainland, at_capital)
regcol_south %>% count(OffMainland, at_capital)

reguni_south %>% 
  ggplot(aes(x = OffMainland, y = Rank_fac, color = at_capital)) +
  geom_jitter(height = .2)
regcol_south %>% 
  ggplot(aes(x = OffMainland, y = Rank_fac, color = at_capital)) +
  geom_jitter(height = .2)
reguni_south %>% 
  filter(OffMainland==T) %>% 
  select(School, State_ab, at_capital, Rank)


regcol_south %>% 
  filter(OffMainland==T) %>% 
  select(School, State_ab, at_capital, Rank)

var_no_OM = vars %>% discard(~.x == "OffMainland")

clm(Rank_fac ~ at_capital + Age, data = reguni_south %>% filter(OffMainland == T)) %>% 
  summary()
make_clm(reguni_south %>% filter(OffMainland == F), y_var, var_no_OM) %>% 
  summary()
make_clm(regcol_south %>% filter(OffMainland == T), y_var, var_no_OM) %>% 
  summary()
make_clm(regcol_south %>% filter(OffMainland == F), y_var, var_no_OM) %>% 
  summary()

reguni_south %>% 
  filter(at_capital == T, OffMainland == F) %>% 
  select(School, Rank, City, State_ab)

south_states = 
  bind_rows(reguni_south %>% select(-Rank_fac), 
            regcol_south %>% select(-Rank_fac)) %>% 
  pull(State_ab) %>% 
  unique()

south_national = 
  nat %>% 
  filter(State_ab %in% south_states)

make_clm(south_national, y_var, vars) %>% summary()

liberal = 
  get_all_usnews %>% 
  filter(list_name == "liberal") %>% 
  pull(school_data) %>% .[[1]]

south_liberal = 
  liberal %>% 
  filter(State_ab %in% south_states)

make_clm(south_liberal, y_var, vars) %>% summary()

nat %>% 
  ggplot(aes(Age, log_rank, color = is_religious)) +
  geom_point()

lm(model_formula_lm, 
   data = nat %>% mutate(OffMainland = !(State_ab %in% c("DC", state.abb)))) %>% 
  summary()


