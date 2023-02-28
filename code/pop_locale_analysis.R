source("code/fxs.R")
library(modelr)
library(gridExtra)

nat = get_usnews("national")

nat_state_prop = 
  nat %>% 
  count(State_ab) %>% 
  mutate(state_prop = n/nrow(nat)) %>% 
  rename(state_n = n)

nat = 
  nat %>% 
  left_join(nat_state_prop, by = "State_ab") 

nat = 
  nat %>% 
  mutate(State_group = ifelse(state_n <= 3, "Small_state", State_ab))

nat_clmm = clmm(model_formula %>% str_replace("sqrt_pop", "(1|State_ab)"), 
                data = nat)

nat_clm = clm(model_formula, data = nat)

nat_clm_states = make_clm(nat, 
                          y_var, 
                          vars %>% str_replace("sqrt_pop", "State_group"))

nat_clm %>% summary()
nat_clm_states %>% summary()

anova(nat_clm, nat_clm_states)
anova(nat_clmm, nat_clm_states)

nat_clm_states_AgeControl = make_clm(nat, 
                          y_var, 
                          vars %>% 
                            str_replace("sqrt_pop", "State_group") %>% 
                            c(., "Age:Control"),
                          force = T)

nat_clm_states_AgeControl %>% summary()

anova(nat_clm_states_AgeControl, nat_clm_states)

anova(nat_clmm, nat_clm_states)

nat %>% 
  filter(!is.na(log_rank)) %>% 
  add_residuals(nat_lm) %>% 
  ggplot(aes(sqrt_pop, resid, color = Locale, group = Locale)) +
  geom_jitter(width = .05) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", formula = "y~x")


#Try to make a State:Locale interaction, doesn't work well
#maybe try clmm or something else?
nat_state_locale_count = 
  nat %>% 
  count(State_ab, Locale) %>% 
  pivot_wider(id_cols = State_ab, 
              names_from = Locale, 
              names_prefix = "count_",
              values_from = n,
              values_fill = 0) %>% 
  rename(count_Rural_Town = "count_Rural/Town")

nat = 
  nat %>% 
  left_join(nat_state_locale_count, by = "State_ab") 

nat = 
  nat %>% 
  mutate(State_group2 = 
           ifelse(state_n <= 5 | 
                    count_Large_City <= 3 | 
                    count_Small_City <= 3 |
                    count_Rural_Town <= 2
                    , "Small_state", State_ab))

nat_clm_state_locale_interaction = make_clm(nat, y_var,
                                            vars %>% 
                                              discard(~.x == "sqrt_pop") %>% 
                                              c(., "State_group2", "State_group2:Locale"),
                                            force = T)

nat_clm_state_locale_interaction %>% summary()

anova(nat_clm_states, nat_clm_state_locale_interaction)

# nat_clmm_state_locale_int = 
#   clmm(model_formula %>% 
#          str_replace("sqrt_pop", "(1|State_group) + (Locale|State_group)"),
#      data = nat)
# 
# anova(nat_clmm_state_locale_int, nat_clm_states)



make_clm(nat %>% filter(Age > 200),
         y_var,
         vars %>% c(., "sqrt_pop:Locale")) %>% 
  summary()

nat_clm_AgeLE200_popLocale = make_clm(nat %>% filter(Age <= 200),
                                      y_var,
                                      vars %>% c(., "sqrt_pop:Locale"))
nat_clm_AgeLE200 = make_clm(nat %>% filter(Age <= 200),
                            y_var,
                            vars)

anova(nat_clm_AgeLE200_popLocale, nat_clm_AgeLE200)




nat_clm_AgeGT200_popLocale = make_clm(nat %>% filter(Age > 200),
                                      y_var,
                                      vars %>% c(., "sqrt_pop:Locale"))
nat_clm_AgeGT200 = make_clm(nat %>% filter(Age > 200),
                            y_var,
                            vars)

anova(nat_clm_AgeGT200_popLocale, nat_clm_AgeGT200)


nat_popLocale_popGT2 = 
  nat %>% 
  filter(!is.na(Rank_fac), sqrt_pop < 6) %>% 
  ggplot(aes(sqrt_pop, Rank_fac, group = Locale, color = Locale)) +
  geom_jitter(height = .2) +
  geom_smooth(method = "lm", formula = "y ~ x")

nat_popLocale = 
  nat %>% 
  filter(!is.na(Rank_fac)) %>% 
  ggplot(aes(sqrt_pop, Rank_fac, group = Locale, color = Locale)) +
  geom_jitter(height = .2) +
  geom_smooth(method = "loess", formula = "y ~ x")

grid.arrange(nat_popLocale, nat_popLocale_popGT2, nrow = 1)


nat_lm = lm(model_formula_lm, data = nat)

nat %>% 
  add_residuals(nat_lm) %>% 
  ggplot(aes(sqrt_pop, resid, group = Locale, color = Locale)) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x")

nat_lm_StateGroup = 
  lm(model_formula_lm %>% str_replace("sqrt_pop", "State_group"),
     data = nat)

nat %>% 
  add_residuals(nat_lm_StateGroup) %>% 
  ggplot(aes(sqrt_pop, resid, group = Locale, color = Locale)) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x")

nat %>% 
  add_clm_res(nat_clm_states) %>% 
  ggplot(aes(sqrt_pop, resid, group = Locale, color = Locale)) +
  geom_jitter(height = .2) +
  geom_smooth(method = "lm", formula = "y ~ x")

nat %>% 
  add_clm_res(nat_clm) %>% 
  ggplot(aes(sqrt_pop, resid, group = Locale, color = Locale)) +
  geom_jitter(height = .2) +
  geom_smooth(method = "lm", formula = "y ~ x")

nat_clm_State_popLocale = 
  nat %>% 
  make_clm(y_var, 
           vars %>% 
             discard(~.x == "sqrt_pop") %>% 
             c(., "State_group", "Locale:sqrt_pop"),
           force = T)
anova(nat_clm_State_popLocale, nat_clm_states)

nat %>% 
  add_residuals(nat_lm) %>% 
  ggplot(aes(sqrt_pop, resid, group = Locale, color = Locale)) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x")


nat_clm_popGT1p75_popLocale =
  make_clm(nat %>% filter(sqrt_pop >= 1.75),
           y_var,
           vars %>% c(., "sqrt_pop:Locale"),
           force = T)

nat_clm_popGT1p75 =
  make_clm(nat %>% filter(sqrt_pop >= 1.75),
           y_var,
           vars %>% c(.),
           force = T)
anova(nat_clm_popGT1p75_popLocale, nat_clm_popGT1p75)

nat %>% filter(sqrt_pop > 1.75, !is.na(Rank_fac)) %>% 
  ggplot(aes(sqrt_pop, Rank_fac, color = Locale, group = Locale)) +
  geom_jitter(height = .3, width = .3) +
  geom_smooth(method = "loess")

nat %>% 
  arrange(sqrt_pop) %>% 
  select(State_ab, sqrt_pop) %>% 
  unique() %>% 
  print(n = 20)


nat_clm_AgeLE200_RankLT360_popLocale =
  make_clm(nat %>% filter(Age <= 200, Rank_num < 360),
           y_var,
           vars %>% c(., "sqrt_pop:Locale"))

nat_clm_AgeLE200_RankLT360 =
  make_clm(nat %>% filter(Age <= 200, Rank_num < 360),
           y_var,
           vars)
anova(nat_clm_AgeLE200_RankLT360_popLocale, nat_clm_AgeLE200_RankLT360)

all_us_combined %>% 
  filter(usnews_type == "reguni") %>% 
  nrow() %>% magrittr::divide_by(4)

all_popLocale = 
  all_us_combined %>% 
  mutate(Rank_num = ifelse(usnews_type == "reguni", Rank_num + 440, Rank_num),
         Rank_num = ifelse(usnews_type == "regcol", Rank_num + 440 + 151, Rank_num)) %>% 
  filter(usnews_type %in% c("national", "reguni", "regcol"), !is.na(Rank_num)) %>% 
  mutate(Rank_fac = ord_factor_maker(Rank_num)) %>% 
  make_clm(y_var, vars %>% c(., "sqrt_pop:Locale"))

all_no_popLocale = 
  all_us_combined %>% 
  mutate(Rank_num = ifelse(usnews_type == "reguni", Rank_num + 440, Rank_num),
         Rank_num = ifelse(usnews_type == "regcol", Rank_num + 440 + 151, Rank_num)) %>% 
  filter(usnews_type %in% c("national", "reguni", "regcol"), !is.na(Rank_num)) %>% 
  mutate(Rank_fac = ord_factor_maker(Rank_num)) %>% 
  make_clm(y_var, vars)

anova(all_popLocale, all_no_popLocale)

all_popLocale =
  all_us_combined %>% 
  mutate(Rank_num = ifelse(usnews_type == "reguni", Rank_num + 440, Rank_num),
         Rank_num = ifelse(usnews_type == "regcol", Rank_num + 440 + 151, Rank_num)) %>% 
  filter(usnews_type %in% c("national", "reguni", "regcol"), !is.na(Rank_num)) %>% 
  mutate(Rank_fac = ord_factor_maker(Rank_num)) %>% 
  ggplot(aes(sqrt_pop, Rank_fac, group = Locale, color = Locale)) +
  geom_jitter(height = .3) +
  geom_smooth(method = "lm", formula = "y ~ x")

nat_popLocale = 
  nat %>% 
  filter(!is.na(Rank_fac)) %>% 
  ggplot(aes(sqrt_pop, Rank_fac, group = Locale, color = Locale)) +
  geom_jitter(height = .2) +
  geom_smooth(method = "lm", formula = "y ~ x")

grid.arrange(nat_popLocale, all_popLocale, nrow = 1)

nat %>% 
  filter(Control == "Private") %>% 
  make_clm(y_var, vars %>% c(., "sqrt_pop:Locale")) %>% 
  summary()

nat %>% 
  filter(Control == "Public") %>% 
  make_clm(y_var, vars %>% c(., "sqrt_pop:Locale")) %>% 
  summary()

nat %>% 
  filter(Control == "Private", Age <= 200) %>% 
  ggplot(aes(sqrt_pop, Rank_fac, group = Locale, color = Locale)) +
  geom_jitter(height = .2) +
  geom_smooth(method = "lm", formula = "y ~ x")

a = nat %>% 
  filter(Control == "Private", Age <= 200) %>% 
  make_clm(y_var, vars)
b = nat %>% 
  filter(Control == "Private", Age <= 200) %>% 
  make_clm(y_var, vars %>% c(., "sqrt_pop:Locale"))
anova(a, b)

make_clm(nat %>% filter(sqrt_pop >= 1.75, sqrt_pop < 6),
         y_var,
         vars %>% c(., "sqrt_pop:Locale")) %>% 
  summary()
