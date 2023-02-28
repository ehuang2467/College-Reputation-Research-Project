library(modelr)
source("code/fxs.R")
library(gridExtra)

test_prop = .2
train_prop = 1-test_prop

usnews_data = get_all_usnews %>% 
  mutate(train_index = 
           map(school_data, 
                   ~sample((1:nrow(.x)), 
                           size = (nrow(.x)*train_prop) %>% floor())),
         test_index = map2(school_data, train_index,
                           ~discard(1:nrow(.x), function(z) z %in% .y)))
  
lm_formula = str_c(vars, collapse = " + ") %>% 
  str_c("log_rank", ., sep = " ~ ")

usnews_data = usnews_data %>% 
  mutate(clm = map2(school_data, train_index,
                    ~make_clm(.x[.y,], "Rank_fac", vars)),
         lm = map2(school_data, train_index, 
                   ~lm(lm_formula, data = .x[.y,])))

usnews_data = usnews_data %>% 
  mutate(data2 = map2(school_data, clm, add_clm_res),
         data2 = map2(data2, lm, ~add_predictions(.x, .y, var = "lm_pred")))


#converts raw ranks to factor ranks, based on the factors in variable "pred"
#if log = T, then numeric ranks are in log, and we reverse the log first
lmpred_to_facpred = function(data, log = T){
  lm_pred = data %>% pull("lm_pred")
  fac_pred = data %>% pull("pred")
  breaks = fac_pred %>% 
    na.omit() %>%
    unique() %>% 
    str_split(",") %>% 
    unlist() %>% 
    str_remove_all("[:punct:]|[:alpha:]") %>% 
    unique() %>% 
    as.numeric() %>% 
    sort()
  lm_pred[lm_pred < min(breaks)] = 1
  lm_pred[lm_pred > max(breaks)] = max(breaks)
  lm_pred_vec = cut(lm_pred, breaks = breaks)
  return_data = data %>% 
    mutate("lm_pred_fac" = lm_pred_vec)
  return(return_data)
}

usnews_data = usnews_data %>% 
  mutate(data2 = map2(data2, clm, 
                     ~.x %>% 
                       mutate(lm_pred = exp(lm_pred)) %>% 
                       lmpred_to_facpred() %>% 
                       mutate(lm_resid = fac_diff(lm_pred_fac, 
                                                  Rank_fac, 
                                                  .y["y.levels"] %>% .[[1]]))))

#usnews national lm predictions with strange ranks
usnews_data %>% 
  filter(list_name == "national") %>% 
  pull(data2) %>% .[[1]] %>% 
  select(School, lm_pred) %>% 
  filter(lm_pred < 1 | lm_pred > 443)
  
get_resids = function(big_tibble, type){
  if(type == "clm"){
    return(big_tibble %>% 
             pull(data2) %>% 
             map(~pull(.x, resid)) %>% 
             unlist())
  }
  else if (type == "lm"){
    return(big_tibble %>% 
             pull(data2) %>% 
             map(~pull(.x, lm_resid)) %>% 
             unlist())
  }
}

#0, 1 error
#clm
usnews_data %>% 
  get_resids("clm") %>% 
  map_dbl(~ifelse(.x == 0, 0, 1)) %>% 
  mean(na.rm = T)
#lm
usnews_data %>% 
  get_resids("lm") %>% 
  map_dbl(~ifelse(.x == 0, 0, 1)) %>% 
  mean(na.rm = T)

#mean resid error
#clm
usnews_data %>% 
  get_resids("clm") %>% 
  abs() %>% 
  mean(na.rm = T)
#lm
usnews_data %>% 
  get_resids("lm") %>% 
  abs() %>% 
  mean(na.rm = T)

sq = function(x) x^2
#mean square resid error
#clm
usnews_data %>% 
  get_resids("clm") %>% 
  sq() %>% 
  mean(na.rm = T)
#lm
usnews_data %>% 
  get_resids("lm") %>% 
  sq() %>% 
  mean(na.rm = T)

par(mfrow = c(1,2))
usnews_data %>% 
  filter(list_name == "national") %>% 
  pull(data2) %>% .[[1]] %>% 
  pull(resid) %>% 
  boxplot(., main = "clm", ylab = "resids")
usnews_data %>% 
  filter(list_name == "national") %>% 
  pull(data2) %>% .[[1]] %>% 
  pull(lm_resid) %>% 
  boxplot(., main = "lm", ylab = "resids")

nat_clm = usnews_data %>% 
  filter(list_name == "national") %>% 
  pull(clm) %>% .[[1]]
age_x = seq(50, 350, by = 10)
test_data = tibble(Age = age_x, 
                   Control = "Private",
                   sqrt_pop = sqrt(10),
                   SpecialPurpose = F,
                   Locale = "Large_City",
                   OffMainland = F,
                   is_religious = F,
                   at_capital=F)


#create plot of probabilities of each group for clm
#This illustrates the CLM probabilites
predict(nat_clm, newdata = test_data, type = "prob") %>% 
  .[["fit"]] %>% 
  cbind(age_x) %>% 
  as_tibble() %>% 
  pivot_longer(cols = -ncol(.), names_to = "fac", values_to = "prob") %>% 
  mutate(fac_min = fac %>% 
           str_extract("\\(.*,") %>% 
           str_extract("[:digit:]+") %>% 
           as.numeric() %>% 
           factor()) %>% 
  arrange(fac_min) %>% 
  ggplot(aes(x = age_x, y = prob, color = fac_min)) + 
  geom_smooth(method = "loess", formula = "y~x")
ggsave("plots/clm_probabilities.jpg", width = 5, height = 5)  

nat_clm_AgeControl = make_clm(get_usnews("national"), 
                              y_var,
                              vars %>% c(., "Age:Control"))
age_x = seq(50, 350, by = 10)
test_data_private = tibble(Age = age_x, 
                   Control = "Private",
                   sqrt_pop = sqrt(10),
                   SpecialPurpose = F,
                   Locale = "Large_City",
                   OffMainland = F,
                   is_religious = F,
                   at_capital=F)
test_data_public = tibble(Age = age_x, 
                           Control = "Public",
                           sqrt_pop = sqrt(10),
                           SpecialPurpose = F,
                           Locale = "Large_City",
                           OffMainland = F,
                           is_religious = F,
                           at_capital=F)


#This illustrates Public vs Private probs on 
#Age:Control interaction model, Nat
priv = predict(nat_clm_AgeControl, newdata = test_data_private, type = "prob") %>% 
  .[["fit"]] %>% 
  cbind(age_x) %>% 
  as_tibble() %>% 
  pivot_longer(cols = -ncol(.), names_to = "fac", values_to = "prob") %>% 
  mutate(fac_min = fac %>% 
           str_extract("\\(.*,") %>% 
           str_extract("[:digit:]+") %>% 
           as.numeric() %>% 
           factor()) %>% 
  arrange(fac_min) %>% 
  ggplot(aes(x = age_x, y = prob, color = fac_min)) + 
  geom_smooth(method = "loess", formula = "y~x") +
  ylim(0, .8) +
  labs(subtitle = "Private")

pub = predict(nat_clm_AgeControl, newdata = test_data_public, type = "prob") %>% 
  .[["fit"]] %>% 
  cbind(age_x) %>% 
  as_tibble() %>% 
  pivot_longer(cols = -ncol(.), names_to = "fac", values_to = "prob") %>% 
  mutate(fac_min = fac %>% 
           str_extract("\\(.*,") %>% 
           str_extract("[:digit:]+") %>% 
           as.numeric() %>% 
           factor()) %>% 
  arrange(fac_min) %>% 
  ggplot(aes(x = age_x, y = prob, color = fac_min)) + 
  geom_smooth(method = "loess", formula = "y~x") +
  ylim(0, .8) +
  labs(subtitle = "Public")

gridExtra::grid.arrange(priv, pub, nrow = 1, 
                        top = "Age:Control Interaction")


#This illustrates Public vs Private probs on no-interaction model, Nat
priv = predict(nat_clm, newdata = test_data_private, type = "prob") %>% 
  .[["fit"]] %>% 
  cbind(age_x) %>% 
  as_tibble() %>% 
  pivot_longer(cols = -ncol(.), names_to = "fac", values_to = "prob") %>% 
  mutate(fac_min = fac %>% 
           str_extract("\\(.*,") %>% 
           str_extract("[:digit:]+") %>% 
           as.numeric() %>% 
           factor()) %>% 
  arrange(fac_min) %>% 
  ggplot(aes(x = age_x, y = prob, color = fac_min)) + 
  geom_smooth(method = "loess", formula = "y~x") +
  ylim(0, .8) +
  labs(subtitle = "Private")

pub = predict(nat_clm, newdata = test_data_public, type = "prob") %>% 
  .[["fit"]] %>% 
  cbind(age_x) %>% 
  as_tibble() %>% 
  pivot_longer(cols = -ncol(.), names_to = "fac", values_to = "prob") %>% 
  mutate(fac_min = fac %>% 
           str_extract("\\(.*,") %>% 
           str_extract("[:digit:]+") %>% 
           as.numeric() %>% 
           factor()) %>% 
  arrange(fac_min) %>% 
  ggplot(aes(x = age_x, y = prob, color = fac_min)) + 
  geom_smooth(method = "loess", formula = "y~x") +
  ylim(0, .8) +
  labs(subtitle = "Public")

gridExtra::grid.arrange(priv, pub, nrow = 1, top = "No Interaction")

get_usnews("national") %>% 
  ggplot(aes(Age, Control, color = Control)) +
  geom_jitter(height = .2)

#This model shows that the trend of Public -> positive effect
#Public:Age -> Public lowers the positive effect of Age
#still holds even not considered the old schools
make_clm(get_usnews("national") %>% filter(Age < 200), 
         y_var,
         vars %>% c(., "Age:Control")) %>% 
  summary()




