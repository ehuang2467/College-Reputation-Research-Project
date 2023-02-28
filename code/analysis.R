library(ordinal)
library(tidyverse)
source("code/fxs.R")
#I don't think this file does anything useful right now
args = commandArgs(trailingOnly=TRUE)
data_name = args[1]

usnews_data = get_usnews(data_name)

logit_factor = 1/1e10
logit = function(x) {
  m = max(x)
  log((x-1+logit_factor) / (m + logit_factor - x))
}

make_models = function(){
  usnews_data = usnews_data %>% 
    mutate(Rank_fac = Rank_num %>% ord_factor_maker())
  model_all_notrans = clm(model_formula, 
                         data = usnews_data %>% filter(Control != "For_profit")
  )
  cat("\nAll Schools\n")
  summary(model_all_notrans) %>% print()
  
  model_formula_con = model_formula %>% str_replace_all("Control \\+","")
  model_public_notrans = clm(model_formula_con, 
                         data = usnews_data %>% 
                           filter(Control == "Public")
  )
  cat("\nPublic Schools\n")
  summary(model_public_notrans) %>% print()
  
  model_private_notrans = clm(model_formula_con, 
                            data = usnews_data %>% 
                              filter(Control == "Private")
  )
  cat("\nPrivate Schools\n")
  summary(model_private_notrans) %>% print()
  return()
}
foo = make_models()

# model3 = lm(Rank_num ~ .,
#             data = usnews_data %>% 
#               filter(Control == "Public") %>% 
#               select(Age, Black_count:White_not_Hispanic_or_Latino_count,
#                      Rank_num, OffMainland, at_capital, Locale,
#                      SpecialPurpose)
# )
# 
# test = usnews_data %>% 
#   filter(Control == "Public") %>% 
#   group_by(State) %>% 
#   filter(n() > 3) %>% 
#   summarise(age_sig_level = lm(Rank_num ~ Age) %>% 
#             summary() %>% 
#             coefficients() %>% 
#             .["Age", "Pr(>|t|)"]) %>% 
#   mutate(significant = age_sig_level <= .05) %>% 
#   filter(significant == T)
# state_age = function(state_name){
#   lm(Rank_2022_num ~ Age, 
#      data = us_news_national %>% 
#        filter(State == state_name, 
#               Control == "Public")) %>% 
#     summary() %>% 
#     print()
#   us_news_national %>% 
#     filter(State == state_name, 
#            Control == "Public") %>% 
#     select(School, Age, Rank_2022_num) %>% 
#     arrange(desc(Age)) %>% 
#   print()
# }



