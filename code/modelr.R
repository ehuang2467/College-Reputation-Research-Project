library(tidyverse)
library(modelr)
source("code/fxs.R")
#First arg = dataset name
#Second arg = x axis
#Optional args: group=, add=

args_input = commandArgs(trailingOnly=TRUE)

if(args_input[1] == "all"){
  args_list = 
    c(data_names, "CC", "usnews_type") %>% 
    map(~c(.x, args_input[-1]))
  make_plots_bool = F
} else{
  args_list = 
    list(args_input)
  make_plots_bool = T
}

modelr_script = function(args, make_plots_bool){
  
  data_name = args[1]
  x_axis = args[2]
  cat(str_c("\n\nDataset is: ", data_name, "  \n"))
  #assign group_axis
  if(length(args) > 2 && str_detect(args[3], "^group=")){
    group_axis = args[3] %>% str_remove("^group=")
    args = args[-3]
  } else{
    group_axis = NA
  }
  
  
  #assign add_var (only one at a time)
  if(length(args) > 2 && sum (str_detect(args, "^add=")) == 1){
    add_var = args[str_detect(args, "^add=")] %>% str_remove("^add=")
    args[str_detect(args, "^add=")] = 
      args[str_detect(args, "^add=")] %>% str_remove("^add=")
  } else{
    add_var = NA
  }
  
  #assign effects
  if(length(args) > 2){
    effects = args[-c(1, 2)] %>% str_replace_all("power", "^")
  } else{
    effects = NA
  }
  
  if(!data_name %in% c("CC", "usnews_type")){
    school_data = get_usnews(data_name) %>% 
      select(all_of(c(y_var, vars))) %>% 
      filter(!is.na(y_var))
  } else{
    school_data = all_us_combined
    y_var = ifelse(data_name == "CC", "Type", "usnews_type")
    make_plots_bool = F
  }
  
  #I might change this if I turn unranked into a numeric
  if(effects %>% is.na() %>% all()){
    x_vars = vars
  } else{
    x_vars = c(vars, effects)
  }
  
  
  x_formula = ifelse(is.na(effects), 
                     vars %>% str_c(collapse = " + "), 
                     c(vars, effects) %>% str_c(collapse = " + "))
  
  make_plot = function(y_axis = "resid", x_axis, group_axis=NA, filename, model){
    if(make_plots_bool == F){
      return(NULL)
    }
    school_data = school_data %>% filter(Control != "For_profit") %>% 
      add_clm_res(., model)
    if(is.na(group_axis)){
      aes_list = aes(y = school_data %>% pull(all_of(y_axis)), 
                     x = school_data %>% pull(all_of(x_axis)))
    } else{
      aes_list = aes(y = school_data %>% pull(all_of(y_axis)),
                     x = school_data %>% pull(all_of(x_axis)), 
                     group = school_data %>% pull(all_of(group_axis)),
                     color = school_data %>% pull(all_of(group_axis)))
    }
    
    school_data %>% 
      ggplot(aes_list) + 
      geom_jitter(height = .25) + 
      geom_smooth(method = "lm", formula = y ~ x) +
      geom_hline(yintercept = 0, color = "yellow") +
      labs(x = x_axis, y = y_axis, color = group_axis)
    ggsave(filename = filename)
  }
  
  model_formula = x_formula %>% str_c(y_var, ., sep = " ~ ")
  
  if(is.na(add_var)){
    model_no_add = make_clm(school_data, y_var, vars)
    print(summary(model_no_add))
    
    make_plot(x_axis = x_axis, group_axis = group_axis, filename="modelr.jpg", 
              model = model_no_add)
  } else{
    model_before = make_clm(school_data, y_var, x_vars %>% discard(~.x == add_var))
    model_after = make_clm(school_data, y_var, x_vars)
    cat(str_c("added var is ", add_var, "\n"))
    print(anova(model_before, model_after))
    print(summary(model_after))
    
    make_plot(x_axis = x_axis, group_axis = group_axis, 
              filename="modelr_before.jpg", model = model_before)
    make_plot(x_axis = x_axis, group_axis = group_axis, 
              filename="modelr_after.jpg", model = model_after)
  }
  
}

null = args_list %>% map(~modelr_script(.x, make_plots_bool))



