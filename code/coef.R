source("code/fxs.R")
suppressPackageStartupMessages({library(broom); library(modelr)})
#Either First arg = print_coef to print the coefficients for the model
#and write it to a csv, and second arg is lm or clm to choose model
#or First arg = step to print out the steps for the model
#Second arg = lm or clm and 
#Third arg = list_name to step on
#optional arg: add=xxx, which adds xxx to vars
args = commandArgs(trailingOnly=TRUE)

add_index = args %>% str_detect("^add=")

add_vars = args[add_index] %>% str_remove("^add=")

vars_coef = c(vars, add_vars)

args = args[!add_index]


if(args[2] == "clm"){
  usnews = get_all_usnews %>% 
    mutate(clm_model = map2(school_data, list_name, 
                           ~make_clm(.x, "Rank_fac", vars_coef, name = .y)),
           n = map_dbl(school_data, ~nrow(.x)))
  
} else if(args[2] == "lm"){
  usnews = get_all_usnews %>% 
    mutate(clm_model = map(school_data, ~lm(model_formula_lm, data = .x)),
           n = map_dbl(school_data, ~nrow(.x)))
}

if(args[1] == "print_coef"){
  usnews_coef = usnews %>% 
    mutate(coef = map(clm_model, ~ .x %>% tidy() %>% pull("estimate") %>% round(4)),
           pval = map(clm_model, ~ .x %>% tidy() %>% pull("p.value") %>% round(2)),
           coef_names = map(clm_model, ~ .x %>% tidy() %>% pull("term")))
  
  usnews_coef = usnews_coef %>% 
    unnest(cols = c(coef, coef_names, pval)) %>% 
    select(-c(school_data, clm_model)) %>% 
    pivot_wider(names_from = coef_names, 
                values_from = c(coef, pval), 
                names_glue = "{coef_names}_{.value}") %>% 
    select(sort(colnames(.))) %>% 
    select(list_name, n, everything()) %>% 
    select(-contains("]"))

  
  # usnews_coef %>%
  #   print(width = Inf)
  return(usnews_coef)
  write_csv(usnews_coef, str_c("plots/usnews_coef_", args[2], ".csv"))
  
} else if(args[1] == "step"){
  print("Step")
  usnews = usnews %>% filter(list_name == args[3]) %>% 
    mutate(step_model = map(school_data, function(sch_data){
      #step doesn't work with make_clm for some reason
      # first_model = make_clm(sch_data, y_var, vars_coef)
      if(args[2] == "clm"){
        first_model = clm(model_formula, data = sch_data)
        step_model = step(first_model, 
                          trace = 0, 
                          scope = as.formula("Rank_fac ~ (.)^2"),
                          direction = "forward")
        print(step_model)
        added_step_vars = step_model[["formula"]] %>% 
          as.character() %>% .[3] %>% 
          str_split(" \\+ ") %>% .[[1]] %>% 
          discard(~.x %in% vars_coef)
        print(added_step_vars)
        write_lines(added_step_vars,
                   str_c("plots/step_", args[3], "_clm.txt"))
        return(step_model)
      } else if (args[2] == "lm"){
        #lm step doesn't work for some reason
        #seems to do with the fact that the data frame isn't found during the step call?
        print(class(sch_data))
        print(model_formula_lm)
        first_model = lm(model_formula_lm, data = as.data.frame(sch_data))
        print(first_model)
        browser()
        step_model = step(first_model,
                          trace = 1,
                          scope = as.formula("log_rank ~ (.)^2"),
                          direction = "forward")
      }

      
      return(step_model)
    }),
    step_model = map(step_model, broom::tidy))
}

# test = usnews[7,] %>% pull(school_data) %>% .[[1]] 
# lm(model_formula %>% str_remove("\\+ Control"), data = test %>% filter(State_ab != "DC", Control =="Private")) %>% 
#   summary()


#for us news coeff, want to take note of differences in both
#coef sign and signif, and possibly size
